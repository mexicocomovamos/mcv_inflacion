#------------------------------------------------------------------------------#
# Proyecto:   MICROSITIO DE INFLACIÓN
# Objetivo:   Migración de las visualizaciones de incidencia de genéricos al
#             flujo alternativo (XLSX descargados del portal INEGI), en
#             sustitución del loop `inegi_series(...)` de
#             01_03_01_inflación_incidencia_monitoreo.R que quedó roto con el
#             cambio del API.
#
# Insumos:
#   - 01_datos_crudos/INPC_mensual_indices.xlsx     (si v_quincena == 2)
#   - 01_datos_crudos/INPC_quincenal_indices.xlsx   (si v_quincena == 1)
#   - 01_datos_crudos/01_03_inpc_complete_NewVersion.xlsx
#   - 04_infobites/00_plantillas/01_inegi_long.pdf  (fondo de la gráfica)
#
# Salidas (en 04_infobites/):
#   - 01_01_incidencia_{mensual|quincenal}.png
#   - 01_02_incidencia_anual.png
#   - 99_svg/01_03_03_01_01_incidencia_{mensual|quincenal}.svg
#------------------------------------------------------------------------------#

# 0. Configuración -------------------------------------------------------------
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(ggrepel)
library(ggimage)
library(extrafont)

loadfonts(device = "pdf")
loadfonts(device = "postscript")

####################################################
# Seleccionar corrida: 1 = primera quincena, 2 = mensual
v_quincena <- 2
####################################################

# Factor de encadenamiento del INPC general (base 2ª quincena de julio 2018).
f_encad_general <- 1.3609522607803

# Cortes de validez por la actualización INEGI 2024 (ver 01_03_05_incidencia_ALT.R).
fecha_corte_ponderadores <- as.Date("2024-07-01")
if (v_quincena == 2) {
  fecha_inicio_incidencia_periodo <- as.Date("2024-08-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-07-01")
} else {
  fecha_inicio_incidencia_periodo <- as.Date("2024-09-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-08-01")
}

paste_inp  <- function(x) paste0("01_datos_crudos/", x)
paste_info <- function(x) paste0("04_infobites/",    x)

# Paleta MCV (idéntica al script legado)
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")

# Abreviador de etiquetas largas (idéntico al legado)
str_wrap_long <- function(stringr, width = 40, string_limit = 40) {
  ifelse(
    nchar(stringr) > 40,
    str_wrap(paste0(substr(stringr, 1, 40 - 5), "[...]"), width),
    str_wrap(stringr, width)
  )
}

# 1. Datos de incidencia (flujo alternativo) ----------------------------------
archivo_indices <- if (v_quincena == 2) {
  paste_inp("INPC_mensual_indices.xlsx")
} else {
  paste_inp("INPC_quincenal_indices.xlsx")
}

inpc_indices <- read_excel(archivo_indices) %>%
  mutate(
    nombre = str_to_lower(nombre),
    valor  = as.numeric(valor),
    date   = as.Date(date)
  )

if (v_quincena == 1) {
  inpc_indices <- inpc_indices %>% filter(str_detect(fecha, "1Q"))
}

catalogo <- read_excel(paste_inp("01_03_inpc_complete_NewVersion.xlsx")) %>%
  mutate(
    nombre     = str_to_lower(ccif),
    code       = id_ccif_4,
    ponderador = ponderador_inpc_id_ccif_4,
    has_values = !is.na(ponderador) & !is.na(encadenamiento)
  ) %>%
  arrange(nombre, code, desc(has_values)) %>%
  group_by(nombre, code) %>%
  slice(1) %>%
  ungroup() %>%
  filter(!is.na(ponderador), !is.na(encadenamiento), !is.na(code)) %>%
  select(nombre, code, ccif_original = ccif, id_ccif_0, ponderador, encadenamiento)

inpc_general <- inpc_indices %>%
  filter(nombre == "índice general") %>%
  arrange(date) %>%
  transmute(date, inpc = valor, inpc_a = valor / f_encad_general)

lag_periodo <- 1
lag_anual   <- 12

incidencias <- inpc_indices %>%
  filter(nombre != "índice general") %>%
  inner_join(catalogo, by = c("nombre", "code")) %>%
  left_join(inpc_general, by = "date") %>%
  arrange(nombre, date) %>%
  group_by(nombre) %>%
  mutate(
    values_a           = valor / encadenamiento,
    var_periodo        = (valor    - lag(valor,    lag_periodo)) / lag(valor,    lag_periodo),
    var_anual          = (valor    - lag(valor,    lag_anual))   / lag(valor,    lag_anual),
    incidencia_periodo = ((values_a - lag(values_a, lag_periodo)) / lag(inpc_a, lag_periodo)) * ponderador,
    incidencia_anual   = ((values_a - lag(values_a, lag_anual))   / lag(inpc_a, lag_anual))   * ponderador
  ) %>%
  ungroup() %>%
  mutate(
    incidencia_periodo = if_else(date >= fecha_inicio_incidencia_periodo, incidencia_periodo, NA_real_),
    incidencia_anual   = if_else(date >= fecha_inicio_incidencia_anual,   incidencia_anual,   NA_real_)
  ) %>%
  filter(date >= fecha_corte_ponderadores)

# 2. Adaptación a los nombres que esperan los bloques ggplot del legado --------
# Renombramos para no tocar el código de visualización.
ultimo <- incidencias %>% filter(date == max(date))

if (v_quincena == 1) {
  d_incidencia_prods_last <- ultimo %>%
    transmute(
      fecha = date, id_ccif_0, ccif = ccif_original,
      var_quincenal = var_periodo,
      incidencia_quincenal = incidencia_periodo
    ) %>%
    arrange(desc(incidencia_quincenal)) %>%
    mutate(n = row_number())

  d_incidencia_prods_last_20 <- d_incidencia_prods_last %>%
    filter(!is.na(var_quincenal), !is.na(incidencia_quincenal)) %>%
    filter(n <= 10 | n > nrow(.) - 10)
} else {
  d_incidencia_prods_last <- ultimo %>%
    transmute(
      fecha = date, id_ccif_0, ccif = ccif_original,
      var_mensual = var_periodo,
      incidencia_mensual = incidencia_periodo
    ) %>%
    arrange(desc(incidencia_mensual)) %>%
    mutate(n = row_number())

  d_incidencia_prods_last_20 <- d_incidencia_prods_last %>%
    filter(!is.na(var_mensual), !is.na(incidencia_mensual)) %>%
    filter(n <= 10 | n > nrow(.) - 10)
}

d_incidencia_anual_prods_last <- ultimo %>%
  transmute(fecha = date, id_ccif_0, ccif = ccif_original, var_anual, incidencia_anual) %>%
  filter(!is.na(incidencia_anual)) %>%
  arrange(desc(incidencia_anual)) %>%
  mutate(n = row_number())

d_incidencia_anual_prods_last_20 <- d_incidencia_anual_prods_last %>%
  filter(!is.na(var_anual)) %>%
  filter(n <= 10 | n > nrow(.) - 10)

# 3. Gráfica 01_01: Incidencia mensual/quincenal -------------------------------
## (bloque ggplot copiado de 01_03_01_inflación_incidencia_monitoreo.R)

titulo <- if (v_quincena == 1) {
  "Genéricos con mayor y\nmenor incidencia quincenal"
} else {
  "Genéricos con mayor y\nmenor incidencia mensual"
}

subtitulo <- if (v_quincena == 1) {
  paste0(
    "1ª quincena de ",
    as.character(month(d_incidencia_prods_last$fecha[1], label = TRUE, abbr = FALSE)), " ",
    as.character(year(d_incidencia_prods_last$fecha[1])),
    " | Entre corchetes se indica la variación quincenal."
  )
} else {
  paste0(
    str_to_sentence(as.character(month(d_incidencia_prods_last$fecha[1], label = TRUE, abbr = FALSE))),
    " ",
    as.character(year(d_incidencia_prods_last$fecha[1])),
    " | Entre corchetes se indica la variación mensual."
  )
}

nota <- if (v_quincena == 1) {
  "La incidencia quincenal es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
} else {
  "La incidencia mensual es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
}

if (v_quincena == 1) {
  g <- ggplot(
    d_incidencia_prods_last_20,
    aes(
      y = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia_quincenal),
      x = incidencia_quincenal,
      fill = ifelse(n <= 10, "1", "2"),
      label = paste0(
        round(incidencia_quincenal, 3), "\n[", round(var_quincenal * 100, 2), "%]"
      )
    )
  ) +
    geom_col() +
    geom_text(
      hjust = case_when(
        between(d_incidencia_prods_last_20$incidencia_quincenal, 0, 0.05)  ~ -0.1,
        between(d_incidencia_prods_last_20$incidencia_quincenal, -0.05, 0) ~  1.1,
        d_incidencia_prods_last_20$incidencia_quincenal < -0.05            ~ -0.1,
        d_incidencia_prods_last_20$incidencia_quincenal >  0.05            ~  1.1
      ),
      family = "Ubuntu", size = 4, fontface = "bold"
    )
} else {
  g <- ggplot(
    d_incidencia_prods_last_20,
    aes(
      y = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia_mensual),
      x = incidencia_mensual,
      fill = ifelse(n <= 10, "1", "2"),
      label = paste0(
        round(incidencia_mensual, 3), "\n[", round(var_mensual * 100, 2), "%]"
      )
    )
  ) +
    geom_col() +
    geom_text(
      hjust = case_when(
        between(d_incidencia_prods_last_20$incidencia_mensual, 0, 0.05)  ~ -0.1,
        between(d_incidencia_prods_last_20$incidencia_mensual, -0.05, 0) ~  1.1,
        d_incidencia_prods_last_20$incidencia_mensual < -0.05            ~ -0.1,
        d_incidencia_prods_last_20$incidencia_mensual >  0.05            ~  1.1
      ),
      family = "Ubuntu", size = 4, fontface = "bold"
    )
}

g <- g +
  scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.1),
    expand = expansion(c(0.15, 0.15))
  ) +
  labs(
    title    = titulo,
    subtitle = str_wrap(subtitulo, 40),
    caption  = str_wrap(nota, 70)
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
    plot.subtitle    = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.margin      = margin(0.4, 0.4, 2, 0.4, "cm"),
    plot.caption     = element_text(size = 15),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.title.y     = element_blank(),
    axis.title.x     = element_blank(),
    axis.text.x      = element_text(size = 20),
    axis.text.y      = element_text(size = 15),
    text             = element_text(family = "Ubuntu"),
    legend.position  = "none"
  )

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi_long.pdf"))

archivo_periodo <- if (v_quincena == 1) {
  paste_info("01_01_incidencia_quincenal.png")
} else {
  paste_info("01_01_incidencia_mensual.png")
}

ggsave(g, filename = archivo_periodo,
       width = 10, height = 15, dpi = 200, bg = "transparent")

# SVG para la versión traducida
archivo_svg <- if (v_quincena == 1) {
  paste_info("99_svg/01_03_03_01_01_incidencia_quincenal.svg")
} else {
  paste_info("99_svg/01_03_03_01_01_incidencia_mensual.svg")
}

ggsave(g, filename = archivo_svg,
       width = 10, height = 15, dpi = 200, bg = "transparent")

# 4. Gráfica 01_02: Incidencia anual -------------------------------------------

if (nrow(d_incidencia_anual_prods_last_20) == 0) {
  # Antes de jul-2025 (mensual) / ago-2025 (1Q), la incidencia anual queda en
  # blanco por diseño. Evitamos escribir un PNG vacío.
  message(
    "No hay incidencia anual válida para la fecha de corte (",
    max(incidencias$date),
    "). Primer corte válido: ", fecha_inicio_incidencia_anual,
    ". Se omite 01_02_incidencia_anual.png."
  )
} else {

  titulo <- "Genéricos con mayor y\nmenor incidencia anual"
  nota   <- "La incidencia anual es la contribución en puntos porcentuales que cada genérico aporta a la inflación general"

  subtitulo <- if (v_quincena == 1) {
    paste0(
      "1ª quincena de ",
      as.character(month(d_incidencia_prods_last$fecha[1], label = TRUE, abbr = FALSE)), " ",
      as.character(year(d_incidencia_prods_last$fecha[1])),
      " | Entre corchetes se indica la variación anual."
    )
  } else {
    paste0(
      str_to_sentence(as.character(month(d_incidencia_prods_last$fecha[1], label = TRUE, abbr = FALSE))),
      " ",
      as.character(year(d_incidencia_prods_last$fecha[1])),
      " | Entre corchetes se indica la variación anual."
    )
  }

  g <- ggplot(
    d_incidencia_anual_prods_last_20,
    aes(
      y = reorder(str_wrap_long(stringr = ccif, width = 25), incidencia_anual),
      x = incidencia_anual,
      fill = ifelse(n <= 10, "1", "2"),
      label = paste0(
        round(incidencia_anual, 3), "\n[", round(var_anual * 100, 2), "%]"
      )
    )
  ) +
    geom_col() +
    geom_text(
      hjust = case_when(
        between(d_incidencia_anual_prods_last_20$incidencia_anual, 0, 0.1)  ~ -0.1,
        between(d_incidencia_anual_prods_last_20$incidencia_anual, -0.1, 0) ~  1.1,
        d_incidencia_anual_prods_last_20$incidencia_anual < -0.1            ~ -0.1,
        d_incidencia_anual_prods_last_20$incidencia_anual >  0.1            ~  1.1
      ),
      family = "Ubuntu", size = 4, fontface = "bold"
    ) +
    scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
    scale_x_continuous(
      labels = scales::number_format(accuracy = 0.1),
      expand = expansion(c(0.15, 0.15))
    ) +
    labs(
      title    = titulo,
      subtitle = str_wrap(subtitulo, 40),
      caption  = str_wrap(nota, 70)
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
      plot.subtitle    = element_text(size = 30, colour = "#777777", hjust = 0.5),
      plot.margin      = margin(0.4, 0.4, 2, 0.4, "cm"),
      plot.caption     = element_text(size = 15),
      panel.background = element_rect(fill = "transparent", colour = NA),
      axis.title.y     = element_blank(),
      axis.title.x     = element_blank(),
      axis.text.x      = element_text(size = 20),
      axis.text.y      = element_text(size = 15),
      text             = element_text(family = "Ubuntu"),
      legend.position  = "none"
    )

  g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi_long.pdf"))

  ggsave(g, filename = paste_info("01_02_incidencia_anual.png"),
         width = 10, height = 15, dpi = 200, bg = "transparent")
}

print(paste0("Gráficas generadas en ", paste_info("")))
