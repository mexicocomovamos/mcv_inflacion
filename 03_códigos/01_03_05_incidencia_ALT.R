#------------------------------------------------------------------------------#
# Proyecto:   MICROSITIO DE INFLACIÓN
# Objetivo:   Cálculo y visualización de incidencias de genéricos sobre la
#             inflación general, usando la fuente alternativa (XLSX
#             descargados del portal INEGI) en sustitución del loop
#             `inegi_series(...)` roto con el cambio del API.
#
#             Este script fusiona el pipeline de incidencias con las
#             visualizaciones que antes vivían en dos archivos separados.
#
# Insumos:
#   - 01_datos_crudos/INPC_mensual_indices.xlsx     (si v_quincena == 2)
#   - 01_datos_crudos/INPC_quincenal_indices.xlsx   (si v_quincena == 1)
#   - 01_datos_crudos/01_03_inpc_complete_NewVersion.xlsx
#   - 04_infobites/00_plantillas/01_inegi_long.pdf        (fondo genéricos)
#   - 04_infobites/00_plantillas/02_plantilla_SIE_BANXICO.pdf (fondo Banxico)
#   - SIE de Banxico (secciones 12-14): cuadros CP154/CP155
#
# Salidas:
#   - 02_datos_limpios/incidencia_generica_{mensual|quincenal}.xlsx
#       · panel_completo       — panel histórico con variaciones e incidencias
#       · top_bottom_periodo   — 10 con mayor y 10 con menor incidencia del
#                                último corte
#       · top_bottom_anual     — 10 con mayor y 10 con menor incidencia anual
#       · validacion           — suma de incidencias del último corte
#   - 04_infobites/01_01_incidencia_{mensual|quincenal}.png
#   - 04_infobites/01_02_incidencia_anual.png
#   - 04_infobites/99_svg/01_03_03_01_01_incidencia_{mensual|quincenal}.svg
#   - 04_infobites/01_04_incidencia_anual_componente_{mensual|quincenal}.png
#   - 04_infobites/01_05_incidencia_anual_concepto_{mensual|quincenal}.png
#   - 04_infobites/01_06_tweet_incidencia_{mensual|quincenal}.html
#   - 05_aplicacion/total_datos_inflacion_{mes|quincenas}.rds
#------------------------------------------------------------------------------#

# 0. Configuración -------------------------------------------------------------
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
library(scales)
library(ggrepel)
library(ggimage)
library(extrafont)
library(httr)         # para secciones 12-14 (SIE Banxico)
library(jsonlite)     # para secciones 12-14 (SIE Banxico)

loadfonts(device = "pdf")
loadfonts(device = "postscript")

####################################################
# Seleccionar corrida: 1 = primera quincena, 2 = mensual
v_quincena <- 1

# Fecha de inicio del eje X para las gráficas de componente y concepto
# (secciones 13-14). Por defecto se fija en agosto de 2024 (primer mes
# completo bajo la actualización INEGI de ponderadores 2024, que entró en
# vigor en la 2ª quincena de julio de 2024).
fecha_inicio_grafica <- as.Date("2024-08-01")
####################################################

# Token personal del SIE de Banxico (definido en 00_token.R)
source("03_códigos/00_token.R")   # trae v_token_banxico

# Factor de encadenamiento del INPC general (base 2ª quincena de julio 2018).
# Actualizar si INEGI cambia la base del INPC.
f_encad_general <- 1.3609522607803

# Fechas de corte de los nuevos ponderadores (Actualización INEGI 2024; entrada
# en vigor 2ª quincena de julio de 2024). Los ponderadores del catálogo
# NewVersion son los nuevos, por lo que:
#   - `fecha_corte_ponderadores` marca desde cuándo los índices quedan
#     reportados con los nuevos pesos → filtramos el output a partir de aquí.
#   - `fecha_inicio_incidencia_periodo` es el primer corte cuyo lag(1) ya cae
#     dentro del nuevo régimen.
#   - `fecha_inicio_incidencia_anual` es el primer corte cuyo lag(12) cae ya
#     dentro del nuevo régimen.
# Antes de cada una de esas fechas, la incidencia correspondiente mezcla
# estructuras y debe quedar en blanco (NA).
fecha_corte_ponderadores <- as.Date("2024-07-01")

if (v_quincena == 2) {
  fecha_inicio_incidencia_periodo <- as.Date("2024-08-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-07-01")
} else {
  fecha_inicio_incidencia_periodo <- as.Date("2024-09-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-08-01")
}

paste_inp  <- function(x) paste0("01_datos_crudos/",  x)
paste_out  <- function(x) paste0("02_datos_limpios/", x)
paste_info <- function(x) paste0("04_infobites/",     x)
paste_app  <- function(x) paste0("05_aplicacion/",    x)

# Paleta MCV (idéntica al script legado)
mcv_semaforo    <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_morados     <- c("#6950D8", "#A99BE9")
mcv_blacks      <- c("black", "#D2D0CD", "#777777")
mcv_discrete_7  <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D", "#E8B32E", "#0A93C4", "#974DF0")
mcv_discrete_12 <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D",
                     "#C6B2E3", "#E8B32E", "#0A93C4", "#974DF0",
                     "#00D2D1", "#FF43FA", mcv_blacks[3], mcv_blacks[2])

# Abreviador de etiquetas largas (idéntico al legado)
str_wrap_long <- function(stringr, width = 40, string_limit = 40) {
  ifelse(
    nchar(stringr) > 40,
    str_wrap(paste0(substr(stringr, 1, 40 - 5), "[...]"), width),
    str_wrap(stringr, width)
  )
}

# 1. Índices de genéricos (fuente alternativa) ---------------------------------
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

# Corrida quincenal: nos quedamos solo con la 1Q de cada mes y la tratamos como
# observación mensual (convención de 01_03_02_MONITOREO_ALT.R).
if (v_quincena == 1) {
  inpc_indices <- inpc_indices %>% filter(str_detect(fecha, "1Q"))
}

# 2. Catálogo de ponderadores y encadenamiento ---------------------------------
# Deduplicamos por (nombre, code) — no solo por nombre — porque en los XLSX de
# índices un mismo `nombre` puede aparecer en varios niveles de la jerarquía
# INPC. El código del genérico vive en `id_ccif_4` del catálogo y coincide con
# `code` de los XLSX de índices.
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

# 3. INPC general en forma aditiva --------------------------------------------
inpc_general <- inpc_indices %>%
  filter(nombre == "índice general") %>%
  arrange(date) %>%
  transmute(
    date,
    inpc   = valor,
    inpc_a = valor / f_encad_general
  )

# 4. Cálculo de incidencias ----------------------------------------------------
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
  # Blanqueamos las incidencias cuyos lags caen en el régimen viejo de
  # ponderadores (mezcla de estructuras). Variaciones del índice (`var_*`) sí
  # son válidas antes de esas fechas porque los índices son continuos vía
  # encadenamiento.
  mutate(
    incidencia_periodo = if_else(
      date >= fecha_inicio_incidencia_periodo, incidencia_periodo, NA_real_
    ),
    incidencia_anual = if_else(
      date >= fecha_inicio_incidencia_anual,   incidencia_anual,   NA_real_
    )
  ) %>%
  # Acotamos el panel al periodo en que los índices ya reflejan los nuevos
  # ponderadores.
  filter(date >= fecha_corte_ponderadores)

# 5. Validación rápida ---------------------------------------------------------
# La suma de incidencias del corte más reciente debe aproximar la variación del
# INPC general (pequeñas diferencias por redondeo/cambios de base).
validacion <- incidencias %>%
  filter(date == max(date)) %>%
  summarise(
    fecha             = max(date),
    inc_periodo_total = sum(incidencia_periodo, na.rm = TRUE),
    inc_anual_total   = sum(incidencia_anual,   na.rm = TRUE)
  )

print(validacion)

# 6. Top 10 / bottom 10 del corte más reciente ---------------------------------
top_bottom <- function(df, col_inc, col_var) {
  df %>%
    filter(!is.na({{ col_inc }}), !is.na({{ col_var }})) %>%
    arrange(desc({{ col_inc }})) %>%
    mutate(n = row_number()) %>%
    filter(n <= 10 | n > nrow(.) - 10) %>%
    transmute(
      date, id_ccif_0, nombre, ccif_original, ponderador,
      variacion  = {{ col_var }},
      incidencia = {{ col_inc }},
      n
    )
}

ultimo <- incidencias %>% filter(date == max(date))
top_bottom_periodo <- top_bottom(ultimo, incidencia_periodo, var_periodo)
top_bottom_anual   <- top_bottom(ultimo, incidencia_anual,   var_anual)

# 7. Exportar tabla de incidencias ---------------------------------------------
sufijo <- if (v_quincena == 1) "quincenal" else "mensual"
archivo_salida <- paste_out(paste0("incidencia_generica_", sufijo, ".xlsx"))

write.xlsx(
  list(
    panel_completo     = incidencias,
    top_bottom_periodo = top_bottom_periodo,
    top_bottom_anual   = top_bottom_anual,
    validacion         = validacion
  ),
  file = archivo_salida
)

print(paste0("Archivo generado: ", archivo_salida))

# 8. Adaptación de nombres para los bloques ggplot del legado ------------------
# Los ggplot originales (de 01_03_01_inflación_incidencia_monitoreo.R) esperan
# columnas `fecha`, `ccif`, `incidencia_mensual`/`incidencia_quincenal`,
# `var_mensual`/`var_quincenal`, `incidencia_anual`, `var_anual`, `n`.
if (v_quincena == 1) {
  d_incidencia_prods_last <- ultimo %>%
    transmute(
      fecha = date, id_ccif_0, ccif = ccif_original,
      var_quincenal        = var_periodo,
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
      var_mensual        = var_periodo,
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

# 9. Gráfica 01_01: Incidencia mensual / quincenal -----------------------------
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

# SVG para versión traducida
archivo_svg <- if (v_quincena == 1) {
  paste_info("99_svg/01_03_03_01_01_incidencia_quincenal.svg")
} else {
  paste_info("99_svg/01_03_03_01_01_incidencia_mensual.svg")
}

ggsave(g, filename = archivo_svg,
       width = 10, height = 15, dpi = 200, bg = "transparent")

# 10. Gráfica 01_02: Incidencia anual ------------------------------------------

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

# 11. Regeneración de RDS para la app Shiny ------------------------------------
# `05_aplicacion/app.R` antes leía los RDS que producía el loop `inegi_series`
# del script legado `01_03_01_inflación_incidencia_monitoreo.R`. Aquí los
# regeneramos desde la fuente alternativa (XLSX) con la misma estructura de
# columnas que la app espera: date, ccif, values, id_ccif_0, date_shortcut.
#
# Esta sección corre independientemente de `v_quincena` y regenera SIEMPRE los
# dos RDS (mensual y quincenal), porque la app carga ambos.

catalogo_nombres <- read_excel(paste_inp("01_03_inpc_complete_NewVersion.xlsx")) %>%
  mutate(nombre_lower = str_to_lower(ccif)) %>%
  select(nombre_lower, ccif_original = ccif) %>%
  distinct(nombre_lower, .keep_all = TRUE)

armar_datos_app <- function(archivo_xlsx, periodicidad) {
  read_excel(archivo_xlsx) %>%
    mutate(
      nombre = str_to_lower(nombre),
      valor  = as.numeric(valor),
      date   = as.Date(date),
      code   = as.character(code)
    ) %>%
    # `date_shortcut` es lo único que la app usa para derivar la quincena: su
    # regex saca un dígito y evalúa módulo 2 (par = 2Q, impar = 1Q).
    mutate(
      date_shortcut = case_when(
        periodicidad == "quincenal" & str_detect(fecha, "1Q") ~ "1",
        periodicidad == "quincenal" & str_detect(fecha, "2Q") ~ "2",
        TRUE ~ "2"
      )
    ) %>%
    # Trae el `ccif` con el casing correcto desde el catálogo. El INPC general
    # se renombra a "Total" porque la app hace `filter(ccif == "Total")` y
    # luego lo rebautiza a "General" en el layer de presentación.
    left_join(catalogo_nombres, by = c("nombre" = "nombre_lower")) %>%
    mutate(
      ccif = case_when(
        nombre == "índice general" ~ "Total",
        !is.na(ccif_original) ~ ccif_original,
        TRUE ~ str_to_sentence(nombre)
      ),
      # id_ccif_0 = code del XLSX: conserva la longitud variable del código
      # (p. ej. "1.1." vs "009") para que el filtro `str_length(min)` de la
      # app colapse nombres duplicados al nivel más específico.
      id_ccif_0 = code
    ) %>%
    select(date, ccif, values = valor, id_ccif_0, date_shortcut) %>%
    distinct()
}

datos_m_app <- armar_datos_app(paste_inp("INPC_mensual_indices.xlsx"),   "mensual")
datos_q_app <- armar_datos_app(paste_inp("INPC_quincenal_indices.xlsx"), "quincenal")

saveRDS(datos_m_app, paste_app("total_datos_inflacion_mes.rds"))
saveRDS(datos_q_app, paste_app("total_datos_inflacion_quincenas.rds"))

print(paste0("RDS de app regenerados: ",
             paste_app("total_datos_inflacion_mes.rds"), " y ",
             paste_app("total_datos_inflacion_quincenas.rds")))

# 12. Diccionario de series SIE Banxico ----------------------------------------
# Los XLSX del portal INEGI no traen los agregados subyacente / no subyacente;
# el API del BIE tampoco los expone desde la actualización 2024 ("apicalipsis").
# Banxico publica los mismos índices con IDs SP estables en los cuadros CP154
# (mensual) y CP155 (quincenal). Ponderadores tomados de la Actualización de
# Canasta INEGI 2024; los 9 conceptos son aproximaciones (verificar contra la
# publicación oficial si se requiere precisión total).
series_dict_bmx <- tribble(
  ~tipo,           ~componente,      ~concepto,                       ~sp_mensual, ~sp_quincenal, ~ponderador,
  "Total",         "Total",          "Total",                         "SP1",       "SP8664",      100.0000,
  "Subyacente",    "Subyacente",     "Subyacente",                    "SP74625",   "SP74632",      76.7415,
  "Subyacente",    "Mercancías",     "Mercancías",                    "SP74626",   "SP74633",      37.5338,
  "Subyacente",    "Mercancías",     "Alimentos, bebidas y tabaco",   "SP66540",   "SP66536",      16.8130,
  "Subyacente",    "Mercancías",     "Mercancías no alimenticias",    "SP74627",   "SP74634",      20.7208,
  "Subyacente",    "Servicios",      "Servicios",                     "SP74628",   "SP74635",      39.2077,
  "Subyacente",    "Servicios",      "Vivienda",                      "SP66542",   "SP66538",      16.1307,
  "Subyacente",    "Servicios",      "Educación (colegiaturas)",      "SP56339",   "SP56384",       3.0055,
  "Subyacente",    "Servicios",      "Otros servicios",               "SP74629",   "SP74636",      20.0715,
  "No subyacente", "No subyacente",  "No subyacente",                 "SP74630",   "SP74637",      23.2585,
  "No subyacente", "Agropecuarios",  "Agropecuarios",                 "SP56337",   "SP56378",      10.6577,
  "No subyacente", "Agropecuarios",  "Frutas y verduras",             "SP56385",   "SP56379",       4.6115,
  "No subyacente", "Agropecuarios",  "Pecuarios",                     "SP56386",   "SP56380",       6.0462,
  "No subyacente", "Energéticos",    "Energéticos y tarifas",         "SP74631",   "SP74638",      12.6008,
  "No subyacente", "Energéticos",    "Energéticos",                   "SP56373",   "SP56382",       8.8079,
  "No subyacente", "Energéticos",    "Tarifas autorizadas gobierno",  "SP74640",   "SP74639",       3.7929
)

# 13. Descarga y cálculo de incidencias por componente / concepto --------------
descargar_sp <- function(sp_id, token) {
  url <- paste0("https://www.banxico.org.mx/SieAPIRest/service/v1/series/",
                sp_id, "/datos")
  r <- GET(url, add_headers(`Bmx-Token` = token))
  stop_for_status(r)
  cont <- content(r, as = "parsed", encoding = "UTF-8")
  datos <- cont$bmx$series[[1]]$datos

  tibble(
    sp_id = sp_id,
    date  = dmy(map_chr(datos, "fecha")),
    valor = as.numeric(str_replace_all(map_chr(datos, "dato"), ",", ""))
  )
}

col_sp  <- if (v_quincena == 2) "sp_mensual" else "sp_quincenal"
ids_bmx <- series_dict_bmx[[col_sp]]

indices_bmx <- ids_bmx %>%
  map_dfr(function(id) {
    message("Descargando ", id)
    descargar_sp(id, v_token_banxico)
  }) %>%
  left_join(series_dict_bmx %>% rename(sp_id = all_of(col_sp)), by = "sp_id") %>%
  arrange(concepto, date)

# Quincenal: 24 observaciones (12 meses) como lag anual; mensual: 12.
lag_anual_bmx <- if (v_quincena == 2) 12 else 24

# Aproximación Laspeyres: incidencia ≈ var_anual × (ponderador / 100). Banxico
# publica el índice ya encadenado (base 2Q-jul-2018), por lo que el factor de
# encadenamiento no aplica aquí. Difiere ~1-10% de la fórmula INEGI con
# encadenamiento por genérico, pero es suficiente para el área apilada.
incidencias_bmx <- indices_bmx %>%
  group_by(concepto) %>%
  mutate(
    var_anual        = (valor / lag(valor, lag_anual_bmx)) - 1,
    incidencia_anual = var_anual * 100 * (ponderador / 100)
  ) %>%
  ungroup()

# 14. Gráfica 01_04: Incidencia anual por componente ---------------------------
orden_componentes <- c("Servicios", "Mercancías",
                       "Energéticos y tarifas", "Agropecuarios")

df_comp <- incidencias_bmx %>%
  filter(concepto %in% orden_componentes) %>%
  mutate(concepto = factor(concepto, levels = orden_componentes)) %>%
  filter(!is.na(incidencia_anual))

df_total <- incidencias_bmx %>%
  filter(concepto == "Total") %>%
  mutate(inflacion = (valor / lag(valor, lag_anual_bmx) - 1) * 100) %>%
  filter(!is.na(inflacion)) %>%
  select(date, inflacion)

fecha_max_bmx <- max(df_comp$date)
etiqueta_fecha <- if (v_quincena == 2) {
  format(fecha_max_bmx, "%B %Y") %>% str_to_sentence()
} else {
  paste0("1ª quincena de ",
         format(fecha_max_bmx, "%B %Y") %>% str_to_sentence())
}

inf_ult <- df_total %>% filter(date == fecha_max_bmx) %>% pull(inflacion)

etiquetas_leyenda <- df_comp %>%
  filter(date == fecha_max_bmx) %>%
  transmute(concepto, etiqueta = paste0(
    concepto, "\n", format(round(incidencia_anual, 3), nsmall = 3)
  ))

colores_comp <- setNames(
  c("#2FA4D3", "#A6D87A", "#F4B678", "#E89BC4"),
  orden_componentes
)

g_comp <- ggplot(df_comp %>% filter(date >= fecha_inicio_grafica),
                 aes(x = date, y = incidencia_anual, fill = concepto)) +
  geom_area(alpha = 0.85) +
  geom_hline(yintercept = 0) +
  geom_point(data = df_total %>% filter(date == fecha_max_bmx),
             aes(x = date, y = inflacion),
             inherit.aes = FALSE, color = mcv_semaforo[4], size = 2) +
  annotate("text",
           x = fecha_max_bmx, y = inf_ult,
           label = paste0("Inflación: ", round(inf_ult, 2), "%"),
           color = mcv_semaforo[4], hjust = 1.05, vjust = -0.8,
           family = "Ubuntu", fontface = "bold", size = 4) +
  scale_fill_manual(values = colores_comp,
                    labels = etiquetas_leyenda$etiqueta[
                      match(orden_componentes, etiquetas_leyenda$concepto)
                    ]) +
  scale_x_date(breaks = seq.Date(from = max(df_total$date),
                                 to   = fecha_inicio_grafica,
                                 by   = "-3 months"),
               date_labels = "%b-%y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title    = "Incidencia anual por componente del INPC",
    subtitle = str_c(
      "La incidencia anual es la contribución en puntos porcentuales que cada\n",
      "componente aporta a la inflación general. ", etiqueta_fecha, "."
    ),
    caption  = NULL,
    x = NULL,
    y = "Puntos aportados a la inflación general"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 24, face = "bold", colour = "#6950D8"),
    plot.subtitle    = element_text(size = 14, colour = "#777777"),
    plot.caption     = element_text(size = 10, colour = "#777777"),
    plot.margin      = margin(0.4, 0.4, 1.2, 0.4, "cm"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x      = element_text(size = 10, angle = 90, vjust = 0.5),
    axis.text.y      = element_text(size = 10),
    axis.title.y     = element_text(size = 11, colour = "#777777"),
    text             = element_text(family = "Ubuntu"),
    legend.position  = "right",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10, lineheight = 1)
  )

g_comp <- ggimage::ggbackground(
  g_comp, paste_info("00_plantillas/02_plantilla_SIE_BANXICO.pdf")
)

archivo_comp <- paste_info(paste0(
  "01_04_incidencia_anual_componente_",
  if (v_quincena == 2) "mensual" else "quincenal",
  ".png"
))
ggsave(g_comp, filename = archivo_comp,
       width = 12, height = 6, dpi = 200, bg = "transparent")
message("Generado: ", archivo_comp)

# 15. Gráfica 01_05: Incidencia anual por concepto -----------------------------
conceptos_subyacente <- c(
  "Otros servicios", "Vivienda", "Educación (colegiaturas)",
  "Alimentos, bebidas y tabaco", "Mercancías no alimenticias"
)
conceptos_no_subyacente <- c(
  "Tarifas autorizadas gobierno", "Energéticos",
  "Pecuarios", "Frutas y verduras"
)

colores_concepto <- c(
  # Subyacente (tonos fríos)
  "Alimentos, bebidas y tabaco"  = "#A6D87A",
  "Mercancías no alimenticias"   = "#6FB4E8",
  "Vivienda"                     = "#4D8FD0",
  "Educación (colegiaturas)"     = "#2F6BB0",
  "Otros servicios"              = "#C6B2E3",
  # No subyacente (tonos cálidos)
  "Frutas y verduras"            = "#F4B678",
  "Pecuarios"                    = "#E89BC4",
  "Energéticos"                  = "#A4D4D2",
  "Tarifas autorizadas gobierno" = "#6BC1BC"
)

df_conc <- incidencias_bmx %>%
  filter(concepto %in% c(conceptos_subyacente, conceptos_no_subyacente)) %>%
  mutate(
    panel = if_else(concepto %in% conceptos_subyacente,
                    "Subyacente", "No subyacente"),
    panel = factor(panel, levels = c("Subyacente", "No subyacente")),
    concepto = factor(concepto,
                      levels = c(conceptos_subyacente, conceptos_no_subyacente))
  ) %>%
  filter(!is.na(incidencia_anual))

etiquetas_conc <- df_conc %>%
  filter(date == fecha_max_bmx) %>%
  transmute(concepto, etiqueta = paste0(
    as.character(concepto), "\n",
    format(round(incidencia_anual, 3), nsmall = 3)
  ))

conceptos_breaks <- c(conceptos_subyacente, conceptos_no_subyacente)
etiquetas_conc_vec <- etiquetas_conc$etiqueta[
  match(conceptos_breaks, as.character(etiquetas_conc$concepto))
]

g_conc <- ggplot(df_conc %>% filter(date >= fecha_inicio_grafica),
                 aes(x = date, y = incidencia_anual, fill = concepto)) +
  geom_area(alpha = 0.85) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = colores_concepto,
                    breaks = conceptos_breaks,
                    labels = etiquetas_conc_vec) +
  scale_x_date(breaks = seq.Date(from = fecha_max_bmx,
                                 to   = fecha_inicio_grafica,
                                 by   = "-3 months"),
               date_labels = "%b-%y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title    = "Incidencia anual por componente y concepto del INPC",
    subtitle = str_c(
      "La incidencia anual es la contribución en puntos porcentuales que cada\n",
      "concepto aporta a la inflación general. ", etiqueta_fecha, "."
    ),
    caption  = NULL,
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 22, face = "bold", colour = "#6950D8"),
    plot.caption     = element_text(size = 10, colour = "#777777", hjust = 0),
    strip.text       = element_text(size = 14, face = "bold", colour = "#6950D8"),
    plot.margin      = margin(0.4, 0.4, 1.2, 0.4, "cm"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    axis.text.x      = element_text(size = 10, angle = 90, vjust = 0.5),
    axis.text.y      = element_text(size = 10),
    text             = element_text(family = "Ubuntu"),
    legend.position  = "right",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 10, lineheight = 1)
  )

g_conc <- ggimage::ggbackground(
  g_conc, paste_info("00_plantillas/02_plantilla_SIE_BANXICO.pdf")
)

archivo_conc <- paste_info(paste0(
  "01_05_incidencia_anual_concepto_",
  if (v_quincena == 2) "mensual" else "quincenal",
  ".png"
))
ggsave(g_conc, filename = archivo_conc,
       width = 12, height = 6, dpi = 200, bg = "transparent")
message("Generado: ", archivo_conc)

# 16. HTML con hilo de tweets de incidencia -----------------------------------
# Genera un HTML con el hilo de 3 tweets listos para copiar (texto + imagen):
#   1. Genéricos con mayor y menor incidencia del corte (del período).
#   2. Top 3 conceptos con mayor incidencia anual + variación del primero.
#   3. Principal componente: aporte en puntos y % del total de la inflación
#      anual.
# Funciona para ambas corridas; ajusta texto ("esta quincena"/"este mes",
# "1ª quincena de X"/"X") e imágenes según `v_quincena`.

periodicidad_texto <- if (v_quincena == 1) "quincenal" else "mensual"

# --- Tweet 1: genéricos con mayor / menor incidencia del período --------------
if (v_quincena == 1) {
  top3 <- d_incidencia_prods_last %>%
    filter(!is.na(incidencia_quincenal)) %>%
    arrange(desc(incidencia_quincenal)) %>%
    slice_head(n = 3) %>%
    pull(ccif) %>%
    str_to_lower()

  bottom3 <- d_incidencia_prods_last %>%
    filter(!is.na(incidencia_quincenal)) %>%
    arrange(incidencia_quincenal) %>%
    slice_head(n = 3) %>%
    pull(ccif) %>%
    str_to_lower()
} else {
  top3 <- d_incidencia_prods_last %>%
    filter(!is.na(incidencia_mensual)) %>%
    arrange(desc(incidencia_mensual)) %>%
    slice_head(n = 3) %>%
    pull(ccif) %>%
    str_to_lower()

  bottom3 <- d_incidencia_prods_last %>%
    filter(!is.na(incidencia_mensual)) %>%
    arrange(incidencia_mensual) %>%
    slice_head(n = 3) %>%
    pull(ccif) %>%
    str_to_lower()
}

# Formatea un vector de strings como "X, Y y Z"
listar_es <- function(items) {
  n <- length(items)
  if (n == 0) return("")
  if (n == 1) return(items[1])
  if (n == 2) return(paste(items, collapse = " y "))
  paste0(paste(items[1:(n - 1)], collapse = ", "), " y ", items[n])
}

tweets <- list(
  list(
    texto = paste0(
      "⬆️ Los genéricos con mayor incidencia ", periodicidad_texto,
      " en el alza de precios fueron ", listar_es(top3), ".\n",
      "⬇️ A la baja fueron ", listar_es(bottom3), "."
    ),
    imagen = if (v_quincena == 1) "01_01_incidencia_quincenal.png"
             else                  "01_01_incidencia_mensual.png",
    alt = paste0("Genéricos con mayor y menor incidencia ", periodicidad_texto)
  )
)

# --- Tweets 2 y 3: conceptos y componentes (anual, ambas corridas) -----------
# Blindaje de la fecha de corte de Banxico: en la corrida quincenal
# (v_quincena == 1) filtramos a observaciones con día <= 15, porque Banxico
# puede haber publicado la 2Q antes de que corramos la 1Q de INEGI. Sin este
# filtro, `fecha_max_bmx` apuntaría a la 2Q y el texto "1ª quincena de X"
# quedaría desalineado. En la corrida mensual usamos el último dato tal cual.
if (v_quincena == 1) {
  corte_bmx <- incidencias_bmx %>%
    filter(day(date) <= 15) %>%
    pull(date) %>%
    max()
} else {
  corte_bmx <- max(incidencias_bmx$date)
}

inflacion_corte <- df_total %>%
  filter(date == corte_bmx) %>%
  pull(inflacion)

# Concordancia gramatical por concepto leaf (artículo + verbo en plural/sing.)
articulos_concepto <- c(
  "alimentos, bebidas y tabaco"  = "los",
  "mercancías no alimenticias"   = "las",
  "vivienda"                     = "la",
  "educación (colegiaturas)"     = "la",
  "otros servicios"              = "",
  "frutas y verduras"            = "las",
  "pecuarios"                    = "los",
  "energéticos"                  = "los",
  "tarifas autorizadas gobierno" = "las"
)
verbo_concepto <- c(
  "alimentos, bebidas y tabaco"  = "presentan",
  "mercancías no alimenticias"   = "presentan",
  "vivienda"                     = "presenta",
  "educación (colegiaturas)"     = "presenta",
  "otros servicios"              = "presentan",
  "frutas y verduras"            = "presentan",
  "pecuarios"                    = "presentan",
  "energéticos"                  = "presentan",
  "tarifas autorizadas gobierno" = "presentan"
)

conceptos_leaf <- c(
  "Alimentos, bebidas y tabaco", "Mercancías no alimenticias",
  "Vivienda", "Educación (colegiaturas)", "Otros servicios",
  "Frutas y verduras", "Pecuarios", "Energéticos",
  "Tarifas autorizadas gobierno"
)

top_conceptos <- incidencias_bmx %>%
  filter(date == corte_bmx, concepto %in% conceptos_leaf) %>%
  arrange(desc(incidencia_anual)) %>%
  slice_head(n = 3)

nombres_lower <- str_to_lower(top_conceptos$concepto)
arts <- unname(articulos_concepto[nombres_lower])
arts[is.na(arts)] <- ""

items_listados <- ifelse(
  arts == "", nombres_lower, paste(arts, nombres_lower)
)

sujeto_primero <- if (arts[1] == "") {
  str_to_sentence(nombres_lower[1])
} else {
  paste(str_to_sentence(arts[1]), nombres_lower[1])
}
verbo_primero   <- verbo_concepto[[nombres_lower[1]]]
var_primero_pct <- top_conceptos$var_anual[1] * 100

imagen_conceptos <- if (v_quincena == 1) {
  "01_05_incidencia_anual_concepto_quincenal.png"
} else {
  "01_05_incidencia_anual_concepto_mensual.png"
}

tweets[[length(tweets) + 1]] <- list(
  texto = paste0(
    "Los conceptos del Índice Nacional de Precios al Consumidor con mayor ",
    "incidencia en la inflación anual observada son ",
    listar_es(items_listados), ". ",
    sujeto_primero, " ", verbo_primero,
    " además una variación anual de ",
    format(round(var_primero_pct, 1), nsmall = 1),
    "% en el nivel de precios."
  ),
  imagen = imagen_conceptos,
  alt = "Incidencia anual por componente y concepto del INPC"
)

# Principal componente (top 1 de los 4 componentes)
top_componente <- incidencias_bmx %>%
  filter(date == corte_bmx, concepto %in% orden_componentes) %>%
  arrange(desc(incidencia_anual)) %>%
  slice(1)

componente_nombre <- str_to_lower(top_componente$concepto)
tipo_nombre       <- str_to_lower(top_componente$tipo)
incidencia_val    <- top_componente$incidencia_anual
pct_total         <- (incidencia_val / inflacion_corte) * 100

if (v_quincena == 1) {
  periodo_corto <- "esta quincena"
  periodo_mes   <- paste0(
    "la 1ª quincena de ", str_to_lower(format(corte_bmx, "%B"))
  )
} else {
  periodo_corto <- "este mes"
  periodo_mes   <- str_to_lower(format(corte_bmx, "%B"))
}

imagen_componente <- if (v_quincena == 1) {
  "01_04_incidencia_anual_componente_quincenal.png"
} else {
  "01_04_incidencia_anual_componente_mensual.png"
}

tweets[[length(tweets) + 1]] <- list(
  texto = paste0(
    "El principal componente de la inflación durante ", periodo_corto,
    " fue el de ", componente_nombre, " de la inflación ", tipo_nombre,
    ", que aportó ", format(round(incidencia_val, 1), nsmall = 1),
    " puntos del ", format(round(inflacion_corte, 1), nsmall = 1),
    "% de la inflación anual de ", periodo_mes,
    " (un ", format(round(pct_total, 1), nsmall = 1), "% del total)."
  ),
  imagen = imagen_componente,
  alt = "Incidencia anual por componente del INPC"
)

# --- Construcción del HTML ---------------------------------------------------
fecha_corte_txt <- format(max(ultimo$date), "%d/%m/%Y")
n_tweets <- length(tweets)

# `lapply` sobre el índice porque necesitamos numerar "Tweet i / n".
tweets_html <- lapply(seq_along(tweets), function(i) {
  t <- tweets[[i]]
  paste0(
    '  <article class="tweet">\n',
    '    <header class="tweet-num">Tweet ', i, ' / ', n_tweets, '</header>\n',
    '    <p class="tweet-texto" id="tweet-texto-', i, '">', t$texto, '</p>\n',
    '    <img src="', t$imagen, '" alt="', t$alt, '">\n',
    '    <p class="conteo">Caracteres: ', nchar(t$texto), ' / 280</p>\n',
    '    <div class="acciones">\n',
    '      <button onclick="copiarTexto(', i, ')">Copiar texto</button>\n',
    '      <button onclick="copiarImagen(\'', t$imagen, '\')">Copiar imagen</button>\n',
    '    </div>\n',
    '  </article>'
  )
})
tweets_html <- paste(unlist(tweets_html), collapse = "\n")

html_content <- paste0(
  '<!DOCTYPE html>\n',
  '<html lang="es">\n',
  '<head>\n',
  '  <meta charset="UTF-8">\n',
  '  <title>Hilo de tweets de incidencia ', periodicidad_texto, '</title>\n',
  '  <style>\n',
  '    body { font-family: "Ubuntu", "Helvetica Neue", sans-serif;\n',
  '           max-width: 640px; margin: 2em auto; padding: 1em; color: #333; }\n',
  '    h1 { color: #6950D8; font-size: 1.3em; margin-bottom: 0.2em; }\n',
  '    .meta { color: #777; font-size: 0.9em; margin-bottom: 1.5em; }\n',
  '    .tweet { background: #f7f7f7; border-left: 4px solid #6950D8;\n',
  '             padding: 1em 1.2em; margin-bottom: 1.5em;\n',
  '             border-radius: 0 4px 4px 0; }\n',
  '    .tweet-num { color: #6950D8; font-size: 0.85em;\n',
  '                 font-weight: bold; margin-bottom: 0.6em;\n',
  '                 text-transform: uppercase; letter-spacing: 0.05em; }\n',
  '    .tweet-texto { white-space: pre-wrap; font-size: 1.05em;\n',
  '                   line-height: 1.55; margin: 0 0 0.8em 0; }\n',
  '    .tweet img { display: block; max-width: 100%; height: auto;\n',
  '                 border-radius: 4px; }\n',
  '    .conteo { color: #777; font-size: 0.85em; margin: 0.6em 0 0 0; }\n',
  '    .acciones { margin-top: 0.8em; }\n',
  '    button { background: #6950D8; color: white; border: none;\n',
  '             padding: 0.5em 1em; cursor: pointer; margin-right: 0.4em;\n',
  '             border-radius: 4px; font-size: 0.95em; }\n',
  '    button:hover { background: #4D3BAD; }\n',
  '  </style>\n',
  '</head>\n',
  '<body>\n',
  '  <h1>Hilo de tweets de incidencia ', periodicidad_texto, '</h1>\n',
  '  <p class="meta">Fecha de corte: ', fecha_corte_txt, '</p>\n',
  tweets_html, '\n',
  '  <script>\n',
  '    function copiarTexto(i) {\n',
  '      const texto = document.getElementById("tweet-texto-" + i).innerText;\n',
  '      navigator.clipboard.writeText(texto).then(function () {\n',
  '        alert("Texto del tweet " + i + " copiado al portapapeles");\n',
  '      });\n',
  '    }\n',
  '    async function copiarImagen(src) {\n',
  '      try {\n',
  '        const res = await fetch(src);\n',
  '        const blob = await res.blob();\n',
  '        await navigator.clipboard.write([\n',
  '          new ClipboardItem({ [blob.type]: blob })\n',
  '        ]);\n',
  '        alert("Imagen copiada al portapapeles");\n',
  '      } catch (e) {\n',
  '        alert("No se pudo copiar la imagen: " + e.message);\n',
  '      }\n',
  '    }\n',
  '  </script>\n',
  '</body>\n',
  '</html>\n'
)

archivo_tweet <- paste_info(paste0(
  "01_06_tweet_incidencia_", periodicidad_texto, ".html"
))

writeLines(html_content, archivo_tweet, useBytes = TRUE)
message("Tweet HTML generado: ", archivo_tweet)
