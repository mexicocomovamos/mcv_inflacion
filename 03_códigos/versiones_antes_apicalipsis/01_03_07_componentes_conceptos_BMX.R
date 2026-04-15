#------------------------------------------------------------------------------#
# Proyecto:   MICROSITIO DE INFLACIÓN
# Objetivo:   Gráficas de incidencia anual por componente (subyacente/no
#             subyacente) y por concepto (9 subcategorías) del INPC. Son las
#             visualizaciones que antes producían los bloques
#             `01_04_incidencia_anual_componente.png` y
#             `01_05_incidencia_anual_concepto.png` de
#             `01_03_01_inflación_incidencia_monitoreo.R` (hoy archivado).
#
# Fuente:     API del SIE de Banco de México. Se decidió usar Banxico porque:
#               (a) los XLSX del portal INEGI no contienen los agregados
#                   subyacente/no subyacente (solo la jerarquía CCIF + genéricos),
#               (b) el API del INEGI (base BIE) dejó de exponer estas series
#                   con la actualización 2024 ("apicalipsis"),
#               (c) Banxico replica los mismos índices de INEGI, con IDs SP
#                   estables y documentados (cuadros CP154 mensual y CP155
#                   quincenal).
#
# Identificadores SP descubiertos el 2026-04-15 extrayendo los SP del HTML de
# los cuadros CP154 / CP155 y probándolos contra la API. Siempre devuelven el
# índice en nivel (base 2Q-julio-2018 = 100).
#
# Salidas (en 04_infobites/):
#   - 01_04_incidencia_anual_componente_{mensual|quincenal}.png
#   - 01_05_incidencia_anual_concepto_{mensual|quincenal}.png
#------------------------------------------------------------------------------#

# 0. Configuración -------------------------------------------------------------
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(jsonlite)
library(ggimage)
library(extrafont)

loadfonts(device = "pdf")
loadfonts(device = "postscript")

####################################################
# Seleccionar corrida: 1 = primera quincena, 2 = mensual
v_quincena <- 2

# Fecha de inicio del eje X para ambas gráficas. Por defecto se fija en
# agosto de 2024 (primer mes completo bajo la actualización INEGI de
# ponderadores 2024, que entró en vigor en la 2ª quincena de julio de 2024).
fecha_inicio_grafica <- as.Date("2018-08-01")
####################################################

source("03_códigos/00_token.R")   # trae v_token_banxico

paste_info <- function(x) paste0("04_infobites/", x)

# Paleta MCV
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_morados  <- c("#6950D8", "#A99BE9")
mcv_blacks   <- c("black", "#D2D0CD", "#777777")
mcv_discrete_7 <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D", "#E8B32E", "#0A93C4", "#974DF0")
mcv_discrete_12 <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D",
                     "#C6B2E3", "#E8B32E", "#0A93C4", "#974DF0",
                     "#00D2D1", "#FF43FA", mcv_blacks[3], mcv_blacks[2])

# 1. Diccionario de series -----------------------------------------------------
# Los SP IDs de Banxico se agrupan en dos bloques contiguos:
#   - Mensual  (cuadro CP154): SP1 (Total) y SP74625-SP74631 + SP56337, SP56339,
#                              SP56373, SP56385, SP56386, SP66540, SP66542,
#                              SP74640.
#   - Quincenal (cuadro CP155): SP8664 (Total) y SP74632-SP74639 + SP56378,
#                              SP56379, SP56380, SP56382, SP56384, SP66536,
#                              SP66538.
#
# Ponderadores publicados por INEGI en la Actualización de Canasta 2024
# (vigentes desde 2Q-julio-2024). Los 4 componentes vienen de
# `01_03_04_experimento.r`; los 9 conceptos están tomados de la nota técnica
# INEGI y son aproximaciones — verificar contra la publicación oficial.
series_dict <- tribble(
  ~tipo,          ~componente,      ~concepto,                       ~sp_mensual, ~sp_quincenal, ~ponderador,
  "Total",        "Total",          "Total",                         "SP1",       "SP8664",      100.0000,
  "Subyacente",   "Subyacente",     "Subyacente",                    "SP74625",   "SP74632",      76.7415,
  "Subyacente",   "Mercancías",     "Mercancías",                    "SP74626",   "SP74633",      37.5338,
  "Subyacente",   "Mercancías",     "Alimentos, bebidas y tabaco",   "SP66540",   "SP66536",      16.8130,
  "Subyacente",   "Mercancías",     "Mercancías no alimenticias",    "SP74627",   "SP74634",      20.7208,
  "Subyacente",   "Servicios",      "Servicios",                     "SP74628",   "SP74635",      39.2077,
  "Subyacente",   "Servicios",      "Vivienda",                      "SP66542",   "SP66538",      16.1307,
  "Subyacente",   "Servicios",      "Educación (colegiaturas)",      "SP56339",   "SP56384",       3.0055,
  "Subyacente",   "Servicios",      "Otros servicios",               "SP74629",   "SP74636",      20.0715,
  "No subyacente","No subyacente",  "No subyacente",                 "SP74630",   "SP74637",      23.2585,
  "No subyacente","Agropecuarios",  "Agropecuarios",                 "SP56337",   "SP56378",      10.6577,
  "No subyacente","Agropecuarios",  "Frutas y verduras",             "SP56385",   "SP56379",       4.6115,
  "No subyacente","Agropecuarios",  "Pecuarios",                     "SP56386",   "SP56380",       6.0462,
  "No subyacente","Energéticos",    "Energéticos y tarifas",         "SP74631",   "SP74638",      12.6008,
  "No subyacente","Energéticos",    "Energéticos",                   "SP56373",   "SP56382",       8.8079,
  "No subyacente","Energéticos",    "Tarifas autorizadas gobierno",  "SP74640",   "SP74639",       3.7929
)

# 2. Consulta al SIE de Banxico ------------------------------------------------
# Devuelve un tibble con date, valor, id. Banxico sí permite hasta 20 series en
# una sola llamada separadas por coma, pero las procesamos una a una para
# mantener el error handling simple.
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

col_sp <- if (v_quincena == 2) "sp_mensual" else "sp_quincenal"
ids <- series_dict[[col_sp]]

# Mensaje de progreso sin paquetes extra
indices <- ids %>%
  map_dfr(function(id) {
    message("Descargando ", id)
    descargar_sp(id, v_token_banxico)
  }) %>%
  left_join(series_dict %>% rename(sp_id = all_of(col_sp)), by = "sp_id") %>%
  arrange(concepto, date)

# 3. Variaciones anuales e incidencia ------------------------------------------
# Para datos mensuales usamos lag(12). Para quincenales con alternancia 1Q/2Q,
# Banxico publica ambas quincenas en la misma serie, así que lag(24) es el
# año atrás (24 observaciones quincenales = 12 meses).
lag_anual <- if (v_quincena == 2) 12 else 24

# Para los agregados Banxico publica el índice ya encadenado (mismo base
# 2Q-jul-2018); por lo tanto la variación anual se obtiene sin el factor de
# encadenamiento. La incidencia se aproxima con
#   incidencia ≈ var_anual * (ponderador / 100).
# Esta aproximación (Laspeyres) difiere de la fórmula INEGI en 1-10% por efecto
# de encadenamiento, pero es consistente con lo que publican la mayoría de
# analistas y basta para un gráfico apilado.
incidencias <- indices %>%
  group_by(concepto) %>%
  mutate(
    var_anual        = (valor / lag(valor, lag_anual)) - 1,
    incidencia_anual = var_anual * 100 * (ponderador / 100)
  ) %>%
  ungroup()

# 4. Gráfica 01_04: Incidencia anual por componente ----------------------------
# Imagen objetivo: 4 áreas apiladas (Mercancías, Servicios, Agropecuarios,
# Energéticos y tarifas) más una línea con la inflación total (INPC).

orden_componentes <- c("Servicios", "Mercancías",
                       "Energéticos y tarifas", "Agropecuarios")

df_comp <- incidencias %>%
  filter(concepto %in% orden_componentes) %>%
  mutate(concepto = factor(concepto, levels = orden_componentes)) %>%
  filter(!is.na(incidencia_anual))

df_total <- incidencias %>%
  filter(concepto == "Total") %>%
  mutate(inflacion = (valor / lag(valor, lag_anual) - 1) * 100) %>%
  filter(!is.na(inflacion)) %>%
  select(date, inflacion)

fecha_max <- max(df_comp$date)
etiqueta_fecha <- if (v_quincena == 2) {
  format(fecha_max, "%B %Y") %>% str_to_sentence()
} else {
  paste0("1ª quincena de ",
         format(fecha_max, "%B %Y") %>% str_to_sentence())
}

inf_ult <- df_total %>% filter(date == fecha_max) %>% pull(inflacion)

etiquetas_leyenda <- df_comp %>%
  filter(date == fecha_max) %>%
  transmute(concepto, etiqueta = paste0(
    concepto, "\n", format(round(incidencia_anual, 3), nsmall = 3)
  ))

colores_comp <- setNames(
  c("#2FA4D3", "#A6D87A", "#F4B678", "#E89BC4"),   # 4 tonos del área apilada
  orden_componentes
)


df_total$date

g_comp <- ggplot(df_comp %>% filter(date >= fecha_inicio_grafica),
                 aes(x = date, y = incidencia_anual, fill = concepto)) +
  geom_area(alpha = 0.85) +
    geom_hline(yintercept = 0) + 
  # geom_line(data = df_total, aes(x = date, y = inflacion),
  #           inherit.aes = FALSE, color = mcv_semaforo[4], linewidth = 0.6) +
  geom_point(data = df_total %>% filter(date == fecha_max),
             aes(x = date, y = inflacion),
             inherit.aes = FALSE, color = mcv_semaforo[4], size = 2) +
  annotate("text",
           x = fecha_max, y = inf_ult,
           label = paste0("Inflación: ", round(inf_ult, 2), "%"),
           color = mcv_semaforo[4], hjust = 1.05, vjust = -0.8,
           family = "Ubuntu", fontface = "bold", size = 4) +
  scale_fill_manual(values = colores_comp,
                    labels = etiquetas_leyenda$etiqueta[
                      match(orden_componentes, etiquetas_leyenda$concepto)
                    ]) +
  scale_x_date(breaks = seq.Date(from = max(df_total$date), to = fecha_inicio_grafica, by = "-3 months"), 
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

# 5. Gráfica 01_05: Incidencia anual por concepto ------------------------------
# Imagen objetivo: dos paneles (Subyacente / No subyacente), cada uno con áreas
# apiladas por concepto.

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

df_conc <- incidencias %>%
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
  filter(date == fecha_max) %>%
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
  scale_x_date(breaks = seq.Date(from = fecha_max,
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
