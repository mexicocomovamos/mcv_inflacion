# ------------------------------------------------------------------------------
# Proyecto:   MICROSITIO DE INFLACIÓN
# Objetivo:   Herramienta MANUAL para replicar las gráficas
#               - 04_infobites/01_01_incidencia_mensual.png
#               - 04_infobites/01_01_incidencia_quincenal.png
#             sin correr el pipeline completo. Los datos (genérico, incidencia y
#             variación porcentual) se capturan a mano en la tibble de la
#             sección 1.
#
# Uso:
#   1. Ajustar `v_quincena`, `v_anio` y `v_mes` en la sección 0.
#   2. Capturar los 20 genéricos en `datos_manuales` (sección 1):
#        - `generico`      : nombre tal como lo publica INEGI.
#        - `incidencia`    : incidencia del periodo en puntos porcentuales
#                            (p. ej. 0.075 o -0.048).
#        - `variacion_pct` : variación porcentual del periodo tal como se
#                            publica (p. ej. 32.7 para 32.7 %, -10.66 para
#                            -10.66 %).
#   3. Correr todo el script desde el root del proyecto (mcv_inflacion.Rproj).
#
# Nota sobre colores: igual que en el pipeline oficial, los 10 genéricos con
# mayor incidencia se pintan en verde y los 10 con menor incidencia en rojo.
# El orden de captura no importa: el script ordena por incidencia.
#
# Salidas (con el sufijo `v_sufijo`, por defecto "_manual" para no pisar los
# PNG oficiales; poner `v_sufijo <- ""` para reemplazarlos):
#   - 04_infobites/01_01_incidencia_{mensual|quincenal}{sufijo}.png
#   - 04_infobites/99_svg/01_03_03_01_01_incidencia_{mensual|quincenal}{sufijo}.svg
# ------------------------------------------------------------------------------

# 0. Configuración -------------------------------------------------------------
options(scipen = 999)

library(tidyverse)
library(lubridate)
library(scales)
library(ggimage)
library(extrafont)

loadfonts(device = "pdf")
loadfonts(device = "postscript")

####################################################
# Seleccionar corrida: 1 = primera quincena, 2 = mensual
v_quincena <- 2

# Periodo de referencia de los datos capturados
v_anio <- 2026
v_mes  <- 9       # 1 = enero, ..., 12 = diciembre

# Sufijo para los archivos de salida. "" reemplaza los PNG oficiales.
v_sufijo <- "_manual"

# Número de genéricos que se pintan en verde (los de mayor incidencia). Los
# restantes se pintan en rojo.
v_n_verde <- 10
####################################################

paste_info <- function(x) paste0("04_infobites/", x)

# Paleta MCV (idéntica al pipeline oficial)
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260")
mcv_morados  <- c("#6950D8", "#A99BE9")
mcv_blacks   <- c("black", "#D2D0CD", "#777777")

# Abreviador de etiquetas largas (idéntico al pipeline oficial)
str_wrap_long <- function(stringr, width = 40, string_limit = 40) {
  ifelse(
    nchar(stringr) > 40,
    str_wrap(paste0(substr(stringr, 1, 40 - 5), "[...]"), width),
    str_wrap(stringr, width)
  )
}

# Nombres de meses en español (evita depender del locale del sistema)
meses_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
  "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)

# 1. Captura manual de datos ---------------------------------------------------
# Capturar aquí los 20 genéricos (10 con mayor y 10 con menor incidencia).
# `incidencia` en puntos porcentuales; `variacion_pct` en por ciento.
# Fuente de los valores actuales: INEGI, INPC, Cuadro 2, septiembre de 2026.
datos_manuales <- tribble(
  ~generico,                                               ~incidencia, ~variacion_pct,
  # --- Productos genéricos con precios al alza ---
  "Jitomate",                                                    0.133,          30.25,
  "Cebolla",                                                     0.070,          23.00,
  "Gas doméstico LP",                                            0.042,           3.14,
  "Vivienda propia",                                             0.030,           0.22,
  "Pollo",                                                       0.029,           1.78,
  "Primaria",                                                    0.028,           6.00,
  "Loncherías, fondas, torterías y taquerías",                   0.024,           0.42,
  "Universidad",                                                 0.018,           1.50,
  "Otras frutas",                                                0.016,           6.13,
  "Transporte aéreo",                                            0.016,           6.59,
  # --- Productos genéricos con precios a la baja ---
  "Papa y otros tubérculos",                                    -0.056,         -13.92,
  "Servicios profesionales",                                    -0.042,         -16.44,
  "Paquetes de internet, telefonía y televisión de paga",       -0.022,          -2.06,
  "Aguacate",                                                   -0.012,          -8.11,
  "Suavizantes y limpiadores",                                  -0.010,          -1.30,
  "Naranja",                                                    -0.008,          -5.65,
  "Tequila",                                                    -0.007,          -2.62,
  "Automóviles",                                                -0.007,          -0.29,
  "Servicio de internet",                                       -0.006,          -0.78,
  "Productos para el cabello",                                  -0.006,          -0.78
)

# 2. Validación de la captura --------------------------------------------------
stopifnot(
  v_quincena %in% c(1, 2),
  v_mes %in% 1:12,
  nrow(datos_manuales) > 0,
  !any(is.na(datos_manuales$generico)),
  !any(is.na(datos_manuales$incidencia)),
  !any(is.na(datos_manuales$variacion_pct)),
  !any(duplicated(datos_manuales$generico))
)

# Aviso si la variación parece venir como fracción (p. ej. 0.32 en vez de 32)
if (max(abs(datos_manuales$variacion_pct)) < 1) {
  warning(
    "Todas las variaciones son menores a 1 en valor absoluto. ",
    "Verificar que `variacion_pct` esté en por ciento (32.7) y no en fracción (0.327)."
  )
}

# 3. Preparación de la tabla para graficar -------------------------------------
# Se ordena por incidencia y se asigna el rango `n`: los primeros `v_n_verde`
# van en verde y el resto en rojo (misma lógica que el pipeline oficial).
d_grafica <- datos_manuales %>%
  mutate(
    ccif  = generico,
    fecha = make_date(v_anio, v_mes, 1)
  ) %>%
  arrange(desc(incidencia)) %>%
  mutate(
    n     = row_number(),
    grupo = ifelse(n <= v_n_verde, "1", "2"),
    etiqueta = paste0(round(incidencia, 3), "\n[", round(variacion_pct, 2), "%]"),
    # Posición de la etiqueta: dentro de la barra si es larga, fuera si es corta
    hjust_etiqueta = case_when(
      between(incidencia, 0, 0.05)  ~ -0.1,
      between(incidencia, -0.05, 0) ~  1.1,
      incidencia < -0.05            ~ -0.1,
      incidencia >  0.05            ~  1.1
    )
  )

# 4. Textos de la gráfica ------------------------------------------------------
titulo <- if (v_quincena == 1) {
  "Genéricos con mayor y\nmenor incidencia quincenal"
} else {
  "Genéricos con mayor y\nmenor incidencia mensual"
}

subtitulo <- if (v_quincena == 1) {
  paste0(
    "1ª quincena de ", meses_es[v_mes], " ", v_anio,
    " | Entre corchetes se indica la variación quincenal."
  )
} else {
  paste0(
    str_to_sentence(meses_es[v_mes]), " ", v_anio,
    " | Entre corchetes se indica la variación mensual."
  )
}

nota <- if (v_quincena == 1) {
  "La incidencia quincenal es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
} else {
  "La incidencia mensual es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
}

# 5. Gráfica 01_01 (bloque ggplot idéntico al pipeline oficial) ----------------
g <- ggplot(
  d_grafica,
  aes(
    y     = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia),
    x     = incidencia,
    fill  = grupo,
    label = etiqueta
  )
) +
  geom_col() +
  # Línea vertical de referencia en cero
  geom_vline(xintercept = 0, colour = mcv_blacks[1], linewidth = 0.6) +
  geom_text(
    hjust    = d_grafica$hjust_etiqueta,
    family   = "Ubuntu",
    size     = 4,
    fontface = "bold"
  ) +
  scale_fill_manual("", values = c("1" = mcv_semaforo[1], "2" = mcv_semaforo[4])) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.001),
    expand = expansion(c(0.15, 0.15))
  ) +
  labs(
    title    = titulo,
    subtitle = str_wrap(subtitulo, 40),
    caption  = str_wrap(nota, 70)
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(size = 40, face = "bold", colour = mcv_morados[1], hjust = 0.5),
    plot.subtitle    = element_text(size = 30, colour = mcv_blacks[3], hjust = 0.5),
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

# 6. Exportar ------------------------------------------------------------------
nombre_periodo <- if (v_quincena == 1) "quincenal" else "mensual"

archivo_png <- paste_info(
  paste0("01_01_incidencia_", nombre_periodo, v_sufijo, ".png")
)
archivo_svg <- paste_info(
  paste0("99_svg/01_03_03_01_01_incidencia_", nombre_periodo, v_sufijo, ".svg")
)

ggsave(g, filename = archivo_png,
       width = 10, height = 15, dpi = 200, bg = "transparent")

ggsave(g, filename = archivo_svg,
       width = 10, height = 15, dpi = 200, bg = "transparent")

print(paste0("Gráfica guardada en: ", archivo_png))
print(paste0("SVG guardado en: ", archivo_svg))
