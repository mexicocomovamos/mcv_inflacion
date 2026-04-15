#------------------------------------------------------------------------------#
# Proyecto:   MICROSITIO DE INFLACIÓN
# Objetivo:   Calcular incidencias de los genéricos sobre la inflación general
#             usando la fuente alternativa (XLSX descargados del portal del
#             INEGI) en lugar de la API de INEGI, que ya no expone los índices
#             a nivel genérico.
#
# Insumos:
#   - 01_datos_crudos/INPC_mensual_indices.xlsx       (si v_quincena == 2)
#   - 01_datos_crudos/INPC_quincenal_indices.xlsx     (si v_quincena == 1)
#   - 01_datos_crudos/01_03_inpc_complete_NewVersion.xlsx  (ponderadores +
#                                                           factor de
#                                                           encadenamiento)
#
# Salidas (en 02_datos_limpios/):
#   - incidencia_generica_{mensual|quincenal}.xlsx
#       · panel_completo       — panel histórico con variaciones e incidencias
#       · top_bottom_periodo   — 10 con mayor y 10 con menor incidencia del
#                                último corte (mensual o quincenal)
#       · top_bottom_anual     — 10 con mayor y 10 con menor incidencia anual
#       · validacion           — suma de incidencias del último corte (debe
#                                aproximar la variación del INPC general)
#------------------------------------------------------------------------------#

# 0. Configuración -------------------------------------------------------------
Sys.setlocale("LC_TIME", "es_ES")
options(scipen = 999)

library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)

####################################################
# Seleccionar corrida: 1 = primera quincena, 2 = mensual
v_quincena <- 2
####################################################

# Factor de encadenamiento del INPC general (base 2ª quincena de julio 2018).
# Actualizar si INEGI cambia la base del INPC.
f_encad_general <- 1.3609522607803

# Fechas de corte de los nuevos ponderadores (Actualización INEGI 2024; entrada
# en vigor 2ª quincena de julio de 2024). Los ponderadores del catálogo
# NewVersion son los nuevos, por lo que:
#   - `fecha_corte_ponderadores` marca desde cuándo los índices quedan
#     reportados con los nuevos pesos → filtramos el output a partir de aquí.
#   - `fecha_inicio_incidencia_periodo` es el primer corte cuyo lag(1) ya cae
#     dentro del nuevo régimen (mismo conjunto de ponderadores en t y t-1).
#   - `fecha_inicio_incidencia_anual` es el primer corte cuyo lag(12) cae ya
#     dentro del nuevo régimen.
# Antes de cada una de esas fechas, la incidencia correspondiente mezcla
# estructuras y debe quedar en blanco (NA).
fecha_corte_ponderadores <- as.Date("2024-07-01")

if (v_quincena == 2) {
  # Corrida mensual.
  fecha_inicio_incidencia_periodo <- as.Date("2024-08-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-07-01")
} else {
  # Corrida quincenal filtrada a 1Q: el primer 1Q con nuevos pesos es
  # 1Q-ago-2024; el primer periodo vs el 1Q previo es 1Q-sep-2024, y el primer
  # anual es 1Q-ago-2025.
  fecha_inicio_incidencia_periodo <- as.Date("2024-09-01")
  fecha_inicio_incidencia_anual   <- as.Date("2025-08-01")
}

paste_inp <- function(x) paste0("01_datos_crudos/", x)
paste_out <- function(x) paste0("02_datos_limpios/", x)

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

# En la corrida quincenal, replicamos la convención de 01_03_02_MONITOREO_ALT.R:
# quedarnos solo con la primera quincena de cada mes (marca "1Q" en `fecha`) y
# tratarla como observación mensual para los cálculos de variación.
if (v_quincena == 1) {
  inpc_indices <- inpc_indices %>% filter(str_detect(fecha, "1Q"))
}

# 2. Catálogo de ponderadores y encadenamiento ---------------------------------
# Deduplicamos por (nombre, code) — no solo por nombre — porque en los XLSX de
# índices un mismo `nombre` puede aparecer en varios niveles de la jerarquía
# INPC (p. ej. "aceites y grasas vegetales comestibles" existe como subgrupo y
# como genérico). El código del genérico vive en `id_ccif_4` del catálogo y
# coincide con `code` de los XLSX de índices.
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
  select(nombre, code, id_ccif_0, ponderador, encadenamiento)

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
# En el flujo mensual usamos lag=1 para la incidencia del período y lag=12 para
# la anual. En el quincenal, como filtramos solo 1Q por mes, los mismos lags
# siguen representando un mes y un año, respectivamente.
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
      date, id_ccif_0, nombre, ponderador,
      variacion  = {{ col_var }},
      incidencia = {{ col_inc }},
      n
    )
}

ultimo <- incidencias %>% filter(date == max(date))
top_bottom_periodo <- top_bottom(ultimo, incidencia_periodo, var_periodo)
top_bottom_anual   <- top_bottom(ultimo, incidencia_anual,   var_anual)

# 7. Exportar ------------------------------------------------------------------
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
