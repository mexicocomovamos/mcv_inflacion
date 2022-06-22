#------------------------------------------------------------------------------#
# Proyecto:                   MICROSITIO DE INFLACIÓN 
# Objetivo:                   Preparar datos para tablas en Data Wrapper 
#
# Encargadas:     
# Correos:                    katia@mexicocomovamos.mx | regimedina19@gmail.com
# 
# Fecha de creación:          15 de junio de 2022
# Última actualización:       16 de junio de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

## Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck"))  install.packages("hot.deck")  & require("hot.deck")
if(!require("zoo"))       install.packages("zoo")       & require("zoo")
if(!require("stringi"))   install.packages("stringi")   & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes"))  install.packages("ggthemes")  & require("ggthemes")
if(!require("magick"))    install.packages("magick")    & require("magick")
if(!require("scales"))    install.packages("scales")    & require("scales")
if(!require("foreign"))   install.packages("foreign")   & require("foreign")
if(!require("srvyr"))     install.packages("srvyr")     & require("srvyr")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")
if(!require("reticulate")) install.packages("reticulate") & require("reticulate")
if(!require("ggalt")) install.packages("ggalt") & require("ggalt")
if(!require("ggpubr")) install.packages("ggpubr") & require("ggpubr")
if(!require("mxmaps")) install.packages("mxmaps") & require("mxmaps")
if(!require("inegiR")) install.packages("inegiR") & require("inegiR")
if(!require("ggalluvial")) install.packages("ggalluvial") & require("ggalluvial")
if(!require("DatawRappr")) install.packages("DatawRappr") & require("DatawRappr")
require(extrafont)

loadfonts(device="pdf")
loadfonts(device="postscript")

require(tidyverse)

## Credenciales de google ----
v_usuaria <- "regina"
# v_usuaria <- "katia"
googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

## Funciones ----
# Directorios de las carpetas 
paste_inp   <- function(x){paste0("01_datos_crudos/" , x)}
paste_out   <- function(x){paste0("02_datos_limpios/", x)}
paste_code  <- function(x){paste0("03_códigos/"      , x)}
paste_info  <- function(x){paste0("04_infobites/"    , x)}

# Función para abreviar términos
str_wrap_long <- function(stringr, 
                          width = 40,
                          string_limit = 40) {
    ifelse(nchar(stringr) > 40,
           stringr::str_wrap(paste0(substr(stringr,
                                           1, 40-5), "[...]"),
                             width),
           stringr::str_wrap(stringr,
                             width))
}

## Colores MCV -----
mcv_discrete <- c(
    "#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_discrete_7 <- c(
    "#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D", "#E8B32E", "#0A93C4", "#974DF0"
)

mcv_semaforo <- c(
    "#00b783", # verde
    "#E8D92E", # amarillo
    "#ffbd41", # naranja
    "#ff6260" # rojo
)

mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

mcv_blacks <- c("black", "#D2D0CD", "#777777")

mcv_discrete_12 <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D", 
                     "#C6B2E3", "#E8B32E", "#0A93C4", "#974DF0", 
                     "#00D2D1", "#FF43FA", mcv_blacks[3], mcv_blacks[2])


## Identificadores INEGI ----
# Token para API del INEGI
source(paste_code("00_token.R"))

# 1. Procesamiento para tabla de variación --------------------------------------------------
## 1.0. Abrir INPC complete ----
d_inpc <- readRDS(paste_out("01_03_inpc_complete_prods_ccif.RDS")) %>% 
    glimpse()

v_quincena <- 2
## 1.1. Identificadores de productos para seguimiento ----
v_prods_suby <- c(
    "01_011_0111_014", 
    "01_011_0114_036", 
    "01_011_0115_043",
    "01_011_0111_009",
    "11_111_1111_272",
    "01_012_0122_100",
    "07_073_0733_232",
    "11_111_1111_276",
    "09_096_0960_261",
    "07_071_0711_211",
    "04_042_0421_139"
)

v_prods_nosuby <- c(
    "07_072_0722_219",
    "04_045_0452_144",
    "04_045_0451_143",
    "01_011_0112_018",
    "01_011_0112_022",
    "01_011_0117_062",
    "01_011_0116_046",
    "01_011_0117_071",
    "01_011_0116_049",
    "01_011_0114_032",
    "01_011_0117_066"
)

## 1.2. Limpieza de datos ----
if(v_quincena == 1){
    
    d_monitoreo <- 
        d_inpc %>% 
        filter(date > "2014-12-17") %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        filter(id_ccif_0 %in% v_prods_suby) %>% 
        mutate(tipo = "Subyacente") %>% 
        bind_rows(
            d_inpc %>% 
                filter(date > "2014-12-17") %>% 
                filter(!date_shortcut %% 2 == 0) %>% 
                filter(id_ccif_0 %in% v_prods_nosuby) %>% 
                mutate(tipo = "No subyacente")
        ) %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        left_join(
            d_inpc %>% 
                filter(id_ccif_0=="00") %>% 
                filter(date > "2014-12-17") %>% 
                filter(!date_shortcut %% 2 == 0) %>% 
                select(fecha = date, inpc = values) %>% 
                arrange(fecha)
        ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        mutate(
            ccif = case_when(
                id_ccif_0 == "11_111_1111_272" ~ "Loncherías, fondas, torterías y taquerías",
                id_ccif_0 == "04_045_0452_144" ~ "Gas LP",
                T ~ ccif
            ),
            var_mensual = (values - lag(values))/lag(values),
            incidencia_mensual = ((values - lag(values))/lag(inpc))*ponderador,
            var_anual = (values - lag(values, 12))/lag(values, 12),
            incidencia_anual = ((values - lag(values, 12))/lag(inpc, 12))*ponderador,
        ) %>% 
        ungroup() %>% 
        glimpse()
    
} else{
    
    d_monitoreo <- 
        d_inpc %>% 
        filter(date > "2014-12-17") %>% 
        filter(id_ccif_0 %in% v_prods_suby) %>% 
        mutate(tipo = "Subyacente") %>% 
        bind_rows(
            d_inpc %>% 
                filter(date > "2014-12-17") %>% 
                filter(id_ccif_0 %in% v_prods_nosuby) %>% 
                mutate(tipo = "No subyacente")
        ) %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        left_join(
            d_inpc %>% 
                filter(id_ccif_0=="00") %>% 
                filter(date > "2014-12-17") %>% 
                select(fecha = date, inpc = values) %>% 
                arrange(fecha)
        ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        mutate(
            ccif = case_when(
                id_ccif_0 == "11_111_1111_272" ~ "Loncherías, fondas, torterías y taquerías",
                id_ccif_0 == "04_045_0452_144" ~ "Gas LP",
                T ~ ccif
            ),
            var_mensual = (values - lag(values))/lag(values),
            incidencia_mensual = ((values - lag(values))/lag(inpc))*ponderador,
            var_anual = (values - lag(values, 12))/lag(values, 12),
            incidencia_anual = ((values - lag(values, 12))/lag(inpc, 12))*ponderador,
        ) %>% 
        ungroup() %>% 
        glimpse()
}

## 1.3. Preparación web -----

# Función para llamar a los logos de github
pegar_logo <- function(x){
    paste0(
        # Nombre de la imagen 
        "![", x, "]", 
        # URL
        "(https://raw.githubusercontent.com/mexicocomovamos/mcv_inflacion/main/00_documentaci%C3%B3n/00_%C3%ADconos/", 
        x, ".png)")}

# Guardar nombres productos en orden alfabético 
v_productos <- sort(unique(d_monitoreo$ccif))

# Clasificación según tipo de inflación 
v_subyacente <- c(
    v_productos[20], v_productos[13], v_productos[1], v_productos[16], 
    v_productos[15], v_productos[12], v_productos[21], v_productos[18], 
    v_productos[19], v_productos[3], v_productos[22])

v_nosubyacente <- v_productos[!(v_productos %in% v_subyacente)]

# Procesamiento (formato largo)
df_formato <- d_monitoreo %>% 
    # Crear variables para tabla (Markdown y html)
    mutate(
        # Símbolo de cambio 
        # cambio = case_when(
        # var_anual >  0 ~ "▲",
        # var_anual <  0 ~ "▼",
        # var_anual == 0 ~ "~" ),
        # HTML
        # var_anual >  0 ~ "<font color = \"#00b783\"> ▲ </font>",
        # var_anual <  0 ~ "<font color = \"#ff6260\"> ▼ </font>",
        # var_anual == 0 ~ "<font color = \"#777777\"> ~ </font>"), 
        # CSS
        # var_anual >  0 ~ "<p style = \"color:#00b783\"> ▲ </p>",
        # var_anual <  0 ~ "<p style = \"color:#ff6260\"> ▼ </p>",
        # var_anual == 0 ~ "<p style = \"color:#777777\"> ~ </p>"),
        # Texto para la tabla
        texto = paste0(
            # Nombre del producto en negritas 
            "**", ccif,"**", "<br>", 
            "Anual: "  , scales::percent(var_anual  , accuracy = 0.1), "<br>", 
            "Mensual: ", scales::percent(var_mensual, accuracy = 0.1)
        ), 
        # Logotipo para la tabla
        logo = case_when(
            ccif == v_productos[1] ~ pegar_logo("01_Aceite"),
            ccif == v_productos[2] ~ pegar_logo("02_Aguacate"),
            ccif == v_productos[3] ~ pegar_logo("03_Automovil"),
            ccif == v_productos[4] ~ pegar_logo("04_Res"),
            ccif == v_productos[5] ~ pegar_logo("05_Cebolla"),
            ccif == v_productos[6] ~ pegar_logo("06_Serrano"),
            ccif == v_productos[7] ~ pegar_logo("07_Electricidad"),
            ccif == v_productos[8] ~ pegar_logo("08_Gas"),
            ccif == v_productos[9] ~ pegar_logo("09_Gasolina"),
            ccif == v_productos[10] ~ pegar_logo("10_Huevo"),
            ccif == v_productos[11] ~ pegar_logo("11_Jitomate"),
            ccif == v_productos[12] ~ pegar_logo("12_Jugos"),
            ccif == v_productos[13] ~ pegar_logo("13_Leche"),
            ccif == v_productos[14] ~ pegar_logo("14_Limon"),
            ccif == v_productos[15] ~ pegar_logo("15_Taqueria"),
            ccif == v_productos[16] ~ pegar_logo("16_PanDeCaja"),
            ccif == v_productos[17] ~ pegar_logo("17_Pollo"),
            ccif == v_productos[18] ~ pegar_logo("18_Restaurantes"),
            ccif == v_productos[19] ~ pegar_logo("19_Paquete_turistico"),
            ccif == v_productos[20] ~ pegar_logo("20_Tortilla"),
            ccif == v_productos[21] ~ pegar_logo("21_Transporte_aereo"),
            ccif == v_productos[22] ~ pegar_logo("22_Vivienda")
        )) %>% 
    # Dejar solo datos de la última actualización 
    filter(fecha == max(fecha)) %>% 
    # Distinguir entre productos subyacentes y no subyacentes
    mutate(tipo = if_else(
        ccif %in% v_subyacente, "subyacente", "nosubyacente")) %>% 
    select(id_ccif_0, tipo, logo, texto) %>% 
    left_join(
        d_monitoreo %>%
            select(id_ccif_0, fecha, values) %>%
            filter(year(fecha) == max(year(fecha))) %>% 
            pivot_wider(names_from = "fecha", values_from = "values")
    ) %>% 
    glimpse()

# Cambiar a formato ancho (distinguir subyacente y no subyacente)
df_web <- df_formato                %>% 
    filter(tipo == "subyacente")    %>% 
    mutate(id = 1:11)               %>%  # El id solo sirve para pegar
    left_join(
        df_formato            %>% 
            filter(tipo != "subyacente") %>% 
            mutate(id = 1:11), 
        by = "id")            %>% 
    select(-c(starts_with("id"), starts_with("tipo")))

## 1.4. Guardar en Drive -------------------------------------------------------------------

# Obtener identificador del archivo en drive
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1r1etquU3ClNcyOf8gxJDn3gqGGWYZH1T5EGxe5GdOB8/edit#gid=844309712")[1, 2])

# Escribir datos en drive 
googlesheets4::write_sheet(ss = v_id, data = df_web, sheet = "test_tasas")

## 1.5. Republicar en DW ----

DatawRappr::dw_publish_chart(chart_id = "zt54l", api_key = dw_token)

# 2. Procesamiento para tabla de incidencia --------------------------------------------------
## 2.3. Preparación web -----


# Procesamiento (formato largo)
df_formato <- d_monitoreo %>% 
    # Dejar solo datos de la última actualización 
    filter(fecha == max(fecha)) %>% 
    # Crear variables para tabla (Markdown y html)
    mutate(
        # Símbolo de cambio 
        cambio = case_when(
            incidencia_anual >  0 ~ "▲",
            incidencia_anual <  0 ~ "▼",
            incidencia_anual == 0 ~ "~" ),
        # HTML
        # var_anual >  0 ~ "<font color = \"#00b783\"> ▲ </font>",
        # var_anual <  0 ~ "<font color = \"#ff6260\"> ▼ </font>",
        # var_anual == 0 ~ "<font color = \"#777777\"> ~ </font>"), 
        # CSS
        # var_anual >  0 ~ "<p style = \"color:#00b783\"> ▲ </p>",
        # var_anual <  0 ~ "<p style = \"color:#ff6260\"> ▼ </p>",
        # var_anual == 0 ~ "<p style = \"color:#777777\"> ~ </p>"),
        # Texto para la tabla
        texto = paste0(
            # Nombre del producto en negritas 
            "**", ccif,"**", "<br>", 
            "Anual: "  , scales::number(incidencia_anual  , accuracy = 0.001), "<br>", 
            "Mensual: ", scales::number(incidencia_mensual, accuracy = 0.001)
        ), 
        # Logotipo para la tabla
        logo = case_when(
            ccif == v_productos[1] ~ pegar_logo("01_Aceite"),
            ccif == v_productos[2] ~ pegar_logo("02_Aguacate"),
            ccif == v_productos[3] ~ pegar_logo("03_Automovil"),
            ccif == v_productos[4] ~ pegar_logo("04_Res"),
            ccif == v_productos[5] ~ pegar_logo("05_Cebolla"),
            ccif == v_productos[6] ~ pegar_logo("06_Serrano"),
            ccif == v_productos[7] ~ pegar_logo("07_Electricidad"),
            ccif == v_productos[8] ~ pegar_logo("08_Gas"),
            ccif == v_productos[9] ~ pegar_logo("09_Gasolina"),
            ccif == v_productos[10] ~ pegar_logo("10_Huevo"),
            ccif == v_productos[11] ~ pegar_logo("11_Jitomate"),
            ccif == v_productos[12] ~ pegar_logo("12_Jugos"),
            ccif == v_productos[13] ~ pegar_logo("13_Leche"),
            ccif == v_productos[14] ~ pegar_logo("14_Limon"),
            ccif == v_productos[15] ~ pegar_logo("15_Taqueria"),
            ccif == v_productos[16] ~ pegar_logo("16_PanDeCaja"),
            ccif == v_productos[17] ~ pegar_logo("17_Pollo"),
            ccif == v_productos[18] ~ pegar_logo("18_Restaurantes"),
            ccif == v_productos[19] ~ pegar_logo("19_Paquete_turistico"),
            ccif == v_productos[20] ~ pegar_logo("20_Tortilla"),
            ccif == v_productos[21] ~ pegar_logo("21_Transporte_aereo"),
            ccif == v_productos[22] ~ pegar_logo("22_Vivienda")
        )) %>% 
    # Distinguir entre productos subyacentes y no subyacentes
    mutate(tipo = if_else(
        ccif %in% v_subyacente, "subyacente", "nosubyacente")) %>% 
    select(tipo, logo, texto, cambio) 

# Cambiar a formato ancho (distinguir subyacente y no subyacente)
df_web <- df_formato                %>% 
    filter(tipo == "subyacente")    %>% 
    mutate(id = 1:11)               %>%  # El id solo sirve para pegar
    left_join(
        df_formato            %>% 
            filter(tipo != "subyacente") %>% 
            mutate(id = 1:11), 
        by = "id")            %>% 
    select(-c(starts_with("id"), starts_with("tipo")))

## 2.4. Guardar en Drive -------------------------------------------------------------------

# Obtener identificador del archivo en drive
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1r1etquU3ClNcyOf8gxJDn3gqGGWYZH1T5EGxe5GdOB8/edit#gid=0")[1, 2])

# Escribir datos en drive 
googlesheets4::write_sheet(ss = v_id, data = df_web, sheet = "incidencia")

## 2.5. Republicar en DW ----

DatawRappr::dw_publish_chart(chart_id = "61FSx", api_key = dw_token)


# FIN --------------------------------------------------------------------------
