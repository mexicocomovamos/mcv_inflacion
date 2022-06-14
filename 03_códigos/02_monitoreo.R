Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

# Paquetes ----
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
require(extrafont)

loadfonts(device="pdf")
loadfonts(device="postscript")

require(tidyverse)


# Funciones con direcciones de las carpetas
paste_inp               <- function(x){paste0("01_datos_crudos/", x)}
paste_out               <- function(x){paste0("02_datos_limpios/", x)}
paste_csv               <- function(x){paste0("04_csvs/", x)}
paste_info              <- function(x){paste0("05_infobites/", x)}
paste_des               <- function(x){paste0("07_descargables/", x)}

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


# Colores MCV -----
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

mcv_blacks <- c("black", "#D2D0CD", "#777777")

mcv_discrete_12 <- c("#4D5BF0", "#0ACF5F", "#E84D9A", "#E8866D", 
                     "#C6B2E3", "#E8B32E", "#0A93C4", "#974DF0", 
                     "#00D2D1", "#FF43FA", mcv_blacks[3], mcv_blacks[2])


# Identificadores INEGI ----
# Token para API del INEGI
v_token_inegi           <- "682ad7f9-19fe-47f0-abec-e4c2ab2f2948"

# 0. Abrir INPC complete ----
d_inpc <- readRDS(paste_out("01_01_inpc_complete_prods_ccif.RDS")) %>% 
    glimpse()

# 1. Procesamiento para tabla ----
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
    "01_011_0112_022",
    "01_011_0117_062",
    "01_011_0116_046",
    "01_011_0117_071",
    "01_011_0116_049",
    "01_011_0114_032",
    "01_011_0117_066"
)

## 1.2. Limpieza de datos ----
d_monitoreo <- 
    d_inpc %>% 
    filter(date > "2015-12-17") %>% 
    filter(id_ccif_0 %in% v_prods_suby) %>% 
    mutate(tipo = "Subyacente") %>% 
    bind_rows(
        d_inpc %>% 
            filter(date > "2015-12-17") %>% 
            filter(id_ccif_0 %in% v_prods_nosuby) %>% 
            mutate(tipo = "No subyacente")
    ) %>% 
    select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
    arrange(fecha) %>% 
    left_join(
        d_inpc %>% 
            filter(id_ccif_0=="00") %>% 
            filter(date > "2015-12-17") %>% 
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
