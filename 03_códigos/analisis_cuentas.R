
# Catálogo de productos del INPC
# https://www.inegi.org.mx/app/indicesdeprecios/Estructura.aspx?idEstructura=112001300040&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=INPC%20Nacional%20


#Sys.sleep((60*60*4)+(5))
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
paste_info              <- function(x){paste0("04_infobites/", x)}
paste_code  <- function(x){paste0("03_códigos/"      , x)}

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
source(paste_code("00_token.R"))

d_inpc_complete <- readxl::read_excel(paste_inp("01_03_inpc_complete.xlsx")) %>% 
    glimpse

####################################################
# Seleccionar quincena 
v_quincena <- 1
####################################################

# 0. Procesamiento en loop -----------------------------------------------------
d_inpc <- data.frame()
# Histórico: 
# d_inpc <- readRDS(paste_out("01_03_inpc_complete_prods_ccif.RDS"))

if(v_quincena==1){
    
    for(i in 1:length(unique(d_inpc_complete$id_ccif_0))) {
        
        print(paste0(d_inpc_complete$id_ccif_0[i], " - ", d_inpc_complete$ccif[i]))
        tempo <- inegi_series(
            serie    = d_inpc_complete$id_inegi_q[i],
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(
                ccif = d_inpc_complete$ccif[i],
                id_ccif_0 = d_inpc_complete$id_ccif_0[i],
                id_ccif = d_inpc_complete$id_ccif[i],
                id_ccif_1 = d_inpc_complete$id_ccif_1[i],
                ponderador_inpc_id_ccif_1 = d_inpc_complete$ponderador_inpc_id_ccif_1[i],
                id_ccif_2 = d_inpc_complete$id_ccif_2[i],
                ponderador_inpc_id_ccif_2 = d_inpc_complete$ponderador_inpc_id_ccif_2[i],
                id_ccif_3 = d_inpc_complete$id_ccif_3[i],
                ponderador_inpc_id_ccif_3 = d_inpc_complete$ponderador_inpc_id_ccif_3[i],
                id_ccif_4 = d_inpc_complete$id_ccif_4[i],
                ponderador_inpc_id_ccif_4 = d_inpc_complete$ponderador_inpc_id_ccif_4[i]
            )
        
        d_inpc <- bind_rows(d_inpc, tempo)
        rm(tempo)
        
        Sys.sleep(0.3)
        
    }
    
} else{
    
    for(i in 1:length(unique(d_inpc_complete$id_ccif_0))) {
        
        print(paste0(d_inpc_complete$id_ccif_0[i], " - ", d_inpc_complete$ccif[i]))
        tempo <- inegi_series(
            serie    = d_inpc_complete$id_inegi_m[i],
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(
                ccif = d_inpc_complete$ccif[i],
                id_ccif_0 = d_inpc_complete$id_ccif_0[i],
                id_ccif = d_inpc_complete$id_ccif[i],
                id_ccif_1 = d_inpc_complete$id_ccif_1[i],
                ponderador_inpc_id_ccif_1 = d_inpc_complete$ponderador_inpc_id_ccif_1[i],
                id_ccif_2 = d_inpc_complete$id_ccif_2[i],
                ponderador_inpc_id_ccif_2 = d_inpc_complete$ponderador_inpc_id_ccif_2[i],
                id_ccif_3 = d_inpc_complete$id_ccif_3[i],
                ponderador_inpc_id_ccif_3 = d_inpc_complete$ponderador_inpc_id_ccif_3[i],
                id_ccif_4 = d_inpc_complete$id_ccif_4[i],
                ponderador_inpc_id_ccif_4 = d_inpc_complete$ponderador_inpc_id_ccif_4[i]
            )
        
        d_inpc <- bind_rows(d_inpc, tempo)
        rm(tempo)
        
        Sys.sleep(0.3)
        
    }
    
}


datos <- d_inpc %>% 
    as_tibble()

unique(datos$ccif) %>% sort()

pejcado <- datos %>% 
    filter(ccif %in% c("Alimentos",
                        "Pescados y mariscos",
                       # "Atún y sardina en lata",
                       "Camarón"
                       # ,
                       # "Otros mariscos",
                       # "Otros pescados y mariscos en conserva",
                       # "Pescado"
                       ))

evens <- function(x) subset(x, x %% 2 == 0)

dups <- pejcado %>% select(date, ccif) %>% mutate(es.dup = duplicated(.)) %>% pull(es.dup) %>% which()
pejcado$date[dups] <- pejcado$date[dups]+14

pejcado %>% 
    unique() %>% 
    filter(date >= "2018-07-01") %>% 
    ggplot(aes(x = date, 
               y = values, 
               group = ccif, 
               color = ccif)) + 
    geom_line() + 
    geom_text(data = pejcado %>% filter(date >= "2018-07-01") %>% filter(date == max(date) | (date == min(date) & ccif == "Camarón")),
               aes(label = round(values, 2)), 
              vjust = 1.3, family = "Ubuntu", fontface = "bold",
              show.legend = F) + 
    labs(title = "Evolución del componente del INPC para Alimentos,\nPescados y Mariscos y Camarones", 
         subtitle = "Julio 2018 a Noviembre 2023",
         x = NULL, y = NULL, color = "Componente:" ) + 
    theme_minimal() + 
    scale_color_manual(values = mcv_discrete[c(1,4,3)]) +
    scale_x_date(date_breaks = "2 months", 
                 date_labels = "%m-%Y") + 
    theme(text = element_text(family = "Ubuntu"), 
          axis.text = element_text(size = 12),
          axis.line = element_line(color = "gray50"),
          panel.grid = element_line(linetype = 2),
          plot.title = element_text(color = mcv_discrete[1], 
                                    face = "bold", 
                                    size = 20), 
          plot.subtitle = element_text(color = "gray50", 
                                       face = "bold", 
                                       size = 15), 
          plot.title.position = "plot", 
          legend.position = "bottom",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

inflacion_pescado = pejcado %>% 
    filter(month(date) == 1 & day(date) == 1) %>% 
    select(date, ccif, values) %>% 
    group_by(ccif) %>% 
    arrange(date) %>% 
    mutate(diff = c(diff(values, 1), 0), 
           pp = 100*(diff/values))

inflacion_pescado = pejcado %>% 
    # filter(month(date) == 1 & day(date) == 1) %>% 
    filter(year(date) == 2023) %>% 
    filter(date %in% c(min(date), max(date))) %>% 
    select(date, ccif, values) %>% 
    group_by(ccif) %>% 
    arrange(date) %>% 
    mutate(diff = c(diff(values, 1), 0), 
           pp = 100*(diff/values))

# plotly::ggplotly(plt)
