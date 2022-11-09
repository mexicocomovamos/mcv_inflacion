#------------------------------------------------------------------------------#
# Proyecto:                   ATOMATIZACIÓN DE SEMÁFOROS
# Objetivo:                   Automatizar estimación del semáforo nacional 01 
#                               01. Crecimiento Económico (PIB)
#
# Encargadas:     
# Correos:                    katia@mexicocomovamos.mx | regimedina19@gmail.com
# 
# Fecha de creación:          02 de junio de 2022
# Última actualización:       24 de agosto de 2022
#------------------------------------------------------------------------------#

Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

# CCIF del INPC: https://www.inegi.org.mx/app/indicesdeprecios/Estructura.aspx?idEstructura=112001300090&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=Clasificaci%C3%B3n%20del%20consumo%20individual%20por%20finalidades(CCIF)%20(mensual)
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
require(extrafont)

loadfonts(device="pdf")
loadfonts(device="postscript")

require(tidyverse)


# Funciones con direcciones de las carpetas
paste_inp   <- function(x){paste0("01_datos_crudos/", x)}
paste_out   <- function(x){paste0("02_datos_limpios/", x)}
paste_code  <- function(x){paste0("03_códigos/"      , x)}
paste_info  <- function(x){paste0("04_infobites/", x)}


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


# Identificadores INEGI ----
# Token para API del INEGI
source(paste_code("00_token.R"))

d_inpc_ccif_ids <- readxl::read_excel(paste_inp("01_03_inpc_ccif_ids.xlsx")) %>% 
    glimpse

# 1. Clasificación del consumo individual por finalidades(CCIF) ----
v_quincena <- 2
nota <- "*Las desagregaciones del INPC solo tienen valor informativo."

if(v_quincena == 1){
    v_total <- "628222"
    v_alimentos <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Alimentos"]
    v_vivienda <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Vivienda"]
    v_salud <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Salud"]
    v_servicios_transporte <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Servicios de transporte"]
    v_educacion <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Educación"]
    v_rest_hoteles <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Restaurantes y hoteles"]
    
} else{
    v_total <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Total"]
    v_alimentos <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Alimentos"]
    v_vivienda <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Vivienda"]
    v_salud <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Salud"]
    v_servicios_transporte <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Servicios de transporte"]
    v_educacion <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Educación"]
    v_rest_hoteles <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Restaurantes y hoteles"]
    
}

d_01_ccif <- inegi_series(
    serie    = v_total,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "General") %>% 
    bind_rows(
        inegi_series(
            serie    = v_alimentos,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Alimentos")
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_vivienda,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Vivienda")
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_salud,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Salud")
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_servicios_transporte,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Servicios de transporte")
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_educacion,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Educación")
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_rest_hoteles,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Restaurantes y hoteles")
    ) %>% 
    filter(date >= "2002-07-01") %>% 
    glimpse

ifelse(
    v_quincena == 1,
    d_01_ccif <- d_01_ccif %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_01_ccif %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor por clasificación del \nconsumo individual por finalidades seleccionadas"
ifelse(
    v_quincena == 1, 
    subtitulo <- paste0(
        "A la 1ª quincena de ", 
        month(max(d_01_ccif$date), label = T, abbr = F),
        " de ", year(max(d_01_ccif$date)), 
        "; [tasa de variación anual]"
    ),
    subtitulo <- paste0(
        str_to_sentence(month(max(d_01_ccif$date), label = T, abbr = F)),
        " de ", year(max(d_01_ccif$date)),
        "; [tasa de variación anual]"
    )
)

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
ggplot(data = d_01_ccif %>% 
           arrange(date) %>% 
           group_by(tipo) %>% 
           mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
           filter(date >= "2015-01-01"),
       aes(
           x = date,
           y = values,
           group = tipo,
           col = tipo,
           label = ifelse(
               date == max(date),
               paste0(str_wrap(tipo,12), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
               NA
           )
       ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "General", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo)),
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5, segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "General", mcv_semaforo[4], tipo),
                        y = ifelse(date == max(date), values, NA)),
               size = 4, show.legend = F) +
    scale_color_manual(
        "",
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_01_ccif$date))-1))+91), "month"), 
                          to = floor_date(as.Date(max(d_01_ccif$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 37, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "bottom")

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_01_ccif.png"), 
       width = 16, height = 9, 
       #type = "cairo", device = "png", 
       dpi = 200, bg= "transparent")

# 2. Alimentos ----
## 2.1. Pan y cereales ----
if(v_quincena == 1){
    
    v_ali_pan_cereales <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pan y cereales"]
    v_ali_pan_cer_harinas_trigo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Harinas de trigo"]
    v_ali_pan_cer_maiz <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Maíz"]
    v_ali_pan_cer_pan_caja <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pan de caja"]
    v_ali_pan_cer_pan_dulce <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pan dulce"]
    v_ali_pan_cer_tortilla <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
    
} else{
    
    v_ali_pan_cereales <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pan y cereales"]
    v_ali_pan_cer_harinas_trigo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Harinas de trigo"]
    v_ali_pan_cer_maiz <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Maíz"]
    v_ali_pan_cer_pan_caja <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pan de caja"]
    v_ali_pan_cer_pan_dulce <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pan dulce"]
    v_ali_pan_cer_tortilla <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
    
}


d_02_01_pan_cereales <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cereales,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Pan y cereales", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cer_harinas_trigo,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Harinas de trigo", ord = 3)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cer_maiz,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Maíz", ord = 4)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cer_pan_caja,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Pan de caja", ord = 5)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cer_pan_dulce,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Pan dulce", ord = 6)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_ali_pan_cer_tortilla,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Tortilla", ord = 7)
    )  %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_02_01_pan_cereales <- d_02_01_pan_cereales %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_01_pan_cereales %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de pan y\ncereales seleccionados"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
ggplot(data = d_02_01_pan_cereales %>% 
           arrange(date) %>% 
           group_by(tipo) %>% 
           mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
           filter(date >= "2015-01-01"),
       aes(
           x = date,
           y = values,
           group = reorder(tipo, ord),
           col = reorder(tipo, ord),
           label = ifelse(
               date == max(date),
               paste0(tipo, "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
               NA
           )
       ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5.5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_01_pan_cereales$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_01_pan_cereales$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "bottom")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_01_ali_pan_cer.png"), 
       #device = "png", type = "cairo", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_ali_pan_cer"))

## 2.2. Carnes ----
if(v_quincena == 1){
    
    v_carnes <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Carnes"]
    v_carne_res <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Carne de res"]
    v_carne_cerdo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
    v_carne_pollo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pollo"]
    
    
} else{
    
    v_carnes <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Carnes"]
    v_carne_res <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Carne de res"]
    v_carne_cerdo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
    v_carne_pollo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pollo"]
    
}

d_02_02_carnes <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_carnes,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Carnes", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_carne_res,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Res", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_carne_cerdo,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Cerdo", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_carne_pollo,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Pollo", ord = 5)
    ) %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_02_02_carnes <- d_02_02_carnes %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_02_carnes %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de carnes\nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
ggplot(data = d_02_02_carnes %>% 
           arrange(date) %>% 
           group_by(tipo) %>% 
           mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
           filter(date >= "2015-01-01"),
       aes(
           x = date,
           y = values,
           group = reorder(tipo, ord),
           col = reorder(tipo, ord),
           label = ifelse(
               date == max(date),
               paste0(tipo, "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
               NA
           )
       ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5.5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_02_carnes$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_02_carnes$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("01_03_02_02_02_ali_carnes.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")



rm(list=ls(pattern="^v_carne"))

## 2.3. Leche, quesos y huevo ----
if(v_quincena == 1){
    
    v_lácteos_leche_quesos_huevos <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Leche quesos y huevos"]
    v_lácteos_leche <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
    v_lácteos_queso_oax <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Queso oaxaca y asadero"]
    v_lácteos_huevo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Huevo"]
    
    
} else{
    
    v_lácteos_leche_quesos_huevos <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Leche quesos y huevos"]
    v_lácteos_leche <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
    v_lácteos_queso_oax <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Queso oaxaca y asadero"]
    v_lácteos_huevo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Huevo"]
    
}


d_02_03_lácteos <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_leche_quesos_huevos,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Leche, quesos y huevos", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_leche,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Leche", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_queso_oax,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Queso oaxaca y asadero", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_huevo,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Huevo", ord = 5)
    ) %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_02_03_lácteos <- d_02_03_lácteos %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_03_lácteos %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de lácteos y\nhuevo seleccionados"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_02_03_lácteos %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_03_lácteos$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_03_lácteos$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_03_ali_lácteos.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


rm(list=ls(pattern="^v_lácteos"))


## 2.4. Frutas ----
if(v_quincena == 1){
    
    v_fruta <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Frutas"]
    v_fruta_aguacate <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Aguacate"]
    v_fruta_limón <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Limón"]
    v_fruta_manzana <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Manzana"]
    v_fruta_pera <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pera"]
    v_fruta_plátanos <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Plátanos"]
    v_fruta_uva <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Uva"]
    
    
} else{
    
    v_fruta <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Frutas"]
    v_fruta_aguacate <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Aguacate"]
    v_fruta_limón <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Limón"]
    v_fruta_manzana <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Manzana"]
    v_fruta_pera <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pera"]
    v_fruta_plátanos <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Plátanos"]
    v_fruta_uva <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Uva"]
    
}


d_02_04_frutas <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Fruta", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_aguacate,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Aguacate", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_limón,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Limón", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_manzana,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Manzana", ord = 5)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_pera,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Pera", ord = 6)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_plátanos,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Plátanos", ord = 7)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_fruta_uva,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Uva", ord = 8)
    ) %>% 
    filter(date >= "2002-07-01")
ifelse(
    v_quincena == 1,
    d_02_04_frutas <- d_02_04_frutas %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_04_frutas %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de frutas \nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_02_04_frutas %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5.5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_04_frutas$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_04_frutas$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_04_ali_frutas.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_fruta"))

## 2.5. Legumbres ----
if(v_quincena == 1){
    
    v_legum <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Legumbres y hortalizas"]
    v_legum_calabacita <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Calabacita"]
    v_legum_chile <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Chile serrano"]
    v_legum_jitomate <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Jitomate"]
    v_legum_tomate_v <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Tomate verde"]
    v_legum_nopales <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Nopales"]
    v_legum_papa <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
    
    
} else{
    
    v_legum <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Legumbres y hortalizas"]
    v_legum_calabacita <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Calabacita"]
    v_legum_chile <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Chile serrano"]
    v_legum_jitomate <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Jitomate"]
    v_legum_tomate_v <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Tomate verde"]
    v_legum_nopales <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Nopales"]
    v_legum_papa <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
    
}


d_02_05_legum <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Legumbres y hortalizas", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_calabacita,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Calabacita", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_chile,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Chile serrano", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_jitomate,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Jitomate", ord = 5)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_tomate_v,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Tomate verde", ord = 6)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_nopales,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Nopales", ord = 7)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_legum_papa,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Papa", ord = 8)
    ) %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_02_05_legum <- d_02_05_legum %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_05_legum %>% 
        glimpse
)


titulo <- "Índice de precios al consumidor de legumbres\nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_02_05_legum %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 4.5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_05_legum$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_05_legum$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_05_ali_legum.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_legum"))

## 2.6. Aceites y grasas ----
if(v_quincena == 1){
    
    v_lácteos_aceites_grasas <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Aceites y grasas"]
    v_lácteos_aceites_vegetales <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
    v_lácteos_aceites_manteca <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Manteca de cerdo"]
    v_lácteos_aceites_mantequilla <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Mantequilla"]
    
    
} else{
    
    v_lácteos_aceites_grasas <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Aceites y grasas"]
    v_lácteos_aceites_vegetales <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
    v_lácteos_aceites_manteca <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Manteca de cerdo"]
    v_lácteos_aceites_mantequilla <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Mantequilla"]
    
}


d_02_06_aceites <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_aceites_grasas,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Aceites y grasas", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_aceites_vegetales,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Aceites vegetales", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_aceites_manteca,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Manteca", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_lácteos_aceites_mantequilla,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Mantequilla", ord = 5)
    ) %>% 
    filter(date >= "2002-07-01")
ifelse(
    v_quincena == 1,
    d_02_06_aceites <- d_02_06_aceites %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_06_aceites %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de aceites\ny grasas seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_02_06_aceites %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_06_aceites$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_06_aceites$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_06_ali_aceites.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_acei"))

## 2.7. Azúcares ----
if(v_quincena == 1){
    
    v_azucares <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Azúcar mermeladas miel chocolates y dulces"]
    v_azucares_azucar <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Azúcar"]
    v_azucares_chocolate <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Chocolate y productos de confitería"]
    v_azucares_gela_miel_merm <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Gelatina miel y mermeladas"]
    v_azucares_helados <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Helados nieves y paletas de hielo"]
    
} else{
    
    v_azucares <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Azúcar mermeladas miel chocolates y dulces"]
    v_azucares_azucar <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Azúcar"]
    v_azucares_chocolate <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Chocolate y productos de confitería"]
    v_azucares_gela_miel_merm <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Gelatina miel y mermeladas"]
    v_azucares_helados <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Helados nieves y paletas de hielo"]
    
}


d_02_07_azucares <- inegi_series(
    serie    = v_alimentos,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Alimentos", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_azucares,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Azúcares", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_azucares_azucar,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Azúcar", ord = 3)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_azucares_chocolate,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Chocolate", ord = 4)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_azucares_gela_miel_merm,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Gelatina, miel y mermeladas", ord = 5)
    )  %>% 
    bind_rows(
        inegi_series(
            serie    = v_azucares_helados,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Helados", ord = 6)
    ) %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_02_07_azucares <- d_02_07_azucares %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_02_07_azucares %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de\nazúcares seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_02_07_azucares %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_02_07_azucares$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_02_07_azucares$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L),
                       limits = c(50,150)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "bottom")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_02_07_ali_azucar.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_azu"))

#

# 3. Bebidas no alcohólicas ----

if(v_quincena == 1){
    
    v_bebidas <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Bebidas no alcohólicas"]
    v_bebidas_cafe_te <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Café té y cacao"]
    v_bebidas_agua <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Agua embotellada"]
    v_bebidas_jugos <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Jugos o néctares envasados"]
    v_bebidas_refresco <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Refrescos envasados"]
    
    
} else{
    
    v_bebidas <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Bebidas no alcohólicas"]
    v_bebidas_cafe_te <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Café té y cacao"]
    v_bebidas_agua <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Agua embotellada"]
    v_bebidas_jugos <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Jugos o néctares envasados"]
    v_bebidas_refresco <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Refrescos envasados"]
    
}


d_03_bebidas <- inegi_series(
    serie    = v_bebidas,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE) %>% 
    mutate(tipo = "Bebidas no alcohólicas", ord = 1) %>% 
    bind_rows(
        inegi_series(
            serie    = v_bebidas_cafe_te,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Café, té y cacao", ord = 2)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_bebidas_agua,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Agua embotellada", ord = 3)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_bebidas_jugos,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Jugos envasados", ord = 4)
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_bebidas_refresco,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = "Refrescos envasados", ord = 5)
    ) %>% 
    filter(date >= "2002-07-01")

ifelse(
    v_quincena == 1,
    d_03_bebidas <- d_03_bebidas %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_03_bebidas %>% 
        glimpse
)

titulo <- "Índice de precios al consumidor de bebidas\nno alcohólicas seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_03_bebidas %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Bebidas no alcohólicas", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Bebidas no alcohólicas", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Bebidas no alcohólicas", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Bebidas no alcohólicas", mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_03_bebidas$date))))+91), "month"), 
                          to = floor_date(as.Date(max(d_03_bebidas$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_02_03_bebidas.png"),
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")

rm(list=ls(pattern="^v_bebi"))

# 5. Productos de salud --------------------------------------------------------

# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.1 Productos, artefactos y equipos médicos >
# 06.1.1 Productos farmacéuticos 

# ---- Enlistar productos del catálogo
v_productos <- c(
    "Productos farmacéuticos", "Analgésicos", "Antibióticos", "Antigripales", 
    "Cardiovasculares", "Medicamentos para diabetes")

# ---- Seleccionar productos del catálogo de identificadores 
df_productos <- d_inpc_ccif_ids     %>% 
    filter(ccif %in% v_productos)   

# ---- Obtener identificadores de productos seleccionados
v_ids_q <- unique(df_productos$id_inegi_q) # Identificadores de la quincena
v_ids_m <- unique(df_productos$id_inegi_m) # Identificadores del mes
v_ids   <- if_else(rep(v_quincena == 1, length(v_ids_q)), # Evaluar quincena
                   v_ids_q, v_ids_m)  # Seleccionar identificadores adecuados

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    
    # Imprimir vuelta y producto
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    # Importar datos del producto de INEGI
    df_data <- inegi_series(
        serie = v_ids[i], 
        token = v_token_inegi, 
        database = "BIE", 
        as_tt = TRUE) %>% 
        mutate(
            tipo = v_productos[i], 
            ord  = i)
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 

# Filtrar fechas 
df_06_01_farmaceuticos <- df_series                      %>% 
    filter(date >= "2002-07-01")

# Dejar datos mensuales o quincenales
ifelse(v_quincena == 1, 
       # Para la serie quincenal, dejar solo datos de la primera quincena
       df_06_01_farmaceuticos <- df_06_01_farmaceuticos %>% 
           filter(!date_shortcut %% 2 == 0)             %>% 
           glimpse, 
       # Serie mensual 
       df_06_01_farmaceuticos %>% 
           glimpse
       )

# ---- Gráfica 
# titulo <- "Índice de precios al consumidor de azúcares seleccionadas"
# eje_y <- "Índice base 2ª quincena de julio 2018 = 100"

titulo  <- "Índice de precios al consumidor de productos\nfarmacéuticos seleccionados"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
    df_06_01_farmaceuticos %>% 
               arrange(date) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(date >= "2015-01-01"),
           aes(
               x = date,
               y = values,
               group = reorder(tipo, desc(ord)),
               col = reorder(tipo, desc(ord)),
               label = ifelse(
                   date == max(date),
                   paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == v_productos[1], "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(df_06_01_farmaceuticos$date))))+91), "month"), 
                          to = floor_date(as.Date(max(df_06_01_farmaceuticos$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    # scale_y_continuous(labels = scales::number_format(accuracy = 1L),
    #                    limits = c(70, 130)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("01_03_02_05_01_farma.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")



# 6. Servicios de salud para pacientes externos --------------------------------
# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.2. Servicios para pacientes externos 

# ---- Enlistar productos del catálogo
v_productos <- c("Servicios para pacientes externos", "Servicios médicos", 
                 "Servicios dentales", "Servicios paramédicos")

# ---- Seleccionar productos del catálogo de identificadores 
df_productos <- d_inpc_ccif_ids %>% 
    filter(ccif %in% v_productos)

# ---- Obtener identificadores de productos seleccionados
v_ids_q <- unique(df_productos$id_inegi_q) # Identificadores de la quincena
v_ids_m <- unique(df_productos$id_inegi_m) # Identificadores del mes 
v_ids   <- if_else(rep(v_quincena == 1, length(v_ids_q)), # Evaluar quincena
                   v_ids_q, v_ids_m)  # Seleccionar identificadores adecuados

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    df_data <- inegi_series(
        serie = v_ids[i], 
        token = v_token_inegi, 
        database = "BIE", 
        as_tt = TRUE) %>% 
        mutate(
            tipo = v_productos[i], 
            ord  = i)
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 

# Filtrar fechas 
df_06_02_servicios_pacientes <- df_series                      %>% 
    filter(date >= "2002-07-01")

# Dejar datos mensuales o quincenales
ifelse(v_quincena == 1, 
       # Para la serie quincenal, dejar solo datos de la primera quincena
       df_06_02_servicios_pacientes <- df_06_02_servicios_pacientes %>% 
           filter(!date_shortcut %% 2 == 0)             %>% 
           glimpse, 
       # Serie mensual 
       df_06_02_servicios_pacientes %>% 
           glimpse
)

# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de servicios\n para pacientes"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_06_02_servicios_pacientes %>% 
            arrange(date) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(date >= "2015-01-01"),
        aes(
            x = date,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                date == max(date),
                paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == v_productos[1], "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(df_06_02_servicios_pacientes$date))))+91), "month"), 
                          to = floor_date(as.Date(max(df_06_02_servicios_pacientes$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L),
                       limits = c(75, 125)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("01_03_02_05_02_pacientes.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 7. Servicios de hospital -----------------------------------------------------

# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.3. Servicios de hospital > 06.3.0 Servicios de hospital 

# ---- Enlistar productos del catálogo
v_productos <- c(
                # "Servicios de hospital (categoría)", 
                 "Servicios de hospital",
                 "Atención médica durante el parto", 
                 "Hospitalización general", "Hospitalización parto", 
                 "Operación quirúrgica")

# ---- Seleccionar productos del catálogo de identificadores 
df_productos <- d_inpc_ccif_ids %>% 
    # Cambiar el nombre de la categoría general (homónima de categoría específica)
    mutate(ccif = if_else(
        id_ccif_0 == "06_063", "Servicios de hospital (categoría)", ccif)) %>% 
    filter(ccif %in% v_productos)

# ---- Obtener identificadores de productos seleccionados
v_ids_q <- unique(df_productos$id_inegi_q) # Identificadores de la quincena
v_ids_m <- unique(df_productos$id_inegi_m) # Identificadores del mes 
v_ids   <- if_else(rep(v_quincena == 1, length(v_ids_q)), # Evaluar quincena
                   v_ids_q, v_ids_m)  # Seleccionar identificadores adecuados

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    df_data <- inegi_series(
        serie = v_ids[i], 
        token = v_token_inegi, 
        database = "BIE", 
        as_tt = TRUE) %>% 
        mutate(
            tipo = v_productos[i], 
            ord  = i)
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 

# Filtrar fechas 
df_06_03_servicios_hospital <- df_series                        %>% 
    filter(date >= "2002-07-01")                                %>% 
    # Ordenar factores 
    # mutate(tipo = factor(tipo, levels = v_productos)) %>% 
    glimpse()

# Dejar datos mensuales o quincenales
ifelse(v_quincena == 1, 
       # Para la serie quincenal, dejar solo datos de la primera quincena
       df_06_03_servicios_hospital <- df_06_03_servicios_hospital %>% 
           filter(!date_shortcut %% 2 == 0)             %>% 
           glimpse, 
       # Serie mensual 
       df_06_03_servicios_hospital %>% 
           glimpse
)


# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de servicios\nde hospital"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_06_03_servicios_hospital %>% 
            arrange(date) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(date >= "2015-01-01"),
        aes(
            x = date,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                date == max(date),
                paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == v_productos[1], "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(df_06_03_servicios_hospital$date))))+91), "month"), 
                          to = floor_date(as.Date(max(df_06_03_servicios_hospital$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("01_03_02_05_04_hospitales.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 8. Productos de la peda ------------------------------------------------------

# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.3. Servicios de hospital > 06.3.0 Servicios de hospital 

# ---- Enlistar productos del catálogo
v_productos <- c(
    "Tequila", "Cerveza", "Cigarrillos", "Papas fritas", "Analgésicos")

# ---- Seleccionar productos del catálogo de identificadores 
df_productos <- d_inpc_ccif_ids %>% 
    # Cambiar el nombre de la categoría general (homónima de categoría específica)
    mutate(ccif = if_else(
        id_ccif_0 == "02_021_0213", "Cerveza (categoría)", ccif)) %>% 
    filter(ccif %in% v_productos)

# ---- Obtener identificadores de productos seleccionados
v_ids_q <- unique(df_productos$id_inegi_q) # Identificadores de la quincena
v_ids_m <- unique(df_productos$id_inegi_m) # Identificadores del mes 
v_ids   <- if_else(rep(v_quincena == 1, length(v_ids_q)), # Evaluar quincena
                   v_ids_q, v_ids_m)  # Seleccionar identificadores adecuados

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    df_data <- inegi_series(
        serie = v_ids[i], 
        token = v_token_inegi, 
        database = "BIE", 
        as_tt = TRUE) %>% 
        mutate(
            tipo = v_productos[i], 
            ord  = i)
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 
# Filtrar fechas 
df_fiesta <- df_series                      %>% 
    filter(date >= "2002-07-01")            %>% 
    # Ordenar factores 
    # mutate(tipo = factor(tipo, levels = v_productos)) %>% 
    glimpse()

# Dejar datos mensuales o quincenales
ifelse(v_quincena == 1, 
       # Para la serie quincenal, dejar solo datos de la primera quincena
       df_fiesta <- df_fiesta %>% 
           filter(!date_shortcut %% 2 == 0)             %>% 
           glimpse, 
       # Serie mensual 
       df_fiesta %>% 
           glimpse
)

# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de productos para la fiesta "
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_fiesta %>% 
            arrange(date) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(date >= "2015-01-01"),
        aes(
            x = date,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                date == max(date),
                paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              # aes(
              # color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
              # linetype = if_else(tipo == v_productos[1], "solid", "dashed")
              # )
              ) + 
    ggrepel::geom_text_repel(
        # aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        # color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(date == max(date), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(df_fiesta$date))))+91), "month"), 
                          to = floor_date(as.Date(max(df_fiesta$date)), "month"), 
                          by = "6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo, caption = nota,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 25, colour = "#777777"),
          plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          legend.text = element_text(size = 30),
          legend.position = "none")

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("01_03_02_05_05_peda.png"),
       #type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 13. Productos canasta básica PROFECO ----

if(v_quincena == 1){
    v_total <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Total"]
    
    v_pacic_leche <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
    v_pacic_atún_sardina <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Atún y sardina en lata"]
    v_pacic_res <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Carne de res"]
    v_pacic_cerdo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
    v_pacic_pollo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pollo"]
    v_pacic_huevo <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Huevo"]
    
    v_pacic_frijol <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Frijol"]
    v_pacic_arroz <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Arroz"]
    v_pacic_pan_caja <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pan de caja"]
    v_pacic_pasta_sopa <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Pasta para sopa"]
    v_pacic_tortilla <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
    v_pacic_aceite <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
    v_pacic_azúcar <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Azúcar"]
    
    v_pacic_cebolla <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Cebolla"]
    v_pacic_chile <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Chiles envasados"]
    v_pacic_jitomate <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Jitomate"]
    v_pacic_limón <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Limón"]
    v_pacic_manzana <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Manzana"]
    v_pacic_plátano <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Plátanos"]
    v_pacic_zanahoria <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Zanahoria"]
    v_pacic_papa <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
    
    v_pacic_jabón <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Jabón de tocador"]
    v_pacic_papel_higie <- d_inpc_ccif_ids$id_inegi_q[d_inpc_ccif_ids$ccif=="Papel higiénico y pañuelos desechables"]
    
} else{
    v_total <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Total"]
    
    v_pacic_leche <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
    v_pacic_atún_sardina <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Atún y sardina en lata"]
    v_pacic_res <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Carne de res"]
    v_pacic_cerdo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
    v_pacic_pollo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pollo"]
    v_pacic_huevo <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Huevo"]
    
    v_pacic_frijol <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Frijol"]
    v_pacic_arroz <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Arroz"]
    v_pacic_pan_caja <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pan de caja"]
    v_pacic_pasta_sopa <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Pasta para sopa"]
    v_pacic_tortilla <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
    v_pacic_aceite <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
    v_pacic_azúcar <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Azúcar"]
    
    v_pacic_cebolla <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Cebolla"]
    v_pacic_chile <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Chiles envasados"]
    v_pacic_jitomate <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Jitomate"]
    v_pacic_limón <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Limón"]
    v_pacic_manzana <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Manzana"]
    v_pacic_plátano <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Plátanos"]
    v_pacic_zanahoria <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Zanahoria"]
    v_pacic_papa <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
    
    v_pacic_jabón <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Jabón de tocador"]
    v_pacic_papel_higie <- d_inpc_ccif_ids$id_inegi_m[d_inpc_ccif_ids$ccif=="Papel higiénico y pañuelos desechables"]
    
}

v_pacic_list <- c(
    
    v_pacic_leche,
    v_pacic_atún_sardina,
    v_pacic_res,
    v_pacic_cerdo,
    v_pacic_pollo,
    v_pacic_huevo,
    v_pacic_frijol,
    v_pacic_arroz,
    v_pacic_pan_caja,
    v_pacic_pasta_sopa,
    v_pacic_tortilla,
    v_pacic_aceite,
    v_pacic_azúcar,
    v_pacic_cebolla,
    v_pacic_chile,
    v_pacic_jitomate,
    v_pacic_limón,
    v_pacic_manzana,
    v_pacic_plátano,
    v_pacic_zanahoria,
    v_pacic_papa,
    v_pacic_jabón,
    v_pacic_papel_higie
    
)

v_pacic_list_labs <- c(
    
    "Leche",
    "Atún y sardina en lata",
    "Carne de res",
    "Carne de cerdo",
    "Pollo",
    "Huevo",
    "Frijol",
    "Arroz",
    "Pan de caja",
    "Pasta para sopa",
    "Tortilla de maíz",
    "Aceite vegetal",
    "Azúcar",
    "Cebolla",
    "Chiles envasados",
    "Jitomate",
    "Limón",
    "Manzana",
    "Plátano",
    "Zanahoria",
    "Papa",
    "Jabón de tocador",
    "Papel higiénico y pañuelos desechables"
    
)

d_04_pacic <- data.frame()

for(i in 1:length(v_pacic_list)){
    print(paste0(str_pad(i,2,"l","0"), " - ", v_pacic_list_labs[i]))
    Sys.sleep(0.5)
    
    tempo <- inegi_series(
        serie    = v_pacic_list[i],
        token    = v_token_inegi, 
        database = "BIE", 
        as_tt    = TRUE) %>% 
        mutate(tipo = v_pacic_list_labs[i])
    
    d_04_pacic <- bind_rows(d_04_pacic, tempo)
    
}


d_04_pacic <- d_04_pacic %>% 
    mutate(
        cat = case_when(
            tipo == "Leche" ~ "1. Productos de origen animal",
            tipo == "Atún y sardina en lata" ~ "1. Productos de origen animal",
            tipo == "Carne de res" ~ "1. Productos de origen animal",
            tipo == "Carne de cerdo" ~ "1. Productos de origen animal",
            tipo == "Pollo" ~ "1. Productos de origen animal",
            tipo == "Huevo" ~ "1. Productos de origen animal",
            tipo == "Frijol" ~ "2. Despensa",
            tipo == "Arroz" ~ "2. Despensa",
            tipo == "Pan de caja" ~ "2. Despensa",
            tipo == "Pasta para sopa" ~ "2. Despensa",
            tipo == "Tortilla de maíz" ~ "2. Despensa",
            tipo == "Aceite vegetal" ~ "2. Despensa",
            tipo == "Azúcar" ~ "2. Despensa",
            tipo == "Cebolla" ~ "3. Frutas y verduras",
            tipo == "Chiles envasados" ~ "3. Frutas y verduras",
            tipo == "Jitomate" ~ "3. Frutas y verduras",
            tipo == "Limón" ~ "3. Frutas y verduras",
            tipo == "Manzana" ~ "3. Frutas y verduras",
            tipo == "Plátano" ~ "3. Frutas y verduras",
            tipo == "Zanahoria" ~ "3. Frutas y verduras",
            tipo == "Papa" ~ "3. Frutas y verduras",
            tipo == "Jabón de tocador" ~ "4. Aseo personal",
            tipo == "Papel higiénico y pañuelos desechables" ~ "4. Aseo personal",
            T ~ NA_character_
        )
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_total,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = " General", cat = "1. Productos de origen animal") 
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_total,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = " General", cat = "2. Despensa") 
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_total,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = " General", cat = "3. Frutas y verduras") 
    ) %>% 
    bind_rows(
        inegi_series(
            serie    = v_total,
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = " General", cat = "4. Aseo personal") 
    ) %>% 
    arrange(cat, tipo)

ifelse(
    v_quincena == 1,
    d_04_pacic <- d_04_pacic %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        glimpse,
    d_04_pacic %>% 
        glimpse
)

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo.\n**La línea punteada representa la implementación del PACIC."
v_pacic_loop <- unique(d_04_pacic$cat)

for(i in 1:4){
    
    d_plot <- d_04_pacic %>% 
        filter(cat == v_pacic_loop[i]) %>% 
        arrange(date) %>% 
        group_by(cat,tipo) %>% 
        mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
        ungroup() %>% 
        filter(date >= "2015-01-01")
    
    titulo <- paste0("Índice de precios al consumidor de canasta PROFECO\n",
                     v_pacic_loop[i])
    g <- 
        ggplot(data = d_plot,
               aes(
                   x = date,
                   y = values,
                   group = tipo,
                   col = tipo,
                   label = ifelse(
                       date == max(date),
                       paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                       NA
                   )
               ))+
        geom_vline(xintercept = as.Date("2022-05-01"), col = mcv_semaforo[4], size = 2,
                   linetype = 2) +
        geom_line(size = 2.5, lineend = "round", show.legend = F, 
                  aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                      linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
        ggrepel::geom_text_repel(
            aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
            nudge_x = 100, direction = "y", hjust = "left",
            size = 4,
            segment.curvature = -0.1,
            segment.ncp = 3,
            segment.angle = 20,
            family = "Ubuntu", fontface = "bold", show.legend = F
        ) +
        geom_point(aes(
            color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
            y = ifelse(date == max(date), values, NA)),
            size = 4, show.legend = F) +
        scale_color_manual(
            "", 
            values = c(mcv_semaforo[4], mcv_discrete_7, mcv_blacks[2])
        ) +
        scale_x_date(
            date_labels = "%b %y",
            breaks = seq.Date(from = floor_date(as.Date("2015-01-01")+(((month(max(d_04_pacic$date))))+91), "month"), 
                              to = floor_date(as.Date(max(d_04_pacic$date)), "month"), 
                              by = "6 month"),
            expand = expansion(mult = c(0.02, 0.15))
        ) +
        #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
        scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
        theme_minimal() +
        labs(
            title = titulo,
            subtitle = subtitulo, caption = nota,
            color="", shape="", y = eje_y
        ) +
        theme(plot.title = element_text(size = 35, face = "bold", colour = "#6950D8"),
              plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
              plot.caption = element_text(size = 25, colour = "#777777"),
              plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(family = "Ubuntu"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 25),
              axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
              axis.text.y = element_text(size = 20),
              legend.text = element_text(size = 30),
              legend.position = "none")
    
    g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
    ggsave(g, filename = paste_info(
        paste0("01_03_02_13_0", 
               tolower(str_replace_all(str_remove_all(v_pacic_loop[i], "\\."), " ", "_")),
               "_canasta_profeco.png")), 
        # type = "cairo", device = "png", 
        width = 16, height = 9,  dpi = 200, bg= "transparent")
    
}

rm(list=ls(pattern="^v_pacic"))



# FIN. -------------------------------------------------------------------------
