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

# Catálogo de productos del INPC
# https://www.inegi.org.mx/app/indicesdeprecios/Estructura.aspx?idEstructura=112001300040&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=INPC%20Nacional%20


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
# v_usuaria <- "regina"
# v_usuaria <- "katia"
# googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
# googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

## Funciones ----
# Directorios de las carpetas 
paste_inp   <- function(x){paste0("01_datos_crudos/" , x)}
paste_out   <- function(x){paste0("02_datos_limpios/", x)}
paste_code  <- function(x){paste0("03_códigos/"      , x)}
paste_info  <- function(x){paste0("04_infobites/"    , x)}


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

## 1.1. INPC lomo de cerdo ----
### 1.1.1. Peso en el índice ----
v_carne_cerdo_cantidad <- 150
v_naranja_cantidad <- 66
v_mostaza_cantidad <- 3
v_vino_de_mesa_cantidad <- 30
v_otros_condimentos_cantidad <- 17.5
v_cebolla_cantidad <- 50
v_sal_cantidad <- 15

### 1.1.2. Etiquetas para filtros ----
v_carne_cerdo_filtro <- "Carne de cerdo"
v_naranja_filtro <- "Naranja"
v_mostaza_filtro <- "Mayonesa y mostaza" 
v_vino_de_mesa_filtro <- "Vino de mesa" 
v_otros_condimentos_filtro <- "Otros condimentos" 
v_cebolla_filtro <- "Cebolla"
v_sal_filtro <- "Concentrados de pollo y sal"

### 1.1.3 Consolidación de base de datos ----
v_quincena <- 1
v_fecha_10_anio <- max(d_inpc$date)-(365.2*10)

if(v_quincena == 1){
    
    d_lomo_de_cerdo <- d_inpc %>% 
        filter(ccif == v_carne_cerdo_filtro) %>% 
        mutate(cantidad = v_carne_cerdo_cantidad) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_naranja_filtro) %>% 
                mutate(cantidad = v_naranja_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_mostaza_filtro) %>% 
                mutate(cantidad = v_mostaza_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_vino_de_mesa_filtro) %>% 
                mutate(cantidad = v_vino_de_mesa_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_condimentos_filtro) %>% 
                mutate(cantidad = v_otros_condimentos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_cebolla_filtro) %>% 
                mutate(cantidad = v_cebolla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_sal_filtro) %>% 
                mutate(cantidad = v_sal_cantidad)
        ) %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        arrange(date) %>% 
        group_by(ccif) %>% 
        mutate(
            suma_pond = values*cantidad
        ) %>% 
        group_by(date, date_shortcut) %>% 
        summarise(suma_ponderada = sum(values, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(suma_ponderada_base = ifelse(date == v_fecha_10_anio, suma_ponderada, NA)) %>% 
        fill(suma_ponderada_base,.direction = "downup") %>% 
        mutate(value_index = suma_ponderada/suma_ponderada_base*100) %>% 
        glimpse
    
} else{
    
    d_lomo_de_cerdo <- d_inpc %>% 
        filter(ccif == v_carne_cerdo_filtro) %>% 
        mutate(cantidad = v_carne_cerdo_cantidad) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_naranja_filtro) %>% 
                mutate(cantidad = v_naranja_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_mostaza_filtro) %>% 
                mutate(cantidad = v_mostaza_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_vino_de_mesa_filtro) %>% 
                mutate(cantidad = v_vino_de_mesa_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_condimentos_filtro) %>% 
                mutate(cantidad = v_otros_condimentos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_cebolla_filtro) %>% 
                mutate(cantidad = v_cebolla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_sal_filtro) %>% 
                mutate(cantidad = v_sal_cantidad)
        ) %>% 
        arrange(date) %>% 
        group_by(ccif) %>% 
        mutate(
            suma_pond = values*cantidad
        ) %>% 
        group_by(date, date_shortcut) %>% 
        summarise(suma_ponderada = sum(values, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(suma_ponderada_base = ifelse(date == v_fecha_10_anio, suma_ponderada, NA)) %>% 
        fill(suma_ponderada_base,.direction = "downup") %>% 
        mutate(value_index = suma_ponderada/suma_ponderada_base*100) %>% 
        glimpse
    
}

## 1.1. INPC Pierna de cerdo ----
### 1.1.1. Peso en el índice ----
v_carne_cerdo_cantidad <- 600
v_cebolla_cantidad <- 100
v_otras_conservas_de_frutas_cantidad <- 50
v_otras_legumbres_secas_cantidad <- 30
v_otras_frutas_cantidad <- 30
v_vino_de_mesa_cantidad <- 30
v_otros_cultivos_cantidad <- 20
v_sal_cantidad <- 15
v_azucar_cantidad <- 15
v_mantequilla_cantidad <- 25
v_jugos_nectares_envasados_cantidad <- 30
v_otros_condimentos_cantidad <- 17.5


### 1.1.2. Etiquetas para filtros ----
v_carne_cerdo_filtro <- "Carne de cerdo"
v_cebolla_filtro <- "Cebolla"
v_vino_de_mesa_filtro <- "Vino de mesa" 
v_otros_condimentos_filtro <- "Otros condimentos" 
v_sal_filtro <- "Concentrados de pollo y sal"
v_otras_conservas_de_frutas_filtro <- "Otras conservas de frutas"
v_otras_legumbres_secas_filtro <- "Otras legumbres secas"
v_otras_frutas_filtro <- "Otras frutas"
v_otros_cultivos_filtro <- "Otros cultivos de hortalizas"
v_azucar_filtro <- "Azúcar"
v_mantequilla_filtro <- "Mantequilla"
v_jugos_nectares_envasados_filtro <- "Jugos o néctares envasados"


### 1.1.3 Consolidación de base de datos ----
v_quincena <- 1
if(v_quincena == 1){
    
    d_pierna_de_cerdo <- d_inpc %>% 
        filter(ccif == v_carne_cerdo_filtro) %>% 
        mutate(cantidad = v_carne_cerdo_cantidad) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_vino_de_mesa_filtro) %>% 
                mutate(cantidad = v_vino_de_mesa_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_condimentos_filtro) %>% 
                mutate(cantidad = v_otros_condimentos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_cebolla_filtro) %>% 
                mutate(cantidad = v_cebolla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_sal_filtro) %>% 
                mutate(cantidad = v_sal_cantidad)
        ) %>% 
        
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_conservas_de_frutas_filtro) %>% 
                mutate(cantidad = v_otras_conservas_de_frutas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_legumbres_secas_filtro) %>% 
                mutate(cantidad = v_otras_legumbres_secas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_frutas_filtro) %>% 
                mutate(cantidad = v_otras_frutas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_cultivos_filtro) %>% 
                mutate(cantidad = v_otros_cultivos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_azucar_filtro) %>% 
                mutate(cantidad = v_azucar_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_mantequilla_filtro) %>% 
                mutate(cantidad = v_mantequilla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_jugos_nectares_envasados_filtro) %>% 
                mutate(cantidad = v_jugos_nectares_envasados_cantidad)
        ) %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        arrange(date) %>% 
        group_by(ccif) %>% 
        mutate(
            suma_pond = values*cantidad
        ) %>% 
        group_by(date, date_shortcut) %>% 
        summarise(suma_ponderada = sum(values, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(suma_ponderada_base = ifelse(date == v_fecha_10_anio, suma_ponderada, NA)) %>% 
        fill(suma_ponderada_base,.direction = "downup") %>% 
        mutate(value_index = suma_ponderada/suma_ponderada_base*100) %>% 
        glimpse
    
} else{
    
    d_pierna_de_cerdo <- d_inpc %>% 
        filter(ccif == v_carne_cerdo_filtro) %>% 
        mutate(cantidad = v_carne_cerdo_cantidad) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_vino_de_mesa_filtro) %>% 
                mutate(cantidad = v_vino_de_mesa_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_condimentos_filtro) %>% 
                mutate(cantidad = v_otros_condimentos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_cebolla_filtro) %>% 
                mutate(cantidad = v_cebolla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_sal_filtro) %>% 
                mutate(cantidad = v_sal_cantidad)
        ) %>% 
        
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_conservas_de_frutas_filtro) %>% 
                mutate(cantidad = v_otras_conservas_de_frutas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_legumbres_secas_filtro) %>% 
                mutate(cantidad = v_otras_legumbres_secas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otras_frutas_filtro) %>% 
                mutate(cantidad = v_otras_frutas_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_otros_cultivos_filtro) %>% 
                mutate(cantidad = v_otros_cultivos_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_azucar_filtro) %>% 
                mutate(cantidad = v_azucar_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_mantequilla_filtro) %>% 
                mutate(cantidad = v_mantequilla_cantidad)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == v_jugos_nectares_envasados_filtro) %>% 
                mutate(cantidad = v_jugos_nectares_envasados_cantidad)
        ) %>% 
        arrange(date) %>% 
        group_by(ccif) %>% 
        mutate(
            suma_pond = values*cantidad
        ) %>% 
        group_by(date, date_shortcut) %>% 
        summarise(suma_ponderada = sum(values, na.rm = T)) %>% 
        ungroup() %>% 
        mutate(suma_ponderada_base = ifelse(date == v_fecha_10_anio, suma_ponderada, NA)) %>% 
        fill(suma_ponderada_base,.direction = "downup") %>% 
        mutate(value_index = suma_ponderada/suma_ponderada_base*100) %>% 
        glimpse
    
}

# 2. Infobite ----
## 2.1. Índice de la cena navideña ----
d_plot <- 
    d_lomo_de_cerdo %>% 
    mutate(tipo = "Lomo de cerdo") %>% 
    bind_rows(
        d_pierna_de_cerdo %>%
            mutate(tipo = "Pierna de cerdo")
    ) %>% 
    filter(date>=v_fecha_10_anio) %>% 
    group_by(tipo) %>% 
    mutate(tasa_anual = (value_index/lag(value_index, 12))-1) %>% 
    glimpse

titulo <- "Índice de la cena navideña MCV"

ifelse(
    v_quincena == 1, 
    subtitulo <- paste0(
        "A la 1ª quincena de ", 
        month(max(d_plot$date), label = T, abbr = F),
        " de ", year(max(d_plot$date)), 
        "; [tasa de variación anual]",
        "\n1ªq de ", as.yearmon(v_fecha_10_anio, "%B %Y"), " = 100"
    ),
    subtitulo <- paste0(
        str_to_sentence(month(max(d_plot$date), label = T, abbr = F)),
        " de ", year(max(d_plot$date)),
        "; [tasa de variación anual]",
        "\n1ªq de ", as.yearmon(v_fecha_10_anio, "%B %Y"), " = 100"
    )
)

eje_y <- "Valor del índice"
mcv_infobite <- c(mcv_semaforo[1], mcv_semaforo[4])

g <- 
    ggplot(
        d_plot,
        aes(
            x = date,
            y = value_index,
            group = tipo,
            col = tipo,
            label = ifelse(
                date == max(date),
                paste0(str_wrap(tipo,12), "\n", round(value_index,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        )
    ) +
    geom_line(size = 2)  +
    # geom_text(size = 10, family = "Ubuntu", fontface = "bold", hjust = -0.2, show.legend = F) +
    geom_point(size = 3, aes(y = ifelse(date==max(date), value_index, NA)))  +
    ggrepel::geom_text_repel(
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5, segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    scale_colour_manual("", values = mcv_infobite) +
    scale_x_date(labels = date_format("%b-%y"),
                 expand = expansion(mult = c(0.01, 0.1)),
                 breaks = seq.Date(from = v_fecha_10_anio, 
                                   to = max(d_inpc$date), 
                                   by = "6 month")) +
    scale_y_continuous(breaks = seq(90,210,10)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777"),
          plot.margin= margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 30),
          legend.position = "none")
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(filename = paste_info("99_tempo_navidad_index.png"), 
       width = 16, height = 9,
       dpi = 200, bg= "transparent")

## 2.2. Aumento de índice vs 2021, 2020, 2019 y 2018 ----

d_plot <- d_lomo_de_cerdo %>% 
    mutate(tipo = "Lomo de cerdo") %>% 
    bind_rows(
        d_pierna_de_cerdo %>%
            mutate(tipo = "Pierna de cerdo")
    ) %>% 
    filter(date>=v_fecha_10_anio) %>% 
    arrange(date) %>% 
    filter(month(date) == last(month(date))) %>% 
    select(tipo, date, value_index) %>% 
    group_by(tipo) %>% 
    mutate(
        tasa_anual_1 = (value_index/lag(value_index, 1))-1,
        tasa_anual_2 = (value_index/lag(value_index, 2))-1,
        tasa_anual_3 = (value_index/lag(value_index, 3))-1,
        tasa_anual_4 = (value_index/lag(value_index, 4))-1,
        tasa_anual_5 = (value_index/lag(value_index, 5))-1
    ) %>% 
    glimpse

titulo <- "Variación del índice de la cena navideña MCV,\nrespecto a distintos años"
eje_y <- "Variación porcentual"

ifelse(
    v_quincena == 1, 
    subtitulo <- paste0(
        "A la 1ª quincena de ", 
        month(max(d_plot$date), label = T, abbr = F),
        " de ", year(max(d_plot$date))
    ),
    subtitulo <- paste0(
        str_to_sentence(month(max(d_plot$date), label = T, abbr = F)),
        " de ", year(max(d_plot$date))
    )
)

g <- 
ggplot(
    d_plot %>% 
        pivot_longer(
            starts_with("tasa"),
            names_to = "anio",
            names_prefix = "tasa_anual_",
            values_to = "variación"
        ) %>% 
        filter(date == max(date)) %>% 
        mutate(
            anio_lab = case_when(
                anio == "1" ~ "2021",
                anio == "2" ~ "2020",
                anio == "3" ~ "2019",
                anio == "4" ~ "2018",
                anio == "5" ~ "2017"
            )
        ),
    aes(
        x = reorder(anio_lab, as.numeric(anio)),
        y = variación, 
        fill = tipo,
        label = percent(variación, accuracy = 0.1)
    )
) + 
    geom_col(position = position_dodge2(), width = 0.4) + 
    geom_text(
        col = "white",
        position = position_dodge2(width = 0.4),
        angle = 90,
        hjust = 1.2, family = "Ubuntu", fontface = "bold",
        size = 6
    )  +
    scale_fill_manual("", values = mcv_infobite) +
    scale_y_continuous(labels = percent_format(accuracy = 1L)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        color="", shape="", y = eje_y
    ) +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777"),
          plot.margin= margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 25),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 30),
          legend.position = "top")
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(filename = paste_info("99_tempo_navidad_index_variaciones.png"), 
       width = 16, height = 9,
       dpi = 200, bg= "transparent")
