# Catálogo de productos del INPC
# https://www.inegi.org.mx/app/indicesdeprecios/Estructura.aspx?idEstructura=112001300040&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=INPC%20Nacional%20


#Sys.sleep((60*60*4)+(5))
Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

####################################################
# Seleccionar quincena 
v_quincena <- 2
####################################################

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
                     "#00D2D1", "#FF43FA", mcv_blacks[4],
                     mcv_blacks[3], mcv_blacks[2])


# Identificadores INEGI ----
source(paste_code("00_token.R"))

# # Vieja versión: 
# d_inpc_complete <- readxl::read_excel(paste_inp("01_03_inpc_complete.xlsx")) %>% 
#     glimpse
d_inpc_complete <- readxl::read_excel(paste_inp("01_03_inpc_complete_NewVersion.xlsx")) %>% 
    glimpse


d_inpc_complete

d_inpc <- data.frame()

#de aquí
library(readxl)


if (v_quincena == 2) {
    INPC_all_series <- read_excel("01_datos_crudos/INPC_mensual_indices.xlsx") %>%
        mutate(nombre = tolower(nombre))
} else if (v_quincena == 1) {
    INPC_all_series <- read_excel("01_datos_crudos/INPC_quincenal_indices.xlsx") %>%
        mutate(nombre = tolower(nombre))
}



d_inpc_ccif_ids <- "es la modificación del NEW con los ccif, etc."

#####termina ----

df_raw <- INPC_all_series %>% 
    mutate(code = case_when(
        nombre == "índice general" ~ "000",
        TRUE ~ code
    ))%>% 
    filter(!nombre %in% c("no duradero", "duradero", "servicios", "semiduradero"))

any(is.na(df_raw$code)) #tiene que salir FALSE

## 4.1. Clasificación del consumo individual por finalidades(CCIF) ----

nota <- "*Las desagregaciones del INPC solo tienen valor informativo."

# General, Alimentos, Vivienda, Salud, Servicios de transporte de pasajeros, servicios educativos, "Restaurantes y servicios de alojamiento"

#selecciona 
targets <- tribble(
    ~nombre,                                                             ~code,   ~nombre_clean,
    "índice general",                                                             "000",   "General",
    "alimentos",                                                                  "1.1.",  "Alimentos",
    "vivienda",                                                                   "3.",    "Vivienda",
    "salud",                                                                      "5.1.",  "Salud",
    "transporte público",                                                         "6.1.",  "Transporte público",
    "educación y esparcimiento",                                                  "7.",    "Educación",
    "servicios de alojamiento temporal y de preparación de alimentos y bebidas",  "72.",   "Servicios de alojamiento y restaurantes"
)

# 2. Use inner_join to keep ONLY those exact matches
df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

titulo <- "Índice de precios al consumidor por clasificación del \nconsumo individual por finalidades seleccionadas"
ifelse(
    v_quincena == 1, 
    subtitulo <- paste0(
        "A la 1ª quincena de ", 
        month(max(INPC_all_series$date), label = T, abbr = F),
        " de ", year(max(INPC_all_series$date)), 
        "; [tasa de variación anual]"
    ),
    subtitulo <- paste0(
        str_to_sentence(month(max(INPC_all_series$date), label = T, abbr = F)),
        " de ", year(max(INPC_all_series$date)),
        "; [tasa de variación anual]"
    )
)

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
                   paste0(str_wrap(tipo,18), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
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
        segment.ncp = 5,
        force = 10,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "General", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "",
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to =floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))-1))), "month"), 
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_01_ccif.png"), 
       width = 16, height = 9, 
       #type = "cairo", device = "png", 
       dpi = 200, bg= "transparent")

# Formato .svg para documento traducido al inglés 
ggsave(g, filename = paste_info("99_svg/01_03_02_01_ccif.svg"),
       width = 16, height = 9,
       dpi = 200, bg= "transparent")



## 4.2. Alimentos ----
### 4.2.1. Pan y cereales ----

#Alimentos 1.1. , Pan y cereales, harinas de trigo 005, , maíz 006, pan de caja 009, pan dulce 010, tortilla de maíz 014
#selecciona
targets <- tribble(
    ~nombre,                                 ~code,   ~nombre_clean,
    "alimentos",                             "1.1.",  "Alimentos",
    "harinas de trigo",                      "005",   "Harinas de trigo",
    "maíz",                                  "006",   "Maíz",
    "pan de caja",                           "009",   "Pan de caja",
    "pan dulce",                             "010",   "Pan dulce",
    "tortilla de maíz",                      "014",   "Tortilla de maíz"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de pan y\ncereales seleccionados"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
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
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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


g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_01_ali_pan_cer.png"), 
       #device = "png", type = "cairo", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.2. Carnes ----

#alimentos1.1.,carnes 1.1.2, carne de res 018, carne de cerdo 017, pollo 022
#selecciona
targets <- tribble(
    ~nombre,              ~code,    ~nombre_clean,
    "alimentos",          "1.1.",   "Alimentos",
    "carnes",             "1.1.2.", "Carnes",
    "carne de res",       "018",    "Res",
    "carne de cerdo",     "017",    "Cerdo",
    "pollo",              "022",    "Pollo"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar


titulo <- "Índice de precios al consumidor de carnes\nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
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
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))

ggsave(g, filename = paste_info("02_02_02_ali_carnes.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")



### 4.2.3. Leche, quesos y huevo ----

#selecciona
targets <- tribble(
    ~nombre,                                    ~code,    ~nombre_clean,
    "alimentos",                                "1.1.",   "Alimentos",
    "leche, derivados de leche y huevo",        "1.1.4.", "Lácteos y huevo",
    "leche pasteurizada y fresca",              "034",    "Leche",
    "queso oaxaca y asadero",                   "039",    "Queso Oaxaca y asadero",
    "huevo",                                    "031",    "Huevo"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de lácteos y\nhuevo seleccionados"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
                   paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                   NA
               )
           ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_03_ali_lácteos.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.4. Frutas ----

#alimentos 1.1., frutas frescas 16 , aguacate 045, manzana 049, pera 053, plátanos 055, uva 057, 
#selecciona
targets <- tribble(
    ~nombre,              ~code,   ~nombre_clean,
    "alimentos",          "1.1.",  "Alimentos",
    "frutas frescas",     "16", "Frutas frescas",
    "aguacate",           "045",   "Aguacate",
    "manzana",            "049",   "Manzana",
    "pera",               "053",   "Pera",
    "plátanos",           "055",   "Plátanos",
    "uva",                "057",   "Uva"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar


titulo <- "Índice de precios al consumidor de frutas \nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
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
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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


g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_04_ali_frutas.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.5. Legumbres ----
#alimentos 1.1., frutas y horatlizas 1.1.6 "Calabacita" 060, "Chile serrano" 065, "Jitomate" 070, "Tomate verde" 076, nopales 072, "Papa y otros tubérculos" 073
#selecciona
targets <- tribble(
    ~nombre,                        ~code,    ~nombre_clean,
    "alimentos",                    "1.1.",   "Alimentos",
    "frutas y hortalizas",          "1.1.6.", "Frutas y hortalizas",
    "calabacita",                   "060",    "Calabacita",
    "chile serrano",                "065",    "Chile serrano",
    "jitomate",                     "070",    "Jitomate",
    "tomate verde",                 "076",    "Tomate verde",
    "nopales",                      "072",    "Nopales",
    "papa y otros tubérculos",      "073",    "Papa"
)
df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de legumbres\nseleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
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
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_05_ali_legum.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.6. Aceites y grasas ----
#alimentos 1.1., "Aceites y grasas vegetales comestibles" 042, "Manteca de cerdo" 043, mantequilla 044
targets <- tribble(
    ~nombre,                                         ~code,    ~nombre_clean,
    "alimentos",                                     "1.1.",   "Alimentos",
    "aceites y grasas vegetales comestibles",        "042",    "Aceite vegetal",
    "manteca de cerdo",                              "043",    "Manteca de cerdo",
    "mantequilla",                                   "044",    "Mantequilla"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de aceites\ny grasas seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
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
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_06_ali_aceites.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.7. Azúcares ----
#alimentos 1.1., azúcar 082, "Chocolate y productos de confitería" 084, "Helados, nieves y paletas de hielo" 086, "Gelatina, miel y mermeladas" 085
#seleccionar
targets <- tribble(
    ~nombre,                                            ~code,    ~nombre_clean,
    "alimentos",                                        "1.1.",   "Alimentos",
    "azúcar",                                           "082",    "Azúcar",
    "chocolate y productos de confitería",              "084",    "Chocolates",
    "helados, nieves y paletas de hielo",               "086",    "Helados",
    "gelatina, miel y mermeladas",                      "085",    "Mermeladas y miel"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de\nazúcares seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = df_filtered %>% 
               unique() %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = tipo,
               col = tipo,
               label = ifelse(
                   fecha == max(fecha),
                   paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
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
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1L),
                       expand = expansion(c(0.3, 0.3))
                       # limits = c(50,150)
    ) +
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_02_07_ali_azucar.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.8 Frutas y verduras seleccionadas ----
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
# \n**La línea punteada representa la implementación del PACIC.

#seleccionar
targets <- tribble(
    ~nombre,                          ~code,   ~nombre_clean,
    "índice general",                 "000",   "General",
    "limón",                          "048",   "Limón",
    "cebolla",                        "061",   "Cebolla",
    "plátanos",                       "055",   "Plátanos",
    "jitomate",                       "070",   "Jitomate",
    "chile serrano",                  "065",   "Chile serrano",
    "zanahoria",                      "078",   "Zanahoria",
    "manzana",                        "049",   "Manzana",
    "papa y otros tubérculos",        "073",   "Papa"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar


g <- ggplot(data = df_filtered %>% 
           unique() %>% 
           arrange(fecha) %>% 
           group_by(tipo) %>% 
           mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
           filter(fecha >= "2015-06-01"),
       aes(
           x = fecha,
           y = values,
           group = tipo,
           col = tipo,
           label = ifelse(
               fecha == max(fecha),
               paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
               NA
           )
       ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "General", "solid", "dashed"))) + 
    geom_point(aes(
        color = if_else(tipo == "General", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    scale_discrete_manual(aesthetics = "linewidth", values = c(1,2)) + 
    scale_discrete_manual(aesthetics = "fontface", values = c("bold","bold")) + 
    scale_color_manual(values = c("General" = "#4D5BF0",
                                  "Limón" = "#0ACF5F",
                                  "Manzana" = "#E84D9A",
                                  "Plátanos" = "#E8B32E",
                                  "Chile serrano" = "#974DF0",
                                  "Cebolla" = "gray30",
                                  "Jitomate" = "#ff6260",
                                  "Papa" = "#0A93C4",
                                  "Zanahoria" = "#E8866D")) + 
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"),
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"),
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) + 
    scale_y_continuous(expand = expansion(c(0.1, 0.1))) + 
    theme_minimal() +
    labs(
        title = "Índice de precios al consumidor de\nfrutas y verduras seleccionadas",
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

g

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_09_01_indice_de_precios_frutas_y_verduras_seleccionadas.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")

### 4.2.9 Frutas y legumbres ----

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
# \n**La línea punteada representa la implementación del PACIC.

#seleccionar
targets <- tribble(
    ~nombre,                    ~code,     ~nombre_clean,
    "índice general",           "000",     "General",
    "alimentos",                "1.1.",    "Alimentos",
    "frutas frescas",           "16",  "Frutas",
    "hortalizas frescas",       "17",  "Hortalizas",
    "legumbres secas",           "18",  "Legumbres secas",
    "frutas y legumbres procesadas","19",  "Frutas y legumbres procesadas",
    "frutas y hortalizas",      "1.1.6.",  "Frutas y hortalizas"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar


g <- ggplot(data = df_filtered %>% 
                unique() %>% 
                arrange(fecha) %>% 
                group_by(tipo) %>% 
                mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
                filter(fecha >= "2015-06-01"),
            aes(
                x = fecha,
                y = values,
                group = tipo,
                col = tipo,
                label = ifelse(
                    fecha == max(fecha),
                    paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                    NA
                )
            ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "General", "solid", "dashed"))) + 
    geom_point(aes(
        color = if_else(tipo == "General", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    scale_discrete_manual(aesthetics = "linewidth", values = c(2,3)) + 
    scale_discrete_manual(aesthetics = "alpha", values = c(0.8,0.95)) + 
    scale_discrete_manual(aesthetics = "fontface", values = c("bold","bold")) + 
    scale_color_manual(
        values = c(
            "General"                       = "#4D5BF0",
            "Alimentos"                     = mcv_semaforo[4],
            "Frutas"                        = "#E84D9A",
            "Hortalizas"                    = "#0ACF5F",
            "Legumbres secas"               = "#E8B32E",
            "Frutas y legumbres procesadas" = "#974DF0",
            "Frutas y hortalizas"           = "#f99260"
        )
    )+
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"),
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"),
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) + 
    scale_y_continuous(expand = expansion(c(0.1, 0.1))) + 
    theme_minimal() +
    labs(title = "Índice de precios al consumidor de frutas y\nlegumbres",
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

g

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_10_01_indice_de_precios_frutas_y_legumbres.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.3. Bebidas no alcohólicas ----
#café 21, agua embotellada 099, 095 Jugos o néctares envasados, refrescos 100, 

targets <- tribble(
    ~nombre,                        ~code,  ~nombre_clean,
    "alimentos",                    "1.1.", "Alimentos",
    "café",                         "21",   "Café",
    "agua embotellada",             "099",  "Agua embotellada",
    "jugos o néctares envasados",   "095",  "Jugos o néctares",
    "refrescos envasados",          "100",  "Refrescos"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

titulo <- "Índice de precios al consumidor de bebidas\nno alcohólicas seleccionadas"
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- ggplot(data = df_filtered %>% 
                unique() %>% 
                arrange(fecha) %>% 
                group_by(tipo) %>% 
                mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
                filter(fecha >= "2015-06-01"),
            aes(
                x = fecha,
                y = values,
                group = tipo,
                col = tipo,
                label = ifelse(
                    fecha == max(fecha),
                    paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                    NA
                )
            ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Alimentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 6,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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


g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
ggsave(g, filename = paste_info("02_03_bebidas.png"),
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.5. Productos de salud --------------------------------------------------------

# ---- Enlistar productos del catálogo
targets <- tribble(
    ~nombre,                          ~code,  ~nombre_clean,
    "salud",                      "5.1.", "Salud", # Referencia para comparar
    "medicamentos",                   "55",   "Medicamentos",
    "analgésicos",                    "184",  "Analgésicos",
    "antibióticos",                   "185",  "Antibióticos",
    "antigripales",                   "186",  "Antigripales",
    "cardiovasculares",               "188",  "Cardiovasculares",
    "medicamentos para diabetes",     "193",  "Diabetes"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de productos\nfarmacéuticos seleccionados"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_filtered %>% 
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = tipo,
            col = tipo,
            label = ifelse(
                fecha == max(fecha),
                paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Salud", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Salud" | tipo == "Medicamentos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Salud", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Salud", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    scale_y_continuous(
        labels = scales::number_format(accuracy = 1L)
    ) +
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))

ggsave(g, filename = paste_info("02_05_farma.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 4.7. Servicios de hospital -----------------------------------------------------

# salud 5.1., servicios médicos 57,  201 Atención médica durante el parto, 202 Hospitalización general, 203 Hospitalización parto,  204 Operación quirúrgica
targets <- tribble(
    ~nombre,                                ~code,   ~nombre_clean,
    "salud",                                "5.1.",  "Salud",
    "servicios médicos",                    "57",    "Servicios Médicos",
    "atención médica durante el parto",     "201",   "Atención durante el parto",
    "hospitalización general",              "202",   "Hospitalización",
    "hospitalización parto",                "203",   "Hospitalización (parto)",
    "operación quirúrgica",                 "204",   "Cirugía"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar


# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de servicios\nde hospital"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"


g <- 
    ggplot(
        df_filtered %>% 
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = tipo,
            col = tipo,
            label = ifelse(
                fecha == max(fecha),
                paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == "Salud", mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == "Salud" | tipo == "Servicios Médicos", "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == "Salud", mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Salud", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month") , 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))

ggsave(g, filename = paste_info("02_07_hospitales.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.8. Productos de la peda ------------------------------------------------------

#tequila 105, cerveza 108, cigarillos 109, papas fritas 074, analgésicos 184
#selecciona
targets <- tribble(
    ~nombre,              ~code,   ~nombre_clean,
    "tequila",            "105",   "Tequila",
    "cerveza",            "108",   "Cerveza",
    "cigarrillos",        "109",   "Cigarros",
    "papas fritas",       "074",   "Papas fritas",
    "analgésicos",        "184",   "Analgésicos"
)

df_filtered <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre)

unique(df_filtered$nombre) #verificar

# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de productos\npara la fiesta "
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_filtered %>% 
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = tipo,
            col = tipo,
            label = ifelse(
                fecha == max(fecha),
                paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              
    ) + 
    ggrepel::geom_text_repel(
        nudge_x = 100, direction = "y", hjust = "left",
        size = 5,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_filtered$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_filtered$fecha))))), "month"), 
                          by = "-6 month"),
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

g
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))

ggsave(g, filename = paste_info("02_08_peda.png"),
       #type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 4.13. Productos canasta básica PROFECO ----

targets <- tribble(
    ~nombre,                                                ~code,    ~nombre_clean,          ~categoria,
    "índice general",                                              "000",    "General",       "General",
    
    # 1. Origen Animal
    "leche pasteurizada y fresca",                          "11",     "Leche",                "1.Origen Animal",
    "atún y sardina en lata",                               "026",     "Atún y Sardina",       "1.Origen Animal",
    "carne de cerdo",                                       "017",    "Cerdo",                "1.Origen Animal",
    "carne de res",                                         "018",    "Res",                  "1.Origen Animal",
    "pollo",                                                "022",    "Pollo",                "1.Origen Animal",
    "huevo",                                                "031",    "Huevo",                "1.Origen Animal",
    
    # 2. Despensa
    "frijol",                                               "068",    "Frijol",               "2.Despensa",
    "arroz",                                                "001",    "Arroz",                "2.Despensa",
    "pan de caja",                                          "009",    "Pan de caja",          "2.Despensa",
    "pasta para sopa",                                      "011",    "Pasta para sopa",      "2.Despensa",
    "tortilla de maíz",                                     "014",    "Tortilla de maíz",     "2.Despensa",
    "aceites y grasas vegetales comestibles",               "042",    "Aceite vegetal",       "2.Despensa",
    "azúcar",                                               "082",    "Azúcar",               "2.Despensa",
    
    # 3. Frutas y Verduras
    "cebolla",                                              "061",    "Cebolla",              "3.Frutas y Verduras",
    "chiles envasados",                                     "066",    "Chiles envasados",     "3.Frutas y Verduras",
    "jitomate",                                             "070",    "Jitomate",             "3.Frutas y Verduras",
    "limón",                                                "048",    "Limón",                "3.Frutas y Verduras",
    "manzana",                                              "049",    "Manzana",              "3.Frutas y Verduras",
    "plátanos",                                             "055",    "Plátanos",             "3.Frutas y Verduras",
    "zanahoria",                                            "078",    "Zanahoria",            "3.Frutas y Verduras",
    "papa y otros tubérculos",                              "073",    "Papa",                 "3.Frutas y Verduras",
    
    # 4. Aseo Personal
    "jabón de tocador",                                     "277",    "Jabón de tocador",                     "4.Aseo Personal",
    "papel higiénico y pañuelos desechables, papel higiénico */",            "281",    "Papel higiénico",     "4.Aseo Personal", # Se repite código en la imagen
    "papel higiénico y pañuelos desechables, pañuelos desechables */",            "281",    "Pañuelos desechables","4.Aseo Personal" # Se repite código en la imagen
)

df_filtered_profeco <- df_raw %>%
    inner_join(targets, by = c("nombre", "code"))%>%
    mutate(valor = as.numeric(valor))%>%
    filter(date > "2002-06-01")%>%
    select(values=valor,fecha=date, tipo=nombre_clean, code, nombre, categoria)

unique(df_filtered_profeco$nombre) #verificar


eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo.\n**La línea punteada representa la implementación del PACIC."

categorias <- c("1.Origen Animal", "2.Despensa", "3.Frutas y Verduras", "4.Aseo Personal")

for(i in seq_along(categorias)){
    
    # 1. Filtrar datos (Incluyendo siempre el General)
    d_plot <- df_filtered_profeco %>% 
        filter(categoria == categorias[i] | categoria == "General") %>% 
        group_by(tipo) %>% 
        mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
        filter(fecha >= "2015-06-01")
    
    # 2. Nombre del archivo (01, 02, 03, 04)
    num <- sprintf("%02d", i)
    nombre_clean <- categorias[i] %>% 
        tolower() %>% 
        str_remove("^[0-9]\\.") %>% 
        str_replace_all(" ", "_") %>% 
        chartr("áéíóú", "aeiou", .) # Quita acentos rápido
    
    file_name <- paste0("02_13_", num, "_", nombre_clean, "_canasta_profeco.png")
    
        g <- ggplot(data = d_plot,
                aes(x = fecha,
                    y = values,
                    group = tipo,
                    col = tipo,
                    label = ifelse(
                        fecha == max(fecha),
                        paste0(str_wrap(tipo, 14), "\n", round(values, 1), 
                               " [", scales::percent(tasa_anual, accuracy = 0.01), "]"),
                        NA
                    ))) +
        # PACIC Vertical Line
        geom_vline(xintercept = as.Date("2022-05-01"), col = mcv_semaforo[4], linewidth = 1.5,
                   linetype = 2) +
        # Lines: Solid for General, Dashed for others
        geom_line(linewidth = 2.5, lineend = "round", show.legend = F, 
                  aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo),
                      linetype = if_else(tipo == "General", "dashed", "solid"))) + 
        # Labels
        ggrepel::geom_text_repel(
            aes(color = if_else(tipo == "General", mcv_semaforo[4], tipo)), 
            nudge_x = 100, direction = "y", hjust = "left",
            size = 4.5,
            segment.color = NA,
            family = "Ubuntu", fontface = "bold", show.legend = F
        ) +
        # End Points
        geom_point(aes(
            color = if_else(tipo == "General", mcv_semaforo[4], tipo),
            y = ifelse(fecha == max(fecha), values, NA)),
            size = 4, show.legend = F) +
        # Colors
        scale_color_manual(values = c(mcv_semaforo[4], mcv_discrete_7, mcv_blacks[2])) +
        scale_linetype_identity() +
        # X Axis
        scale_x_date(
            date_labels = "%b %y",
            breaks = seq.Date(from = as.Date("2015-06-01"), 
                              to = max(d_plot$fecha), 
                              by = "6 month"),
            expand = expansion(mult = c(0.02, 0.2))
        ) +
        scale_y_continuous(labels = scales::number_format(accuracy = 1L)) +
        theme_minimal() +
        labs(title = str_wrap(paste0("Índice de precios al consumidor de canasta PROFECO\n", categorias[i]),45),
             y = eje_y, 
             subtitle = subtitulo, caption = nota) +
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
    
    g_final <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.png"))
    ggsave(g_final, filename = paste_info(file_name), width = 16, height = 9, dpi = 200)}

# FIN. -------------------------------------------------------------------------

