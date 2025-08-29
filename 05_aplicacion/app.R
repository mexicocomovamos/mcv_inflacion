# Opciones ----
Sys.setlocale("LC_TIME", "es_ES")
# Sys.setlocale("LC_TIME", "es_ES.UTF-8")
# Sys.setlocale("LC_TIME", "español")

options(scipen=999)

# Librerias ----
library(tidyverse)
library(ggrepel)
library(scales)
library(DT)
library(shinycssloaders)
library(openxlsx)


# 01. Datos ----

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

# Ponderadores y claves: 
# d_inpc_complete <- readxl::read_excel("01_datos_crudos/01_03_inpc_complete_NewVersion.xlsx") %>% 
#         mutate(ponderador = ponderador_inpc_id_ccif_1) %>%
#         mutate(ponderador = ifelse(is.na(ponderador), yes = ponderador_inpc_id_ccif_2, no = ponderador)) %>%
#         mutate(ponderador = ifelse(is.na(ponderador), yes = ponderador_inpc_id_ccif_3, no = ponderador)) %>%
#         mutate(ponderador = ifelse(is.na(ponderador), yes = ponderador_inpc_id_ccif_4, no = ponderador))

# Datos mensuales 

datos_m <- readRDS("total_datos_inflacion_mes.rds") %>% 
    # readxl::read_xlsx("total_datos_inflacion_mes.xlsx") %>% 
    as_tibble() %>% 
    mutate(date_shortcut2 = as.numeric(str_extract(date_shortcut, pattern = "\\d"))) %>% 
    mutate(quincena = 2) %>% 
    unique() %>% 
    mutate(date = as.POSIXct(date,format="%Y-%m-%d"))

class(datos_m$date)

# Datos quincenales 
datos_q <- readRDS("total_datos_inflacion_quincenas.rds") %>% 
    # readxl::read_xlsx("total_datos_inflacion_quincenas.xlsx") %>% 
    as_tibble() %>% 
    mutate(date_shortcut2 = as.numeric(str_extract(date_shortcut, pattern = "\\d+"))) %>% 
    mutate(quincena = ifelse((date_shortcut2 %% 2 == 0), yes = 2, no = 1))  %>% 
    unique() %>% 
    mutate(date = as.POSIXct(date,format="%Y-%m-%d"))

# datos_q$date2 <- NA_POSIXct_
datos_q$date[datos_q$quincena == 1] <- datos_q$date[datos_q$quincena == 1] + days(15)
datos_q$date[datos_q$quincena == 2] <- datos_q$date[datos_q$quincena == 2] + days(28)

# Fecha máxima y quincena máxima: 
# maxima_fecha <- datos$date %>% max() # Obtiene la fecha máxima con información disponible
# v_quincena   <- datos %>% filter(date == maxima_fecha) %>% pull(quincena) %>% max() # Obtiene la quincena máxima con información disponible

# 02. Controles: ----
sel_genericos <- unique(datos_m$ccif) %>% sort() # Obtenemos el vector de genéricos para generar el control

# 03. Gráficas ----

## 03.1 Gráfica de evolución ----

# Argumentos de prueba
# genericos = c("Renta de vivienda")
# tipo_datos = "Datos quincenales"
# fecha_inicio = "2018-12-01"
gen_grafica <- function(genericos, 
                        fecha_inicio = "2018-12-01", 
                        tipo_datos = "Datos quincenales", 
                        tipo_grafica = "Evolución niveles"){
    
    # eje_y <- "Índice base\n2ª quincena de julio 2018 = 100"
    nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
    
    if(tipo_datos == "Datos mensuales"){
        
        datos = datos_m
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        subtitulo <- str_c("Al mes de ", str_replace_all(month(maxima_fecha),
                                                         c("^1$" = "enero", 
                                                           "^2$" = "febrero", 
                                                           "^3$" = "marzo", 
                                                           "^4$" = "abril", 
                                                           "^5$" = "mayo", 
                                                           "^6$" = "junio", 
                                                           "^7$" = "julio", 
                                                           "^8$" = "agosto",
                                                           "^9$" = "septiembre", 
                                                           "^10$" = "octubre", 
                                                           "^11$" = "noviembre", 
                                                           "^12$" = "diciembre")),
                           " del ", year(maxima_fecha))
        lag_m = 1
        
    } else {
        datos = datos_q
        # datos$date[datos$date_shortcut  %% 2 == 0] <- datos$date[datos$date_shortcut  %% 2 == 0] + days(14)
        
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        subtitulo <- str_c(ifelse(v_quincena == 1, 
                                  yes = "A la 1ª quincena de ", 
                                  no = "A la 2ª quincena de "), 
                           str_replace_all(month(maxima_fecha),
                                           c("^1$" = "enero", 
                                             "^2$" = "febrero", 
                                             "^3$" = "marzo", 
                                             "^4$" = "abril", 
                                             "^5$" = "mayo", 
                                             "^6$" = "junio", 
                                             "^7$" = "julio", 
                                             "^8$" = "agosto",
                                             "^9$" = "septiembre", 
                                             "^10$" = "octubre", 
                                             "^11$" = "noviembre", 
                                             "^12$" = "diciembre")),
                           " del ", year(maxima_fecha))
        lag_m = 2
        
    }
    
    if(tipo_grafica == "Evolución INPC"){
        eje_y <- "Índice base\n2ª quincena de julio 2018 = 100"
    fyvs <- datos %>% 
        as_tibble() %>% 
        filter(ccif %in% genericos) %>% 
        mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
        select(date_shortcut, ccif, fecha = date, values, quincena) %>% 
        filter(fecha >= fecha_inicio) %>% 
        mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
        group_by(ccif) %>%
        arrange(ccif) %>% 
        mutate(tasa = (lag(values, 12*lag_m) - values)/values) %>% 
        mutate(tasa = lead(tasa, 12*lag_m)) %>%
        mutate(cat = str_c(str_wrap(ccif,20), "\n", 
                           format(round(values, 1), nsmall = 1) %>% str_squish(), 
                           " [", format(round(tasa*100, 2), nsmall = 2) %>% str_squish(), "%]")) %>% 
        mutate(ccif = factor(ccif, levels = genericos %>% str_replace_all(c("Total" = "General"))))
    
    } else if(tipo_grafica == "Evolución niveles"){
        
        fyvs <- datos %>% 
            as_tibble() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(date_shortcut, ccif, fecha = date, values, quincena) %>% 
            filter(fecha >= fecha_inicio) %>% 
            mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
            group_by(ccif) %>%
            arrange(ccif) %>% 
            mutate(tasa = 100*(values/last(values))) %>% 
            mutate(values = 100*(values/last(values))) %>% 
            mutate(cat = str_c(str_wrap(ccif,20), "\n", 
                               format(round(values, 1), nsmall = 1) %>% str_squish(), 
                               " [", format(round(tasa-100, 2), nsmall = 2) %>% str_squish(), "%]")) %>% 
            mutate(ccif = factor(ccif, levels = genericos %>% str_replace_all(c("Total" = "General"))))
        
        fecha_mas_minima <- fyvs %>% filter(fecha == min(fecha)) %>% filter(quincena == min(quincena))
        
        eje_y <- str_c("Valor = 100 ", 
                       ifelse(tipo_datos == "Datos mensuales", 
                              yes = 
                                  str_c("correspondiente al mes de ", 
                                        month(fecha_mas_minima$fecha, label = T, abbr = F), 
                                        " de ", year(fecha_mas_minima$fecha)       
                                        ), 
                              no = str_c(
                                  "en la ", fecha_mas_minima$quincena, "ª quincena de ", 
                                  month(fecha_mas_minima$fecha, label = T, abbr = F), 
                                  " de ", year(fecha_mas_minima$fecha)))) %>% 
            str_wrap(25)
        
    }
    
 
    if(length(genericos) <= 17){
        paleta <- c(mcv_discrete[c(4,3,1,5,6)], mcv_discrete_7, mcv_discrete_12) %>% unique() %>% na.omit()
    } else {
        paleta <- rainbow(n = length(genericos))
    }
    
    # class(max(fyvs$fecha))
    
    g <- fyvs %>% 
        ggplot(aes(x = fecha, 
                   y = values, 
                   group = ccif, 
                   color = ccif)) +
        geom_line(aes(linewidth = grosor),
                  lineend = "round",
                  show.legend = F) +
        geom_point(data = fyvs %>% filter(fecha == max(fecha)) %>% unique(),
                   size = 2.5,
                   show.legend = F) +
        geom_text_repel(data = fyvs %>% filter(fecha == max(fecha)) %>% filter(quincena == max(quincena)) %>% unique(),
                        aes(label = cat,
                            fontface = grosor), 
                        direction = "y",
                        family = "Arial", 
                        nudge_x = 100, 
                        hjust = "left",
                        size = 5,
                        segment.curvature = -0.1,
                        segment.ncp = 3,
                        segment.angle = 20,
                        segment.color = NA) + 
        scale_discrete_manual(aesthetics = "linewidth", values = c(2,1)) + 
        scale_discrete_manual(aesthetics = "fontface", values = c("bold","italic")) + 
        scale_color_manual(values = paleta) +
        scale_x_datetime(
            date_labels = "%b %y",
            breaks = seq.POSIXt(from = max(fyvs$fecha),
                                to = min(fyvs$fecha),
                                by = "-6 month"),
            expand = expansion(mult = c(0.02, 0.2))
        ) + 
        scale_y_continuous(expand = expansion(c(0.1, 0.1))) + 
        theme_minimal() +
        labs(
            title = "Índice de precios al consumidor de genéricos seleccionados",
            subtitle = subtitulo, 
            caption = nota,
            color="", shape="", y = eje_y
        ) +
        theme(plot.title = element_text(size = 22, face = "bold", colour = "#6950D8"),
              plot.title.position = "plot",
              plot.subtitle = element_text(size = 18, colour = "#777777", margin=margin(0,0,20,0)),
              plot.caption = element_text(size = 16, colour = "#777777"),
              plot.margin= margin(0.2, 0.3, 0.8, 0.2, "cm"), # margin(top,right, bottom,left)
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(family = "Arial"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 18),
              axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5),
              axis.text.y = element_text(size = 14),
              legend.text = element_text(size = 16),
              legend.position = "none")
    g
}

# genericos = c("Aceites y grasas")
gen_barras_cambio_anual <- function(genericos, fecha_inicio = "2018-12-01", 
                                    tipo_datos = "Datos mensuales"){
    
    nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
    
    if(tipo_datos == "Datos mensuales"){
        datos = datos_m
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        subtitulo <- str_c("Al mes de ", str_replace_all(month(maxima_fecha),
                                                         c("^1$" = "enero", 
                                                           "^2$" = "febrero", 
                                                           "^3$" = "marzo", 
                                                           "^4$" = "abril", 
                                                           "^5$" = "mayo", 
                                                           "^6$" = "junio", 
                                                           "^7$" = "julio", 
                                                           "^8$" = "agosto",
                                                           "^9$" = "septiembre", 
                                                           "^10$" = "octubre", 
                                                           "^11$" = "noviembre", 
                                                           "^12$" = "diciembre")),
                           " del ", year(maxima_fecha))
        lag_m = 1
        
    } else {
        datos = datos_q
        # datos$date[datos$date_shortcut  %% 2 == 0] <- datos$date[datos$date_shortcut  %% 2 == 0] + days(14)
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        subtitulo <- str_c(ifelse(v_quincena == 1, 
                                  yes = "A la 1ª quincena de ", 
                                  no = "A la 2ª quincena de "), 
                           str_replace_all(month(maxima_fecha),
                                           c("^1$" = "enero", 
                                             "^2$" = "febrero", 
                                             "^3$" = "marzo", 
                                             "^4$" = "abril", 
                                             "^5$" = "mayo", 
                                             "^6$" = "junio", 
                                             "^7$" = "julio", 
                                             "^8$" = "agosto",
                                             "^9$" = "septiembre", 
                                             "^10$" = "octubre", 
                                             "^11$" = "noviembre", 
                                             "^12$" = "diciembre")),
                           " del ", year(maxima_fecha))
        lag_m = 2
        
    }
    
    dpl <- datos %>% 
        as_tibble() %>% 
        filter(ccif %in% genericos) %>% 
        mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
        select(date_shortcut, ccif, fecha = date, values, quincena) %>% 
        filter(fecha >= fecha_inicio) %>% 
        mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
        # arrange(fecha) %>% 
        group_by(ccif) %>%
        # mutate(tasa = (values/lag(values, 12))-1) %>% 
        mutate(tasa = (lag(values, 12*lag_m) - values)/values) %>% 
        mutate(tasa = lead(tasa, 12*lag_m)) %>%
        mutate(cat = str_c(ccif, "\n", 
                           format(round(values, 1), nsmall = 1) %>% str_squish(), 
                           " [", format(round(tasa*100, 1), nsmall = 1) %>% str_squish(), "%]")) %>% 
        filter(fecha == max(fecha)) %>% 
        filter(quincena == max(quincena)) %>%
        mutate(ccif = factor(ccif, levels = genericos %>%
                                 str_replace_all(c("^Total$" = "General")) %>% rev()))
    
    
    if(sum(dpl$tasa < 0) > 0){
        expansiones = c(0.2, 0.2)
    } else {
        expansiones = c(0, 0.2)
    }
    
    tamanio_letra <- case_when(length(genericos) <= 1 ~ 8,
                               between(length(genericos), 2,3)  ~ 6,
                               between(length(genericos), 4,10)  ~ 5,
                               between(length(genericos), 11, 20) > 10 ~ 4,
                               TRUE ~ 1 
    )
    
    if(length(genericos) <= 17){
        paleta <- c(mcv_discrete[c(4,3,1,5,6)], mcv_discrete_7, mcv_discrete_12) %>% unique() %>% na.omit()
    } else {
        paleta <- rainbow(n = length(genericos))
    }
    
    dpl %>% 
        ggplot(aes(x = ccif, y = tasa*100, fill = ccif, color = ccif)) + 
        geom_col() + 
        geom_text(aes(label = str_c(round(tasa*100, 2), "%"), 
                      hjust = as.character(sign(tasa))), 
                  size = tamanio_letra,
                  color = "black",
                  fontface = "bold", 
                  family = "Arial") + 
        geom_hline(yintercept = 0, color = "black") +
        scale_discrete_manual(aesthetics = "hjust", values = c("1" = -0.1,
                                                               "-1" = 1.1)) + 
        scale_y_continuous(expand = expansion(expansiones), 
                           labels = scales::comma_format(suffix = "%")) + 
        coord_flip() + 
        theme_minimal() +
        labs(
            title = "Inflación anual en genéricos seleccionados",
            x = "Genéricos\nseleccionados\n",
            subtitle = subtitulo, 
            caption = nota,
            color="", shape=""
        ) +
        scale_fill_manual(values = paleta) +
        scale_color_manual(values = paleta) +
        theme(plot.title = element_text(size = 22, face = "bold", colour = "#6950D8"),
              plot.title.position = "plot",
              plot.subtitle = element_text(size = 18, colour = "#777777", margin=margin(0,0,20,0)),
              plot.caption = element_text(size = 16, colour = "#777777"),
              plot.margin= margin(0.2, 0.3, 0.8, 0.2, "cm"), # margin(top,right, bottom,left)
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(family = "Arial"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 18),
              axis.text.x = element_text(size = 16, angle = 0, vjust = 0.5),
              axis.text.y = element_text(size = 16),
              legend.text = element_text(size = 16),
              legend.position = "none")
    
}

# Función para generar datos de la gráfica de evolución
gen_datos_evolucion <- function(genericos, 
                               fecha_inicio = "2018-12-01", 
                               tipo_datos = "Datos quincenales", 
                               tipo_grafica = "Evolución niveles"){
    
    if(tipo_datos == "Datos mensuales"){
        datos = datos_m
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 1
    } else {
        datos = datos_q
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 2
    }
    
    if(tipo_grafica == "Evolución INPC"){
        datos_tabla <- datos %>% 
            as_tibble() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(Fecha = date, Genérico = ccif, `Valor Índice` = values, Quincena = quincena) %>% 
            filter(Fecha >= fecha_inicio) %>% 
            group_by(Genérico) %>%
            arrange(Genérico, Fecha) %>% 
            mutate(`Tasa Anual (%)` = round((lag(`Valor Índice`, 12*lag_m) - `Valor Índice`)/`Valor Índice` * 100, 2)) %>% 
            mutate(`Tasa Anual (%)` = lead(`Tasa Anual (%)`, 12*lag_m)) %>%
            mutate(`Valor Índice` = round(`Valor Índice`, 2)) %>%
            arrange(Fecha, Genérico)
            
    } else if(tipo_grafica == "Evolución niveles"){
        datos_tabla <- datos %>% 
            as_tibble() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(Fecha = date, Genérico = ccif, values, Quincena = quincena) %>% 
            filter(Fecha >= fecha_inicio) %>% 
            group_by(Genérico) %>%
            arrange(Genérico, Fecha) %>% 
            mutate(`Valor Normalizado` = round(100*(values/last(values)), 2)) %>% 
            mutate(`Cambio vs Último (%)` = round(100*(values/last(values)) - 100, 2)) %>%
            select(Fecha, Genérico, `Valor Original` = values, `Valor Normalizado`, `Cambio vs Último (%)`, Quincena) %>%
            mutate(`Valor Original` = round(`Valor Original`, 2)) %>%
            arrange(Fecha, Genérico)
    }
    
    return(datos_tabla)
}

# Función para generar datos de la gráfica de cambio anual
gen_datos_cambio_anual <- function(genericos, fecha_inicio = "2018-12-01", tipo_datos = "Datos mensuales"){
    
    if(tipo_datos == "Datos mensuales"){
        datos = datos_m
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 1
    } else {
        datos = datos_q
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 2
    }
    
    datos_tabla <- datos %>% 
        as_tibble() %>% 
        filter(ccif %in% genericos) %>% 
        mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
        select(Fecha = date, Genérico = ccif, `Valor Índice` = values, Quincena = quincena) %>% 
        filter(Fecha >= fecha_inicio) %>% 
        group_by(Genérico) %>%
        mutate(`Tasa Anual (%)` = round((lag(`Valor Índice`, 12*lag_m) - `Valor Índice`)/`Valor Índice` * 100, 2)) %>% 
        mutate(`Tasa Anual (%)` = lead(`Tasa Anual (%)`, 12*lag_m)) %>%
        filter(Fecha == max(Fecha)) %>% 
        filter(Quincena == max(Quincena)) %>%
        mutate(`Valor Índice` = round(`Valor Índice`, 2)) %>% 
        select(Genérico, `Valor Índice`, `Tasa Anual (%)`, Fecha, Quincena) %>%
        arrange(desc(`Tasa Anual (%)`))
    
    return(datos_tabla)
}

# Generacion de tablas 
# genericos = "Ron"
gen_tabla <- function(genericos, mostrar_todos = F, tipo_datos = "Datos quincenales", fecha_inicio = "2018-12-01"){
    
    fecha_inicio_s <- "2018-12-01"
    
    if(tipo_datos == "Datos mensuales"){
        datos = datos_m
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 1
    } else {
        datos = datos_q
        maxima_fecha <- datos$date %>% max()
        v_quincena = datos$quincena[1]
        lag_m = 2
    }
    
    if(mostrar_todos == F){
        dx <- datos %>% 
            as_tibble() %>% 
            unique() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", 
                                 yes = "General", 
                                 no = ccif)) %>% 
            select(date_shortcut, ccif, fecha = date, values,quincena) %>% 
            filter(fecha >= fecha_inicio_s) %>% 
            mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
            arrange(fecha) %>% 
            group_by(ccif) %>%
            mutate(tasa = (values/lag(values, 12*lag_m))-1) %>% 
            mutate(tasa = tasa*100) %>% 
            mutate(tasa = round(tasa, 2)) %>% 
            mutate(tasa_m = (values/lag(values, 1*lag_m))-1) %>%
            mutate(tasa_m = tasa_m*100) %>% 
            mutate(tasa_m = round(tasa_m, 2)) %>% 
            mutate(tasa_sexenal = (values/first(values))-1) %>% 
            mutate(tasa_sexenal = tasa_sexenal*100) %>% 
            mutate(tasa_sexenal = round(tasa_sexenal, 2)) %>% 
            filter(fecha == maxima_fecha) %>% 
            filter(quincena == max(quincena)) %>% 
            mutate(values = round(values, 2)) %>% 
            select(Fecha = fecha, 
                   `Genérico` = ccif,
                   `Valor Índice` = values, 
                   `Crecimiento anual (%)` = tasa, 
                   `Crecimiento mensual (%)` = tasa_m,
                   `Crecimiento sexenio (%)` = tasa_sexenal) %>% 
            unique()
        
        dx2 <- datos %>% 
            as_tibble() %>% 
            unique() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", 
                                 yes = "General", 
                                 no = ccif)) %>% 
            select(date_shortcut, ccif, fecha = date, values,quincena) %>% 
            filter(fecha >= fecha_inicio) %>% 
            group_by(ccif) %>% 
            filter(fecha %in% c(min(fecha), max(fecha))) %>% 
            filter(date_shortcut %in% c(min(date_shortcut), max(date_shortcut))) %>% 
            group_by(ccif) %>% 
            summarise(`Tasa acumulada desde fecha inicio:` = 100*(-diff(values)/last(values)) %>% round(2)) %>% 
            rename(`Genérico` = ccif)
        
        dx <- left_join(dx, dx2)
        
    } else {
        dx <- datos %>% 
            unique() %>% 
            as_tibble() %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(date_shortcut,id_ccif_0, ccif, fecha = date, values,quincena) %>% 
            filter(fecha >= fecha_inicio_s) %>% 
            mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
            arrange(fecha) %>% 
            group_by(ccif) %>%
            mutate(tasa = (values/lag(values, 12*lag_m))-1) %>% 
            mutate(tasa = tasa*100) %>% 
            mutate(tasa = round(tasa, 2)) %>% 
            mutate(tasa_m = (values/lag(values, 1*lag_m))-1) %>%
            mutate(tasa_m = tasa_m*100) %>% 
            mutate(tasa_m = round(tasa_m, 2)) %>% 
            mutate(tasa_sexenal = (values/first(values))-1) %>% 
            mutate(tasa_sexenal = tasa_sexenal*100) %>% 
            mutate(tasa_sexenal = round(tasa_sexenal, 2)) %>% 
            mutate(values = round(values, 2)) %>% 
            filter(fecha == maxima_fecha) %>% 
            filter(quincena == max(quincena)) %>% 
            arrange(-tasa) %>% 
            select(
                # Fecha = fecha, 
                    id_ccif_0, 
                   `Genérico` = ccif,
                   `Valor Índice` = values, 
                   `Crecimiento anual (%)` = tasa, 
                   `Crecimiento mensual (%)` = tasa_m,
                   `Crecimiento sexenio (%)` = tasa_sexenal) %>% 
            unique()
        
        dx2 <- datos %>% 
            # filter(ccif == "Electricidad") %>% 
            as_tibble() %>% 
            unique() %>% 
            # filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", 
                                 yes = "General", 
                                 no = ccif)) %>% 
            select(date_shortcut, id_ccif_0, ccif, fecha = date, values,quincena) %>% 
            filter(fecha >= fecha_inicio) %>% 
            group_by(id_ccif_0, ccif) %>% 
            filter(fecha %in% c(min(fecha), max(fecha))) %>% 
            filter(date_shortcut %in% c(min(date_shortcut), max(date_shortcut))) %>% 
            group_by(id_ccif_0, ccif) %>% 
            summarise(`Tasa acumulada desde fecha inicio:` = 100*(-diff(values)/last(values)) %>% round(2)) %>% 
            rename(`Genérico` = ccif) %>% 
            unique()
        
        # dx2 %>% 
        #     filter(Genérico == "Electricidad")
        
        dx <- left_join(dx, dx2)
        
    }
    
    DT::datatable(dx,
                  options = list(
                      # autoWidth = TRUE,
                      language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      pageLength = 25,
                      scrollX = TRUE,
                      initComplete = JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
                          "}"
                      )
                  )
    )
    
}

# Aplicación: ----

library(shiny)

ui <- fluidPage(
  # Incluir Google Fonts para Ubuntu y Font Awesome para iconos
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/png", href = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg"),
    
    # Meta tags para compartir en redes sociales
    tags$meta(property = "og:title", content = "México ¿Cómo vamos? - Monitor de Inflación"),
    tags$meta(property = "og:description", content = "Monitor interactivo de inflación en México"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "México ¿Cómo vamos? - Monitor de Inflación"),
    tags$meta(name = "twitter:description", content = "Monitor interactivo de inflación en México"),
    
    # Title
    tags$title("México ¿Cómo vamos? - Monitor de Inflación"),
    
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Ubuntu:wght@300;400;500;700&display=swap"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    ),
    tags$style(
      HTML("
        body {
          margin: 0;
          padding: 0;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Cantarell, 'Ubuntu', sans-serif;
          background-color: #f7f7f7;
        }
        
        .mcv-top-bar a:hover {
          transform: scale(1.1);
          transition: transform 0.2s ease;
        }
        
        .mcv-main-header a:hover {
          text-decoration: none !important;
        }
        
        /* Responsive */
        @media (max-width: 1024px) {
          .mcv-main-header > div > div {
            flex-direction: column;
            gap: 20px;
          }
          
          .mcv-main-header > div > div > div {
            flex-wrap: wrap;
            justify-content: center;
            gap: 15px;
          }
        }
        
        @media (max-width: 768px) {
          .mcv-top-bar > div > div {
            flex-direction: column;
            gap: 10px;
          }
          
          .mcv-main-header a {
            font-size: 10px;
          }
        }
        
        /* Estilos para el contenedor principal */
        .main-content {
          background-color: #f7f7f7;
          min-height: 100vh;
          padding: 5px 0;
        }
        
        /* Estilos para wellPanel */
        .well {
          background-color: white !important;
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
          margin-bottom: 15px;
          padding: 15px !important;
        }
        
        /* Estilos para la sección de gráficas */
        .graph-container {
          background-color: white;
          border-radius: 12px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
        }
        
        /* Optimización para sidebar */
        .sidebar {
          padding: 15px !important;
        }
        
        /* Contenedor principal más ancho */
        .main-container {
          max-width: 1400px !important;
          margin: 0 auto;
          padding: 10px 15px;
        }
        
        /* Ajuste de columnas para mejor aprovechamiento */
        @media (min-width: 1200px) {
          .col-sm-4 {
            max-width: 300px;
            flex: 0 0 300px;
          }
          .col-sm-8 {
            flex: 1;
            max-width: calc(100% - 300px);
          }
        }
        
        /* Estilos para tabs */
        .nav-tabs {
          border-bottom: 3px solid #6551D0;
        }
        
        .nav-tabs > li > a {
          color: #666;
          font-weight: 500;
          font-family: 'Ubuntu', sans-serif;
        }
        
        .nav-tabs > li.active > a {
          color: #6551D0;
          font-weight: 700;
          border-bottom: 3px solid #6551D0;
        }
        
        /* Estilos para botones */
        .btn-primary {
          background-color: #6551D0;
          border-color: #6551D0;
          font-family: 'Ubuntu', sans-serif;
          font-weight: 500;
          border-radius: 8px;
        }
        
        .btn-primary:hover {
          background-color: #5441C0;
          border-color: #5441C0;
        }
        
        .btn-success {
          background-color: #00b783;
          border-color: #00b783;
          font-family: 'Ubuntu', sans-serif;
          font-weight: 500;
          border-radius: 8px;
        }
        
        .btn-success:hover {
          background-color: #009670;
          border-color: #009670;
        }
        
        /* Estilos para inputs */
        .form-control {
          border-radius: 8px;
          border: 2px solid #e9ecef;
          font-family: 'Ubuntu', sans-serif;
        }
        
        .form-control:focus {
          border-color: #6551D0;
          box-shadow: 0 0 0 0.2rem rgba(101, 81, 208, 0.25);
        }
        
        .control-label {
          font-weight: 600;
          color: #333;
          font-family: 'Ubuntu', sans-serif;
          margin-bottom: 8px;
        }
      ")
    )
  ),
  
  # Barra superior con redes sociales
  div(
    class = "mcv-top-bar",
    style = "
      background-color: #6551D0;
      color: white;
      padding: 10px 0;
      font-size: 12px;
      position: relative;
      font-family: 'Ubuntu', sans-serif;
    ",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          span("Síguenos en:", style = "font-weight: 500;"),
          a(
            href = "https://twitter.com/mexicocomovamos",
            target = "_blank",
            style = "color: white; text-decoration: none;",
            tags$i(class = "fab fa-twitter", style = "font-size: 16px;")
          ),
          a(
            href = "https://www.facebook.com/MexicoComoVamos",
            target = "_blank",
            style = "color: white; text-decoration: none;",
            tags$i(class = "fab fa-facebook-f", style = "font-size: 16px;")
          ),
          a(
            href = "https://www.linkedin.com/company/mexico-como-vamos",
            target = "_blank",
            style = "color: white; text-decoration: none;",
            tags$i(class = "fab fa-linkedin-in", style = "font-size: 16px;")
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 20px;",
          span("Monitor de Inflación 2025", style = "font-weight: 600;")
        )
      )
    )
  ),
  
  # Barra principal con navegación
  div(
    class = "mcv-main-header",
    style = "
      background-color: white;
      color: #333333;
      padding: 20px 0;
      font-family: 'Ubuntu', sans-serif;
      border-bottom: 1px solid #e9ecef;
    ",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        div(
          a(
            href = "https://mexicocomovamos.mx",
            target = "_blank",
            img(
              src = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg",
              alt = "México ¿Cómo vamos?",
              style = "height: 40px;"
            )
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 30px;",
          a(
            href = "https://mexicocomovamos.mx/equipo/",
            style = "
              color: #333333;
              text-decoration: none;
              font-size: 12px;
              font-weight: 600;
              padding: 8px 0;
              border-bottom: 2px solid transparent;
              font-family: 'Ubuntu', sans-serif;
            ",
            onmouseover = "this.style.borderBottom='2px solid #6551D0'",
            onmouseout = "this.style.borderBottom='2px solid transparent'",
            "MÉXICO,",
            br(),
            "¿CÓMO VAMOS?"
          ),
          a(
            href = "https://mexicocomovamos.mx/categoria/inflacion/",
            style = "
              color: #6551D0;
              text-decoration: none;
              font-size: 12px;
              font-weight: 700;
              padding: 8px 0;
              border-bottom: 2px solid #6551D0;
              font-family: 'Ubuntu', sans-serif;
            ",
            "MONITOR DE",
            br(),
            "INFLACIÓN"
          ),
          a(
            href = "https://mexicocomovamos.mx/fichas-por-estado/",
            style = "
              color: #333333;
              text-decoration: none;
              font-size: 12px;
              font-weight: 600;
              padding: 8px 0;
              border-bottom: 2px solid transparent;
              font-family: 'Ubuntu', sans-serif;
            ",
            onmouseover = "this.style.borderBottom='2px solid #6551D0'",
            onmouseout = "this.style.borderBottom='2px solid transparent'",
            "FICHAS",
            br(),
            "POR ESTADO"
          ),
          a(
            href = "https://mexicocomovamos.mx/categoria/publicaciones/",
            style = "
              color: #333333;
              text-decoration: none;
              font-size: 12px;
              font-weight: 600;
              padding: 8px 0;
              border-bottom: 2px solid transparent;
              font-family: 'Ubuntu', sans-serif;
            ",
            onmouseover = "this.style.borderBottom='2px solid #6551D0'",
            onmouseout = "this.style.borderBottom='2px solid transparent'",
            "PUBLICACIONES",
            br(),
            "Y REPORTES"
          )
        )
      )
    )
  ),
  
  # Contenido principal
  div(
    class = "main-content",
    div(
      class = "main-container container-fluid",
      sidebarLayout(
        sidebarPanel = sidebarPanel(
          width = 3,
          style = "background-color: white; border-radius: 12px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border: 1px solid #e9ecef; padding: 15px;",
            h4("Configuración de la Visualización", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;"),
            selectizeInput("selGenerico", 
                           "Seleccione el(los) genérico(s) a analizar", 
                           choices = sel_genericos, 
                           multiple = TRUE, 
                           selected = "Total"), 
            radioButtons("selTipoDatos", 
                           "Seleccione tipo de datos", 
                           choices = c("Datos mensuales", "Datos quincenales"), 
                         inline = T),
            dateInput("selFechaInicio", label = "Seleccione fecha de inicio de la gráfica", 
                      value = "2018-12-01"),
            radioButtons(inputId = "rdTipoGrafica", label = "Seleccione tipo de gráfica", choices = c("Evolución INPC", "Evolución niveles"), inline = T),
            div(
              style = "margin-top: 20px;",
              h5("Descargar Visualización", 
                 style = "color: #333; font-weight: 600; margin-bottom: 10px; font-family: 'Ubuntu', sans-serif;"),
              fluidRow(
                column(12, 
                  downloadButton("descarga_graficas", "Descargue la gráfica", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;")
                ),
                column(12, 
                  downloadButton("descarga_datos", "Descargar datos (Excel)", class = "btn-success", style = "width: 100%;")
                )
              )
            )
        ),
        mainPanel = mainPanel(
          width = 9,
          style = "padding-left: 20px;",
            tabsetPanel(id = "pestañas",
                        tabPanel("Evolución",
                                 div(
                                   class = "graph-container",
                                   plotOutput("grafica_evolucion", height = "80vh") %>% withSpinner(color = "#6551D0")
                                 )
                        ), 
                        tabPanel("Cambio Anual",
                                 div(
                                   class = "graph-container", 
                                   plotOutput("grafica_cambio", height = "80vh") %>% withSpinner(color = "#6551D0")
                                 )
                        ), 
                        tabPanel("Tabla", 
                                 div(
                                   style = "margin-bottom: 20px;",
                                   h4("Incrementos anuales de inflación", 
                                      style = "color: #6551D0; font-weight: 600; margin-bottom: 15px; font-family: 'Ubuntu', sans-serif;"),
                                   p("Con respecto al periodo más reciente", 
                                     style = "color: #666; font-size: 14px; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;")
                                 ),
                                 wellPanel(
                                   style = "background-color: white; border-radius: 12px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); border: 1px solid #e9ecef;",
                                   h5("Configuración de la tabla", 
                                      style = "color: #333; font-weight: 600; margin-bottom: 15px; font-family: 'Ubuntu', sans-serif;"),
                                   radioButtons(inputId = "rdMostrarTodo", 
                                                label = "Información en la tabla",
                                                choices = c("Mostrar todos los genéricos" = TRUE, 
                                                            "Mostrar genéricos seleccionados" = FALSE),
                                                inline = T)
                                 ),
                                 div(
                                   class = "graph-container",
                                   DT::dataTableOutput("tabla") %>% withSpinner(color = "#6551D0")
                                 )
                        )
            )
        )
      )
    )
  ),
  
  # Footer
  div(
    style = "
      background-color: #333333;
      color: white;
      padding: 20px 0;
      margin-top: 50px;
      font-family: 'Ubuntu', sans-serif;
    ",
    div(
      class = "container-fluid",
      style = "max-width: 1200px; margin: 0 auto; padding: 0 20px; text-align: center;",
      p(
        "© 2025 México ¿Cómo vamos? - Monitor de Inflación",
        style = "margin: 0; font-size: 14px; color: #ccc;"
      )
    )
  )
)

# gen_barras_cambio_anual(genericos = "Total")

server <- function(input, output, session) {
    output$grafica_evolucion <- renderPlot({
        gen_grafica(genericos = input$selGenerico, tipo_datos = input$selTipoDatos, fecha_inicio = input$selFechaInicio, tipo_grafica = input$rdTipoGrafica)
    })
    
    output$grafica_cambio <- renderPlot({
        gen_barras_cambio_anual(genericos = input$selGenerico, tipo_datos = input$selTipoDatos,fecha_inicio = input$selFechaInicio)
    })
    
    output$tabla <- DT::renderDT({
        gen_tabla(genericos = input$selGenerico, mostrar_todos = input$rdMostrarTodo, tipo_datos = input$selTipoDatos, fecha_inicio = input$selFechaInicio)
    })
    
    output$descarga_graficas <- downloadHandler(
        filename = function(){
            if(input$pestañas == "Evolución"){
                str_c("evolucion_inflacion.png")
            } else {
                str_c("evolucion_cambio.png")
            }
        }, content = function(file){
            if(input$pestañas == "Evolución"){
                ggsave(file,
                       plot = gen_grafica(genericos = input$selGenerico,
                                          fecha_inicio = input$selFechaInicio, 
                                          tipo_datos = input$selTipoDatos, 
                                          tipo_grafica = input$rdTipoGrafica),
                       device = "png", 
                       height = 6,
                       width = 10)
            } else {
                ggsave(file, 
                       plot = gen_barras_cambio_anual(genericos = input$selGenerico),
                       device = "png", 
                       height = 6, 
                       width = 10)
            }
        }
    )
    
    # Descarga de datos en Excel
    output$descarga_datos <- downloadHandler(
        filename = function(){
            fecha_actual <- format(Sys.Date(), "%Y-%m-%d")
            if(input$pestañas == "Evolución"){
                str_c("datos_evolucion_inflacion_", fecha_actual, ".xlsx")
            } else if(input$pestañas == "Cambio Anual") {
                str_c("datos_cambio_anual_inflacion_", fecha_actual, ".xlsx")
            } else {
                str_c("datos_tabla_inflacion_", fecha_actual, ".xlsx")
            }
        }, 
        content = function(file){
            # Crear un workbook
            wb <- openxlsx::createWorkbook()
            
            if(input$pestañas == "Evolución"){
                # Obtener datos de evolución
                datos_descarga <- gen_datos_evolucion(
                    genericos = input$selGenerico,
                    fecha_inicio = input$selFechaInicio, 
                    tipo_datos = input$selTipoDatos, 
                    tipo_grafica = input$rdTipoGrafica
                )
                
                # Agregar hoja al workbook
                openxlsx::addWorksheet(wb, "Datos Evolución")
                openxlsx::writeData(wb, "Datos Evolución", datos_descarga)
                
                # Añadir metadatos
                openxlsx::addWorksheet(wb, "Metadatos")
                metadatos <- data.frame(
                    Parámetro = c("Genéricos seleccionados", "Tipo de datos", "Fecha inicio", "Tipo de gráfica", "Fecha descarga"),
                    Valor = c(paste(input$selGenerico, collapse = ", "), 
                             input$selTipoDatos, 
                             input$selFechaInicio, 
                             input$rdTipoGrafica,
                             format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                )
                openxlsx::writeData(wb, "Metadatos", metadatos)
                
            } else if(input$pestañas == "Cambio Anual") {
                # Obtener datos de cambio anual
                datos_descarga <- gen_datos_cambio_anual(
                    genericos = input$selGenerico,
                    fecha_inicio = input$selFechaInicio,
                    tipo_datos = input$selTipoDatos
                )
                
                # Agregar hoja al workbook
                openxlsx::addWorksheet(wb, "Datos Cambio Anual")
                openxlsx::writeData(wb, "Datos Cambio Anual", datos_descarga)
                
                # Añadir metadatos
                openxlsx::addWorksheet(wb, "Metadatos")
                metadatos <- data.frame(
                    Parámetro = c("Genéricos seleccionados", "Tipo de datos", "Fecha inicio", "Fecha descarga"),
                    Valor = c(paste(input$selGenerico, collapse = ", "), 
                             input$selTipoDatos, 
                             input$selFechaInicio,
                             format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                )
                openxlsx::writeData(wb, "Metadatos", metadatos)
                
            } else if(input$pestañas == "Tabla") {
                # Obtener datos de la tabla
                datos_descarga <- gen_tabla(
                    genericos = input$selGenerico, 
                    mostrar_todos = input$rdMostrarTodo, 
                    tipo_datos = input$selTipoDatos, 
                    fecha_inicio = input$selFechaInicio
                )
                
                # Convertir DT a dataframe si es necesario
                if("datatables" %in% class(datos_descarga)){
                    # Extraer los datos del objeto DT
                    datos_df <- datos_descarga$x$data
                } else {
                    datos_df <- as.data.frame(datos_descarga)
                }
                
                # Agregar hoja al workbook
                openxlsx::addWorksheet(wb, "Tabla Inflación")
                openxlsx::writeData(wb, "Tabla Inflación", datos_df)
                
                # Añadir metadatos
                openxlsx::addWorksheet(wb, "Metadatos")
                metadatos <- data.frame(
                    Parámetro = c("Genéricos seleccionados", "Mostrar todos", "Tipo de datos", "Fecha inicio", "Fecha descarga"),
                    Valor = c(paste(input$selGenerico, collapse = ", "), 
                             input$rdMostrarTodo, 
                             input$selTipoDatos, 
                             input$selFechaInicio,
                             format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
                )
                openxlsx::writeData(wb, "Metadatos", metadatos)
            }
            
            # Aplicar estilos al encabezado
            headerStyle <- openxlsx::createStyle(fgFill = "#6551D0", fontColour = "white", textDecoration = "bold")
            if(input$pestañas == "Evolución"){
                openxlsx::addStyle(wb, "Datos Evolución", headerStyle, rows = 1, cols = 1:ncol(datos_descarga), gridExpand = TRUE)
            } else if(input$pestañas == "Cambio Anual") {
                openxlsx::addStyle(wb, "Datos Cambio Anual", headerStyle, rows = 1, cols = 1:ncol(datos_descarga), gridExpand = TRUE)
            } else if(input$pestañas == "Tabla") {
                openxlsx::addStyle(wb, "Tabla Inflación", headerStyle, rows = 1, cols = 1:ncol(datos_df), gridExpand = TRUE)
            }
            
            # Guardar el archivo
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
    )
}

shinyApp(ui, server)
