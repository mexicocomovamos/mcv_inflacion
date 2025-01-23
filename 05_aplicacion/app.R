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
                        tipo_grafica = "Evolución INPC"){
    
    eje_y <- "Índice base\n2ª quincena de julio 2018 = 100"
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
        theme(plot.title = element_text(size = 30, face = "bold", colour = "#6950D8"),
              plot.title.position = "plot",
              plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
              plot.caption = element_text(size = 25, colour = "#777777"),
              plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(family = "Arial"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 25),
              axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
              axis.text.y = element_text(size = 20),
              legend.text = element_text(size = 30),
              legend.position = "none")
    g
}

# genericos = c("Aceites y grasas")
gen_barras_cambio_anual <- function(genericos, fecha_inicio = "2018-12-01", tipo_datos = "Datos mensuales"){
    
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
        theme(plot.title = element_text(size = 30, face = "bold", colour = "#6950D8"),
              plot.title.position = "plot",
              plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
              plot.caption = element_text(size = 25, colour = "#777777"),
              plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              text = element_text(family = "Arial"),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 25),
              axis.text.x = element_text(size = 20, angle = 0, vjust = 0.5),
              axis.text.y = element_text(size = 20),
              legend.text = element_text(size = 30),
              legend.position = "none")
    
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
    h1("Monitor de inflación"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
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
            downloadButton("descarga_graficas", "Descargue la gráfica")
        ),
        mainPanel = mainPanel(
            tabsetPanel(id = "pestañas",
                        tabPanel("Evolución",    plotOutput("grafica_evolucion", height = "90vh")), 
                        tabPanel("Cambio Anual", plotOutput("grafica_cambio", height = "90vh")), 
                        tabPanel("Tabla", 
                                 h2("Incrementos anuales de inflación (con respecto al periodo más reciente)", style = "color:#6950D8;"), 
                                 radioButtons(inputId = "rdMostrarTodo", 
                                              label = "Información en la tabla",
                                              choices = c("Mostrar todos los genéricos" = TRUE, 
                                                          "Mostrar genéricos seleccionados" = FALSE),
                                              inline = T),
                                 DT::dataTableOutput("tabla") %>% withSpinner()
                 )
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
                       plot = gen_grafica(genericos = input$selGenerico),
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
}

shinyApp(ui, server)
