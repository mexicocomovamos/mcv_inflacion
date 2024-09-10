# Opciones ----
Sys.setlocale("LC_TIME", "es_ES")
# Sys.setlocale("LC_TIME", "es_ES.UTF-8")
Sys.setlocale("LC_TIME", "español")

options(scipen=999)

# Librerias ----
library(tidyverse)
library(ggrepel)
library(scales)
library(DT)

# 01. Datos ----
datos <- readxl::read_xlsx("02_datos_limpios/total_datos_inflacion.xlsx") %>% 
    as_tibble() %>% 
    mutate(date_shortcut2 = as.numeric(str_extract(date_shortcut, pattern = "\\d"))) %>% 
    mutate(quincena = ifelse((date_shortcut2 %% 2 == 0), yes = 1, no = 2))


# Fecha máxima y quincena máxima: 
maxima_fecha <- datos$date %>% max() # Obtiene la fecha máxima con información disponible
v_quincena   <- datos %>% filter(date == maxima_fecha) %>% pull(quincena) %>% max() # Obtiene la quincena máxima con información disponible

# 02. Controles: ----
sel_genericos <- unique(datos$ccif) %>% sort() # Obtenemos el vector de genéricos para generar el control

# 03. Gráficas ----

## 03.1 Gráfica de evolución ----

# Argumentos de prueba
# genericos = c("Total", "Renta de vivienda", "Ron")
# fecha_inicio = "2015-06-01"
gen_grafica <- function(genericos, fecha_inicio = "2015-06-01"){
    eje_y <- "Índice base\n2ª quincena de julio 2018 = 100"
    nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
    
    subtitulo <- str_c(ifelse(v_quincena == 1, 
                              yes = "A la 1ª quincena de ", 
                              no = "Al mes de "), 
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
    
    fyvs <- datos %>% 
        as_tibble() %>% 
        filter(ccif %in% genericos) %>% 
        mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
        select(date_shortcut, ccif, fecha = date, values) %>% 
        filter(fecha >= fecha_inicio) %>% 
        {if(v_quincena == 1){
            filter(., !date_shortcut %% 2 == 0)
        } else {
            filter(., !date_shortcut %% 2 == 1)
        }} %>% 
        mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
        group_by(ccif) %>%
        arrange(fecha) %>% 
        mutate(tasa = (values/lag(values, 12))-1) %>% 
        mutate(cat = str_c(ccif, "\n", 
                           format(round(values, 1), nsmall = 1) %>% str_squish(), 
                           " [", format(round(tasa*100, 1), nsmall = 1) %>% str_squish(), "%]")) %>% 
        mutate(ccif = factor(ccif, levels = genericos %>% str_replace_all(c("Total" = "General"))))
    
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
        geom_text_repel(data = fyvs %>% filter(fecha == max(fecha)) %>% unique(),
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
        scale_discrete_manual(aesthetics = "linewidth", values = c(1,2)) + 
        scale_discrete_manual(aesthetics = "fontface", values = c("bold","bold")) + 
        scale_color_manual(values = rainbow(n = length(genericos))) +
        scale_x_datetime(
            date_labels = "%b %y",
            breaks = seq.POSIXt(from = max(fyvs$fecha), 
                                to = min(fyvs$fecha), 
                                by = "-6 month"),
            expand = expansion(mult = c(0.02, 0.15))
        ) + 
        scale_y_continuous(expand = expansion(c(0.1, 0.1))) + 
        theme_minimal() +
        labs(
            title = "Índice de precios al consumidor de\ngenéricos seleccionados",
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
gen_barras_cambio_anual <- function(genericos, fecha_inicio = "2015-06-01"){
    nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
    subtitulo <- str_c(ifelse(v_quincena == 1, 
                              yes = "A la 1ª quincena de ", 
                              no = "Al mes de "), 
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
                       " del ", 
                       year(maxima_fecha))
    dpl <- datos %>% 
        as_tibble() %>% 
        filter(ccif %in% genericos) %>% 
        mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
        select(date_shortcut, ccif, fecha = date, values) %>% 
        filter(fecha >= fecha_inicio) %>% 
        {if(v_quincena == 1){
            filter(., !date_shortcut %% 2 == 0)
        } else {
            filter(., !date_shortcut %% 2 == 1)
        }} %>% 
        mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
        arrange(fecha) %>% 
        group_by(ccif) %>%
        mutate(tasa = (values/lag(values, 12))-1) %>% 
        mutate(cat = str_c(ccif, "\n", 
                           format(round(values, 1), nsmall = 1) %>% str_squish(), 
                           " [", format(round(tasa*100, 1), nsmall = 1) %>% str_squish(), "%]")) %>% 
        filter(fecha == max(fecha)) %>% 
        filter(date_shortcut == max(date_shortcut)) %>%
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
    
    dpl %>% 
        ggplot(aes(x = ccif, y = tasa*100, fill = ccif, color = ccif)) + 
        geom_col() + 
        geom_text(aes(label = str_c(round(tasa*100, 1), "%"), 
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
        
        scale_fill_manual(values = rainbow(n = length(genericos)) %>% rev()) +
        scale_color_manual(values = rainbow(n = length(genericos)) %>% rev()) +
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
gen_tabla <- function(genericos, mostrar_todos = F){
    
    if(mostrar_todos == F){
        dx <- datos %>% 
            as_tibble() %>% 
            filter(ccif %in% genericos) %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(date_shortcut, ccif, fecha = date, values) %>% 
            filter(fecha >= fecha_inicio) %>% 
            {if(v_quincena == 1){
                filter(., !date_shortcut %% 2 == 0)
            } else {
                filter(., !date_shortcut %% 2 == 1)
            }} %>% 
            mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
            arrange(fecha) %>% 
            group_by(ccif) %>%
            mutate(tasa = (values/lag(values, 12))-1) %>% 
            mutate(tasa = tasa*100) %>% 
            mutate(tasa = round(tasa, 3)) %>% 
            filter(fecha == maxima_fecha) %>% 
            mutate(values = round(values, 2)) %>% 
            select(Fecha = fecha, 
                   `Genérico` = ccif,
                   `Valor Índice` = values, 
                   `Crecimiento anual` = tasa)
    } else {
        dx <- datos %>% 
            as_tibble() %>% 
            mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
            select(date_shortcut, ccif, fecha = date, values) %>% 
            filter(fecha >= fecha_inicio) %>% 
            {if(v_quincena == 1){
                filter(., !date_shortcut %% 2 == 0)
            } else {
                filter(., !date_shortcut %% 2 == 1)
            }} %>% 
            mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
            arrange(fecha) %>% 
            group_by(ccif) %>%
            mutate(tasa = (values/lag(values, 12))-1) %>% 
            mutate(tasa = tasa*100) %>% 
            mutate(tasa = round(tasa, 3)) %>% 
            mutate(values = round(values, 2)) %>% 
            filter(fecha == maxima_fecha) %>% 
            arrange(-tasa) %>% 
            select(Fecha = fecha, 
                   `Genérico` = ccif,
                   `Valor Índice` = values, 
                   `Crecimiento anual` = tasa)
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
# source("global.R")

ui <- fluidPage(
    h1("Monitor de inflación"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectizeInput("selGenerico", 
                           "Seleccione el(los) genérico(s) a analizar", 
                           choices = sel_genericos, 
                           multiple = TRUE, 
                           selected = "Total"), 
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
                                 DT::dataTableOutput("tabla"))
            )
        )
    ) 
)

# gen_barras_cambio_anual(genericos = "Total")

server <- function(input, output, session) {
    output$grafica_evolucion <- renderPlot({
        gen_grafica(genericos = input$selGenerico)
    })
    
    output$grafica_cambio <- renderPlot({
        gen_barras_cambio_anual(genericos = input$selGenerico)
    })
    
    output$tabla <- DT::renderDT({
        gen_tabla(genericos = input$selGenerico, mostrar_todos = input$rdMostrarTodo)
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
