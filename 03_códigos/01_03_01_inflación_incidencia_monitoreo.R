# Catálogo de productos del INPC
# https://www.inegi.org.mx/app/indicesdeprecios/Estructura.aspx?idEstructura=112001300040&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=INPC%20Nacional%20


#Sys.sleep((60*60*4)+(5))
Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

####################################################
# Seleccionar quincena 
v_quincena <- 1
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

# 0. Procesamiento en loop -----------------------------------------------------
d_inpc <- data.frame()
# Histórico: 
# d_inpc <- readRDS(paste_out("01_03_inpc_complete_prods_ccif.RDS"))

# source("../mcv_infobites/02_códigos/24_inegi_series_juve.R")

tiempo_espera <- 0.1
# ?Sys.sleep()
# i = 449

if(v_quincena==1){
    
    for(i in 1:length(unique(d_inpc_complete$id_ccif_0))) {
        
        repeat {
            tryCatch({
                tiempo_espera_og <- tiempo_espera
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
                Sys.sleep(tiempo_espera_og)
                break
            }, error = function(e){
                message(str_c("Error en ", paste0(d_inpc_complete$id_ccif_0[i], " - ", d_inpc_complete$ccif[i], ". ", "Reintentando!")))
                tiempo_espera_og <- tiempo_espera_og*2
                Sys.sleep(tiempo_espera_og)
            })    
        }
    }
    
} else{
    
    for(i in 1:length(unique(d_inpc_complete$id_ccif_0))) {
        
        repeat {
            tryCatch({
                tiempo_espera_og <- tiempo_espera
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
                Sys.sleep(tiempo_espera_og)
                break
            }, error = function(e){
                message(str_c("Error en ", paste0(d_inpc_complete$id_ccif_0[i], " - ", d_inpc_complete$ccif[i], ". ", "Reintentando!")))
                tiempo_espera_og <- tiempo_espera_og*2
                Sys.sleep(tiempo_espera_og)
            })    
        }
        # print(paste0(d_inpc_complete$id_ccif_0[i], " - ", d_inpc_complete$ccif[i]))
        # tempo <- inegi_series(
        #     serie    = d_inpc_complete$id_inegi_m[i],
        #     token    = v_token_inegi, 
        #     database = "BIE", 
        #     as_tt    = TRUE) %>% 
        #     mutate(
        #         ccif = d_inpc_complete$ccif[i],
        #         id_ccif_0 = d_inpc_complete$id_ccif_0[i],
        #         id_ccif = d_inpc_complete$id_ccif[i],
        #         id_ccif_1 = d_inpc_complete$id_ccif_1[i],
        #         ponderador_inpc_id_ccif_1 = d_inpc_complete$ponderador_inpc_id_ccif_1[i],
        #         id_ccif_2 = d_inpc_complete$id_ccif_2[i],
        #         ponderador_inpc_id_ccif_2 = d_inpc_complete$ponderador_inpc_id_ccif_2[i],
        #         id_ccif_3 = d_inpc_complete$id_ccif_3[i],
        #         ponderador_inpc_id_ccif_3 = d_inpc_complete$ponderador_inpc_id_ccif_3[i],
        #         id_ccif_4 = d_inpc_complete$id_ccif_4[i],
        #         ponderador_inpc_id_ccif_4 = d_inpc_complete$ponderador_inpc_id_ccif_4[i]
        #     )
        # 
        # d_inpc <- bind_rows(d_inpc, tempo)
        # rm(tempo)
        # 
        # Sys.sleep(tiempo_espera)
    }
}

d_inpc <- as_tibble(d_inpc)

if(v_quincena==1){
    
    d_inpc_total <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        unique() %>% 
        filter(date > "2016-01-02") %>% 
        mutate(date = ifelse(date_shortcut %% 2 == 0, ymd(date)+days(15),date),
               date = as.Date.numeric(date)) %>% 
        select(fecha = date, inpc = values) %>% 
        arrange(fecha) %>% 
        glimpse
    
} else{
    
    d_inpc_total <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        unique() %>% 
        filter(date > "2016-01-02") %>% 
        select(fecha = date, inpc = values) %>% 
        arrange(fecha) %>% 
        glimpse
    
}

# Guardamos datos ----

if(v_quincena == 1){
    d_inpc %>%
        as_tibble() %>% 
        openxlsx::write.xlsx("02_datos_limpios/total_datos_inflacion_quincenas.xlsx")    
} else {
    d_inpc %>%
        as_tibble() %>% 
        openxlsx::write.xlsx("02_datos_limpios/total_datos_inflacion_mes.xlsx")    
}

# 1. Incidencia por productos ----
if(v_quincena==1){
    
    d_inpc_prods <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_4) %>% 
        filter(date > "2016-01-02") %>% 
        mutate(date = ifelse(date_shortcut %% 2 == 0, ymd(date)+days(15), date),
               date = as.Date.numeric(date)) %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        unique() %>% 
        glimpse
    
} else{
    
    d_inpc_prods <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_4) %>% 
        filter(date > "2016-01-02") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        unique() %>% 
        glimpse
}

if(v_quincena == 1){
    
    d_incidencia_prods <- d_inpc_prods %>% 
        left_join(d_inpc_total, by = "fecha") %>% 
        unique() %>%
        group_by(ccif, id_ccif_0) %>% 
        filter(ccif == "Jitomate") %>%
        mutate(var_quincenal = ((values - lag(values))/(lag(values)))) %>% 
        mutate(var_anual = c(((values - lag(values, 24))/lag(values, 24)))) %>% 
        mutate(incidencia_quincenal = ((values - lag(values))/lag(inpc))*ponderador) %>% 
        mutate(incidencia_anual = ((values - lag(values, 24))/lag(inpc, 24))*ponderador) %>% 
        arrange(ccif, rev(fecha)) %>% 
        ungroup()

    d_incidencia_prods_last <- d_incidencia_prods %>% 
        filter(fecha == max(fecha)) %>% 
        arrange(-incidencia_quincenal) %>% 
        select(fecha,id_ccif_0,  ccif, var_quincenal, incidencia_quincenal) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    d_incidencia_anual_prods_last <- d_incidencia_prods %>% 
        filter(fecha == max(fecha)) %>% 
        arrange(-incidencia_anual) %>% 
        select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
        filter(!is.na(incidencia_anual)) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    
} else{
    
    d_incidencia_prods <- d_inpc_prods %>% 
        left_join(d_inpc_total, by = "fecha") %>% 
        unique() %>% 
        group_by(ccif, id_ccif_0) %>% 
        mutate(
            # ccif = case_when(
            #     id_ccif_0 == "11_111_1111_272" ~ "Loncherías, fondas, torterías y taquerías",
            #     id_ccif_0 == "04_045_0452_144" ~ "Gas LP",
            #     id_ccif_0 == "08_083_0830_235" ~ "Paquetes de internet, telefonía y televisión de paga",
            #     id_ccif_0 == "04_045_0452_145" ~ "Gas natural",
            #     T ~ ccif
            # ),
            var_mensual = (values - lag(values))/lag(values),
            incidencia_mensual = ((values - lag(values))/lag(inpc))*ponderador,
            var_anual = (values - lag(values, 12))/lag(values, 12),
            incidencia_anual = ((values - lag(values, 12))/lag(inpc, 12))*ponderador
        ) %>% 
        arrange(ccif, rev(fecha)) %>% 
        ungroup() %>% 
        glimpse
    
    d_incidencia_prods_last <- d_incidencia_prods %>% 
        filter(fecha == max(fecha)) %>% 
        arrange(-incidencia_mensual) %>% 
        select(fecha,id_ccif_0,  ccif, var_mensual, incidencia_mensual) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    d_incidencia_anual_prods_last <- d_incidencia_prods %>% 
        filter(fecha == max(fecha)) %>% 
        arrange(-incidencia_anual) %>% 
        select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
        filter(!is.na(incidencia_anual)) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
}

if(v_quincena == 1){
    d_incidencia_prods_last_20 <- d_incidencia_prods_last %>% 
        filter(!is.na(var_quincenal)) %>% 
        filter(n <= 10 | between(n, nrow(.)-10, nrow(.)))    
} else {
    d_incidencia_prods_last_20 <- d_incidencia_prods_last %>% 
        filter(!is.na(var_mensual)) %>% 
        filter(n <= 10 | between(n, nrow(.)-10, nrow(.)))
}

d_incidencia_anual_prods_last_20 <- d_incidencia_anual_prods_last %>% 
    filter(!is.na(var_anual)) %>% 
    filter(n <= 10 | between(n, nrow(.)-10, nrow(.)))

## 1.1. Incidencia mensual/quincenal ----
ifelse(
    v_quincena == 1, 
    titulo <- "Genéricos con mayor y\nmenor incidencia quincenal",
    titulo <- "Genéricos con mayor y\nmenor incidencia mensual"
)

ifelse(
    v_quincena == 1, 
    subtitulo <- 
        paste0(
            "1ª quincena de ", as.character(month(d_incidencia_prods_last$fecha[1], label = T, abbr = F)), " ",
            as.character(year(d_incidencia_prods_last$fecha[1])), 
            " | Entre corchetes se indica la variación quincenal."
        ),
    subtitulo <- 
        paste0(
            str_to_sentence(as.character(month(d_incidencia_prods_last$fecha[1], label = T, abbr = F))), 
            " ",
            as.character(year(d_incidencia_prods_last$fecha[1])), 
            " | Entre corchetes se indica la variación mensual."
        )
)

ifelse(
    v_quincena == 1, 
    nota <- "La incidencia quincenal es la contribución en puntos porcentuales que cada genérico aporta a la inflación general.",
    nota <- "La incidencia mensual es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
)

if(v_quincena==1){
    
    g <- 
        ggplot(
            d_incidencia_prods_last_20,
            aes(
                y = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia_quincenal),
                x = incidencia_quincenal,
                fill = ifelse(n <= 10, "1", "2"),
                label = paste0(
                    round(incidencia_quincenal, 3), "\n[", round(var_quincenal*100, 2), "%]"
                )
            )
        ) +
        geom_col() +
        geom_text(hjust = case_when(between(d_incidencia_prods_last_20$incidencia_quincenal, 0, 0.05)   ~ -0.1, 
                                    between(d_incidencia_prods_last_20$incidencia_quincenal, -0.05, 0)  ~ 1.1, 
                                    d_incidencia_prods_last_20$incidencia_quincenal < -0.05 ~ -0.1, 
                                    d_incidencia_prods_last_20$incidencia_quincenal >  0.05 ~ 1.1 
                                    ), 
                      # if_else(abs(d_incidencia_prods_last_20$incidencia_quincenal)<0.1, "outward", "inward"), 
                  family = "Ubuntu", size = 4, fontface = "bold") + 
        scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
        scale_x_continuous(
            labels = scales::number_format(accuracy = 0.1), 
            expand = expansion(c(0.15, 0.15))
            # limits = 
            #     c((max(abs(d_incidencia_prods_last_20$incidencia_quincenal)))*-1, 
            #            max(abs(d_incidencia_prods_last_20$incidencia_quincenal)))
        ) +
        labs(
            title = titulo,
            subtitle = str_wrap(subtitulo, 40),
            caption = str_wrap(nota, 70)
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
            plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
            plot.margin= margin(0.4, 0.4, 2, 0.4, "cm"), # margin(top,right, bottom,left)
            plot.caption = element_text(size = 15),
            panel.background = element_rect(fill = "transparent",colour = NA),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 15),
            text = element_text(family = "Ubuntu"),
            legend.position = "none"
        )
    
    
} else{
    g <- 
        ggplot(
            d_incidencia_prods_last_20,
            aes(
                y = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia_mensual),
                x = incidencia_mensual,
                fill = ifelse(n <= 10, "1", "2"),
                label = paste0(
                    round(incidencia_mensual, 3), "\n[", round(var_mensual*100, 2), "%]"
                )
            )
        ) +
        geom_col() +
        geom_text(
        hjust = case_when(between(d_incidencia_prods_last_20$incidencia_mensual, 0, 0.05)   ~ -0.1, 
                          between(d_incidencia_prods_last_20$incidencia_mensual, -0.05, 0)  ~ 1.1, 
                          d_incidencia_prods_last_20$incidencia_mensual < -0.05 ~ -0.1, 
                          d_incidencia_prods_last_20$incidencia_mensual >  0.05 ~ 1.1 
        ), 
                  family = "Ubuntu", size = 4, fontface = "bold") +
        scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
        scale_x_continuous(
            labels = scales::number_format(accuracy = 0.1), 
            expand = expansion(c(0.15, 0.15))
            # labels = scales::number_format(accuracy = 0.1), 
            # limits = c((max(abs(d_incidencia_prods_last_20$incidencia_mensual)))*-1, 
            #            max(abs(d_incidencia_prods_last_20$incidencia_mensual)))
        ) +
        labs(
            title = titulo,
            subtitle = str_wrap(subtitulo, 40),
            caption = str_wrap(nota, 70)
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
            plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
            plot.margin= margin(0.4, 0.4, 2, 0.4, "cm"), # margin(top,right, bottom,left)
            plot.caption = element_text(size = 15),
            panel.background = element_rect(fill = "transparent",colour = NA),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 15),
            text = element_text(family = "Ubuntu"),
            legend.position = "none"
        )
}

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi_long.pdf"))

ifelse(
    v_quincena == 1,
    ggsave(g, filename = paste_info("01_01_incidencia_quincenal.png"), 
           width = 10, height = 15, 
           dpi = 200, bg= "transparent"),
    ggsave(g, filename = paste_info("01_01_incidencia_mensual.png"), 
           width = 10, height = 15, 
           dpi = 200, bg= "transparent")
)

# Guardar formato para traducción
g
ggsave(g, filename = paste_info("99_svg/01_03_03_01_01_incidencia_mensual.svg"),
       width = 10, height = 15,
       dpi = 200, bg= "transparent")

## 1.2. Incidencia anual ----
titulo <- "Genéricos con mayor y\nmenor incidencia anual"
nota <- "La incidencia anual es la contribución en puntos porcentuales que cada genérico aporta a la inflación general."
ifelse(
    v_quincena == 1, 
    subtitulo <- 
        paste0(
            "1ª quincena de ", as.character(month(d_incidencia_prods_last$fecha[1], label = T, abbr = F)), " ",
            as.character(year(d_incidencia_prods_last$fecha[1])), 
            " | Entre corchetes se indica la variación anual."
        ),
    subtitulo <- 
        paste0(
            str_to_sentence(as.character(month(d_incidencia_prods_last$fecha[1], label = T, abbr = F))), 
            " ",
            as.character(year(d_incidencia_prods_last$fecha[1])), 
            " | Entre corchetes se indica la variación anual."
        )
)

g <- 
    ggplot(
        d_incidencia_anual_prods_last_20,
        aes(
            y = reorder(str_wrap_long(stringr = ccif, width = 25), incidencia_anual),
            x = incidencia_anual,
            fill = ifelse(n <= 10, "1", "2"),
            label = paste0(
                round(incidencia_anual, 3), "\n[", round(var_anual*100, 2), "%]"
            )
        )
    ) +
    geom_col() +
    geom_text(hjust = case_when(between(d_incidencia_anual_prods_last_20$incidencia_anual, 0, 0.1)   ~ -0.1, 
                                    between(d_incidencia_anual_prods_last_20$incidencia_anual, -0.1, 0)  ~ 1.1, 
                                    d_incidencia_anual_prods_last_20$incidencia_anual < -0.1 ~ -0.1, 
                                    d_incidencia_anual_prods_last_20$incidencia_anual >  0.1 ~ 1.1 
                  )
                  # if_else(abs(d_incidencia_anual_prods_last_20$incidencia_anual)<0.05, "outward", "inward")
              , 
              family = "Ubuntu", size = 4, fontface = "bold") +
    scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
    scale_x_continuous(
        labels = scales::number_format(accuracy = 0.1), 
        expand = expansion(c(0.15, 0.15))
        # limits = c((max(abs(d_incidencia_anual_prods_last_20$incidencia_anual)))*-1, 
        #            max(abs(d_incidencia_anual_prods_last_20$incidencia_anual)))
    ) +
    labs(
        title = titulo,
        subtitle = str_wrap(subtitulo, 40),
        caption = str_wrap(nota, 70)
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
        plot.margin= margin(0.4, 0.4, 2, 0.4, "cm"), # margin(top,right, bottom,left)
        plot.caption = element_text(size = 15),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 15),
        text = element_text(family = "Ubuntu"),
        legend.position = "none"
    )



g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi_long.pdf"))
ggsave(g, filename = paste_info("01_02_incidencia_anual.png"), 
       width = 10, height = 15, 
       dpi = 200, bg= "transparent")

# ggsave(g, filename = paste_info("99_formatos_eps_para_trad/01_03_03_01_02_incidencia_anual.eps"),
#        device = "eps",
#        width = 10, height = 15,
#        dpi = 200, bg= "transparent")


# 2. Incidencia anual por divisiones del CCIF ----

# names(d_inpc)
if(v_quincena==1){
    d_inpc_cats <- d_inpc %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        drop_na(ponderador_inpc_id_ccif_1) %>% 
        filter(date > "2016-01-02") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_1, values) %>% 
        arrange(fecha) %>% 
        glimpse
} else{
    d_inpc_cats <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_1) %>% 
        filter(date > "2016-01-02") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_1, values) %>% 
        arrange(fecha) %>% 
        glimpse
}

d_incidencia_cats <-
    d_inpc_cats %>% 
    left_join(d_inpc_total) %>% 
    group_by(ccif, id_ccif_0) %>% 
    mutate(
        ccif = case_when(
            id_ccif_0 == "03" ~ "Ropa y calzado",
            id_ccif_0 == "04" ~ "Vivienda, electricidad, gas y otros combustibles",
            id_ccif_0 == "05" ~ "Mobiliario y mantenimiento del hogar",
            T ~ ccif)) %>% 
        arrange(fecha, ccif) %>%
    mutate(var_anual = (values - lag(values,12))/lag(values,12)) %>%
    mutate(incidencia_anual = ((values - lag(values,12))/lag(inpc,12))*ponderador) %>%
    ungroup() %>%
    filter(ccif != "Total") %>% 
    glimpse

d_incidencia_cats_last <- d_incidencia_cats %>% 
    filter(fecha == last(fecha)) %>% 
    arrange(-incidencia_anual) %>% 
    select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
    mutate(n = row_number()) %>% 
    glimpse

tt <- d_incidencia_cats                                    %>% 
    distinct() %>% 
    arrange(fecha, desc(incidencia_anual))                        %>% 
    group_by(fecha)                                              %>% 
    # mutate(ranking = 1:13,
    #        ranking = str_pad(ranking, 2, "left", "0"))                                       %>% 
    ungroup() %>% 
    drop_na(incidencia_anual) %>% 
    glimpse
    # d_incidencia_cats %>% 
    # distinct() %>% 
    # arrange(fecha, desc(incidencia_anual))  %>% 
    # group_by(fecha) %>% 
    # # mutate(ranking = 1:14,
    # #        ranking = str_pad(ranking, 2, "left", "0")) %>% 
    # ungroup() %>% 
    # drop_na(incidencia_anual) %>% 
    # glimpse

titulo <- "Incidencia anual por clasificación del\nconsumo individual por finalidades"
subtitulo <- "La incidencia anual es la contribución en puntos porcentuales que cada división aporta a la inflación general."
eje_y <- "Puntos aportados a la inflación general"

if(v_quincena==1){
    nota <- paste0("A la 1ª quincena de ", 
                   as.character(month(max(d_inpc$date), abbr = F, label = T)),
                   " de ", year(max(d_inpc$date)), ".")
} else{
    nota <- paste0(str_to_sentence(as.character(month(max(d_inpc$date), abbr = F, label = T))),
                   " de ", year(max(d_inpc$date)), ".")
}

g <- 
ggplot(
    tt %>% 
        # mutate(
        #     etiqueta = ifelse(fecha == last(fecha), 
        #                       paste0(str_wrap_long(paste0(ranking, " - ", ccif),30), "\n", round(incidencia_anual, 3)), NA)
        # ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        # fill(etiqueta, .direction = "up") %>% 
        ungroup(), 
    aes(
        y = incidencia_anual, 
        x = fecha,
        stratum = ccif, 
        alluvium = id_ccif_0, 
        fill = ccif
    )
)  +
    geom_flow(show.legend = T) +
    geom_text(
        aes(
            y = (d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric)+1.2,
            x = last(tt$fecha),
            label = paste0(
                "Inflación:\n",
                round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric),2),
                # round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric +.01),2),
                "%"
            )
        ),
        col = mcv_semaforo[4],
        hjust = 1,
        # "#ff6260"
        family = "Ubuntu",
        size = 5, 
        fontface = "bold"
    ) +
    # scale_x_date(
    #     expand = expansion(mult = c(0.01, 0.25)),
    #     minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
    #     breaks = seq.Date(to = min(tt$fecha), 
    #                       from = max(tt$fecha), 
    #                       by = "-2 month") %>% rev(),
    #     date_labels = "%b-%y"
    # ) +
    scale_fill_manual("", values = mcv_discrete_12) +
    #scale_y_continuous(labels = scales::comma) +
    labs(
        title = titulo,
        subtitle = str_wrap(subtitulo, 70),
        y = eje_y, x = "",
        caption = nota
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 27, colour = "#777777"),
        plot.margin= margin(0.4, 0.4, 1.5, 0.4, "cm"), # margin(top,right, bottom,left)
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 25),
        text = element_text(family = "Ubuntu"),
        legend.text = element_text(size = 12),
        legend.position = c(1, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(8, 8, 8, 8)
    )

g
ggsave(g + theme(plot.title = element_blank(),
                 plot.subtitle = element_blank(),
                 plot.margin= margin(0.4, 0.4, 0.4, 0.4, "cm")), filename = paste_info("01_03_incidencia_anual_sin_fondo.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_incidencia_anual.png"), 
       width = 9, height = 16, 
       dpi = 200, bg= "transparent")

# 3. Incidencia anual por concepto y componente ----
d_inpc_ponds_comp <- readxl::read_excel(paste_inp("01_03_inpc_concepto_ponds.xlsx")) %>% 
    glimpse

# d_inpc_ponds_comp_prod <- readxl::read_excel(paste_inp("01_03_inpc_concepto_ccif_ponds.xlsx")) %>% 
#     glimpse
d_inpc_ponds_comp_prod <- readxl::read_excel("01_datos_crudos/01_03_inpc_concepto_ccif_ponds_NEW.xlsx") %>% 
    glimpse


names(d_inpc_ponds_comp_prod) %>% writeLines()

if(v_quincena==1){
    
    d_subyacente <- d_inpc %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        filter(date > "2016-01-02") %>% 
        filter(id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$subyacente=="X"]) %>% 
        mutate(
            subyacente_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_mercancias=="X"] ~ "Mercancías",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_servicios=="X"] ~ "Servicios",
                T ~ NA_character_
            ),
            merc_tipo = 
                case_when(
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_merc_alimentos_bebidas_y_tabaco=="X"] ~ "Alimentos, bebidas y tabaco",
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_merc_mercancias_no_alimenticias=="X"] ~ "Mercancías no alimenticias",
                    T ~ NA_character_
                ),
            serv_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_educacion_colegiaturas=="X"] ~ "Educación (colegiaturas)",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_vivienda=="X"] ~ "Vivienda",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_otros_servicios=="X"] ~ "Otros servicios",
                T ~ NA_character_
            ),
            tipo = ifelse(
                is.na(serv_tipo), merc_tipo, serv_tipo
            )
        ) %>%
        glimpse
    
    d_no_subyacente <- d_inpc %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        filter(date > "2016-01-02") %>% 
        filter(id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_subyacente=="X"]) %>% 
        mutate(
            no_subyacente_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_agropecuarios=="X"] ~ "Agropecuarios",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_energeticos_y_tarifas_autorizadas_por_el_gobierno=="X"] ~ "Energéticos y tarifas autorizadas por el gobierno",
                T ~ NA_character_
            ),
            agropec_tipo = 
                case_when(
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_frutas_y_verduras=="X"] ~ "Frutas y verduras",
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_pecuarios=="X"] ~ "Pecuarios",
                    T ~ NA_character_
                ),
            energ_tarif_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_energeticos=="X"] ~ "Energéticos",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_tarifas_autorizadas_por_el_gobierno=="X"] ~ "Tarifas autorizadas por el gobierno",
                T ~ NA_character_
            ),
            tipo = ifelse(
                is.na(energ_tarif_tipo), agropec_tipo, energ_tarif_tipo
            )
        ) %>% 
        glimpse
    
    
} else{
    
    d_subyacente <- d_inpc %>% 
        filter(id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$subyacente=="X"]) %>% 
        mutate(
            subyacente_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_mercancias=="X"] ~ "Mercancías",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_servicios=="X"] ~ "Servicios",
                T ~ NA_character_
            ),
            merc_tipo = 
                case_when(
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_merc_alimentos_bebidas_y_tabaco=="X"] ~ "Alimentos, bebidas y tabaco",
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_merc_mercancias_no_alimenticias=="X"] ~ "Mercancías no alimenticias",
                    T ~ NA_character_
                ),
            serv_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_educacion_colegiaturas=="X"] ~ "Educación (colegiaturas)",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_vivienda=="X"] ~ "Vivienda",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$suby_serv_otros_servicios=="X"] ~ "Otros servicios",
                T ~ NA_character_
            ),
            tipo = ifelse(
                is.na(serv_tipo), merc_tipo, serv_tipo
            )
        ) %>% 
        glimpse
    
    d_no_subyacente <- d_inpc %>% 
        filter(id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_subyacente=="X"]) %>% 
        mutate(
            no_subyacente_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_agropecuarios=="X"] ~ "Agropecuarios",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_energeticos_y_tarifas_autorizadas_por_el_gobierno=="X"] ~ "Energéticos y tarifas autorizadas por el gobierno",
                T ~ NA_character_
            ),
            agropec_tipo = 
                case_when(
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_frutas_y_verduras=="X"] ~ "Frutas y verduras",
                    id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_pecuarios=="X"] ~ "Pecuarios",
                    T ~ NA_character_
                ),
            energ_tarif_tipo = case_when(
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_energeticos=="X"] ~ "Energéticos",
                id_ccif_0 %in% d_inpc_ponds_comp_prod$id_ccif_0[d_inpc_ponds_comp_prod$no_suby_tarifas_autorizadas_por_el_gobierno=="X"] ~ "Tarifas autorizadas por el gobierno",
                T ~ NA_character_
            ),
            tipo = ifelse(
                is.na(energ_tarif_tipo), agropec_tipo, energ_tarif_tipo
            )
        ) %>% 
        glimpse
    
    
}

d_inpc_suby_no_suby_tipo <- d_subyacente %>% 
    mutate(inpc_tipo = "Subyacente") %>% 
    rename(suby_no_suby_tipo = subyacente_tipo) %>% 
    select(fecha = date, ccif, id_ccif_0, inpc_tipo, suby_no_suby_tipo, tipo, values,ponderador = ponderador_inpc_id_ccif_4) %>% 
    bind_rows(
        d_no_subyacente %>% 
            mutate(inpc_tipo = "No subyacente") %>% 
            rename(suby_no_suby_tipo = no_subyacente_tipo) %>% 
            select(fecha = date, ccif, id_ccif_0, inpc_tipo, suby_no_suby_tipo, tipo, values,ponderador = ponderador_inpc_id_ccif_4)
    ) %>% 
    arrange(fecha) %>% 
    glimpse


d_incidencia_suby_no_suby_tipo <- d_inpc_suby_no_suby_tipo %>% 
    group_by(fecha, inpc_tipo, suby_no_suby_tipo, tipo) %>%
    summarise(values = weighted.mean(x = values, w = ponderador, na.rm = T),
              ponderador = sum(ponderador, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(
        d_inpc_total
    ) %>% 
    group_by(inpc_tipo, suby_no_suby_tipo, tipo) %>%
    mutate(
        var_anual = (values - lag(values,12))/lag(values,12),
        incidencia_anual = ((values - lag(values,12))/lag(inpc,12))*ponderador
    ) %>% 
    ungroup() %>% 
    glimpse

d_incidencia_suby_no_suby <- d_inpc_suby_no_suby_tipo %>% 
    group_by(fecha, inpc_tipo, suby_no_suby_tipo) %>%
    summarise(values = weighted.mean(x = values, w = ponderador, na.rm = T),
              ponderador = sum(ponderador, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(
        d_inpc_total
    ) %>% 
    group_by(inpc_tipo, suby_no_suby_tipo) %>%
    mutate(
        var_anual = (values - lag(values,12))/lag(values,12),
        incidencia_anual = ((values - lag(values,12))/lag(inpc,12))*ponderador
    ) %>% 
    ungroup() %>% 
    glimpse

## 3.1. Por componente ----
tt <- d_incidencia_suby_no_suby                                    %>% 
    arrange(fecha, desc(incidencia_anual))                        %>% 
    group_by(fecha)                                              %>% 
    mutate(ranking = row_number(),
           ranking = str_pad(ranking, 2, "left", "0"))                                       %>% 
    ungroup() %>% 
    drop_na(incidencia_anual) %>% 
    glimpse

titulo <- "Incidencia anual por componente del INPC"
subtitulo <- "La incidencia anual es la contribución en puntos porcentuales que cada componente aporta a la inflación general."
eje_y <- "Puntos aportados a la inflación general"
if(v_quincena==1){
    nota <- paste0("A la 1ª quincena de ", 
                   as.character(month(max(d_inpc$date), abbr = F, label = T)),
                   " de ", year(max(d_inpc$date)), ".")
} else{
    nota <- paste0(str_to_sentence(as.character(month(max(d_inpc$date), abbr = F, label = T))),
                   " de ", year(max(d_inpc$date)), ".")
}

g <- 
    ggplot(
        tt %>% 
            mutate(
                etiqueta = ifelse(fecha == last(fecha), 
                                  paste0(inpc_tipo, "\n", str_wrap_long(paste0(ranking, " - ", suby_no_suby_tipo),30), "\n", round(incidencia_anual, 3)), NA)
            ) %>% 
            group_by(suby_no_suby_tipo) %>% 
            fill(etiqueta, .direction = "up") %>% 
            ungroup() %>% 
            mutate(
                ord = case_when(
                    suby_no_suby_tipo == "Mercancías" ~ "01",
                    suby_no_suby_tipo == "Servicios" ~ "02",
                    suby_no_suby_tipo == "Agropecuarios" ~ "03",
                    T ~ "04"
                )
            ), 
        aes(
            y = incidencia_anual, 
            x = fecha,
            stratum = ord, 
            alluvium = suby_no_suby_tipo, 
            fill = reorder(etiqueta, as.numeric(ord))
        )
    )  +
    geom_flow(show.legend = T) +
    geom_text(
        aes(
            y = (d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric)+1,
            # y = 12.5,
            x = last(tt$fecha),
            label = paste0(
                "Inflación:\n",
                # round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric+.01),2),
                round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric),2),
                "%"
            )
        ),
        col = mcv_semaforo[4],
        family = "Ubuntu",
        hjust = 1,
        size = 5, fontface = "bold"
    ) +
    scale_x_date(
        expand = expansion(mult = c(0.01, 0.25)),
        minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
        breaks = seq.Date(to = min(tt$fecha), 
                          from = max(tt$fecha), 
                          by = "-2 month") %>% rev(),
        date_labels = "%b-%y"
    ) +
    scale_fill_manual("", values = c(mcv_discrete_12[7], mcv_discrete_12[9],
                                     mcv_discrete_12[4], mcv_discrete_12[6])) +
    labs(
        title = titulo,
        subtitle = str_wrap(subtitulo, 70),
        y = eje_y, x = "",
        caption = nota
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle = element_text(size = 27, colour = "#777777"),
        plot.margin= margin(0.4, 0.4, 1.5, 0.4, "cm"), # margin(top,right, bottom,left)
        plot.caption = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 25),
        text = element_text(family = "Ubuntu"),
        legend.text = element_text(size = 15),
        legend.position = c(1, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(8, 8, 8, 8)
    )

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_04_incidencia_anual_componente.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")

## 3.2. Por concepto ----

tt <- d_incidencia_suby_no_suby_tipo                                %>% 
    arrange(fecha, desc(incidencia_anual))                          %>% 
    group_by(fecha, inpc_tipo)                                      %>% 
    mutate(ranking = row_number(),
           ranking = str_pad(ranking, 2, "left", "0"))                                       %>% 
    ungroup() %>% 
    drop_na(incidencia_anual) %>% 
    glimpse

titulo <- "Incidencia anual por componente y\nconcepto del INPC"
if(v_quincena==1){
    nota <- paste0("La incidencia anual es la contribución en puntos porcentuales que cada división aporta a la inflación general.",
                   "\nA la 1ª quincena de ", 
                   as.character(month(max(d_inpc$date), abbr = F, label = T)),
                   " de ", year(max(d_inpc$date)), ".")
} else{
    nota <- paste0("La incidencia anual es la contribución en puntos porcentuales que cada división aporta a la inflación general.\n",
                   str_to_sentence(as.character(month(max(d_inpc$date), abbr = F, label = T))),
                   " de ", year(max(d_inpc$date)), ".")
}

eje_y <- "Puntos aportados a la inflación general"
g1 <- 
    ggplot(
        tt %>% 
            mutate(
                etiqueta = ifelse(
                    fecha == last(fecha), 
                    paste0(
                        str_wrap_long(tipo,30), 
                        "\n", round(incidencia_anual, 3)
                    ), NA
                )
                    
            ) %>% 
            group_by(tipo) %>% 
            fill(etiqueta, .direction = "up") %>% 
            ungroup() %>% 
            mutate(
                ord = case_when(
                    tipo == "Alimentos, bebidas y tabaco" ~ "01",
                    tipo == "Mercancías no alimenticias" ~ "02",
                    tipo == "Vivienda" ~ "03",
                    tipo == "Educación (colegiaturas)" ~ "04",
                    tipo == "Otros servicios" ~ "05",
                    tipo == "Frutas y verduras" ~ "06",
                    tipo == "Pecuarios" ~ "07",
                    tipo == "Energéticos" ~ "08",
                    T ~ "09"
                )
            ) %>% 
            filter(inpc_tipo=="Subyacente"), 
        aes(
            y = incidencia_anual, 
            x = fecha,
            stratum = ord, 
            alluvium = tipo, 
            fill = reorder(etiqueta, as.numeric(ord))
        )
    )  +
    facet_wrap(~reorder(inpc_tipo, desc(inpc_tipo)), ncol = 1) +
    scale_x_date(
        
        expand = expansion(mult = c(0.01, 0.25)),
        minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
        breaks = seq.Date(from = max(tt$fecha), 
                          to =  min(tt$fecha), 
                          by = "-2 month"),
        date_labels = "%b-%y"
        
    ) +
    scale_y_continuous("", limits = c(-1,7), breaks = seq(-1,7,1), 
                       labels = scales::number_format(accuracy = 1L, suffix = ".0")) +
    # geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +
    geom_flow(show.legend = T) +
    scale_fill_manual("", values = mcv_discrete_12[1:5]) +
    theme_minimal()  +
    labs(
        title = titulo,
        y = eje_y, 
        x = NULL
    ) +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.margin= margin(0.4, 0.4, 1, 0.4, "cm"), # margin(top,right, bottom,left)
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "red"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        text = element_text(family = "Ubuntu"),
        legend.text = element_text(size = 15),
        legend.position = c(1, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(-2, 3, 6, 8)
    )

df_data <- tt %>% 
    mutate(
        etiqueta = ifelse(
            fecha == last(fecha), 
            paste0(
                str_wrap_long(tipo,30), 
                "\n", round(incidencia_anual, 3)
            ), NA
        )
        
    ) %>% 
    group_by(tipo) %>% 
    fill(etiqueta, .direction = "up") %>% 
    ungroup() %>% 
    mutate(
        ord = case_when(
            tipo == "Alimentos, bebidas y tabaco" ~ "01",
            tipo == "Mercancías no alimenticias" ~ "02",
            tipo == "Vivienda" ~ "03",
            tipo == "Educación (colegiaturas)" ~ "04",
            tipo == "Otros servicios" ~ "05",
            tipo == "Frutas y verduras" ~ "06",
            tipo == "Pecuarios" ~ "07",
            tipo == "Energéticos" ~ "08",
            T ~ "09"
        )
    ) %>% 
    filter(!inpc_tipo=="Subyacente")


g2 <- 
    ggplot(
       df_data, 
        aes(
            y = incidencia_anual, 
            x = fecha,
            stratum = ord, 
            alluvium = tipo, 
            fill = reorder(etiqueta, as.numeric(ord))
        )
    )  +
    facet_wrap(~reorder(inpc_tipo, desc(inpc_tipo)), ncol = 1) +
    scale_x_date(

        expand = expansion(mult = c(0.01, 0.25)),
        minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
        breaks = seq.Date(from = max(tt$fecha),
                          to = min(tt$fecha) ,
                          by = "-2 month"),
        date_labels = "%b-%y"

    ) +
    scale_y_continuous("", 
                       expand = expansion(c(0.1, 0.2)),
                       # limits = c(-2,5),
                       # breaks = seq(-2,5,1), 
                       labels = scales::number_format(accuracy = 1L, suffix = ".0")) +
    # geom_hline(yintercept = 0, color = "black", linewidth = 0.9) +
    geom_flow(show.legend = T) +
    scale_fill_manual("", values = mcv_discrete_12[6:9]) +
    theme_minimal() +
    labs(caption = nota, 
         y = eje_y, 
         x = NULL) +
    theme(
        plot.caption = element_text(size = 12),
        plot.margin= margin(0.4, 0.4, 1.5, 0.4, "cm"), # margin(top,right, bottom,left)
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25, color = "red"),
        axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 15),
        text = element_text(family = "Ubuntu"),
        legend.text = element_text(size = 15),
        legend.position = c(0.985, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 4.5, 6, 8)
    )

g <- grid.arrange(g1, g2)
g <- ggimage::ggbackground(as_ggplot(g), paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_05_incidencia_anual_concepto.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")

saveRDS(
    d_inpc,
    paste_out(
        "01_03_inpc_complete_prods_ccif.RDS"
    )
)


# 4. Monitoreo de productos seleccionados --------------------------------------
# Anterior version: 
# d_inpc_ccif_ids <- readxl::read_excel(paste_inp("01_03_inpc_ccif_ids.xlsx")) %>% 
#     glimpse
d_inpc_ccif_ids <- d_inpc_complete %>% select(ccif, id_ccif_0, id_ccif, id_inegi_m, id_inegi_q)

## 4.1. Clasificación del consumo individual por finalidades(CCIF) ----

nota <- "*Las desagregaciones del INPC solo tienen valor informativo."

if(v_quincena==1){
    
    
    d_01_ccif <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "General") %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Alimentos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Alimentos")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Vivienda"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Vivienda"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Vivienda")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Salud"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Salud"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Salud")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Servicios de transporte de pasajeros"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Servicios de transporte"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Servicios de transporte")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Servicios educativos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Educación"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Educación")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Restaurantes y servicios de alojamiento"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Restaurantes y hoteles"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Restaurantes y hoteles")
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
} else{
    # unique(d_01_ccif$tipo)
    # d_inpc %>%
    #     filter(ccif == "Servicios de transporte de pasajeros")
    
    d_01_ccif <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "General") %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Alimentos") %>% 
                # filter(id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Alimentos")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Vivienda"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Vivienda"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Vivienda")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Salud"
                       # ==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif==
                                                       # "Salud"
                                                       ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Salud")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Servicios de transporte de pasajeros"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Servicios de transporte"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Servicios de transporte")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Servicios educativos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Educación"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Educación")
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Restaurantes y servicios de alojamiento"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Restaurantes y hoteles"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Restaurantes y hoteles")
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}

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
        month(max(d_01_ccif$fecha), label = T, abbr = F),
        " de ", year(max(d_01_ccif$fecha)), 
        "; [tasa de variación anual]"
    ),
    subtitulo <- paste0(
        str_to_sentence(month(max(d_01_ccif$fecha), label = T, abbr = F)),
        " de ", year(max(d_01_ccif$fecha)),
        "; [tasa de variación anual]"
    )
)

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
g <- 
    ggplot(data = d_01_ccif %>% 
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_01_ccif$fecha)), "month"), 
                          to =floor_date(as.Date("2015-09-01")+(((month(max(d_01_ccif$fecha))-1))), "month"), 
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
if(v_quincena == 1){
    
    
    d_02_01_pan_cereales <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan y cereales"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan y cereales"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan y cereales", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Harinas de trigo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Harinas de trigo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Harinas de trigo", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Maíz"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Maíz"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Maíz", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan de caja"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan de caja"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan de caja", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan dulce"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan dulce"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan dulce", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Tortilla de maíz"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Tortilla", ord = 7)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
} else{
    
    d_02_01_pan_cereales <- d_inpc %>% 
        filter(id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan y cereales"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan y cereales"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan y cereales", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Harinas de trigo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Harinas de trigo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Harinas de trigo", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Maíz"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Maíz"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Maíz", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan de caja"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan de caja"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan de caja", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pan dulce"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan dulce"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pan dulce", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Tortilla de maíz"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Tortilla", ord = 7)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 5.5,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_01_pan_cereales$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_01_pan_cereales$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_01_ali_pan_cer.png"), 
       #device = "png", type = "cairo", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.2. Carnes ----
if(v_quincena == 1){
    
    d_02_02_carnes <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carnes"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carnes"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Carnes", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carne de res"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de res"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Res", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carne de cerdo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Cerdo", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pollo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pollo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pollo", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
} else{
    
    d_02_02_carnes <- d_inpc %>% 
        filter(id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carnes"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carnes"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Carnes", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carne de res"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de res"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Res", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Carne de cerdo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Cerdo", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pollo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pollo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pollo", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 5.5,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_02_carnes$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_02_carnes$fecha))))), "month"), 
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("02_02_02_ali_carnes.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")



### 4.2.3. Leche, quesos y huevo ----
if(v_quincena == 1){
    
    
    d_02_03_lácteos <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Leche, otros productos lácteos y huevos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Leche quesos y huevos"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Leche, otros productos lácteos y huevos", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Leche pasteurizada y fresca"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Leche", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Queso Oaxaca y asadero"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Queso oaxaca y asadero"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Queso Oaxaca y asadero", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Huevo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Huevo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Huevo", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
    
} else{
    
    d_02_03_lácteos <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Leche, otros productos lácteos y huevos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Leche quesos y huevos"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Leche, otros productos lácteos y huevos", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Leche", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Queso Oaxaca y asadero"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Queso oaxaca y asadero"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Queso Oaxaca y asadero", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Huevo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Huevo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Huevo", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 4,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_03_lácteos$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_03_lácteos$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_03_ali_lácteos.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.4. Frutas ----
if(v_quincena == 1){
    
    d_02_04_frutas <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Frutas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Frutas"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Fruta", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Aguacate"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aguacate"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aguacate", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Manzana"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Manzana"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Manzana", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pera"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pera"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pera", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Plátanos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Plátanos"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Plátano", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Uva"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Uva"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Uva", ord = 7)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
    
} else{
    
    d_02_04_frutas <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Frutas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Frutas"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Fruta", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Aguacate"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aguacate"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aguacate", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Manzana"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Manzana"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Manzana", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Pera"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pera"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Pera", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Plátanos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Plátanos"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Plátano", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Uva"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Uva"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Uva", ord = 7)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 5.5,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_04_frutas$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_04_frutas$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_04_ali_frutas.png"), 
       #type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.5. Legumbres ----
if(v_quincena == 1){
    
    d_02_05_legum <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Legumbres y hortalizas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Legumbres y hortalizas"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Legumbres y hortalizas", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Calabacita"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Calabacita"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Calabacita", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Chile serrano"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Chile serrano"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Chile serrano", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Jitomate"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jitomate"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Jitomate", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Tomate verde"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Tomate verde"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Tomate verde", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Nopales"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Nopales"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Nopales", ord = 7)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Papa y otros tubérculos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Papa", ord = 8)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
    
} else{
    
    d_02_05_legum <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Legumbres y hortalizas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Legumbres y hortalizas"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Legumbres y hortalizas", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Calabacita"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Calabacita"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Calabacita", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Chile serrano" 
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Chile serrano"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Chile serrano", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Jitomate"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jitomate"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Jitomate", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Tomate verde"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Tomate verde"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Tomate verde", ord = 6)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Nopales"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Nopales"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Nopales", ord = 7)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Papa y otros tubérculos"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Papa", ord = 8)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 4.5,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_05_legum$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_05_legum$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_05_ali_legum.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.6. Aceites y grasas ----
if(v_quincena == 1){
    
    d_02_06_aceites <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aceites y grasas"]) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aceites y grasas", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Aceites y grasas vegetales comestibles"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aceites y grasas vegetales", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Manteca de cerdo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Manteca de cerdo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Manteca", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Mantequilla"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Mantequilla"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Mantequilla", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
    
} else{
    
    d_02_06_aceites <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Aceites y grasas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aceites y grasas"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aceites y grasas", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Aceites y grasas vegetales comestibles"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Aceites y grasas vegetales", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Manteca de cerdo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Manteca de cerdo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Manteca", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Mantequilla"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Mantequilla"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Mantequilla", ord = 5)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}


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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 4,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_06_aceites$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_06_aceites$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_06_ali_aceites.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.7. Azúcares ----
if(v_quincena == 1){
    
    d_02_07_azucares <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Azúcar, productos para confitería y postres" 
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Azúcar mermeladas miel chocolates y dulces"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Azúcares", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Azúcar"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Azúcar"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Azúcar", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Chocolate y productos de confitería"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Chocolate y productos de confitería"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Chocolate", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Gelatina, miel y mermeladas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Gelatina miel y mermeladas"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Gelatina, miel y mermeladas", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Helados, nieves y paletas de hielo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Helados nieves y paletas de hielo"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Helados", ord = 6)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
} else{
    
    d_02_07_azucares <- d_inpc %>% 
        filter(ccif == "Alimentos"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Alimentos"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Alimentos", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Azúcar, productos para confitería y postres"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Azúcar mermeladas miel chocolates y dulces"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Azúcares", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Azúcar"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Azúcar"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Azúcar", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Chocolate y productos de confitería"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Chocolate y productos de confitería"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Chocolate", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Gelatina, miel y mermeladas"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Gelatina miel y mermeladas"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Gelatina, miel y mermeladas", ord = 5)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Helados, nieves y paletas de hielo"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Helados nieves y paletas de hielo"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Helados", ord = 6)
        ) %>% 
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}

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
               unique() %>% 
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
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
        size = 4,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_07_azucares$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_07_azucares$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_02_07_ali_azucar.png"), 
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


### 4.2.8 Frutas y verduras seleccionadas ----
eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
# \n**La línea punteada representa la implementación del PACIC.

# unique(d_inpc$ccif) %>% sort()

fyvs <- d_inpc %>% 
    as_tibble() %>% 
    filter(ccif %in% c("Limón",#
                       "Cebolla",
                       "Plátanos",
                       "Jitomate", #
                       "Chile serrano",
                       "Zanahoria", #
                       "Total", 
                       "Manzana",#
                       "Papa y otros tubérculos")) %>% 
    mutate(ccif = ifelse(ccif == "Papa y otros tubérculos", yes = "Papa", no = ccif), 
           ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>% 
    select(date_shortcut, ccif, fecha = date, values) %>% 
    filter(fecha >= "2015-06-01") %>% 
    {if(v_quincena == 1){
      filter(., !date_shortcut %% 2 == 0)
    } else {
      filter(., !date_shortcut %% 2 == 1)
    }} %>%
    mutate(grosor = ifelse(ccif == "General", yes = "General", no = "Genéricos")) %>% 
    arrange(fecha) %>% 
    group_by(ccif) %>%
    mutate(tasa = (values/lag(values, 12))-1) %>% 
    # mutate(tasa = (values/lag(values, 12))-1) %>% 
    mutate(cat = str_c(ccif, "\n", 
                       format(round(values, 1), nsmall = 1) %>% str_squish(), 
                       " [", format(round(tasa*100, 2), nsmall = 1) %>% str_squish(), "%]"))

g <- fyvs %>% 
    ggplot(aes(x = fecha, 
               y = values, 
               group = ccif, 
               color = ccif)) +
    geom_line(aes(linewidth = grosor),
              lineend = "round",
              show.legend = F) +
    geom_point(data = fyvs %>% filter(fecha == max(fecha)),
               size = 2.5,
               show.legend = F) +
    ggrepel::geom_text_repel(data = fyvs %>% filter(fecha == max(fecha)),
                             aes(label = cat, 
                                 fontface = grosor), 
                             direction = "y",
                             family = "Ubuntu", 
                             nudge_x = 100, 
                             hjust = "left",
                             size = 5,
                             segment.curvature = -0.1,
                             segment.ncp = 3,
                             segment.angle = 20,
                             segment.color = NA,
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
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_01_pan_cereales$fecha)), "month"),
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_01_pan_cereales$fecha))))), "month"),
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_09_01_indice_de_precios_frutas_y_verduras_seleccionadas.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")

### 4.2.9 Frutas y legumbres ----

eje_y <- "Índice base 2ª quincena de julio 2018 = 100"
nota <- "*Las desagregaciones del INPC solo tienen valor informativo."
# \n**La línea punteada representa la implementación del PACIC.

unique(d_inpc$ccif) %>% sort()

iafl <- d_inpc %>% 
    as_tibble() %>% 
    filter(ccif %in% c(
        "Total", 
        "Alimentos", 
        "Frutas y frutos secos", 
        "Hortalizas, tubérculos, plátanos de cocción y legumbres" 
    )) %>% 
    mutate(ccif = ifelse(ccif == "Total", yes = "General", no = ccif)) %>%
    select(date_shortcut, ccif, fecha = date, values) %>% 
    filter(fecha >= "2015-06-01") %>% 
    {if(v_quincena == 1){
      filter(., !date_shortcut %% 2 == 0)
    } else {
      filter(., !date_shortcut %% 2 == 1)
    }} %>%
    mutate(grosor = ifelse(ccif == "Total", yes = "Total", no = "Genéricos")) %>% 
    arrange(fecha) %>% 
    group_by(ccif) %>%
    mutate(tasa = (values/lag(values, 12))-1) %>%
    mutate(cat = str_c(ccif, "\n", 
                       format(round(values, 1), nsmall = 1) %>% str_squish(), 
                       " [", format(round(tasa*100, 2), nsmall = 1) %>% str_squish(), "%]"))

unique(iafl$ccif)

g <- iafl %>% 
    ggplot(aes(x = fecha, 
               y = values, 
               group = ccif, 
               color = ccif)) +
    geom_line(aes(linewidth = grosor, alpha = grosor),
              lineend = "round",
              show.legend = F) +
    geom_point(data = iafl %>% filter(fecha == max(fecha)),
               size = 2.5,
               show.legend = F) +
    ggrepel::geom_text_repel(data = iafl %>% filter(fecha == max(fecha)),
                             aes(label = cat %>% str_wrap(20), 
                                 fontface = grosor), 
                             direction = "y",
                             family = "Ubuntu", 
                             nudge_x = 100, 
                             hjust = "left",
                             size = 4.5,
                             segment.curvature = -0.1,
                             segment.ncp = 3,
                             segment.angle = 20,
                             segment.color = NA,
    ) + 
    scale_discrete_manual(aesthetics = "linewidth", values = c(2,3)) + 
    scale_discrete_manual(aesthetics = "alpha", values = c(0.8,0.95)) + 
    scale_discrete_manual(aesthetics = "fontface", values = c("bold","bold")) + 
    scale_color_manual(values = c("General" = "#4D5BF0",
                                  "Frutas y frutos secos" = "#0ACF5F",
                                  "Hortalizas, tubérculos, plátanos de cocción y legumbres" = "#E84D9A",
                                  "Alimentos" = "#E8866D")) + 
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(d_02_01_pan_cereales$fecha)), "month"),
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_02_01_pan_cereales$fecha))))), "month"),
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_10_01_indice_de_precios_frutas_y_legumbres.png"),
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.3. Bebidas no alcohólicas ----

if(v_quincena == 1){
    
    d_03_bebidas <- d_inpc %>% 
        filter(ccif == "Bebidas no alcohólicas"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Bebidas no alcohólicas"]
            ) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Bebidas no alcohólicas", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Café y sustitutos de cafe"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Café té y cacao"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Café y sustitutos de cafe", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Agua embotellada"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Agua embotellada"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Agua embotellada", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Jugos o néctares envasados"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jugos o néctares envasados"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Jugos envasados", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Refrescos envasados"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Refrescos envasados"]
                    ) %>% 
                select(date_shortcut, fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Refrescos envasados", ord = 5)
        ) %>%
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
    
} else{
    
    d_03_bebidas <- d_inpc %>% 
        filter(ccif == "Bebidas no alcohólicas"
            # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Bebidas no alcohólicas"]
            ) %>% 
        select(fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = "Bebidas no alcohólicas", ord = 1) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Café y sustitutos de cafe"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Café té y cacao"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Café y sustitutos de cafe", ord = 2)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Agua embotellada"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Agua embotellada"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Agua embotellada", ord = 3)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Jugos o néctares envasados"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jugos o néctares envasados"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Jugos envasados", ord = 4)
        ) %>% 
        bind_rows(
            d_inpc %>% 
                filter(ccif == "Refrescos envasados"
                    # id_ccif_0==d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Refrescos envasados"]
                    ) %>% 
                select(fecha = date, values) %>% 
                arrange(fecha) %>% 
                mutate(tipo = "Refrescos envasados", ord = 5)
        ) %>%
        filter(fecha >= "2002-07-01") %>% 
        glimpse
    
}

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
               arrange(fecha) %>% 
               group_by(tipo) %>% 
               mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
               filter(fecha >= "2015-06-01"),
           aes(
               x = fecha,
               y = values,
               group = reorder(tipo, ord),
               col = reorder(tipo, ord),
               label = ifelse(
                   fecha == max(fecha),
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
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == "Bebidas no alcohólicas", mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(d_03_bebidas$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(d_03_bebidas$fecha))))), "month"), 
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


g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("02_03_bebidas.png"),
       # type = "cairo", device = "png", 
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.5. Productos de salud --------------------------------------------------------

# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.1 Productos, artefactos y equipos médicos >
# 06.1.1 Productos farmacéuticos 

# ---- Enlistar productos del catálogo
v_productos <- c(
    "Medicamentos",
    "Analgésicos",
    "Antibióticos", "Antigripales"
    ,
    "Cardiovasculares", "Medicamentos para diabetes"
    )

# ---- Seleccionar productos del catálogo de identificadores 
# which(d_inpc_ccif_ids$ccif == "Analgésicos")
df_productos <- d_inpc_ccif_ids     %>% 
    filter(ccif %in% v_productos)   %>% 
    glimpse

# ---- Obtener identificadores de productos seleccionados
v_ids <- unique(df_productos$id_ccif_0) # Identificadores 

# ---- Importar las series de los productos 
df_series <- data.frame()
# i = 1
for(i in 1:length(v_ids)){
    
    # Imprimir vuelta y producto
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    # Importar datos del producto de INEGI
    df_data <- d_inpc %>% 
        filter(id_ccif_0==v_ids[i]) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = v_productos[i], ord = i) 
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 

# Filtrar fechas 
df_06_01_farmaceuticos <- df_series                      %>% 
    filter(fecha >= "2002-07-01")

# Dejar datos mensuales o quincenales
ifelse(v_quincena == 1, 
       # Para la serie quincenal, dejar solo datos de la primera quincena
       df_06_01_farmaceuticos <- df_06_01_farmaceuticos %>% 
           filter(!date_shortcut %% 2 == 0)             %>% 
           glimpse, 
       # Serie mensual 
       df_06_01_farmaceuticos %>% select(-date_shortcut) %>% 
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
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                fecha == max(fecha),
                paste0(str_wrap(tipo,20), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
                NA
            )
        ))+
    geom_line(size = 2.5, lineend = "round", show.legend = F, 
              aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
                  linetype = if_else(tipo == v_productos[1], "solid", "dashed"))) + 
    ggrepel::geom_text_repel(
        aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
        nudge_x = 100, direction = "y", hjust = "left",
        size = 4,
        segment.curvature = -0.1,
        segment.ncp = 3,
        segment.angle = 20,
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_06_01_farmaceuticos$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_06_01_farmaceuticos$fecha))))), "month"), 
                          by = "-6 month"),
        expand = expansion(mult = c(0.02, 0.15))
    ) +
    #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
    scale_y_continuous(
        labels = scales::number_format(accuracy = 1L),
        #limits = c(70, 130)
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("02_05_farma.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 4.6. Servicios de salud para pacientes externos --------------------------------
# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.2. Servicios para pacientes externos 

# ---- Enlistar productos del catálogo
# v_productos <- c("Servicios para pacientes externos", "Servicios médicos", "Servicios dentales","Servicios paramédicos")
# 
# # ---- Seleccionar productos del catálogo de identificadores 
# df_productos <- d_inpc_ccif_ids %>% 
#     filter(ccif %in% v_productos)
# 
# df_productos
# 
# # ---- Obtener identificadores de productos seleccionados
# v_ids <- unique(df_productos$id_ccif_0) # Identificadores 
# 
# # ---- Importar las series de los productos 
# df_series <- data.frame()
# 
# for(i in 1:length(v_ids)){
#     print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
#     
#     df_data <- 
#         d_inpc %>% 
#         filter(id_ccif_0==v_ids[i]) %>% 
#         select(date_shortcut, fecha = date, values) %>% 
#         arrange(fecha) %>% 
#         mutate(tipo = v_productos[i], ord = i) 
#     
#     df_series <- df_series %>% bind_rows(df_data)
# }
# 
# # ---- Limpiar la info 
# 
# # Filtrar fechas 
# df_06_02_servicios_pacientes <- df_series                      %>% 
#     filter(fecha >= "2002-07-01")
# 
# # Dejar datos mensuales o quincenales
# ifelse(v_quincena == 1, 
#        # Para la serie quincenal, dejar solo datos de la primera quincena
#        df_06_02_servicios_pacientes <- df_06_02_servicios_pacientes %>% 
#            filter(!date_shortcut %% 2 == 0)             %>% 
#            glimpse, 
#        # Serie mensual 
#        df_06_02_servicios_pacientes %>% select(-date_shortcut) %>% glimpse
# )
# 
# # ---- Gráfica 
# titulo  <- "Índice de precios al consumidor de servicios\npara pacientes"
# eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"
# 
# g <- 
#     ggplot(
#         df_06_02_servicios_pacientes %>% 
#             arrange(fecha) %>% 
#             group_by(tipo) %>% 
#             mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
#             filter(fecha >= "2015-06-01"),
#         aes(
#             x = fecha,
#             y = values,
#             group = reorder(tipo, desc(ord)),
#             col = reorder(tipo, desc(ord)),
#             label = ifelse(
#                 fecha == max(fecha),
#                 paste0(str_wrap(tipo,14), "\n", round(values,1), " [", percent(tasa_anual, accuracy = 0.01), "]"), #),
#                 NA
#             )
#         ))+
#     geom_line(size = 2.5, lineend = "round", show.legend = F, 
#               aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
#                   linetype = if_else(tipo == v_productos[1], "solid", "dashed"))) + 
#     ggrepel::geom_text_repel(
#         aes(color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo)), 
#         nudge_x = 100, direction = "y", hjust = "left",
#         size = 5,
#         segment.curvature = -0.1,
#         segment.ncp = 3,
#         segment.angle = 20,
#         segment.color = NA,
#         family = "Ubuntu", fontface = "bold", show.legend = F
#     ) +
#     geom_point(aes(
#         color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
#         y = ifelse(fecha == max(fecha), values, NA)),
#         size = 4, show.legend = F) +
#     scale_color_manual(
#         "", 
#         values = c(mcv_semaforo[4], mcv_discrete_7)
#     ) +
#     scale_x_date(
#         date_labels = "%b %y",
#         breaks = seq.Date(from = floor_date(as.Date(max(df_06_02_servicios_pacientes$fecha)), "month"), 
#                           to = floor_date(as.Date("2015-09-01")+(((month(max(df_06_02_servicios_pacientes$fecha))))), "month"), 
#                           by = "-6 month"),
#         expand = expansion(mult = c(0.02, 0.15))
#     ) +
#     #scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
#     scale_y_continuous(labels = scales::number_format(accuracy = 1L),
#                        expand = expansion(c(0.3, 0.3))
#                        # limits = c(75, 125)
#                        ) +
#     theme_minimal() +
#     labs(
#         title = titulo,
#         subtitle = subtitulo, caption = nota,
#         color="", shape="", y = eje_y
#     ) +
#     theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
#           plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,5,0)),
#           plot.caption = element_text(size = 25, colour = "#777777"),
#           plot.margin= margin(0.3, 0.4, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
#           panel.grid.minor  = element_blank(),
#           panel.background = element_rect(fill = "transparent",colour = NA),
#           text = element_text(family = "Ubuntu"),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 25),
#           axis.text.x = element_text(size = 20, angle = 90, vjust = 0.5),
#           axis.text.y = element_text(size = 20),
#           legend.text = element_text(size = 30),
#           legend.position = "none")
# 
# 
# g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
# 
# ggsave(g, filename = paste_info("02_06_pacientes.png"),
#        # type = "cairo", device = "png",
#        width = 16, height = 9, dpi = 200, bg= "transparent")

# 4.7. Servicios de hospital -----------------------------------------------------

# ---- Ruta dentro del catálogo 
# CCIF > 06 Salud > 06.3. Servicios de hospital > 06.3.0 Servicios de hospital 

# ---- Enlistar productos del catálogo
v_productos <- c(
    # "Servicios de hospital (categoría)", 
    "Servicios curativos y de rehabilitación para pacientes hospitalizados",
    "Atención médica durante el parto", 
    "Hospitalización general",
    "Hospitalización parto", 
    "Operación quirúrgica")

# ---- Seleccionar productos del catálogo de identificadores 
df_productos <- d_inpc_ccif_ids %>% 
    # Cambiar el nombre de la categoría general (homónima de categoría específica)
    mutate(ccif = if_else(
        id_ccif_0 == "06_063", "Servicios de hospital (categoría)", ccif)) %>% 
    filter(ccif %in% v_productos)

# ---- Obtener identificadores de productos seleccionados
v_ids <- unique(df_productos$id_ccif_0) # Identificadores 

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    df_data <- 
        d_inpc %>% 
        filter(id_ccif_0==v_ids[i]) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = v_productos[i], ord = i) 
    
    df_series <- df_series %>% bind_rows(df_data)
}

# unique(df_series$tipo)
# unique(df_data)

# ---- Limpiar la info 

# Filtrar fechas 
df_06_03_servicios_hospital <- df_series                        %>% 
    filter(fecha >= "2002-07-01")                                %>% 
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

df_06_03_servicios_hospital <- df_06_03_servicios_hospital %>% 
    mutate(tipo = ifelse(tipo == "Servicios curativos y de rehabilitación para pacientes hospitalizados", 
                         yes = "Servicios de hospitalización", 
                         no = tipo))

# ---- Gráfica 
titulo  <- "Índice de precios al consumidor de servicios\nde hospital"
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

unique(df_06_03_servicios_hospital$tipo)

g <- 
    ggplot(
        df_06_03_servicios_hospital %>% 
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                fecha == max(fecha),
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
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_06_03_servicios_hospital$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_06_03_servicios_hospital$fecha))))), "month") , 
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("02_07_hospitales.png"),
       # type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")


# 4.8. Productos de la peda ------------------------------------------------------

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
v_ids <- unique(df_productos$id_ccif_0) # Identificadores 

# ---- Importar las series de los productos 
df_series <- data.frame()

for(i in 1:length(v_ids)){
    print(paste("Vuelta", i, "de", length(v_ids), ":", v_productos[i]))
    
    df_data <- 
        d_inpc %>% 
        filter(id_ccif_0==v_ids[i]) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = v_productos[i], ord = i) 
    
    df_series <- df_series %>% bind_rows(df_data)
}

# ---- Limpiar la info 
# Filtrar fechas 
df_fiesta <- df_series                      %>% 
    filter(fecha >= "2002-07-01")            %>% 
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
titulo  <- "Índice de precios al consumidor de productos\npara la fiesta "
eje_y   <- "Índice base 2ª quincena de julio 2018 = 100"

g <- 
    ggplot(
        df_fiesta %>% 
            arrange(fecha) %>% 
            group_by(tipo) %>% 
            mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
            filter(fecha >= "2015-06-01"),
        aes(
            x = fecha,
            y = values,
            group = reorder(tipo, desc(ord)),
            col = reorder(tipo, desc(ord)),
            label = ifelse(
                fecha == max(fecha),
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
        segment.color = NA,
        family = "Ubuntu", fontface = "bold", show.legend = F
    ) +
    geom_point(aes(
        # color = if_else(tipo == v_productos[1], mcv_semaforo[4], tipo),
        y = ifelse(fecha == max(fecha), values, NA)),
        size = 4, show.legend = F) +
    scale_color_manual(
        "", 
        values = c(mcv_semaforo[4], mcv_discrete_7)
    ) +
    scale_x_date(
        date_labels = "%b %y",
        breaks = seq.Date(from = floor_date(as.Date(max(df_fiesta$fecha)), "month"), 
                          to = floor_date(as.Date("2015-09-01")+(((month(max(df_fiesta$fecha))))), "month"), 
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

g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))

ggsave(g, filename = paste_info("02_08_peda.png"),
       #type = "cairo", device = "png",
       width = 16, height = 9, dpi = 200, bg= "transparent")

# 4.13. Productos canasta básica PROFECO ----

v_total <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Total"]

v_pacic_leche <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Leche pasteurizada y fresca"]
v_pacic_atún_sardina <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Atún y sardina en lata"]
v_pacic_res <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de res"]
v_pacic_cerdo <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Carne de cerdo"]
v_pacic_pollo <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pollo"]
v_pacic_huevo <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Huevo"]

v_pacic_frijol <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Frijol"]
v_pacic_arroz <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Arroz"]
v_pacic_pan_caja <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pan de caja"]
v_pacic_pasta_sopa <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Pasta para sopa"]
v_pacic_tortilla <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Tortilla de maíz"]
v_pacic_aceite <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Aceites y grasas vegetales comestibles"]
v_pacic_azúcar <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Azúcar"]

v_pacic_cebolla <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Cebolla"]
v_pacic_chile <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Chiles envasados"]
v_pacic_jitomate <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jitomate"]
v_pacic_limón <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Limón"]
v_pacic_manzana <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Manzana"]
v_pacic_plátano <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Plátanos"]
v_pacic_zanahoria <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Zanahoria"]
v_pacic_papa <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Papa y otros tubérculos"]

v_pacic_jabón <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Jabón de tocador"]
v_pacic_papel_higie <- d_inpc_ccif_ids$id_ccif_0[d_inpc_ccif_ids$ccif=="Papel higiénico y pañuelos desechables"]


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
    print(paste0(str_pad(i,2,"left","0"), " - ", v_pacic_list_labs[i]))
    # Sys.sleep(0.5)
    
    tempo <- d_inpc %>% 
        filter(id_ccif_0==v_pacic_list[i]) %>% 
        select(date_shortcut, fecha = date, values) %>% 
        arrange(fecha) %>% 
        mutate(tipo = v_pacic_list_labs[i])
    
    d_04_pacic <- bind_rows(d_04_pacic, tempo)
    
}

d_04_pacic %>% glimpse

unique(d_04_pacic$tipo)

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
        d_inpc %>% 
            filter(id_ccif_0=="00") %>% 
            select(date_shortcut, fecha = date, values) %>% 
            arrange(fecha) %>% 
            mutate(tipo = " General", cat = "1. Productos de origen animal") 
    ) %>% 
    bind_rows(
        d_inpc %>% 
            filter(id_ccif_0=="00") %>% 
            select(date_shortcut, fecha = date, values) %>% 
            arrange(fecha) %>% 
            mutate(tipo = " General", cat = "2. Despensa") 
    ) %>% 
    bind_rows(
        d_inpc %>% 
            filter(id_ccif_0=="00") %>% 
            select(date_shortcut, fecha = date, values) %>% 
            arrange(fecha) %>% 
            mutate(tipo = " General", cat = "3. Frutas y verduras") 
    ) %>% 
    bind_rows(
        d_inpc %>% 
            filter(id_ccif_0=="00") %>% 
            select(date_shortcut, fecha = date, values) %>% 
            arrange(fecha) %>% 
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
# 

v_pacic_loop <- unique(d_04_pacic$cat)

for(i in 1:4){
    
    d_plot <- d_04_pacic %>% 
        filter(cat == v_pacic_loop[i]) %>% 
        arrange(fecha) %>% 
        group_by(cat,tipo) %>% 
        mutate(tasa_anual = (values/lag(values, 12))-1) %>% 
        ungroup() %>% 
        filter(fecha >= "2015-06-01")
    
    titulo <- paste0("Índice de precios al consumidor de canasta PROFECO\n",
                     v_pacic_loop[i])
    g <- 
        ggplot(data = d_plot,
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
            segment.color = NA,
            family = "Ubuntu", fontface = "bold", show.legend = F
        ) +
        geom_point(aes(
            color = if_else(tipo == "Alimentos", mcv_semaforo[4], tipo),
            y = ifelse(fecha == max(fecha), values, NA)),
            size = 4, show.legend = F) +
        scale_color_manual(
            "", 
            values = c(mcv_semaforo[4], mcv_discrete_7, mcv_blacks[2])
        ) +
        scale_x_date(
            date_labels = "%b %y",
            breaks = seq.Date(from = floor_date(as.Date(max(d_04_pacic$fecha)), "month"), 
                              to = floor_date(as.Date("2015-09-01")+(((month(max(d_04_pacic$fecha))))), "month") , 
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
        paste0("02_13_0", 
               tolower(str_replace_all(str_remove_all(v_pacic_loop[i], "\\."), " ", "_")),
               "_canasta_profeco.png")), 
        # type = "cairo", device = "png", 
        width = 16, height = 9,  dpi = 200, bg= "transparent")
    
}

rm(list=ls(pattern="^v_pacic"))

# FIN. -------------------------------------------------------------------------
