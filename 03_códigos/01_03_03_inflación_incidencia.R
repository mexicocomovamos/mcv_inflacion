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

d_inpc_complete <- readxl::read_excel(paste_inp("01_03_inpc_complete.xlsx")) %>% 
    glimpse
# Seleccionar quincena 
v_quincena <- 2
# 0. Procesamiento en loop ----
d_inpc <- data.frame()
# Histórico: 
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
    # falta en 385
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

if(v_quincena==1){
    
    d_inpc_total <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        filter(date > "2015-12-17") %>% 
        mutate(date = ifelse(date_shortcut %% 2 == 0, ymd(date)+days(15),date),
               date = as.Date.numeric(date)) %>% 
        select(fecha = date, inpc = values) %>% 
        arrange(fecha) %>% 
        glimpse
    
} else{
    
    d_inpc_total <- d_inpc %>% 
        filter(id_ccif_0=="00") %>% 
        filter(date > "2015-12-17") %>% 
        select(fecha = date, inpc = values) %>% 
        arrange(fecha) %>% 
        glimpse
    
}

# 1. Incidencia por productos ----
if(v_quincena==1){
    
    d_inpc_prods <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_4) %>% 
        filter(date > "2015-12-17") %>% 
        mutate(date = ifelse(date_shortcut %% 2 == 0, ymd(date)+days(15), date),
               date = as.Date.numeric(date)) %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        glimpse
    
} else{
    
    d_inpc_prods <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_4) %>% 
        filter(date > "2015-12-17") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_4, values) %>% 
        arrange(fecha) %>% 
        glimpse
}

if(v_quincena == 1){
    
    d_incidencia_prods <- d_inpc_prods %>% 
        left_join(
            d_inpc_total
        ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        mutate(
            ccif = case_when(
                id_ccif_0 == "11_111_1111_272" ~ "Loncherías, fondas, torterías y taquerías",
                id_ccif_0 == "04_045_0452_144" ~ "Gas LP",
                id_ccif_0 == "08_083_0830_235" ~ "Paquetes de internet, telefonía y televisión de paga",
                id_ccif_0 == "04_045_0452_145" ~ "Gas natural",
                T ~ ccif
            ),
            var_quincenal = (values - lag(values))/lag(values),
            incidencia_quincenal = ((values - lag(values))/lag(inpc))*ponderador,
            var_anual = (values - lag(values, 24))/lag(values, 24),
            incidencia_anual = ((values - lag(values, 24))/lag(inpc, 24))*ponderador
        ) %>% 
        ungroup() %>% 
        glimpse
    
    d_incidencia_prods_last <- d_incidencia_prods %>% 
        filter(fecha == last(fecha)) %>% 
        arrange(-incidencia_quincenal) %>% 
        select(fecha,id_ccif_0,  ccif, var_quincenal, incidencia_quincenal) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    d_incidencia_anual_prods_last <- d_incidencia_prods %>% 
        filter(fecha == last(fecha)) %>% 
        arrange(-incidencia_anual) %>% 
        select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    
} else{
    
    d_incidencia_prods <- d_inpc_prods %>% 
        left_join(
            d_inpc_total
        ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        mutate(
            ccif = case_when(
                id_ccif_0 == "11_111_1111_272" ~ "Loncherías, fondas, torterías y taquerías",
                id_ccif_0 == "04_045_0452_144" ~ "Gas LP",
                id_ccif_0 == "08_083_0830_235" ~ "Paquetes de internet, telefonía y televisión de paga",
                id_ccif_0 == "04_045_0452_145" ~ "Gas natural",
                T ~ ccif
            ),
            var_mensual = (values - lag(values))/lag(values),
            incidencia_mensual = ((values - lag(values))/lag(inpc))*ponderador,
            var_anual = (values - lag(values, 12))/lag(values, 12),
            incidencia_anual = ((values - lag(values, 12))/lag(inpc, 12))*ponderador
        ) %>% 
        ungroup() %>% 
        glimpse
    
    d_incidencia_prods_last <- d_incidencia_prods %>% 
        filter(fecha == last(fecha)) %>% 
        arrange(-incidencia_mensual) %>% 
        select(fecha,id_ccif_0,  ccif, var_mensual, incidencia_mensual) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
    d_incidencia_anual_prods_last <- d_incidencia_prods %>% 
        filter(fecha == last(fecha)) %>% 
        arrange(-incidencia_anual) %>% 
        select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
        mutate(n = row_number()) %>% 
        glimpse
    
}

d_incidencia_prods_last_20 <- d_incidencia_prods_last %>% 
    filter(n <= 10 | n >= 290)

d_incidencia_anual_prods_last_20 <- d_incidencia_anual_prods_last %>% 
    filter(n <= 10 | n >= 290)

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
        geom_text(hjust = "inward", family = "Ubuntu", size = 4, fontface = "bold") +
        scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
        scale_x_continuous(
            labels = scales::number_format(accuracy = 0.1), 
            limits = c((max(abs(d_incidencia_prods_last_20$incidencia_quincenal)))*-1, 
                       max(abs(d_incidencia_prods_last_20$incidencia_quincenal)))
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
        geom_text(hjust = "inward", family = "Ubuntu", size = 4, fontface = "bold") +
        scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
        scale_x_continuous(
            labels = scales::number_format(accuracy = 0.1), 
            limits = c((max(abs(d_incidencia_prods_last_20$incidencia_mensual)))*-1, 
                       max(abs(d_incidencia_prods_last_20$incidencia_mensual)))
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
    ggsave(g, filename = paste_info("01_03_03_01_01_incidencia_quincenal.png"), 
           width = 10, height = 15, 
           dpi = 200, bg= "transparent"),
    ggsave(g, filename = paste_info("01_03_03_01_01_incidencia_mensual.png"), 
           width = 10, height = 15, 
           dpi = 200, bg= "transparent")
)

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
            y = reorder(str_wrap_long(stringr = ccif, width = 20), incidencia_anual),
            x = incidencia_anual,
            fill = ifelse(n <= 10, "1", "2"),
            label = paste0(
                round(incidencia_anual, 3), "\n[", round(var_anual*100, 2), "%]"
            )
        )
    ) +
    geom_col() +
    geom_text(hjust = "inward", family = "Ubuntu", size = 4, fontface = "bold") +
    scale_fill_manual("", values = c(mcv_semaforo[1], mcv_semaforo[4])) +
    scale_x_continuous(
        labels = scales::number_format(accuracy = 0.1), 
        limits = c((max(abs(d_incidencia_anual_prods_last_20$incidencia_anual)))*-1, 
                   max(abs(d_incidencia_anual_prods_last_20$incidencia_anual)))
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
ggsave(g, filename = paste_info("01_03_03_01_02_incidencia_anual.png"), 
       width = 10, height = 15, 
       dpi = 200, bg= "transparent")

# 2. Incidencia anual por divisiones del CCIF ----

if(v_quincena==1){
    d_inpc_cats <- d_inpc %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        drop_na(ponderador_inpc_id_ccif_1) %>% 
        filter(date > "2015-12-17") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_1, values) %>% 
        arrange(fecha) %>% 
        glimpse
} else{
    d_inpc_cats <- d_inpc %>% 
        drop_na(ponderador_inpc_id_ccif_1) %>% 
        filter(date > "2015-12-17") %>% 
        select(fecha = date, ccif, id_ccif_0, ponderador = ponderador_inpc_id_ccif_1, values) %>% 
        arrange(fecha) %>% 
        glimpse
}


d_incidencia_cats <- d_inpc_cats %>% 
    left_join(
        d_inpc_total
    ) %>% 
    group_by(ccif, id_ccif_0) %>% 
    mutate(
        ccif = case_when(
            id_ccif_0 == "03" ~ "Ropa y calzado",
            id_ccif_0 == "04" ~ "Vivienda, electricidad, gas y otros combustibles",
            id_ccif_0 == "05" ~ "Mobiliario y mantenimiento del hogar",
            T ~ ccif
        ),
        var_anual = (values - lag(values,12))/lag(values,12),
        incidencia_anual = ((values - lag(values,12))/lag(inpc,12))*ponderador
    ) %>% 
    ungroup() %>% 
    glimpse

d_incidencia_cats_last <- d_incidencia_cats %>% 
    filter(fecha == last(fecha)) %>% 
    arrange(-incidencia_anual) %>% 
    select(fecha,id_ccif_0,  ccif, var_anual, incidencia_anual) %>% 
    mutate(n = row_number()) %>% 
    glimpse

tt <- d_incidencia_cats                                    %>% 
    arrange(fecha, desc(incidencia_anual))                        %>% 
    group_by(fecha)                                              %>% 
    mutate(ranking = 1:12,
           ranking = str_pad(ranking, 2, "l", "0"))                                       %>% 
    ungroup() %>% 
    drop_na(incidencia_anual) %>% 
    glimpse

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
        mutate(
            etiqueta = ifelse(fecha == last(fecha), 
                              paste0(str_wrap_long(paste0(ranking, " - ", ccif),30), "\n", round(incidencia_anual, 3)), NA)
        ) %>% 
        group_by(ccif, id_ccif_0) %>% 
        fill(etiqueta, .direction = "up") %>% 
        ungroup(), 
    aes(
        y = incidencia_anual, 
        x = fecha,
        stratum = ranking, 
        alluvium = id_ccif_0, 
        fill = etiqueta
    )
)  +
    
    geom_text(
        aes(
            y = (d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric)+0.5,
            x = last(tt$fecha),
            label = paste0(
                "Inflación: ",
                round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric),2),
                "%"
            )
        ),
        col = mcv_semaforo[4],
        family = "Ubuntu",
        size = 5, fontface = "bold"
    ) +
    scale_x_date(
        expand = expansion(mult = c(0.01, 0.25)),
        minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
        breaks = seq.Date(from = min(tt$fecha)+31, 
                          to = max(tt$fecha), 
                          by = "2 month"),
        date_labels = "%b-%y"
    ) +
    geom_flow(show.legend = T) +
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
ggsave(g + theme(plot.title = element_blank(),
                 plot.subtitle = element_blank(),
                 plot.margin= margin(0.4, 0.4, 0.4, 0.4, "cm")), filename = paste_info("01_03_03_02_incidencia_anual_sin_fondo.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_03_03_02_incidencia_anual.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")

# 3. Incidencia anual por concepto y componente ----
d_inpc_ponds_comp <- readxl::read_excel(paste_inp("01_03_inpc_concepto_ponds.xlsx")) %>% 
    glimpse

d_inpc_ponds_comp_prod <- readxl::read_excel(paste_inp("01_03_inpc_concepto_ccif_ponds.xlsx")) %>% 
    glimpse

if(v_quincena==1){
    
    d_subyacente <- d_inpc %>% 
        filter(!date_shortcut %% 2 == 0) %>% 
        filter(date > "2015-12-17") %>% 
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
        filter(date > "2015-12-17") %>% 
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
           ranking = str_pad(ranking, 2, "l", "0"))                                       %>% 
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
    
    geom_text(
        aes(
            y = (d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric)+0.5,
            x = last(tt$fecha),
            label = paste0(
                "Inflación: ",
                round((d_incidencia_cats_last %>% summarise(inflacion = sum(incidencia_anual)) %>% as.numeric),2),
                "%"
            )
        ),
        col = mcv_semaforo[4],
        family = "Ubuntu",
        size = 5, fontface = "bold"
    ) +
    scale_x_date(
        expand = expansion(mult = c(0.01, 0.25)),
        minor_breaks = seq.Date(min(tt$fecha), max(tt$fecha), "1 month"),
        breaks = seq.Date(from = min(tt$fecha)+31, 
                          to = max(tt$fecha), 
                          by = "2 month"),
        date_labels = "%b-%y"
    ) +
    geom_flow(show.legend = T) +
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
ggsave(g, filename = paste_info("01_03_03_03_01_incidencia_anual_componente.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")

## 3.2. Por concepto ----

tt <- d_incidencia_suby_no_suby_tipo                                    %>% 
    arrange(fecha, desc(incidencia_anual))                        %>% 
    group_by(fecha, inpc_tipo)                                              %>% 
    mutate(ranking = row_number(),
           ranking = str_pad(ranking, 2, "l", "0"))                                       %>% 
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
        breaks = seq.Date(from = min(tt$fecha)+31, 
                          to = max(tt$fecha), 
                          by = "2 month"),
        date_labels = "%b-%y"
        
    ) +
    scale_y_continuous("", limits = c(-2,6.1), breaks = seq(-2,6,1), 
                       labels = scales::number_format(accuracy = 1L)) +
    geom_flow(show.legend = T) +
    scale_fill_manual("", values = mcv_discrete_12[1:5]) +
    theme_minimal()  +
    labs(
        title = titulo,
        y = eje_y, x = ""
    ) +
    theme(
        plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.margin= margin(0.4, 0.4, 1, 0.4, "cm"), # margin(top,right, bottom,left)
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15),
        text = element_text(family = "Ubuntu"),
        legend.text = element_text(size = 15),
        legend.position = c(1, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(-2, 3, 6, 8)
    )

g2 <- 
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
            filter(!inpc_tipo=="Subyacente"), 
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
        breaks = seq.Date(from = min(tt$fecha)+31, 
                          to = max(tt$fecha), 
                          by = "2 month"),
        date_labels = "%b-%y"
        
    ) +
    scale_y_continuous("", limits = c(-2,6), breaks = seq(-2,6,1), 
                       labels = scales::number_format(accuracy = 1L)) +
    geom_flow(show.legend = T) +
    scale_fill_manual("", values = mcv_discrete_12[6:9]) +
    theme_minimal() +
    labs(caption = nota) +
    theme(
        plot.caption = element_text(size = 12),
        plot.margin= margin(0.4, 0.4, 1.5, 0.4, "cm"), # margin(top,right, bottom,left)
        strip.text.x = element_text(size = 25),
        panel.grid.minor  = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
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
ggsave(g, filename = paste_info("01_03_03_03_02_incidencia_anual_concepto.png"), 
       width = 16, height = 9, 
       dpi = 200, bg= "transparent")

saveRDS(
    d_inpc,
    paste_out(
        "01_03_inpc_complete_prods_ccif.RDS"
    )
)
