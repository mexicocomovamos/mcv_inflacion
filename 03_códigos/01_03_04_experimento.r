
library(tidyverse)
library(inegiR)

xx <- d_inpc %>% 
    # filter(ccif == "Vivienda") %>% 
    filter(date %in% c("2025-09-01", "2024-09-01")) %>% 
    group_by(date) %>% 
    filter(date_shortcut == max(date_shortcut) ) %>% 
    select(ccif, values, encadenamiento, id_ccif_0, contains("ponderador")) %>% 
    filter(str_length(id_ccif_0) == 2)

xx2 <- xx %>% 
    group_by(id_ccif_0, ccif) %>% 
    summarise(tasa_anual = 100*((first(values)-last(values))/last(values)), 
              ponderador = first(ponderador_inpc_id_ccif_1)) %>% 
    mutate(incidencia = tasa_anual*(ponderador/100))

openxlsx::write.xlsx(xx2, "incidencia.xlsx")

sum(xx2$ponderador, na.rm = T)

# Total de la inflación
xx2 %>% 
    filter(ccif == "Total") %>% 
    pull(incidencia) %>% 
    sum(na.rm = T)

# SUma de las incidencias 
xx2 %>% 
    filter(ccif != "Total") %>% 
    pull(incidencia) %>% 
    sum(na.rm = T)

### 


910420 : total
910421 : Subyacente
910422 : Mercancias


ss <- 910438

inflacion <- inegi_series(
    serie    = ss,
    token    = v_token_inegi, 
    database = "BIE", 
    as_tt    = TRUE)

inflacion

# # Inflación 
# i <- inflacion %>% 
#     mutate(values_a = values/1.34848027876032) %>% 
#     filter(date == "2025-09-01" | (date == "2024-09-01" )) %>% 
#     filter(date_shortcut %% 2 == 1) %>% 
#     summarise(inf = 100 * ((first(values_a) - last(values_a))/ last(values_a)))
# 
# i*(39.207/100)
# 
# i * ((76.741538251126)/100)
# # i * (76.741538251126/100)
# # (i/1.34)*(76.74/100)
# # 3.28/4.26 = 76.99531
# # 76.74*1.3
# 
# 76.999531/76.741538251126
# 
# 

# Cálculo nueva gráfica de Incidencia grandes grupos: 
tabla_composicion <- tibble::tribble(
              ~tipo,                                            ~subtipo,                            ~categoria, ~api_quincenal, ~api_mensual, ~ponderador,
            "Total",                                             "Total",                               "Total",        910438L,      910406L,         100,
       "Subyacente",                                             "Total",                               "Total",        910439L,      910407L,     76.7415,
       "Subyacente",                                        "Mercancías",                               "Total",        910440L,      910408L,     37.5338,
       "Subyacente",                                         "Servicios",                               "Total",        910441L,      910409L,     39.2077,
    "No subyacente",                                             "Total",                               "Total",        910442L,      910410L,     23.2585,
    "No subyacente",                                     "Agropecuarios",                               "Total",        910443L,      910411L,     10.6577,
    "No subyacente", "Energéticos y tarifas autorizadas por el gobierno",                               "Total",        910444L,      910412L,     12.6008
    )



if(v_quincena==1){
    codigos_api = tabla_composicion$api_quincenal
} else {
    codigos_api = tabla_composicion$api_mensual
}


inflaciones <- lapply(1:nrow(tabla_composicion), function(k){
    
    if(v_quincena==1){
        
        inegi_series(
            serie    = tabla_composicion$api_quincenal[k],
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = tabla_composicion$tipo[k], 
                   subtipo = tabla_composicion$subtipo[k], 
                   categoria = tabla_composicion$categoria[k], 
                   ponderador = tabla_composicion$ponderador[k]) %>% 
            mutate(incidencia = values*(ponderador/100))
        
    } else {
        
        inegi_series(
            serie    = tabla_composicion$api_mensual[k],
            token    = v_token_inegi, 
            database = "BIE", 
            as_tt    = TRUE) %>% 
            mutate(tipo = tabla_composicion$tipo[k], 
                   subtipo = tabla_composicion$subtipo[k], 
                   categoria = tabla_composicion$categoria[k], 
                   ponderador = tabla_composicion$ponderador[k]) %>% 
            mutate(incidencia = values*(ponderador/100))
        
    }
    
})

inflacion_total <- inflaciones %>% 
    do.call(rbind, .) %>% 
    filter(date_shortcut %% 2 == 1) %>% 
    filter(date == max(date)) %>% 
    filter(tipo == "Total") %>% 
    pull(incidencia)

tt2 <- 
    inflaciones %>% 
    do.call(rbind, .) %>%
    filter(date_shortcut %% 2 == 1) %>% 
    select(fecha = date, inpc_tipo = tipo, 
           suby_no_suby_tipo = subtipo, 
           ponderador, incidencia_anual = incidencia) %>% 
    filter(suby_no_suby_tipo != "Total") %>% 
    arrange(fecha) %>% 
    ungroup() %>% 
    group_by(fecha) %>% 
    mutate(ranking = rank(-incidencia_anual) %>% 
               str_pad(width = 2, side = "left", pad = "0")) %>% 
    arrange(fecha, ranking) %>% 
    ungroup()

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
        tt2 %>% 
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
            x = last(tt$fecha),
            label = paste0(
                "Inflación:\n",round(inflacion_total, 2),"%")
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
    scale_y_continuous(labels = scales::comma_format(suffix = ".0")) + 
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

g
# NO PUBLICAR HASTA AGOSTO DEL 2025, O CUANDO INEGI LO INDIQUE
# ADEMÁS, REVISAR ANTES EL CÁLCULO DE LOS FACTORES DE ENCADENAMIENTO DE GRANDES AGRUPACIONES
g <- ggimage::ggbackground(g, paste_info("00_plantillas/01_inegi.pdf"))
ggsave(g, filename = paste_info("01_04_incidencia_anual_componente.png"),
       width = 16, height = 9,
       dpi = 200, bg= "transparent")


Mercancías aportó 1.573
17.2148/37.5338
20.3190/37.5338
37.5338

(1.576*(17.2148/37.5338)) + (1.576*(20.3190/37.5338))
1.576*(17.2148/37.5338)
1.576*(20.3190/37.5338)



