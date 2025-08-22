
# Código para replicar las líneas de pobreza

# Codigo para replicar las líneas de pobreza ----
# Documentos de referencia: 
# https://www.coneval.org.mx/InformesPublicaciones/InformesPublicaciones/Documents/Lineas_pobreza.pdf
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Nota_tecnica_ajuste_LPI_INPC_2024.pdf
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Contenido_y_valor_monetario_de_las_Lineas_de_Pobreza_por_Ingresos.zip
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Lineas_de_Pobreza_por_Ingresos.zip

# Opciones ----
options(pillar.sigfig = 6)
Sys.setlocale(locale = "es_ES")

# Librerias ---
library(tidyverse)
library(readxl)
library(inegiR)

# 1. Leemos los datos ----
# Estos datos provienen del análisis de la ENIGH 2016, y sirven para establecer qué son las Canastas Básicas 
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Calculo_CA_CNA_ENIGH_16_R.zip

# Claves de los bienes: 
eq_enigh <- readxl::read_xlsx("equivalencia_enigh_inpc.xlsx") %>% rename(clave = cve_enigh_2016) %>% 
  filter(cve_inpc_24 != "-")

# Canasta alimentaria 
c_alim <- readxl::read_xlsx("CA_JUVE.xlsx") %>% 
  filter(!(is.na(consumo_u) & is.na(consumo_r)))

# Canasta no alimentaria
c_n_alim <- read_csv("CANASTA NO ALIMENTARIA.csv") %>% 
  filter(clave != "CA")

# 2. Construimos los datos del INPC ----
# Nos traemos los datos del INPC de los genéricos que se definieron con la ENIGH 2016. 
# No son todos los genéricos del INPC, ni son las mismas claves que usamos para obtener el valor de la inflación. 

# 2.1. Tomamos los datos del INPC de CONEVAL que ya pescó por nosotros
# Esto proviene de un documento de CONEVAL, pero procesado por mí. 
# Y esto desatora lo que me generaba conflicto. 
# Agradecimientos a Marco del CONASAMI por desatorar este problema
inpc_coneval <- readxl::read_xlsx("inpc_coneval.xlsx") %>% 
  select(-name) %>% 
  rename(clave_inpc = Serie, 
         values = value, 
         date = fecha)

# 2.2 Ahora traemos los datos nuevos que no alcanzó a pescar el CONEVAL
v_token_inegi <- "682ad7f9-19fe-47f0-abec-e4c2ab2f2948"

tiempo_espera <- 0.4 # Tratamos bien al API para que INEGI no nos vete el acceso (otra vez).

inpc <- lapply(1:nrow(eq_enigh)
               , function(i){
  repeat{
    tryCatch({
      
      tiempo_espera_og <- tiempo_espera
      inpc_tempo <- inegi_series(
        serie    = eq_enigh$cve_inpc_24[i],
        token    = v_token_inegi, 
        database = "BIE", 
        as_tt    = TRUE) %>% 
        mutate(clave = eq_enigh$clave[i], 
               producto = eq_enigh$producto[i], 
               clave_inpc = eq_enigh$cve_inpc_24[i], 
               clave_inpc_previa = eq_enigh$cve_inpc_18[i]) 
      print(str_c("Listo: ", eq_enigh$clave[i], " ", eq_enigh$producto[i], " (", i, "/", nrow(eq_enigh),  ")"))
      Sys.sleep(tiempo_espera_og)
      return(inpc_tempo)
    }, error = function(e){
      message(str_c("Error en ", paste0(i, " - ", eq_enigh$producto[i], ". ", "Reintentando!")))
      tiempo_espera_og <- tiempo_espera_og*2
      Sys.sleep(tiempo_espera_og)
    })
  }
    
})

# Armamos la tabla
inpc2 <- inpc %>% do.call(rbind, .)

# Guardamos un respaldo
saveRDS(inpc2 , "inpc_especifico_calculo_lpei.rds")

# En caso de que ya hayamos corrido el código de arriba y no haya nuevos datos, descilenciar esta línea en vez de correr el bucle anterior. 
# inpc2 <- readRDS("inpc_especifico_calculo_lpei.rds")

# 2.3 Juntamos todo ----
inpc_coneval2 <- inpc_coneval %>% 
  mutate(clave_inpc = as.character(clave_inpc)) %>% 
  left_join(inpc2 %>% 
              select(clave, clave, producto, clave_inpc) %>% 
              unique(), 
            relationship = "many-to-many") %>% 
  filter(!is.na(clave))

# Generamos el INPC definitivo
inpc2  <- inpc2 %>% 
  filter(date > max(inpc_coneval2$date)) %>% 
  select(-date_shortcut, -notes, -clave_inpc_previa) %>% 
  rbind(inpc_coneval2) %>% 
  arrange(clave, date)

# LÍNEA DE POBREZA EXTREMA POR INGRESOS ----

# fecha_sel <- "2025-03-01" # Parámetro de prueba de la función

gen_lineas_pobreza <- function(fecha_sel){

        # LPEI_rural ----
        c_alim_rur <- c_alim %>% 
          filter(!is.na(consumo_r)) %>% 
          arrange(clave) 
        
        inpc_ago_16 <-   inpc2 %>% 
          mutate(is.ago_2016 = ifelse(date == "2016-08-01", yes = T, no = F)) %>% 
          filter(is.ago_2016) %>% 
          select(clave, clave_inpc, inpc_ago_2016 = values)
          
        deflactores <- inpc2 %>% 
          left_join(inpc_ago_16, relationship = "many-to-many") %>% 
          ungroup() %>% 
          group_by(date, clave) %>% 
          summarise(values = mean(values), 
                    inpc_ago_2016 = mean(inpc_ago_2016)) %>% 
          mutate(def = values/inpc_ago_2016) 
        
        lpei_r <- c_alim_rur %>% 
          select(-contains("_18")) %>% 
          left_join(deflactores, by = c("clave")) %>% 
          filter(date == fecha_sel) %>% 
          select(nombres, consumo_r, clave, precio_mg_r, def) %>% 
          group_by(nombres, clave) %>% 
          summarise(consumo_r = mean(consumo_r, na.rm = T), 
                    precio_mg_r = mean(precio_mg_r, na.rm = T), 
                    def = mean(def)) %>% 
          mutate(precio = precio_mg_r*def) %>% 
          arrange(clave) 
        
        ## Casos especiales ----
        
        ### Chiles rural ----
        chiles_rural <- lpei_r %>% 
            filter(clave %in% c("A115", "A116", "A117", "A118")) %>% 
            mutate(nombres =  "Chile", clave = "AChile") %>% 
            group_by(clave, nombres) %>%
            summarise(consumo_r = sum(consumo_r),
                      precio = mean(precio)) %>%
            mutate(valor_monetario_diario = (consumo_r/1000)*precio) %>%
            mutate(valor_monetario_mensual = valor_monetario_diario*30)
        
        ### Comida afuera ----
        comida_fuera_rural <- lpei_r %>% 
          filter(clave %in% c("A243", "A244", "A245")) %>% 
          mutate(valor_monetario_diario = (consumo_r/1000)*precio) %>%
          ungroup() %>% 
          summarise(nombres = "Alimentos y bebidas consumidos fuera del hogar", 
                    clave = "A243-A245", 
                    consumo_r = sum(consumo_r),
                    precio_mg_r = sum(precio_mg_r), 
                    def = mean(def),
                    valor_monetario_diario = sum(valor_monetario_diario), 
                    valor_monetario_mensual = valor_monetario_diario*30) %>% 
          select(-precio_mg_r, -def)
        
        ### Todo lo demás ----
        todos_demas_rural <- lpei_r %>% 
          filter(!(clave %in% c("A243", "A244", "A245", "A115", "A116", "A117", "A118"))) %>% 
          group_by(clave, nombres) %>%
          summarise(consumo_r = sum(consumo_r),
                    precio = mean(precio)) %>%
          mutate(valor_monetario_diario = (consumo_r/1000)*precio) %>%
          mutate(valor_monetario_mensual = valor_monetario_diario*30)
        
        ### Juntamos ----
        lpei_r2 <- rbind(todos_demas_rural, comida_fuera_rural, chiles_rural)
        lpei_r2_numero = sum(lpei_r2$valor_monetario_mensual) %>% round(2)
        linea_pobreza_extrema_rural = lpei_r2_numero
        
        
        # LPEI URBANO ----
        c_alim_urb <- c_alim %>% 
          filter(!is.na(consumo_u)) %>% 
          arrange(clave) 
        
        inpc_ago_16 <-   inpc2 %>% 
          left_join(eq_enigh %>% select(clave, cve_inpc_24), by = c("clave" = "clave", 
                                                                    "clave_inpc" = "cve_inpc_24")) %>%
          mutate(is.ago_2016 = ifelse(date == "2016-08-01", yes = T, no = F)) %>% 
          filter(is.ago_2016) %>% 
          select(clave, clave_inpc, inpc_ago_2016 = values)
        
        lpei_u <- c_alim_urb %>% 
          select(-contains("_18")) %>% 
          left_join(deflactores, by = c("clave")) %>% 
          filter(date == fecha_sel) %>% 
          select(nombres, consumo_u, clave, precio_mg_u, def) %>% 
          group_by(nombres, clave) %>% 
          summarise(consumo_u = mean(consumo_u, na.rm = T), 
                    precio_mg_u = mean(precio_mg_u, na.rm = T), 
                    def = mean(def)) %>% 
          mutate(precio = precio_mg_u*def) %>% 
          arrange(clave) 
        
        ## Casos especiales ----
        
        ### Chiles ----
        chiles_urb <- lpei_u %>% 
          filter(clave %in% c("A115", "A116", "A117", "A118")) %>% 
          mutate(nombres =  "Chile", clave = "AChile") %>% 
          group_by(clave, nombres) %>%
          summarise(consumo_u = sum(consumo_u),
                    precio = mean(precio)) %>%
          mutate(valor_monetario_diario = (consumo_u/1000)*precio) %>%
          mutate(valor_monetario_mensual = valor_monetario_diario*30)
        
        ### Comida fuera ----
        comida_fuera_urb <- lpei_u %>% 
          filter(clave %in% c("A243", "A244", "A245")) %>% 
          mutate(valor_monetario_diario = (consumo_u/1000)*precio) %>%
          ungroup() %>% 
          summarise(nombres = "Alimentos y bebidas consumidos fuera del hogar", 
                    clave = "A243-A245", 
                    consumo_u = sum(consumo_u),
                    precio_mg_u = sum(precio_mg_u), 
                    def = mean(def),
                    valor_monetario_diario = sum(valor_monetario_diario), 
                    valor_monetario_mensual = valor_monetario_diario*30) %>% 
          select(-precio_mg_u, -def)
        
        ### Todo lo demás ----
        todos_demas_urb <- lpei_u %>% 
          filter(!(clave %in% c("A243", "A244", "A245", "A115", "A116", "A117", "A118"))) %>% 
          group_by(clave, nombres) %>%
          summarise(consumo_u = sum(consumo_u),
                    precio = mean(precio)) %>%
          mutate(valor_monetario_diario = (consumo_u/1000)*precio) %>%
          mutate(valor_monetario_mensual = valor_monetario_diario*30)
        
        lpei_u2 <- rbind(todos_demas_urb, comida_fuera_urb, chiles_urb)
        lpei_u2_numero = sum(round(lpei_u2$valor_monetario_mensual, 2))
        linea_pobreza_extrema_urbana = lpei_u2_numero
        
        
        # LÍNEA DE POBREZA POR INGRESOS ----
        # Canasta no alimentaria --- 
        
        # Urbana ----
        deflactores <- inpc2 %>% 
          left_join(inpc_ago_16, relationship = "many-to-many") %>% 
          ungroup() %>% 
          group_by(date, clave) %>% 
          summarise(values = mean(values), 
                    inpc_ago_2016 = mean(inpc_ago_2016)) %>% 
          mutate(def = values/inpc_ago_2016) 
        
        td <- c_n_alim %>% 
          left_join(deflactores) %>% 
          filter(date == fecha_sel) 
        
        td2_urbana <- td %>% 
          mutate(costo = gib_16_urbano*def) %>%
          group_by(grupo2) %>%
          summarise(costo = sum(costo, na.rm = T)) %>% 
          mutate(grupo2 = factor(grupo2, levels = c(
            "Transporte público",
            "Limpieza y cuidados de la casa",                      
            "Cuidados personales",                                
            "Educación, cultura y recreación",                    
            "Comunicaciones y servicios para vehículos",
            "Vivienda y servicios de conservación",
            "Prendas de vestir, calzado y accesorios",
            "Cristalería, blancos y utensilios domésticos",
            "Cuidados de la salud",
            "Enseres domésticos y mantenimiento de la vivienda",
            "Artículos de esparcimiento",
            "Transporte",
            "Otros gastos"))) %>% 
          arrange(grupo2)
        
        linea_pobreza_urbana = sum(td2_urbana$costo) %>% round(2) # Debe de dar $2,210.32
        
        
        td2_rural <- td %>% 
          mutate(costo = gib_16_rural*def) %>%
          group_by(grupo2) %>%
          summarise(costo = sum(costo, na.rm = T)) %>% 
          mutate(grupo2 = factor(grupo2, levels = c(
            "Transporte público",
            "Limpieza y cuidados de la casa",                      
            "Cuidados personales",                                
            "Educación, cultura y recreación",                    
            "Comunicaciones y servicios para vehículos",
            "Vivienda y servicios de conservación",               
            "Prendas de vestir, calzado y accesorios",
            "Cristalería, blancos y utensilios domésticos",       
            "Cuidados de la salud",                               
            "Enseres domésticos y mantenimiento de la vivienda",  
            "Artículos de esparcimiento",
            "Transporte",                                         
            "Otros gastos"))) %>% 
          arrange(grupo2)
        
        linea_pobreza_rural = sum(td2_rural$costo) %>% round(2) # Debe de dar $2,210.32
        
        # Armamos la tabla
        Anio = fecha_sel %>% year()
        Mes = fecha_sel %>% month() %>% str_pad(width = 2, side = "left", pad = "0")
        Mes_texto = fecha_sel %>% month(label = T) %>% str_to_sentence()
        linea_pobreza_extrema_urbana
        linea_pobreza_extrema_rural
        linea_pobreza_urbana2 = linea_pobreza_extrema_urbana + linea_pobreza_urbana
        linea_pobreza_rural2 = linea_pobreza_rural + linea_pobreza_extrema_rural
        
        tabla_resultado <- tibble(`Año` = Anio, 
               Mes = Mes, 
               `LPEI rural` = linea_pobreza_extrema_rural, 
               `LPEI urbano` = linea_pobreza_extrema_urbana, 
               `LPI rural` = linea_pobreza_rural2, 
               `LPI urbano` = linea_pobreza_urbana2)
        
        return(tabla_resultado)

}

# Obtener el primer día del mes actual
primer_dia_actual <- as.Date( format(Sys.Date(), "%Y-%m-01") )

# Definir la fecha de inicio en enero de 2016
fecha_inicio <- as.Date("2016-01-01")

# Generar la secuencia mensual descendente
fechas_primeros <- seq(from = primer_dia_actual,
                       to   = fecha_inicio,
                       by   = "-1 month")

# Mostrar el vector resultante
fechas_primeros

# f = fechas_primeros[1]
datos_lineas_pobreza <- lapply(fechas_primeros, function(f){
  tryCatch({
    lineas <- gen_lineas_pobreza(fecha_sel = f)
    print(f)
    if(lineas$`LPEI rural` != 0){
      return(lineas)  
    }
  }, error = function(e){
    message(str_c("Error en la fecha ", f))
  })
})

datos_lineas_final <- datos_lineas_pobreza %>% 
  do.call(rbind, .) %>% 
  arrange(Año, as.numeric(Mes))

# # # # # # # # 

# Hacemos las tablas de las líneas de pobreza ----
fecha_sel <- str_c(year(today()), month(today())-1, "01", sep = "-") %>% 
  as.Date()

archivos_canastas <- tibble(archivos = list.files("conformacion_canastas", recursive = T), 
                            archivos_completo = list.files("conformacion_canastas", recursive = T, full.names = T),
                            fecha = str_extract(archivos, pattern = "\\d+\\-\\d+\\-\\d+"), 
                            tipo_canasta = str_remove(archivos, pattern = "\\_\\d+\\-\\d+\\-\\d+\\.rds")) %>% 
  separate(tipo_canasta, into = c("tipo", "area"), sep = "\\/") %>% 
  mutate(fecha = as.Date(fecha, format = "%Y-%m-%d")) %>% 
  filter(fecha == fecha_sel)

ra <- readRDS(archivos_canastas$archivos_completo[1]) %>% 
  select(clave, nombres, `Valor monetario mensual (Rural)` = valor_monetario_mensual)
ua <- readRDS(archivos_canastas$archivos_completo[2]) %>% 
  select(clave, nombres, `Valor monetario mensual (Urbano)` = valor_monetario_mensual)

canasta_alimentaria <- left_join(ra, ua)

cna_r <- readRDS(archivos_canastas$archivos_completo[3]) %>% 
  rename(`Costo mensual (Rural)` = costo)
cna_u <- readRDS(archivos_canastas$archivos_completo[4])  %>% 
  rename(`Costo mensual (Urbano)` = costo)

cna <- left_join(cna_r, cna_u) %>% 
  rename(`Grupo de bienes` = grupo2)


if(is.null(datos_lineas_pobreza[[1]])){
  datos_ya_arriba <- datos_lineas_final
} else {
  datos_ya_arriba <- datos_lineas_final %>% 
    rbind(datos_lineas_pobreza %>% 
            do.call(rbind, .) %>% 
            arrange(Año, as.numeric(Mes)) )
}

# Guardamos las tablas
library(grid)
library(gridExtra)
library(pdftools)
library(png)
library(dplyr)

nb <- function(x, n){
  round(x = x, digits = n) %>% 
    format(nsmall = n) %>% 
    prettyNum(big.mark = ",")
}

# 1) Preparar el fondo: convertir la primera página del PDF a PNG
tpl_pdf <- "../../03_infobites/00_plantillas/01_00_00_plantilla_blanco.pdf"

tpl_png <- pdftools::pdf_convert(
  pdf = tpl_pdf,
  format = "png", 
  pages  = 1,
  dpi    = 300
)[1]

# Leer el PNG como matriz raster
bg_rast <- readPNG(tpl_png)
bg_grob <- rasterGrob(bg_rast,
                      width  = unit(1, "npc"),
                      height = unit(1, "npc"))

# 2) Crear nueva tibble con fila de totales
cna_tot <- cna %>%
  summarise(
    `Costo mensual (Rural)` = sum(`Costo mensual (Rural)`),
    `Costo mensual (Urbano)` = sum(`Costo mensual (Urbano)`)
  ) %>%
  mutate(`Grupo de bienes` = "Canasta básica no alimentaria") %>%
  select(`Grupo de bienes`, everything())

cna2 <- bind_rows(cna, cna_tot)

# 3) Formatear montos con símbolo de pesos y 2 decimales
cna_fmt <- cna2 %>%
  mutate(
    `Costo mensual (Rural)` = str_c("$", nb(`Costo mensual (Rural)`, 2)),
    # sprintf("$%.2f", `Costo mensual (Rural)`),
    `Costo mensual (Urbano)` = str_c("$", nb(`Costo mensual (Urbano)`, 2))
    # sprintf("$%.2f", `Costo mensual (Urbano)`)
  )

# 4) Construir la tabla con estilos
tbl_theme <- ttheme_default(
  core = list(
    fg_params = list(
      col        = "black",
      fontfamily = "Ubuntu",
      fontface   = "plain",
      fontsize   = 12
    )
  ),
  colhead = list(
    fg_params = list(
      col        = "white",
      fontfamily = "Ubuntu",
      fontface   = "bold",
      fontsize   = 14
    ),
    bg_params = list(fill = "#6950d8"),
    padding   = unit(c(12, 12), "pt")  # filas de encabezado más altas
  ),
  base_size = 12
)

tbl <- tableGrob(cna_fmt, rows = NULL, theme = tbl_theme)

# Pintar líneas en morado
for(i in seq_len(nrow(tbl$layout))) {
  l <- tbl$layout[i, ]
  if (l$name %in% c("hline", "vline")) {
    grob_index <- tbl$layout[i, "z"]
    tbl$grobs[[grob_index]]$gp <- gpar(col = "#6950d8", lwd = 1)
  }
}

# 5) Título
title_grob <- textGrob(
  "Montos de los bienes de la canasta básica no alimentaria",
  x    = 0.68, y = 0.92,
  just = "right",
  gp   = gpar(
    fontfamily = "Ubuntu",
    fontface   = "bold",
    fontsize   = 18,
    col        = "#6950d8"
  )
)

fecha_sel
subtitle_grob <- textGrob(
  str_c("Al mes de ", month(fecha_sel, label = T, abbr = F), " del ", year(fecha_sel)),
  x    = 0.275, y = 0.87,
  just = "right",
  gp   = gpar(
    fontfamily = "Ubuntu",
    # fontface   = "bold",
    fontsize   = 16,
    col        = "gray50"
  )
)

caption_grob <- textGrob(
  "Elaborado por México, ¿Cómo vamos? con datos del INEGI y metodología del CONEVAL",
  x    = 0.84, y = 0.026,
  just = "right",
  gp   = gpar(
    fontfamily = "Ubuntu",
    fontface   = "bold",
    fontsize   = 15,
    col        = "white"
  )
)

# 6) Renderizar PNG de 16×9 pulgadas
output_file <- "../../03_infobites/48_02_cna_tabla.png"
png(
  filename = output_file,
  width    = 16*0.65, height = 9*0.65,
  units    = "in", res = 300,
  bg       = "transparent"
)

grid.newpage()
# Fondo
grid.draw(bg_grob)
# Título
grid.draw(title_grob)
grid.draw(subtitle_grob)
grid.draw(caption_grob)
# Tabla (centrada bajo el título)
pushViewport(viewport(y = 0.45, height = 0.8))
grid.draw(tbl)
popViewport()

dev.off()

message("¡Listo! La tabla se guardó en: ", output_file)

# 1) Preparar el fondo: convertir la primera página del PDF a PNG
tpl_pdf <- "../../03_infobites/00_plantillas/01_00_00_plantilla_blanco.pdf"
tpl_png <- pdftools::pdf_convert(
  pdf   = tpl_pdf,
  format = "png", 
  pages  = 1,
  dpi    = 300
)[1]

bg_rast <- readPNG(tpl_png)
bg_grob <- rasterGrob(bg_rast,
                      width  = unit(1, "npc"),
                      height = unit(1, "npc"))

# 2) Crear nueva tibble con fila de totales
can_tot <- canasta_alimentaria %>%
  ungroup() %>% 
  summarise(
    `Valor monetario mensual (Rural)` = sum(`Valor monetario mensual (Rural)`, na.rm = T),
    `Valor monetario mensual (Urbano)` = sum(`Valor monetario mensual (Urbano)`, na.rm = T)
  ) %>%
  mutate(
    clave   = "",
    nombres = "Total canasta alimentaria"
  ) %>%
  select(clave, nombres, everything())

can2 <- bind_rows(canasta_alimentaria, can_tot) %>% 
  rename(Clave = clave, Nombres = nombres)

canasta_alimentaria$clave[canasta_alimentaria$nombres == "Chile"] <- "A115-A118"

# 3) Formatear montos con separador de miles, dos decimales y símbolo $
can_fmt <- can2 %>%
  mutate(
    `Valor monetario mensual (Rural)` = ifelse(is.na(`Valor monetario mensual (Rural)`), yes = "", no = str_c("$", nb(`Valor monetario mensual (Rural)`, 2))),
    `Valor monetario mensual (Urbano)` = ifelse(is.na(`Valor monetario mensual (Urbano)`), yes = "", no = str_c("$", nb(`Valor monetario mensual (Urbano)`, 2)))
  )

# 4) Construir la tabla con estilo
tbl_theme <- ttheme_default(
  core = list(
    fg_params = list(
      col        = "black",
      fontfamily = "Ubuntu",
      fontface   = "plain",
      fontsize   = 12
    )
  ),
  colhead = list(
    fg_params = list(
      col        = "white",
      fontfamily = "Ubuntu",
      fontface   = "bold",
      fontsize   = 14
    ),
    bg_params = list(fill = "#6950d8"),
    padding   = unit(c(12, 12), "pt")
  ),
  base_size = 12
)

tbl <- tableGrob(can_fmt, rows = NULL, theme = tbl_theme)

# Pintar líneas en morado
for(i in seq_len(nrow(tbl$layout))) {
  l <- tbl$layout[i, ]
  if (l$name %in% c("hline", "vline")) {
    grob_index <- tbl$layout[i, "z"]
    tbl$grobs[[grob_index]]$gp <- gpar(col = "#6950d8", lwd = 1)
  }
}

# 5) Negritas en fila de totales (última fila)
n_rows <- nrow(can_fmt)
for(j in seq_along(tbl$grobs)) {
  g <- tbl$grobs[[j]]
  if(inherits(g, "text")) {
    pos <- tbl$layout[j, c("t","l")]
    if(pos["t"] == n_rows + 1) {
      tbl$grobs[[j]]$gp <- gpar(fontface = "bold", fontfamily = "Ubuntu", fontsize = 12)
    }
  }
}

# 6) Título y elementos de texto
title_grob <- textGrob(
  "Montos de la canasta básica alimentaria",
  x    = 0.03, y = 0.95,
  just = "left",
  gp   = gpar(
    fontfamily = "Ubuntu",
    fontface   = "bold",
    fontsize   = 46,
    col        = "#6950d8"
  )
)

subtitle_grob <- textGrob(
  str_c("Al mes de ", month(fecha_sel, label = TRUE, abbr = FALSE),
        " del ", year(fecha_sel)),
  x    = 0.03, y = 0.91,
  just = "left",
  gp   = gpar(
    fontfamily = "Ubuntu",
    fontsize   = 38,
    col        = "gray50"
  )
)

caption_grob <- textGrob(
  "Elaborado por México, ¿Cómo vamos? con datos del INEGI y metodología del CONEVAL",
  x    = 0.03, y = 0.026,
  just = "left",
  gp   = gpar(
    fontfamily = "Ubuntu",
    fontface   = "bold",
    fontsize   = 30,
    col        = "white"
  )
)

# 7) Renderizar PNG de tamaño deseado
output_file <- 
  # "canasta_alimentaria.png"
  "../../03_infobites/48_03_canasta_alimentaria_tabla.png"
png(
  filename = output_file,
  width    = 16*1.6, height = 9*1.6,
  units    = "in", res = 300,
  bg       = "transparent"
)

grid.newpage()
grid.draw(bg_grob)
grid.draw(title_grob)
grid.draw(subtitle_grob)
grid.draw(caption_grob)

pushViewport(viewport(x = 0.5, y = 0.45, width = 0.95, height = 0.85))
grid.draw(tbl)
popViewport()

dev.off()

message("¡Listo! La tabla se guardó en: ", output_file)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Guardamos el dato 
write_csv(datos_lineas_final, "01_09_lpei_coneval.csv")
# Guardamos el dato en la carpeta de Datos Crudos
write_csv(datos_lineas_final, "../../01_datos_crudos/líneas_de_pobreza_por_ingresos.csv")
# Guardamos el dato en el repo de Semáforos
write_csv(datos_lineas_final,"../../../mcv_semaforos/01_datos_crudos/01_09_lpei_coneval.csv")

