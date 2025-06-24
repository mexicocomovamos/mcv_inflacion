
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

# Guardamos el dato 
write_csv(datos_lineas_final, "01_09_lpei_coneval.csv")
# Guardamos el dato en la carpeta de Datos Crudos
write_csv(datos_lineas_final, "../../01_datos_crudos/líneas_de_pobreza_por_ingresos.csv")
# Guardamos el dato en el repo de Semáforos
write_csv(datos_lineas_final,"../../../mcv_semaforos/01_datos_crudos/01_09_lpei_coneval.csv")

