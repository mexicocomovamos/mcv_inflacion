
# Codigo para replicar las líneas de pobreza ----
# Documentos de referencia: 
# https://www.coneval.org.mx/InformesPublicaciones/InformesPublicaciones/Documents/Lineas_pobreza.pdf
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Nota_tecnica_ajuste_LPI_INPC_2024.pdf
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Contenido_y_valor_monetario_de_las_Lineas_de_Pobreza_por_Ingresos.zip
# https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Lineas_de_Pobreza_por_Ingresos.zip

# Librerias ---
library(tidyverse)
library(readxl)
library(inegiR)

# 1. Leemos los datos ----

# Claves de los bienes: 
eq_enigh <- readxl::read_xlsx("equivalencia_enigh_inpc.xlsx") %>% rename(clave = cve_enigh_2016) %>% 
  filter(cve_inpc_24 != "-")

eq_enigh  <- eq_enigh %>% 
  left_join(eq_enigh %>% 
  group_by(clave, producto) %>% 
  count() %>% 
  arrange(-n) %>% 
  mutate(clase = ifelse(n == 1, yes = 1, no = 2)))

# Canasta alimentaria 
c_alim <- readxl::read_xlsx("CA_JUVE.xlsx") %>% 
  # left_join(eq_enigh) %>% 
  filter(!(is.na(consumo_u) & is.na(consumo_r)))

# Canasta no alimentaria
c_n_alim <- read_csv("CANASTA NO ALIMENTARIA.csv") %>% 
  # left_join(eq_enigh) %>% 
  filter(clave != "CA")

# 2. Construimos los datos del INPC ----
# inpc <- readRDS("total_datos_inflacion_mes.rds")

v_token_inegi <- "682ad7f9-19fe-47f0-abec-e4c2ab2f2948"

tiempo_espera <- 0.4

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

inpc2 <- inpc %>% do.call(rbind, .)

inpc2 %>% 
  group_by(clave_inpc) %>% 
  count() %>% 
  arrange(n) 

saveRDS(inpc2 , "inpc_especifico_calculo_lpei.rds")
# inpc2 <- readRDS("inpc_especifico_calculo_lpei.rds")

# Referencia: https://www.coneval.org.mx/Medicion/Documents/Lineas_de_Pobreza_por_Ingresos/Nota_tecnica_ajuste_LPI_INPC_2024.pdf

# LPEI_rural ----
c_alim_rur <- c_alim %>% 
  filter(!is.na(consumo_r)) %>% 
  arrange(clave) 

inpc_ago_16 <-   inpc2 %>% 
  left_join(eq_enigh %>% select(clave, cve_inpc_24), by = c("clave" = "clave", 
                                                           "clave_inpc" = "cve_inpc_24")) %>%
  mutate(is.ago_2016 = ifelse(date == "2016-08-01", yes = T, no = F)) %>% 
  filter(is.ago_2016) %>% 
  select(clave, clave_inpc, inpc_ago_2016 = values)
  
deflactores <- inpc2 %>% 
  left_join(eq_enigh %>% select(clave, cve_inpc_24), 
            by = c("clave" = "clave", 
                   "clave_inpc" = "cve_inpc_24")) %>% 
  left_join(inpc_ago_16) %>% 
  mutate(def = values/inpc_ago_2016) %>% 
  select(-notes)
  
fecha_sel <- "2024-10-01"

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

### Chiles ----
chiles_rural <- lpei_r %>% 
    filter(clave %in% c("A115", "A116", "A117", "A118")) %>% 
    mutate(nombres =  "Chile", clave = "AComidaFuera") %>% 
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
lpei_r2_numero


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

deflactores <- inpc2 %>% 
  left_join(eq_enigh %>% select(clave, cve_inpc_24), 
            by = c("clave" = "clave", 
                   "clave_inpc" = "cve_inpc_24")) %>% 
  left_join(inpc_ago_16) %>% 
  mutate(def = values/inpc_ago_2016) %>% 
  select(-notes)

fecha_sel = "2024-10-01"

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
  mutate(nombres =  "Chile", clave = "AComidaFuera") %>% 
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
lpei_u2_numero

# Canasta no alimentaria --- 
# Hacemos la retropolacion 

td <- c_n_alim %>% 
  left_join(deflactores) %>% 
  filter(date == "2024-08-01") 

grupo_sel = "F"

# PARCHE PARA RELLENAR LOS DATOS DE LA RETROPOLACIÓN ----
td_corregido <- lapply(unique(td$grupo), function(grupo_sel){
  td %>% 
    filter(grupo == grupo_sel) %>% 
    mutate(inpc_ago_2016 = ifelse(is.na(inpc_ago_2016), 
                                  yes = mean(inpc_ago_2016, na.rm = T), 
                                  no = inpc_ago_2016)) %>% 
    mutate(def2 = ifelse(is.na(def), 
                         yes = values/inpc_ago_2016, 
                         no = def))
}) %>% 
  do.call(rbind, .)

td2 <- td_corregido %>% 
  mutate(costo = gib_16_urbano*def) %>%
  group_by(grupo2) %>%
  summarise(costo = sum(costo, na.rm = T) %>% round(2)) %>% 
  mutate(grupo2 = factor(grupo2, levels = c(
    "Transporte público",
    "Limpieza y cuidados de la casa",                      # Debería salir 124
    "Cuidados personales",                                 # Debería salir 239
    "Educación, cultura y recreación",                    
    "Comunicaciones y servicios para vehículos",
    "Vivienda y servicios de conservación",               # 324
    "Prendas de vestir, calzado y accesorios",
    "Cristalería, blancos y utensilios domésticos",       # 22.52
    "Cuidados de la salud",                               # 122.35
    "Enseres domésticos y mantenimiento de la vivienda",  # 37.31
    "Artículos de esparcimiento",
    "Transporte",                                         # 24.17
    "Otros gastos"))) %>% 
  arrange(grupo2)

td2
sum(td2$costo) # Debe de dar $2,210.32
