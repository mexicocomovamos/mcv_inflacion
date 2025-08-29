# Librerias ----
library(tidyverse)
library(googlesheets4)
library(gargle)
library(shinycssloaders)
library(sf)
library(shinyWidgets)
library(leaflet)
library(ggpubr)
library(pdftools)
library(DT)
library(ggtext)
library(ggrepel)
library(svglite)

# Funciones propias 
isDark <- function(colr) { (sum( col2rgb(colr) * c(299, 587,114))/1000 < 123) }
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")

# Tema personalizado México Como Vamos para el IPS
tema_mcv_ips <- theme_minimal() + 
  theme(
    plot.title        = element_text(size = 24, face = "bold", colour = "#6950D8", hjust = 0),
    plot.title.position = "plot",
    plot.subtitle     = element_text(size = 16, 
                                     colour = "#777777", 
                                     margin = margin(0,0,20,0), 
                                     hjust = 0),
    plot.caption      = element_text(size = 12, colour = "#666666", hjust = 0),
    plot.caption.position = "plot",
    plot.margin       = margin(0.8, 0.5, 0.5, 0.5, "cm"),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(colour = "#e0e0e0", linewidth = 0.5),
    panel.background  = element_rect(fill = "transparent", colour = NA),
    text              = element_text(family = "Ubuntu"),
    axis.text         = element_text(size = 12, colour = "#333333"),
    axis.title        = element_text(size = 14, colour = "#333333", face = "bold"),
    legend.text       = element_text(size = 12),
    legend.title      = element_text(size = 14, face = "bold"),
    legend.position   = "bottom",
    legend.margin     = margin(20, 0, 0, 0),
    strip.text        = element_text(size = 12, face = "bold", colour = "white"),
    strip.background  = element_rect(fill = "#6950D8", colour = "#6950D8")
  )

# Carga de los datos 
googlesheets4::gs4_auth(cache = ".secrets",
                        email = "juvenal@mexicocomovamos.mx")
options(gargle_oauth_cache = ".secrets")
gargle::gargle_oauth_cache()
list.files(".secrets/")

nb <- function(x, n = 1) prettyNum(format(round(x, n), nsmall = n), big.mark = ",")    

id_excel = "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1148109682"
hojas <- googlesheets4::sheet_names(id_excel)[-59]
metadatos <- googlesheets4::read_sheet(id_excel, sheet = "00_directorio") %>% 
  select(-nombre_codigo) %>% 
  rbind(readxl::read_xlsx("metadatos_complementarios.xlsx") %>% 
          select(-contains("...")))

id_excel_2 <- "https://docs.google.com/spreadsheets/d/1HOEDNzWmIAwfiMVNCNqJr1dirIEBchRe8nNK6d4RWlg/edit#gid=680643287"
meta_excel_edos <- read_sheet(id_excel_2, sheet = 1)

# Verificando las claves: 
cat_edos2 <- readxl::read_xlsx("catalogo_estatal.xlsx")
opciones_edos <- cat_edos2 %>% 
  slice(1:33) %>% 
  pull(entidad_abr_m)
names(opciones_edos) = cat_edos2$entidad[1:33]

opciones_indicador <- metadatos$id_indicador
names(opciones_indicador) <- str_c(metadatos$id_indicador, "-", metadatos$indicador)

opciones_indicador_edo <- meta_excel_edos$no
names(opciones_indicador_edo) <- meta_excel_edos$indicador

opciones_indicador <- append(opciones_indicador, opciones_indicador_edo)

cat_edos <- readxl::read_xlsx("01_Datos/08_ips_ranking.xlsx") %>% 
  select(cve_ent, entidad_abr_m, grupo_pib) %>% 
  unique() %>% 
  mutate(grupo_edos = case_when(grupo_pib == "PIB per cápita alto" ~ 1, 
                                grupo_pib == "PIB per cápita medio" ~ 2, 
                                grupo_pib == "PIB per cápita bajo" ~ 3))

datos_long <- readxl::read_xlsx("01_Datos/08_02_ips_ranking_series.xlsx")
abb_edos_control <- datos_long %>% select(cve_ent, entidad_abr_m) %>% unique()
abb_control <- abb_edos_control$cve_ent
names(abb_control) <- abb_edos_control$entidad_abr_m

cat <- tibble::tribble(
  ~cve_ent,                  ~nom_ent, ~abbr, ~Region,                          
  "00",            "Nacional" ,   "Nacional", "Nacional", 
  "01",      "Aguascalientes",        "AGS", "Centro-occidente",
  "02",     "Baja California",         "BC", "Noroeste",
  "03", "Baja California Sur",        "BCS", "Noroeste",
  "04",            "Campeche",       "CAMP", "Sur-sureste",
  "05", "Coahuila",       "COAH", "Noreste",
  "06",              "Colima",        "COL", "Centro-occidente",
  "07",             "Chiapas",       "CHIS", "Sur-sureste",
  "08",           "Chihuahua",       "CHIH", "Noreste",
  "09",    "Ciudad de México",       "CDMX", "Centro",
  "10",             "Durango",        "DGO", "Noreste",
  "11",          "Guanajuato",        "GTO", "Centro-occidente",
  "12",            "Guerrero",        "GRO", "Sur-sureste",
  "13",             "Hidalgo",        "HGO", "Centro",
  "14",             "Jalisco",        "JAL", "Centro-occidente",
  "15",              "México",        "MEX", "Centro",
  "16",           "Michoacán",       "MICH", "Centro-occidente",
  "17",             "Morelos",        "MOR", "Centro",
  "18",             "Nayarit",        "NAY", "Centro-occidente",
  "19",          "Nuevo León",         "NL", "Noreste",
  "20",              "Oaxaca",        "OAX", "Sur-sureste",
  "21",              "Puebla",        "PUE", "Sur-sureste",
  "22",           "Querétaro",        "QRO", "Centro",
  "23",        "Quintana Roo",       "QROO", "Sur-sureste",
  "24",     "San Luis Potosí",        "SLP", "Centro-occidente",
  "25",             "Sinaloa",        "SIN", "Noroeste",
  "26",              "Sonora",        "SON", "Noroeste",
  "27",             "Tabasco",        "TAB", "Sur-sureste",
  "28",          "Tamaulipas",      "TAMPS", "Noreste",
  "29",            "Tlaxcala",       "TLAX", "Centro",
  "30",            "Veracruz",        "VER", "Sur-sureste",
  "31",             "Yucatán",        "YUC", "Sur-sureste",
  "32",           "Zacatecas",        "ZAC", "Centro-occidente") %>% 
  select(-Region)

cat_2 <- left_join(cat_edos, cat)

shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  rename(cve_ent = CVE_EDO) %>% 
  mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
         ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])

get_datos <- function(ind_sel){
  h <- is.na(hojas[str_detect(hojas, pattern = str_c("\\_", ind_sel, "\\_"))] %>% first())
  j = ind_sel %in% meta_excel_edos$no
  
  if(h == T & j == T){
    h <- F
  }
  
  if(h){
    datos_seleccionados <- readxl::read_xlsx("01_Datos/08_ips_ranking.xlsx") %>% 
      mutate(id = str_pad(id, width = 6, pad = "0", side = "right")) %>% 
      filter(id == ind_sel) %>%
      mutate(id_dimension = str_extract(id, pattern = "^\\d\\d")) %>% 
      select(anio, cve_ent, entidad_abr_m, 
             id_dimension, id_indicador = id, indicador_value = value) %>% 
      mutate(anio = as.numeric(anio),
             cve_ent = as.character(cve_ent),
             id_dimension = as.numeric(id_dimension),
             id_componente = ifelse(str_length(id_indicador <= 3), 
                                    yes = str_extract(id_indicador, pattern = "\\d\\d$"), 
                                    no =  "00"),
             indicador_value = as.numeric(indicador_value))
  } else if(j){
    
    datos_seleccionados <- read_sheet(id_excel_2, sheet = ind_sel) %>% 
      mutate(cve_ent = str_pad(cve_ent, width = 2, pad = "0", side = "left")) %>% 
      left_join(cat_edos) %>% 
      mutate(anio = as.numeric(year),
             cve_ent = as.character(cve_ent),
             id_dimension = 99,
             id_indicador = no, 
             indicador_value = as.numeric(valor)) %>%
      arrange(as.numeric(cve_ent)) %>% 
      mutate(entidad_abr_m = ifelse(cve_ent == "00", yes = "Nacional", no = entidad_abr_m))
    
  } else {
    datos_seleccionados <- googlesheets4::read_sheet(id_excel, 
                                                     sheet = hojas[str_detect(hojas, pattern = str_c("\\_", ind_sel, "\\_"))] %>% first()) %>%
      select(anio,	cve_ent,	entidad_abr_m,
             id_dimension,	id_indicador,	indicador_value) %>%
      mutate(anio = as.numeric(anio),
             cve_ent = as.character(cve_ent),
             id_dimension = as.numeric(id_dimension),
             indicador_value = as.numeric(indicador_value))
  }
  
  return(datos_seleccionados)
}

get_anios_variables <- function(v1){
  dx <- get_datos(v1)
  dx %>% 
    pull(anio) %>% 
    unique() %>% 
    sort()
}

gen_grafica <- function(edo_sel, datos_sel){
  
  if(sum(str_detect(edo_sel, "Todos los estados")) > 0){
    edo_sel = opciones_edos
  }
  
  # Si los datos vienen del excel del IPS
  if(sum(names(datos_sel) %in% "id_indicador") == 1){
    
    if(str_length(datos_sel$id_indicador[1]) == 2){
      meta <- metadatos %>% 
        filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                         width = 2, 
                                         side = "left", 
                                         pad = "0"))  
    } else {
      meta <- metadatos %>% 
        filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                         width = 2, 
                                         side = "left", 
                                         pad = "0"))  
    }
    
  } else if("no" %in% names(datos_sel) && unique(datos_sel$no) %in% meta_excel_edos$no){
    
    meta <- meta_excel_edos %>% 
      filter(no %in% c(unique(datos_sel$no))) %>% 
      rename(unidad = umedida)
    
  } else {
    meta <- metadatos %>% 
      filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                       width = 2, 
                                       side = "left", 
                                       pad = "0"))  
  }
  
  # Verificar que meta no esté vacío
  if(nrow(meta) == 0) {
    # Si no hay metadatos, extraer el nombre del control de opciones_indicador
    if("id_indicador" %in% names(datos_sel)) {
      id_buscar <- unique(datos_sel$id_indicador)[1]
    } else if("no" %in% names(datos_sel)) {
      id_buscar <- unique(datos_sel$no)[1]
    } else {
      id_buscar <- "desconocido"
    }
    
    # Buscar en opciones_indicador
    titulo_encontrado <- names(opciones_indicador)[opciones_indicador == id_buscar]
    if(length(titulo_encontrado) > 0) {
      titulo <- str_remove(titulo_encontrado[1], "^[^-]+-") # Remover el ID del inicio
    } else {
      titulo <- paste("Indicador", id_buscar)
    }
    unidad <- ""
  } else {
    titulo <- meta %>% pull(indicador) %>% first()
    if(is.na(titulo) || length(titulo) == 0) {
      titulo <- "Indicador Sin Nombre"
    }
    
    # Verificar si existe la columna unidad
    if("unidad" %in% names(meta)) {
      unidad <- ifelse(str_detect(meta$unidad, "orcentaje|ORCENTAJE|%") & !is.na(meta$unidad), 
                       yes = "%", no = "")
    } else {
      unidad <- ""
    }
  }
  
  bd_plt = datos_sel %>% 
    mutate(color = ifelse(entidad_abr_m == "Nacional", 
                          yes = "Nacional", 
                          no = "Otros estados")) %>% 
    filter(entidad_abr_m %in% edo_sel) 
  
  # Verificar que bd_plt no esté vacío
  if(nrow(bd_plt) == 0) {
    # Retornar un gráfico vacío con mensaje
    empty_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No hay datos disponibles para mostrar", 
               size = 6, color = "#6551D0") +
      theme_void() +
      theme(text = element_text(family = "Ubuntu"))
    
    return(plotly::ggplotly(empty_plot))
  }
  
  # Crear subtítulo informativo
  num_entidades <- length(unique(bd_plt$entidad_abr_m))
  periodo <- paste(min(bd_plt$anio), "-", max(bd_plt$anio))
  subtitulo <- paste0("Evolución temporal | ", num_entidades, " entidades | Periodo: ", periodo)
  
  # Determinar colores más sofisticados
  colores_estados <- c("Nacional" = mcv_discrete[4], # Rojo/naranja para destacar Nacional
                       "Otros estados" = "#95a5a6") # Gris suave para otros estados
  
  plt = bd_plt %>% 
    ggplot(aes(x = anio, 
               y = indicador_value, 
               text = paste0("<b style='color:", mcv_discrete[1], "'>", entidad_abr_m, "</b><br>", 
                            "<b>Período:</b> ", anio, "<br>", 
                            "<b>Valor:</b> ", scales::comma(round(indicador_value, 2)), unidad),
               group = entidad_abr_m, 
               color = color)) +
    geom_line(aes(linewidth = color), alpha = 0.8) + 
    geom_point(aes(size = color), alpha = 0.9) + 
    scale_x_continuous(
      breaks = bd_plt$anio %>% unique() %>% sort(),
      expand = expansion(mult = c(0.02, 0.02))
    ) + 
    scale_y_continuous(
      labels = scales::comma_format(suffix = unidad),
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_color_manual(
      name = "Nivel:", 
      values = colores_estados,
      guide = guide_legend(
        override.aes = list(linewidth = c(1.5, 1), size = c(3, 2))
      )
    ) +
    scale_linewidth_manual(
      values = c("Nacional" = 1.5, "Otros estados" = 0.8),
      guide = "none"
    ) + 
    scale_size_manual(
      values = c("Nacional" = 3, "Otros estados" = 2),
      guide = "none"
    ) +
    labs(
      title = str_wrap(titulo, 60), 
      subtitle = subtitulo,
      y = paste0("Valor del indicador", ifelse(unidad != "", paste0(" (", unidad, ")"), "")), 
      x = "Año",
      caption = "Elaborado por México, ¿Cómo vamos? | Fuente: Índice de Progreso Social"
    ) +
    tema_mcv_ips +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.key.width = unit(1.5, "cm"),
      panel.grid.major.x = element_line(colour = "#f0f0f0", linewidth = 0.3),
      panel.grid.major.y = element_line(colour = "#e8e8e8", linewidth = 0.4)
    )
  
  # Usar tryCatch para manejar errores de plotly
  tryCatch({
    plotly::ggplotly(plt, tooltip = "text")
  }, error = function(e) {
    # Si plotly falla, retornar el gráfico ggplot básico
    warning("Error en plotly, mostrando gráfico estático: ", e$message)
    return(plt)
  })
}

gen_grafica_ggplot <- function(edo_sel, datos_sel, grises = TRUE){
  
  if(sum(str_detect(edo_sel, "Todos los estados")) > 0){
    edo_sel = opciones_edos
  }
  
  # Si los datos vienen del excel del IPS
  if(sum(names(datos_sel) %in% "id_indicador") == 1){
    
    if(str_length(datos_sel$id_indicador[1]) == 2){
      meta <- metadatos %>% 
        filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                         width = 2, 
                                         side = "left", 
                                         pad = "0"))  
    } else {
      meta <- metadatos %>% 
        filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                         width = 2, 
                                         side = "left", 
                                         pad = "0"))  
    }
    
  } else if("no" %in% names(datos_sel) && unique(datos_sel$no) %in% meta_excel_edos$no){
    
    meta <- meta_excel_edos %>% 
      filter(no %in% c(unique(datos_sel$no))) %>% 
      rename(unidad = umedida)
    
  } else {
    meta <- metadatos %>% 
      filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), 
                                       width = 2, 
                                       side = "left", 
                                       pad = "0"))  
  }
  
  # Verificar que meta no esté vacío
  if(nrow(meta) == 0) {
    # Si no hay metadatos, extraer el nombre del control de opciones_indicador
    if("id_indicador" %in% names(datos_sel)) {
      id_buscar <- unique(datos_sel$id_indicador)[1]
    } else if("no" %in% names(datos_sel)) {
      id_buscar <- unique(datos_sel$no)[1]
    } else {
      id_buscar <- "desconocido"
    }
    
    # Buscar en opciones_indicador
    titulo_encontrado <- names(opciones_indicador)[opciones_indicador == id_buscar]
    if(length(titulo_encontrado) > 0) {
      titulo <- str_remove(titulo_encontrado[1], "^[^-]+-") # Remover el ID del inicio
    } else {
      titulo <- paste("Indicador", id_buscar)
    }
    unidad <- ""
  } else {
    titulo <- meta$indicador %>% first()
    if(is.na(titulo) || length(titulo) == 0) {
      titulo <- "Indicador Sin Nombre"
    }
    
    # Verificar si existe la columna unidad
    if("unidad" %in% names(meta)) {
      unidad = ifelse((!is.na(meta$unidad) & meta$unidad == "Porcentaje"), 
                      yes = "%", no = "")
    } else {
      unidad <- ""
    }
  }
  
  bd_plt = datos_sel %>% 
    mutate(edo_seleccionado = ifelse(entidad_abr_m %in% c(edo_sel, "Nacional"), 
                                     yes = entidad_abr_m, 
                                     no = "Otros estados"))
  
  # Verificar que bd_plt no esté vacío
  if(nrow(bd_plt) == 0) {
    # Retornar un gráfico vacío con mensaje
    empty_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No hay datos disponibles para mostrar", 
               size = 8, color = "#6551D0") +
      labs(title = titulo) +
      theme_void() +
      theme(text = element_text(family = "Ubuntu"),
            plot.title = element_text(hjust = 0.5, 
                                      color = mcv_discrete[1], 
                                      size = 18,
                                      face = "bold"))
    
    return(empty_plot)
  }
  
  bd_plt = bd_plt %>% 
    mutate(entidad_abr_m = factor(entidad_abr_m, 
                                  levels = c("Nacional", 
                                             edo_sel,
                                             unique(bd_plt$entidad_abr_m)[!(unique(bd_plt$entidad_abr_m) %in% c("Nacional", edo_sel))])[-1] 
    )) %>% 
    mutate(ancho_linea = ifelse(edo_seleccionado == "Otros estados", 
                                yes = "linea_delgada", 
                                no = "linea_gruesa")) %>% 
    mutate(is.nacional = ifelse(is.na(entidad_abr_m), yes = "nac", no = "otros"))
  
  # Crear paleta de colores sofisticada siguiendo lineamientos MCV
  if(length(edo_sel) <= 3){
    colores_edo_sel = mcv_discrete[1:length(edo_sel)]
    names(colores_edo_sel) <- edo_sel
  } else if(length(edo_sel) <= 6) {
    colores_edo_sel = mcv_discrete[1:length(edo_sel)]
    names(colores_edo_sel) <- edo_sel
  } else if(length(edo_sel) <= 10) {
    # Para más estados, usar gradientes del color principal
    colores_edo_sel = colorRampPalette(c(mcv_discrete[1], mcv_discrete[3]))(length(edo_sel))
    names(colores_edo_sel) <- edo_sel
  } else {
    # Para muchos estados, usar tonos grises diferenciados
    colores_edo_sel = colorRampPalette(c("#7f8c8d", "#34495e"))(length(edo_sel))
    names(colores_edo_sel) <- edo_sel
  }
  
  colores_escala = c("Nacional" = mcv_discrete[4], # Rojo/naranja para Nacional
                     "Otros estados" = "#bdc3c7", # Gris claro para otros
                     colores_edo_sel)
  
  # Información para subtítulo
  num_entidades <- length(unique(bd_plt$entidad_abr_m))
  periodo <- paste(min(bd_plt$anio), "-", max(bd_plt$anio))
  subtitulo <- paste0("Evolución temporal de ", num_entidades, " entidades federativas | Periodo: ", periodo)
  
  suma_angulo = ifelse(unique(bd_plt$anio) %>% length() > 10, 
                       yes = 45, 
                       no = 0)
  
  plt = bd_plt %>% 
    ggplot(aes(x = anio, 
               y = indicador_value, 
               group = cve_ent,
               color = edo_seleccionado)) +
    
    # Líneas de fondo para todos los estados (más sutiles)
    geom_line(data = bd_plt %>% filter(edo_seleccionado == "Otros estados"),
              alpha = 0.3, linewidth = 0.7, color = "#bdc3c7") +
    geom_point(data = bd_plt %>% filter(edo_seleccionado == "Otros estados"),
               size = 2, alpha = 0.4, color = "#bdc3c7") +
    
    # Líneas destacadas para estados seleccionados y Nacional
    geom_line(data = bd_plt %>% filter(edo_seleccionado != "Otros estados"), 
              aes(linewidth = ifelse(edo_seleccionado == "Nacional", "gruesa", "normal"),
                  linetype = ifelse(edo_seleccionado == "Nacional", "dashed", "solid")),
              alpha = 0.9) +
    geom_point(data = bd_plt %>% filter(edo_seleccionado != "Otros estados"), 
               aes(size = ifelse(edo_seleccionado == "Nacional", "grande", "normal")),
               alpha = 0.9, stroke = 1, fill = "white", shape = 21) +
    
    # Solo etiquetas para el año final y estados destacados (máximo 5)
    geom_text(
      data = bd_plt %>%
        filter(anio == max(anio)) %>% 
        filter(edo_seleccionado != "Otros estados") %>%
        slice_head(n = 5), # Limitar a máximo 5 etiquetas
      aes(label = edo_seleccionado), 
      family = "Ubuntu", 
      fontface = "bold",
      size = 4,
      hjust = -0.1,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    
    # Escalas mejoradas
    scale_x_continuous(
      breaks = bd_plt$anio %>% unique() %>% sort(), 
      expand = expansion(mult = c(0.05, 0.2))
    ) + 
    scale_y_continuous(
      labels = scales::comma_format(suffix = unidad),
      expand = expansion(mult = c(0.05, 0.1))
    ) +
    scale_color_manual(values = colores_escala) +
    scale_linewidth_manual(
      values = c("gruesa" = 2.5, "normal" = 1.8),
      guide = "none"
    ) +
    scale_size_manual(
      values = c("grande" = 4, "normal" = 3),
      guide = "none"
    ) +
    scale_linetype_manual(
      values = c("dashed" = "dashed", "solid" = "solid"),
      guide = "none"
    ) +
    
    # Labels y tema mejorado para impresión
    labs(
      title = str_wrap(titulo, 50), 
      subtitle = subtitulo,
      y = paste0("Valor del indicador", ifelse(unidad != "", paste0(" (", unidad, ")"), "")), 
      x = "Año",
      caption = "ELABORADO POR MÉXICO, ¿CÓMO VAMOS? CON INFORMACIÓN PÚBLICA"
    ) +
    tema_mcv_ips + 
    theme(
      legend.position = "none",
      # Textos más grandes para impresión
      axis.text.x = element_text(size = 16, angle = suma_angulo, hjust = ifelse(suma_angulo > 0, 1, 0.5), vjust = 0.5),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 18, margin = margin(15, 0, 0, 0)),
      axis.title.y = element_text(size = 18, margin = margin(0, 15, 0, 0)),
      plot.title = element_text(size = 32, margin = margin(0, 0, 15, 0)),
      plot.subtitle = element_text(size = 16, margin = margin(0, 0, 25, 0)),
      plot.caption = element_text(size = 14, margin = margin(20, 0, 0, 0), hjust = 0, face = "bold"),
      # Grid más sutil
      panel.grid.major.x = element_line(colour = "#f5f5f5", linewidth = 0.5),
      panel.grid.major.y = element_line(colour = "#eeeeee", linewidth = 0.6),
      panel.grid.minor = element_blank(),
      # Márgenes optimizados para impresión
      plot.margin = margin(1, 2, 1, 1, "cm")
    )
  
  return(plt)
}

gen_tabla_peores <- function(edo_sel){
  datos_filt <- datos_long %>% 
    group_by(id) %>% 
    filter(anio == max(anio)) %>% 
    filter(cve_ent == edo_sel) %>% 
    arrange(-ips_rank_nacional) %>% 
    select(Año = anio,
           `Clave entidad` = cve_ent,
           `Abr. Entidad` = entidad_abr_m,
           `Nivel` = nivel, 
           `ID` = id,
           `Nombre del indicador` = name, 
           `Valor` = value, 
           `Lugar nacional` = ips_rank_nacional) %>% 
    mutate(`Ranking inverso` = 32 - (`Lugar nacional` - 1))
  
  dicc_dimension <- datos_filt %>% 
    filter(str_length(ID) < 3) %>% 
    select(ID_dimension = ID, Dimensión = `Nombre del indicador`)
  
  datos_filt <- datos_filt %>% 
    mutate(ID_dimension = str_extract(ID, pattern = "^\\d\\d")) %>% 
    left_join(dicc_dimension) %>% 
    select(`Año`, 
           `Clave entidad`,
           `Abr. Entidad`,
           `Nivel`, 
           `ID` ,
           `Dimensión`,
           `Nombre del indicador` , 
           `Valor`, 
           `Lugar nacional`, 
           `Ranking inverso`)
  
  return(datos_filt)
}

gen_mapa_vars_ips <- function(ind_sel, anio_sel, datos, 
                              edos_marcar = NULL, 
                              idioma = "Español"){
  
  if(sum(names(datos) %in% "id_componente") == 1){
    meta_sel <- metadatos %>% 
      filter(id_indicador %in% unique(datos$id_indicador))
  } else if("no" %in% names(datos)){
    meta_sel <- meta_excel_edos %>% 
      filter(no %in% c(unique(datos$no))) %>% 
      rename(unidad = umedida) %>% 
      rename(direccion = sentido)
  } else {
    meta_sel <- metadatos %>% 
      filter(id_indicador %in% str_pad(unique(datos$id_indicador), 
                                       width = 2, 
                                       side = "left", 
                                       pad = "0"))  
  }
  
  unidad = ifelse((!is.na(meta_sel$unidad) & meta_sel$unidad == "Porcentaje"), 
                  yes = "%", no = "")
  sentido_barras <- meta_sel$direccion
  
  mapa <- shp %>% 
    left_join(datos %>% filter(anio == anio_sel)) %>% 
    rename(value = indicador_value)
  
  # Correccion EDOMEX: 
  mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
  mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.1
  mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
  mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35
  
  # Corrección GTO
  mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21
  # Corrección PUE
  mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.27
  
  colores_seleccionados <- mcv_semaforo
  
  reversa <- ifelse(meta_sel$direccion == 1, yes = T, no = F)
  
  paleta <- colorNumeric(domain = mapa$value, 
                         palette = colores_seleccionados, 
                         reverse = reversa)
  color_linea <- "white"
  
  minimapa <- mapa %>% 
    filter(ENTIDAD %in% c("Ciudad de México", 
                          "México",
                          "Morelos", 
                          "Hidalgo", 
                          "Querétaro de Arteaga", 
                          "Tlaxcala", 
                          "Puebla")) %>% 
    mutate(label1 = str_c("[", entidad_abr_m, "]")) %>% 
    mutate(label2 = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")))
  
  minimapa_box = st_as_sfc(st_bbox(minimapa))
  coord_caja <- minimapa_box %>% 
    st_coordinates() %>% 
    as_data_frame() %>% 
    summarise(minX = min(X), 
              maxX = max(X), 
              minY = min(Y), 
              maxY = max(Y))
  
  mp <- minimapa %>% 
    ggplot(aes(fill = value)) + 
    geom_sf(color = color_linea, 
            linewidth = 0.3, 
            fill = paleta(minimapa$value)) + 
    geom_text(aes(x = ctrd_x,
                  y = ctrd_y,
                  label = str_c(label2, unidad)),
              size = 5,
              color = ifelse(sapply(paleta(minimapa$value), isDark),
                             "black", "black"),
              family = "Ubuntu",
              fontface = "bold") +
    geom_text(aes(x = ctrd_x,
                  y = ctrd_y,
                  label = label1),
              vjust = -1.1,
              size = 4,
              color = ifelse(sapply(paleta(minimapa$value), isDark),
                             "black", "black"),
              family = "Ubuntu",
              fontface = "bold") +
    theme_void() + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.border = element_rect(fill = "transparent",
                                      color = mcv_semaforo[1], 
                                      linewidth = 1), 
          legend.position = "none")
  
  titulo_mapa <- ifelse(idioma == "Español", 
                        yes = str_c(meta_sel$indicador, ", ", anio_sel) %>% str_wrap(60), 
                        no = str_c(meta_sel$indicador_name_ingles, ", ", anio_sel) %>% str_wrap(60))
  
  mg <- mapa %>%
    mutate(label1 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("[", entidad_abr_m, "]"), 
                           yes = "")) %>% 
    mutate(label2 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")), 
                           yes = "")) %>% 
    ggplot(aes(fill = value)) + 
    geom_sf(color = color_linea, 
            fill = paleta(mapa$value),
            linewidth = 0.3) +
    geom_sf(data = minimapa_box, 
            color = mcv_semaforo[1], 
            linewidth = 1,
            fill = NA) +
    geom_text(aes(x = ctrd_x,
                  y = ctrd_y,
                  label = str_c(label2, ifelse(label2 == "",
                                               "", unidad))),
              size = 4,
              color = ifelse(sapply(paleta(mapa$value), isDark),
                             "black", "black"),
              family = "Ubuntu",
              fontface = "bold") +
    geom_text(aes(x = ctrd_x,
                  y = ctrd_y,
                  label = label1),
              vjust = -1.1,
              size = 4.5,
              color = ifelse(sapply(paleta(mapa$value), isDark),
                             "black", "black"),
              family = "Ubuntu",
              fontface = "bold") +
    labs(x = NULL,y = NULL,fill = "Ingreso ", 
         title = titulo_mapa) + 
    theme_minimal() +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 20, colour = "#777777", margin=margin(0,0,60,0)),
          plot.margin= margin(1.5, 0.5, 1.5, 0.5, "cm"),
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(2, "lines"),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          legend.text = element_text(size = 25),
          axis.text = element_blank(), 
          panel.grid = element_blank(),
          legend.position = "none")
  
  datos <- datos %>% 
    rename(value = indicador_value) %>% 
    filter(anio == anio_sel) %>% 
    mutate(fill_barra = ifelse(entidad_abr_m %in% edos_marcar, 
                               yes = paleta(value),
                               no = "gray50")) %>% 
    mutate(entidad_abr_m = ifelse(entidad_abr_m == "Nacional", 
                                  yes = "<b style = 'color:#ff6260'>Nacional</b>", 
                                  no = entidad_abr_m))
  
  g_barras <- 
    datos %>% 
    ggplot(aes(x = reorder(entidad_abr_m,
                           sentido_barras*value),y = value)) + 
    geom_col(fill =
               paleta(datos$value)) +
    geom_text(aes(label = str_c(
      prettyNum(format(round(value, 1), nsmall = 1), big.mark = ","),
      unidad)), 
      family = "Ubuntu", 
      vjust = 0.5,
      hjust = -0.1,
      size = 6) + 
    coord_flip() + 
    scale_y_continuous(expand = expansion(c(0.1, 0.4))) +
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          axis.text.y = element_markdown(size = 20, 
                                         vjust = 0.5 ,
                                         hjust = 1),
          text = element_text(family = "Ubuntu"), 
          plot.margin= margin(4.7, 0.5, 2, 0.5, "cm")
    )
  
  g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                  xmin = -99.5,
                                  xmax = -84.5, 
                                  ymin = 22.5,
                                  ymax = 30.5)
  
  g_plot <- 
    ggarrange(g_mapa, g_barras, widths = c(2.2, 1))
  return(g_plot)
}

# Función para generar gráfica de correlación
gen_grafica_corr <- function(v1,v2, anio_1, anio_2, tipo = "1"){
  
  m1 <- metadatos %>% filter(id_indicador == v1) %>% pull(indicador)
  u1 <- metadatos %>% filter(id_indicador == v1) %>% pull(unidad)
  m2 <- metadatos %>% filter(id_indicador == v2) %>% pull(indicador)
  u2 <- metadatos %>% filter(id_indicador == v2) %>% pull(unidad)
  
  if(identical(m1, character(0))){
    m1 <- meta_excel_edos %>% filter(no == v1) %>% pull(indicador)
    u1 <- meta_excel_edos %>% filter(no == v1) %>% pull(umedida)
  }
  
  if(identical(m2, character(0))){
    m2 <- meta_excel_edos %>% filter(no == v2) %>% pull(indicador)
    u2 <- meta_excel_edos %>% filter(no == v2) %>% pull(umedida)
  }
  
  u1 <- ifelse(is.na(u1), yes = "", no = u1)
  u2 <- ifelse(is.na(u2), yes = "", no = u2)
  
  u1 <- ifelse(str_detect(u1, "orcentaje|%"), yes = "%", no = "")
  u2 <- ifelse(str_detect(u2, "orcentaje|%"), yes = "%", no = "")
  
  b1 <- get_datos(v1) %>% 
    filter(anio == anio_1) %>% 
    select(id_indicador, entidad_abr_m, cve_ent, anio, value = indicador_value) %>% 
    mutate(indicador = m1)
  
  b2 <- get_datos(v2) %>% 
    filter(anio == anio_2) %>% 
    select(id_indicador, entidad_abr_m, cve_ent, anio, value = indicador_value) %>% 
    mutate(indicador = m2)
  
  base_cor <- rbind(b1, b2) %>% 
    pivot_wider(id_cols = c("cve_ent", "entidad_abr_m"), 
                values_from = value, 
                names_from = indicador) %>% 
    mutate(entidad_abr_m = ifelse(cve_ent == "00", yes = "Nacional", no = entidad_abr_m)) %>% 
    mutate(is.nacional = ifelse(entidad_abr_m == "Nacional", 
                                yes = "Nacional", 
                                no = "Estados")) %>% 
    left_join(cat_2) %>% 
    mutate(grupo_edos = case_when(grupo_edos == 1 ~ "PIB per cápita alto", 
                                  grupo_edos == 2 ~ "PIB per cápita medio", 
                                  grupo_edos == 3 ~ "PIB per cápita bajo"))
  
  names(base_cor)[3:4] <- c("v1", "v2")
  base_cor$v1 <- as.numeric(base_cor$v1)
  base_cor$v2 <- as.numeric(base_cor$v2)
  
  unique(base_cor$grupo_edos)
  base_cor$grupo_edos <- factor(base_cor$grupo_edos, levels = c("PIB per cápita alto" , 
                                                                "PIB per cápita medio", 
                                                                "PIB per cápita bajo" ) %>% rev())
  
  if(tipo == 1){
    g = base_cor %>% 
      ggplot(aes(x = v1, 
                 y = v2, 
                 color = is.nacional
      )) + 
      geom_smooth(method = "lm", 
                  color = mcv_discrete[3]) + 
      geom_point(fill = "white", 
                 color = mcv_discrete[1],
                 stroke = 1.5,
                 size = 4,
                 pch = 21) + 
      geom_text_repel(aes(label = abbr, 
                          fontface = is.nacional), 
                      family = "Ubuntu", 
                      fontface = "bold",
                      size = 5) + 
      scale_y_continuous(labels = scales::comma_format(suffix = u2)) + 
      scale_x_continuous(labels = scales::comma_format(suffix = u1)) + 
      scale_color_manual(values = mcv_discrete[c(1,4)]) + 
      scale_discrete_manual(aesthetics = "fontface",
                            values = c("plain", "bold")) + 
      labs(x = str_wrap(m1, 60), 
           y = str_wrap(m2, 30), 
           title = "Correlación entre variables del IPS", 
           subtitle = str_c(m1, " (", anio_1, ")",  " vs ", m2, " (", anio_2, ")")  %>% str_wrap(90),
           caption = "ELABORADO POR MÉXICO, ¿CÓMO VAMOS?"
      ) + 
      tema_mcv_ips +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 20),
        axis.ticks = element_line(),
        axis.line = element_line()
      ) 
  } else if(tipo == "2"){
    g = base_cor %>%
      filter(abbr != "Nacional") %>% 
      filter(!is.na(grupo_edos)) %>% 
      ggplot(aes(x = v1,
                 y = v2,
                 color = is.nacional
      )) +
      facet_wrap(~grupo_edos, scales = "free_x") +
      geom_point(aes(fill = grupo_edos),
                 color = mcv_discrete[1],
                 stroke = 1,
                 size = 3,
                 pch = 21) +
      geom_smooth(method = "lm") +
      geom_text_repel(aes(label = abbr, 
                          fontface = is.nacional), 
                      family = "Ubuntu") + 
      scale_y_continuous(labels = scales::comma_format(suffix = u2)) + 
      scale_x_continuous(labels = scales::comma_format(suffix = u1)) + 
      scale_color_manual(values = mcv_discrete[c(1,4)]) + 
      scale_discrete_manual(aesthetics = "fontface",
                            values = c("plain", "bold")) + 
      labs(x = str_wrap(m1, 60), 
           y = str_wrap(m2, 30), 
           title = "Correlación entre variables del IPS", 
           subtitle = str_c(m1, " (", anio_1, ")",  " vs ", m2, " (", anio_2, ")") %>% str_wrap(90),
           caption = "ELABORADO POR MÉXICO, ¿CÓMO VAMOS?"
      ) + 
      tema_mcv_ips +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 20),
        strip.text.x = element_text(size = 16, color = "white"),
        strip.background = element_rect(color = "white", fill = "#6950D8"),
        axis.ticks = element_line(),
        axis.line = element_line()
      ) 
  }
  
  return(g)
}

# Función para generar gráfica de cambio bivariado
gen_grafica_cambio <- function(v1,v2, anio_inicio, anio_fin, c_por_1 = "Absoluto", c_por_2 = "Absoluto"){
  
  m1 <- metadatos %>% filter(id_indicador == v1) %>% pull(indicador)
  m2 <- metadatos %>% filter(id_indicador == v2) %>% pull(indicador)
  u1 <- metadatos %>% filter(id_indicador == v1) %>% pull(unidad)
  u2 <- metadatos %>% filter(id_indicador == v2) %>% pull(unidad)
  s1 <- metadatos %>% filter(id_indicador == v1) %>% pull(direccion)
  s2 <- metadatos %>% filter(id_indicador == v2) %>% pull(direccion)
  
  if(identical(m1, character(0))){
    m1 <- meta_excel_edos %>% filter(no == v1) %>% pull(indicador)
    u1 <- meta_excel_edos %>% filter(no == v1) %>% pull(umedida)
    s1 <- meta_excel_edos %>% filter(no == v1) %>% pull(sentido)
  }
  
  if(identical(m2, character(0))){
    m2 <- meta_excel_edos %>% filter(no == v2) %>% pull(indicador)
    u2 <- meta_excel_edos %>% filter(no == v2) %>% pull(umedida)
    s2 <- meta_excel_edos %>% filter(no == v2) %>% pull(sentido)
  }
  
  u1 <- ifelse(is.na(u1), yes = "", no = u1)
  u2 <- ifelse(is.na(u2), yes = "", no = u2)
  
  u1 <- ifelse(str_detect(u1, "orcentaje|%"), yes = "%", no = "")
  u2 <- ifelse(str_detect(u2, "orcentaje|%"), yes = "%", no = "")
  
  b1 <- get_datos(v1) %>% 
    filter(between(anio, anio_inicio, anio_fin)) %>%
    filter(anio == min(anio) | anio == max(anio)) %>% 
    select(id_indicador, entidad_abr_m, cve_ent, anio, value = indicador_value) %>% 
    mutate(indicador = m1) %>% 
    arrange(anio) %>% 
    pivot_wider(id_cols = c(entidad_abr_m), 
                names_from = anio, 
                values_from = value)
  anios_b1 <- str_c(names(b1)[c(2,3)], collapse = "-")
  names(b1)[c(2,3)] <- c("y1", "y2")
  b1$b1_cambio <- b1$y2-b1$y1
  b1$b1_cambio_pct <- 100*((b1$y2-b1$y1)/b1$y1)
  b1 <- b1 %>% select(entidad_abr_m, contains("b1"))
  
  if(c_por_1 == "Absoluto"){
    b1 <- b1 %>% 
      select(entidad_abr_m, b1_cambio)
    ux <- ""
  } else {
    b1 <- b1 %>% 
      select(entidad_abr_m, b1_cambio_pct) %>% 
      rename(b1_cambio = b1_cambio_pct)
    ux <- "%"
  }
  
  b2 <- get_datos(v2) %>% 
    filter(between(anio, anio_inicio, anio_fin)) %>%
    filter(anio == min(anio) | anio == max(anio)) %>% 
    select(id_indicador, entidad_abr_m, cve_ent, anio, value = indicador_value) %>% 
    mutate(indicador = m2) %>% 
    arrange(anio) %>% 
    pivot_wider(id_cols = c(entidad_abr_m), 
                names_from = anio, 
                values_from = value)
  anios_b2 <- str_c(names(b2)[c(2,3)], collapse = "-")
  names(b2)[c(2,3)] <- c("y1", "y2")
  b2$b2_cambio <- b2$y2-b2$y1
  b2$b2_cambio_pct <- 100*((b2$y2-b2$y1)/b2$y1)
  b2 <- b2 %>% select(entidad_abr_m, contains("b2"))
  
  if(c_por_2 == "Absoluto"){
    b2 <- b2 %>% 
      select(entidad_abr_m, b2_cambio)
    uy <- ""
  } else {
    b2 <- b2 %>% 
      select(entidad_abr_m, b2_cambio_pct) %>% 
      rename(b2_cambio = b2_cambio_pct)
    uy <- "%"
  }
  
  base_cambio <- left_join(b1, b2) %>% 
    left_join(cat_2) %>% 
    mutate(grupo_edos = case_when(grupo_edos == 1 ~ "PIB per cápita alto", 
                                  grupo_edos == 2 ~ "PIB per cápita medio", 
                                  grupo_edos == 3 ~ "PIB per cápita bajo", 
                                  TRUE ~ "Nacional")) %>% 
    mutate(grupo_pib = factor(grupo_edos, levels = c("Nacional",
                                                     "PIB per cápita alto",
                                                     "PIB per cápita medio",
                                                     "PIB per cápita bajo" )))
  
  nota_x = ifelse(c_por_1 == "Absoluto", yes = "Cambio en ", no = "Cambio porcentual en ")
  nota_y = ifelse(c_por_2 == "Absoluto", yes = "Cambio en ", no = "Cambio porcentual en ")
  
  nota_x = ifelse(nota_x == "Cambio en " & u1 == "%", yes = "Cambio en puntos porcentuales: ", no = nota_x)
  nota_y = ifelse(nota_y == "Cambio en " & u2 == "%", yes = "Cambio en puntos porcentuales: ", no = nota_y)
  
  dist_x <- (max(base_cambio$b1_cambio) - min(base_cambio$b1_cambio))*0.1
  dist_y <- (max(base_cambio$b2_cambio) - min(base_cambio$b2_cambio))*0.1
  
  if(s1 == -1){
    colores_x_etiqueta <- c("red", "#236344")
  } else {
    colores_x_etiqueta <- c("#236344", "red")
  }
  
  if(s2 == -1){
    colores_y_etiqueta <- c("red", "#236344")
  } else {
    colores_y_etiqueta <- c("#236344", "red")
  }
  
  base_cambio %>% 
    ggplot(aes(x = b1_cambio, y = b2_cambio)) + 
    geom_vline(xintercept = 0, linetype = 2, linewidth = 1, color = "red", alpha = 0.6) + 
    geom_hline(yintercept = 0, linetype = 2, linewidth = 1, color = "red", alpha = 0.6) + 
    annotate(geom = "richtext",
             x = c(min(base_cambio$b1_cambio)-dist_x,
                   min(base_cambio$b1_cambio)-dist_x,
                   max(base_cambio$b1_cambio)+dist_x,
                   max(base_cambio$b1_cambio)+dist_x),
             y = c(min(base_cambio$b2_cambio)-dist_y,
                   max(base_cambio$b2_cambio)+dist_y,
                   min(base_cambio$b2_cambio)-dist_y,
                   max(base_cambio$b2_cambio)+dist_y),
             label = c(
               str_c("<b style = 'color:", colores_x_etiqueta[1], ";'>Disminución ", m2, "</b><br><b style = 'color:", colores_y_etiqueta[1], ";'>Disminución ", m1, "</b>") %>% str_wrap(100),
               str_c("<b style = 'color:", colores_x_etiqueta[2], ";'>Aumento ", m2, "</b><br><b style = 'color:",     colores_y_etiqueta[1], ";'>Disminución ", m1, "</b>") %>% str_wrap(100),
               str_c("<b style = 'color:", colores_x_etiqueta[1], ";'>Disminución ", m2, "</b><br><b style = 'color:", colores_y_etiqueta[2], ";'>Aumento ", m1, "</b>")     %>% str_wrap(100),
               str_c("<b style = 'color:", colores_x_etiqueta[2], ";'>Aumento ", m2, "</b><br><b style = 'color:",     colores_y_etiqueta[2], ";'>Aumento ", m1, "</b>")     %>% str_wrap(100) 
             ),
             hjust = c(0, 0, 1, 1),
             family = "Ubuntu",
             size = 3.5) + 
    geom_point(size = 4, 
               pch = 21, 
               fill = "white", stroke = 2,
               aes(color = grupo_pib)) + 
    ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                             family = "Ubuntu",
                             color = "gray50",
                             fontface = "bold",
                             size = 3.5) + 
    scale_y_continuous(labels = scales::comma_format(accuracy = 0.1, suffix = uy), expand = expansion(c(0.05, 0.05))) + 
    scale_x_continuous(labels = scales::comma_format(accuracy = 0.1, suffix = ux), expand = expansion(c(0.03, 0.03))) + 
    scale_color_manual(values = mcv_discrete[c(4,3,2,5)]) +
    labs(x = str_c(nota_x, m1, " ", anios_b1) %>% str_wrap(60), 
         y = str_c(nota_y, m2, " ", anios_b2) %>% str_wrap(30),
         title = str_c("Cambio en ", m1, " vs cambio en ", m2) %>% str_wrap(70),
         subtitle = str_c("Periodo ", anio_inicio, "-", anio_fin),
         color = NULL,
         caption = "ELABORADO POR MÉXICO, ¿CÓMO VAMOS?") + 
    tema_mcv_ips + 
    theme(
      legend.position = "top",
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      axis.title = element_text(size = 16),
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 20),
      legend.text = element_text(size = 16),
      legend.key.width = unit(2, "cm"),
      axis.line = element_line()
    ) + 
    guides(color = guide_legend(ncol = 4))
}

# Función para generar regresiones
gen_regresiones <- function(v1){
  
  datos_dims <- rbind(get_datos("000000"),get_datos("010000"),
                      get_datos("030000"),get_datos("020000")) %>% 
    select(cve_ent, id_indicador, dim_value = indicador_value)
  
  m1 <- metadatos %>% filter(id_indicador == v1) %>% pull(indicador)
  unidad <- metadatos %>% filter(id_indicador == v1) %>% pull(unidad)
  
  if(identical(m1, character(0))){
    m1 <- meta_excel_edos %>% filter(no == v1) %>% pull(indicador)
    unidad <- meta_excel_edos %>% filter(no == v1) %>% pull(umedida)
  }
  
  if(unidad == "Porcentaje"){
    unidad <- "%"
  } else {
    unidad <- ""
  }
  
  datos_variable <- get_datos(v1) %>% 
    filter(anio == max(anio))
  
  anio_max <- unique(datos_variable$anio)
  
  datos_variable <- datos_variable%>% 
    select(cve_ent, entidad_abr_m,
           value = indicador_value) 
  
  d <- left_join(datos_dims, datos_variable) %>% 
    mutate(nombre_dimension = case_when(id_indicador == "000000" ~ "0. Índice de Progreso Social", 
                                        id_indicador == "010000" ~ "1. Necesidades Humanas Básicas", 
                                        id_indicador == "020000" ~ "2. Fundamentos del Bienestar", 
                                        id_indicador == "030000" ~ "3. Oportunidades"
    )) %>% 
    mutate(nombre_dimension = factor(nombre_dimension, levels = c("0. Índice de Progreso Social", 
                                                                  "1. Necesidades Humanas Básicas", 
                                                                  "2. Fundamentos del Bienestar", 
                                                                  "3. Oportunidades")))
  
  titulo <- str_c(m1, " vs Índice de Progreso Social") %>% str_wrap(80)
  subtitulo <- "Intervalos de confianza al 95%"
  eje_x <- str_c(m1, ", ",anio_max)
  eje_y <- "Puntaje 2023"
  nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
  
  g <- 
    ggplot(d, aes(x = value,y = dim_value,label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + 
    geom_point(col = mcv_discrete[1], size = 3) +
    ggrepel::geom_text_repel(family = "Ubuntu", size = 4) +
    facet_wrap(~nombre_dimension, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
    labs(
      title = titulo, 
      subtitle = subtitulo, 
      x = eje_x, 
      y = eje_y, 
      color = "",
      caption = nota_pie
    ) + 
    tema_mcv_ips +
    theme(
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 28),
      plot.subtitle = element_text(size = 22),
      strip.text.x = element_text(size = 18, color = "white"),
      strip.background = element_rect(color = "white", fill = "#6950D8"),
      legend.position = "none"
    )
  
  return(g)
}

# Función para generar gráfica de cambio simple
gen_grafica_simple_cambio <- function(ind_sel, anio_1, anio_2){
  
  require(scales)
  
  m1 <- metadatos %>% filter(id_indicador == ind_sel) %>% pull(indicador)
  u1 <- metadatos %>% filter(id_indicador == ind_sel) %>% pull(unidad)
  s1 <- metadatos %>% filter(id_indicador == ind_sel) %>% pull(direccion)
  
  if(identical(m1, character(0))){
    m1 <- meta_excel_edos %>% filter(no == ind_sel) %>% pull(indicador)
    u1 <- meta_excel_edos %>% filter(no == ind_sel) %>% pull(umedida)
    s1 <- meta_excel_edos %>% filter(no == ind_sel) %>% pull(sentido)
  }
  
  u1 <- ifelse(is.na(u1), yes = "", no = u1)
  u1 <- ifelse(str_detect(u1, "orcentaje|%"), yes = "%", no = "")
  
  b1 <- get_datos(ind_sel) %>% 
    select(id_indicador, entidad_abr_m, cve_ent, anio, value = indicador_value) %>% 
    mutate(indicador = m1) %>% 
    filter(between(anio, anio_1, anio_2)) %>% 
    filter(anio == min(anio) | anio == max(anio)) 
  
  anios <- unique(b1$anio) %>% sort() %>% rev()
  
  b1 <- b1 %>% 
    arrange(cve_ent, anio) %>% 
    group_by(cve_ent, entidad_abr_m) %>% 
    summarise(diff = diff(value))
  
  if(s1 == -1){
    pal_colores_barras <- c("1" = "#ff6260", "-1" = "#00b783")
  } else {
    pal_colores_barras <- c("-1" = "#ff6260", "1" = "#00b783")
  }
  
  g = b1 %>% 
    ggplot(aes(x = reorder(entidad_abr_m, 
                           diff), 
               y = diff)) + 
    geom_col(aes(fill = sign(diff) %>% factor())) + 
    geom_hline(yintercept = 0, linetype = 1, linewidth = 1.1, color = "gray10") + 
    geom_text(aes(label = str_c(nb(diff), ""),
                  hjust = sign(diff) %>% factor(), 
                  color = sign(diff) %>% factor()), 
              size = 8, 
              family = "Ubuntu",
              fontface = "bold",
              vjust = 0.5,
              angle = 90) + 
    labs(y = str_c("Variación ", str_c(anios, collapse = "-")), 
         x = NULL, 
         title = str_wrap(str_c("Variación ", m1), 90), 
         subtitle = str_wrap(str_c("Periodo: ", str_c(anios, collapse = "-")), 90),
         caption = "ELABORADO POR MÉXICO, ¿CÓMO VAMOS?") +
    scale_y_continuous(expand = expansion(c(0.25, 0.3)), 
                       labels = comma_format(suffix = "", accuracy = 0.1)) +
    scale_fill_manual(values = c(pal_colores_barras)) + 
    scale_color_manual(values = c(pal_colores_barras)) + 
    scale_discrete_manual(aesthetics = "hjust", values = c("-1" = 1.1, "1" = -0.1)) +
    tema_mcv_ips + 
    theme(
      legend.position = "none",
      axis.text.x = ggtext::element_markdown(size = 20, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 20),
      axis.title = element_text(size = 18),
      plot.title = element_text(size = 28),
      plot.subtitle = element_text(size = 22),
      panel.grid.major = element_line(linetype = 2)
    )
  
  return(g)
}

# Función para crear el header de México Como Vamos
create_mcv_header <- function() {
  tagList(
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
          
          # Redes sociales izquierda
          div(
            style = "display: flex; align-items: center; gap: 15px;",
            tags$a(
              href = "https://twitter.com/mexicocomovamos", 
              style = "color: white; font-size: 18px; text-decoration: none;",
              HTML('<i class="fab fa-twitter"></i>')
            ),
            tags$a(
              href = "https://facebook.com/mexicocomovamos", 
              style = "color: white; font-size: 18px; text-decoration: none;",
              HTML('<i class="fab fa-facebook"></i>')
            ),
            tags$a(
              href = "https://youtube.com/mexicocomovamos", 
              style = "color: white; font-size: 18px; text-decoration: none;",
              HTML('<i class="fab fa-youtube"></i>')
            ),
            tags$a(
              href = "https://instagram.com/mexicocomovamos", 
              style = "color: white; font-size: 18px; text-decoration: none;",
              HTML('<i class="fab fa-instagram"></i>')
            )
          ),
          
          # Mensaje central
          div(
            style = "color: white; font-weight: 500; font-family: 'Ubuntu', sans-serif;",
            "México, ¿cómo vamos? 🇲🇽"
          ),
          
          # Espacio derecha
          div(
            style = "width: 150px;"
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
          
          # Navegación izquierda
          div(
            style = "display: flex; align-items: center; gap: 30px; flex: 1;",
            
            tags$a(
              href = "https://mexicocomovamos.mx/equipo/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "MÉXICO,",
              br(),
              "¿CÓMO VAMOS?"
            ),
            
            tags$a(
              href = "https://mexicocomovamos.mx/indice-de-progreso-social/",
              style = "
                color: #6551D0;
                text-decoration: none;
                font-size: 12px;
                font-weight: 700;
                padding: 8px 0;
                border-bottom: 2px solid #6551D0;
                font-family: 'Ubuntu', sans-serif;
              ",
              "ÍNDICE DE",
              br(),
              "PROGRESO SOCIAL"
            )
          ),
          
          # Logo central
          div(
            style = "flex: 0 0 auto; margin: 0 20px;",
            tags$a(
              href = "https://mexicocomovamos.mx/",
              tags$img(
                src = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg",
                alt = "México Como Vamos",
                style = "height: 60px; width: auto;"
              )
            )
          ),
          
          # Navegación derecha
          div(
            style = "display: flex; align-items: center; gap: 30px; flex: 1; justify-content: flex-end;",
            
            tags$a(
              href = "https://mexicocomovamos.mx/fichas-por-estado/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "FICHAS",
              br(),
              "POR ESTADO"
            ),
            
            tags$a(
              href = "https://mexicocomovamos.mx/categoria/publicaciones/",
              style = "
                color: #333333;
                text-decoration: none;
                font-size: 12px;
                font-weight: 500;
                padding: 8px 0;
                border-bottom: 2px solid transparent;
                transition: all 0.3s ease;
                font-family: 'Ubuntu', sans-serif;
              ",
              onmouseover = "this.style.borderBottom='2px solid #6551D0'",
              onmouseout = "this.style.borderBottom='2px solid transparent'",
              "PUBLICACIONES",
              br(),
              "Y MEDIOS"
            )
          )
        )
      )
    )
  )
}

# SHINY ----

library(shiny)

ui <- fluidPage(
  # Incluir Google Fonts para Ubuntu y Font Awesome para iconos
  tags$head(
    # Favicon
    tags$link(rel = "icon", type = "image/png", href = "https://mexicocomovamos.mx/wp-content/uploads/2024/03/mcv-10aniv.svg"),
    
    # Meta tags para compartir en redes sociales
    tags$meta(property = "og:title", content = "México ¿Cómo vamos? - Análisis del IPS"),
    tags$meta(property = "og:description", content = "Análisis interactivo del Índice de Progreso Social de México"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "México ¿Cómo vamos? - Análisis del IPS"),
    tags$meta(name = "twitter:description", content = "Análisis interactivo del Índice de Progreso Social de México"),
    
    # Title
    tags$title("México ¿Cómo vamos? - Análisis del IPS"),
    
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
          padding: 10px 0;
        }
        
        /* Estilos para wellPanel */
        .well {
          background-color: white !important;
          border-radius: 12px;
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
          margin-bottom: 20px;
        }
        
        /* Estilos para la sección de gráficas */
        .graph-container {
          background-color: white;
          border-radius: 12px;
          padding: 25px;
          margin: 20px 0;
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border: 1px solid #e9ecef;
        }
        
        /* Estilos para el hero header */
        .hero-header {
          position: relative;
          height: 180px;
          display: flex;
          align-items: center;
          justify-content: center;
          margin: 0 0 25px 0;
          background: linear-gradient(135deg, #6551D0 0%, #8B5FBF 100%);
          border-radius: 12px;
          overflow: hidden;
        }
        
        .hero-header::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(0, 0, 0, 0.2);
          z-index: 1;
        }
        
        .hero-title {
          position: relative;
          z-index: 2;
          color: white;
          text-align: center;
          font-size: 2.8rem;
          font-weight: 700;
          text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
          margin: 0;
          padding: 0 20px;
          white-space: pre-line;
          line-height: 1.2;
          font-family: 'Ubuntu', sans-serif;
        }
        
        @media (max-width: 768px) {
          .hero-header {
            height: 140px;
          }
          
          .hero-title {
            font-size: 1.8rem;
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
        
        /* Estilos para labels */
        .control-label {
          font-weight: 600;
          color: #333;
          font-family: 'Ubuntu', sans-serif;
          margin-bottom: 8px;
        }
      ")
    )
  ),
  
  # Header de México Como Vamos (ocupa 100% del ancho)
  div(
    style = "width: 100%; margin: 0; padding: 0;",
    create_mcv_header()
  ),
  
  # Contenido principal con fondo gris
  div(
    class = "main-content",
    div(
      style = "max-width: 1400px; margin: 0 auto; padding: 0 20px;",
      
      # Hero header con gradiente
      div(
        class = "hero-header",
        h1("Análisis del Índice de\nProgreso Social", 
           class = "hero-title")
      ),
      
      # Contenedor de tabs con diseño moderno
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        tabPanel(
          title = div(icon("map"), "Mapa y Barras"),
          value = "mapa",
          
          br(),
          
          wellPanel(
            style = "margin-bottom: 20px;",
            
            h4("Configuración de la Visualización", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;"),
            
            fluidRow(
              column(4, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput("selIndicadorMapa",
                            label = div(icon("chart-line"), "Seleccione Indicador"),
                            choices = opciones_indicador)
                ),
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdIdioma", 
                             label = div(icon("language"), "Idioma"), 
                             choices = c("Inglés", "Español"), 
                             selected = "Español", 
                             inline = TRUE)
                )
              ), 
              column(4, 
                div(
                  style = "margin-bottom: 15px;",
                  uiOutput("selAnioMapa")
                ),
                div(
                  style = "margin-bottom: 15px;",
                  uiOutput("selEdosMarcar")
                )
              ), 
              column(4, 
                div(
                  style = "margin-bottom: 15px;",
                  h5("Descargar Visualización", 
                     style = "color: #333; font-weight: 600; margin-bottom: 10px;"),
                  fluidRow(
                    column(6, 
                      downloadButton("descargarGrafica", 
                                   label = "SVG",
                                   class = "btn-primary btn-sm",
                                   style = "width: 100%; margin-bottom: 5px;",
                                   icon = icon("download"))
                    ),
                    column(6, 
                      downloadButton("descargarGraficaPNG",
                                   label = "PNG",
                                   class = "btn-success btn-sm",
                                   style = "width: 100%; margin-bottom: 5px;",
                                   icon = icon("download"))
                    )
                  )
                )
              )
            )
          ),
          
          # Contenedor de gráfica
          div(
            class = "graph-container",
            plotOutput("mapa_barra", width = "100%", height = "85vh") %>% withSpinner(color = "#6551D0")
          )
        ),
        
        tabPanel(
          title = div(icon("table"), "Desempeño por Estado"),
          value = "tabla",
          
          br(),
          
          div(
            style = "margin-bottom: 20px;",
            h4("Ranking de Indicadores por Estado", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 15px; font-family: 'Ubuntu', sans-serif;"),
            p("Esta tabla permite ver, ordenados del peor al mejor, el lugar que ocupa cada entidad en cada uno de los indicadores que conforman el IPS.", 
              style = "color: #666; font-size: 14px; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;")
          ),
          
          wellPanel(
            h5("Configuración", 
               style = "color: #333; font-weight: 600; margin-bottom: 15px;"),
            fluidRow(
              column(6, 
                selectInput("selEstado2", 
                          label = div(icon("map-marker-alt"), "Seleccione Estado"),
                          choices = c(abb_control), 
                          selected = abb_control[1],
                          multiple = FALSE)
              ),
              column(6,
                div(
                  style = "padding-top: 25px;",
                  p("Seleccione un estado para ver su desempeño detallado en todos los indicadores del IPS.",
                    style = "color: #666; font-size: 13px; font-style: italic;")
                )
              )
            )
          ),
          
          div(
            class = "graph-container",
            DT::DTOutput("tabla_peores")
          )
        ),
        
        tabPanel(
          title = div(icon("chart-line"), "Series de Tiempo"),
          value = "series",
          
          br(),
          
          wellPanel(
            h4("Evolución Temporal de Indicadores", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;"),
            
            fluidRow(
              column(4, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput("selIndicador", 
                            label = div(icon("chart-bar"), "Seleccione Indicador"),
                            choices = opciones_indicador)
                )
              ), 
              column(4, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput("selEstado", 
                            label = div(icon("map-marker-alt"), "Seleccione Estado(s)"),
                            choices = c("Todos los estados", opciones_edos), 
                            multiple = TRUE, 
                            selected = "Todos los estados")
                )
              ), 
              column(4, 
                div(
                  style = "margin-bottom: 15px; padding-top: 25px;",
                  downloadButton("descargarGrafica2", 
                               label = "Descargar Gráfica",
                               class = "btn-primary",
                               style = "width: 100%;",
                               icon = icon("download"))
                )
              )
            )
          ),
          
          div(
            class = "graph-container",
            tabsetPanel(
              id = "chart_tabs",
              tabPanel(
                title = div(icon("mouse-pointer"), "Interactiva"), 
                value = "interactive",
                br(),
                plotly::plotlyOutput("grafica_lineas", height = "75vh") %>% withSpinner(color = "#6551D0")
              ), 
              tabPanel(
                title = div(icon("print"), "Para Imprimir"), 
                value = "static",
                br(),
                plotOutput("grafica_lineas2", height = "75vh") %>% withSpinner(color = "#6551D0")
              )
            )
          )
        ),
        
        tabPanel(
          title = div(icon("project-diagram"), "Correlación"),
          value = "correlacion",
          
          br(),
          
          wellPanel(
            h4("Análisis de Correlación entre Variables", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;"),
            
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons("rdTipo", 
                             label = div(icon("chart-scatter"), "Tipo de Correlación"),
                             choiceNames = list("Todos los estados", "Por grupo de estados por PIB per cápita"), 
                             choiceValues = c(1,2),
                             inline = TRUE)
                ),
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdFormatoCorr", 
                             label = div(icon("file-download"), "Formato de Descarga"), 
                             choices = c("svg", "png"), 
                             inline = TRUE)
                ),
                div(
                  style = "margin-bottom: 15px;",
                  downloadButton("downGraficaCorr", 
                               "Descargar Gráfica", 
                               class = "btn-success",
                               style = "width: 100%;",
                               icon = icon("download"))
                )
              )
            ),
            
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV1", 
                            label = div(icon("chart-bar"), "Variable 1 (Eje X)"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[1])
                ),
                uiOutput("sld1")
              ), 
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV2", 
                            label = div(icon("chart-bar"), "Variable 2 (Eje Y)"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[5])
                ),
                uiOutput("sld2")
              )
            )
          ),
          
          div(
            class = "graph-container",
            plotOutput("grafica_corr", height = "75vh") %>% withSpinner(color = "#6551D0")
          )
        ),
        
        tabPanel(
          title = div(icon("exchange-alt"), "Cambios Bivariados"),
          value = "cambios_bivariados",
          
          br(),
          
          wellPanel(
            h4("Análisis de Cambios Bivariados", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;"),
            
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV1_cambios", 
                            label = div(icon("chart-bar"), "Variable 1 (Eje X)"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[1])
                ),
                div(
                  style = "margin-bottom: 15px;",
                  sliderInput(inputId = "sldAnioInicio", 
                            label = div(icon("calendar"), "Año de Inicio"),
                            min = 2015, max = 2024, value = 2015)
                ),
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdTipoCambioV1", 
                             label = "Tipo de Cambio V1", 
                             choices = c("Absoluto", "Porcentaje"),
                             inline = TRUE)
                )
              ), 
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV2_cambios", 
                            label = div(icon("chart-bar"), "Variable 2 (Eje Y)"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[5])
                ),
                div(
                  style = "margin-bottom: 15px;",
                  sliderInput(inputId = "sldAnioFin",
                            label = div(icon("calendar"), "Año de Fin"),
                            min = 2015, max = 2024, value = 2024)
                ),
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdTipoCambioV2", 
                             label = "Tipo de Cambio V2", 
                             choices = c("Absoluto", "Porcentaje"),
                             inline = TRUE)
                )
              )
            ),
            
            fluidRow(
              column(6,
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdFormatoCambio", 
                             label = div(icon("file-download"), "Formato de Descarga"), 
                             choices = c("png", "svg"), 
                             inline = TRUE)
                )
              ),
              column(6,
                div(
                  style = "margin-bottom: 15px; padding-top: 25px;",
                  downloadButton(outputId = "btnDescargaGraficaCambio", 
                               label = "Descargar Gráfica", 
                               class = "btn-success",
                               style = "width: 100%;",
                               icon = icon("download"))
                )
              )
            )
          ),
          
          div(
            class = "graph-container",
            plotOutput("grafica_cambio", height = "75vh") %>% withSpinner(color = "#6551D0")
          )
        ),
        
        tabPanel(
          title = div(icon("chart-line"), "Regresión"),
          value = "regresion",
          
          br(),
          
          div(
            style = "margin-bottom: 20px;",
            h4("Modelos de Regresión del IPS", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 15px; font-family: 'Ubuntu', sans-serif;"),
            p("Se generan los modelos de regresiones del puntaje más reciente del IPS contra el valor más reciente del indicador seleccionado", 
              style = "color: #666; font-size: 14px; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;")
          ),
          
          wellPanel(
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV1_reg", 
                            label = div(icon("chart-bar"), "Variable para Regresión (Eje X)"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[1])
                )
              ), 
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  radioButtons(inputId = "rdFormatoRegresion", 
                             label = div(icon("file-download"), "Formato de Descarga"), 
                             choices = c("png", "svg"), 
                             inline = TRUE)
                ),
                div(
                  style = "margin-bottom: 15px; padding-top: 15px;",
                  downloadButton("downGraficaRegMod", 
                               "Descargar Gráfica", 
                               class = "btn-success",
                               style = "width: 100%;",
                               icon = icon("download"))
                )
              )
            )
          ),
          
          div(
            class = "graph-container",
            plotOutput("grafica_regresiones", height = "75vh") %>% withSpinner(color = "#6551D0")
          )
        ),
        
        tabPanel(
          title = div(icon("chart-column"), "Cambio Univariado"),
          value = "cambio_univariado",
          
          br(),
          
          div(
            style = "margin-bottom: 20px;",
            h4("Análisis de Cambio Univariado", 
               style = "color: #6551D0; font-weight: 600; margin-bottom: 15px; font-family: 'Ubuntu', sans-serif;"),
            p("Analiza el cambio de una variable específica entre dos períodos temporales", 
              style = "color: #666; font-size: 14px; margin-bottom: 20px; font-family: 'Ubuntu', sans-serif;")
          ),
          
          wellPanel(
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  selectInput(inputId = "selV0_cambios", 
                            label = div(icon("chart-bar"), "Variable para Analizar"),
                            choices = opciones_indicador, 
                            selected = opciones_indicador[1])
                )
              )
            ),
            
            fluidRow(
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  sliderInput(inputId = "sldAnioInicioSimple", 
                            label = div(icon("calendar"), "Año de Inicio"),
                            min = 2015, max = 2024, value = 2015)
                )
              ), 
              column(6, 
                div(
                  style = "margin-bottom: 15px;",
                  sliderInput(inputId = "sldAnioFinSimple",
                            label = div(icon("calendar"), "Año de Fin"),
                            min = 2015, max = 2024, value = 2024)
                )
              )
            )
          ),
          
          div(
            class = "graph-container",
            plotOutput("grafica_cambio_simple", height = "75vh") %>% withSpinner(color = "#6551D0")
          )
        )
      )
    )
  ),
  
  # Footer opcional
  div(
    style = "
      background-color: #333333;
      color: white;
      padding: 20px 0;
      margin-top: 40px;
      text-align: center;
      font-family: 'Ubuntu', sans-serif;
    ",
    div(
      style = "max-width: 1200px; margin: 0 auto; padding: 0 20px;",
      p(
        "© 2024 México ¿Cómo vamos? - Análisis del Índice de Progreso Social",
        style = "margin: 0; font-size: 14px; color: #ccc;"
      )
    )
  )
)

server <- function(input, output, session) {
  
  traer_datos <- reactive({get_datos(ind_sel = input$selIndicador)})
  traer_datos_mapa <- reactive({get_datos(ind_sel = input$selIndicadorMapa)})
  
  edos_marcar <- reactive({unique(traer_datos_mapa()$entidad_abr_m) %>% sort()})
  
  output$selEdosMarcar <- renderUI({
    selectInput("selEdosMarcar", 
                "Estados a marcar: ", 
                choices = c("Todos los estados", edos_marcar()), 
                selected = "Todos los estados", 
                multiple = T)
  })
  
  output$selAnioMapa <- renderUI({
    sliderTextInput(inputId = "selAnioMapa", 
                    label = div(icon("calendar"), "Seleccione Año"), 
                    choices = unique(traer_datos_mapa() %>% pull(anio) %>% sort()), 
                    selected = max(unique(traer_datos_mapa() %>% pull(anio) %>% sort())),
                    grid = T
    )
  })
  
  output$mapa_barra <- renderPlot({
    if(sum("Todos los estados" %in% input$selEdosMarcar) > 0){
      edos_a_marcar = edos_marcar()
    } else {
      edos_a_marcar = input$selEdosMarcar
    }
    
    gen_mapa_vars_ips(ind_sel = input$selIndicadorMapa,
                      anio_sel = input$selAnioMapa, 
                      datos = traer_datos_mapa(), 
                      edos_marcar = edos_a_marcar, 
                      idioma = input$rdIdioma)
  })
  
  output$descargarGrafica <- downloadHandler(
    filename = function(){
      str_c(input$selIndicadorMapa, "_",input$selAnioMapa,  ".svg")
    },
    content = function(file){
      
      if(sum("Todos los estados" %in% input$selEdosMarcar) > 0){
        edos_a_marcar = edos_marcar()
      } else {
        edos_a_marcar = input$selEdosMarcar
      }
      
      plot_print = gen_mapa_vars_ips(ind_sel = input$selIndicadorMapa,
                                     anio_sel = input$selAnioMapa, 
                                     datos = traer_datos_mapa(), 
                                     edos_marcar = edos_a_marcar, 
                                     idioma = input$rdIdioma) %>%
        ggimage::ggbackground("00_plantillas/00_IPS.pdf")
      ggsave(file, plot = plot_print, device = "svg", 
             width = 23, height = 12, dpi = 200)
    }
  )
  
  output$descargarGraficaPNG <- downloadHandler(
    filename = function(){
      str_c(input$selIndicadorMapa, "_",input$selAnioMapa,  ".png")
    },
    content = function(file){
      
      if(sum("Todos los estados" %in% input$selEdosMarcar) > 0){
        edos_a_marcar = edos_marcar()
      } else {
        edos_a_marcar = input$selEdosMarcar
      }
      
      plot_print = gen_mapa_vars_ips(ind_sel = input$selIndicadorMapa,
                                     anio_sel = input$selAnioMapa, 
                                     datos = traer_datos_mapa(), 
                                     edos_marcar = edos_a_marcar, 
                                     idioma = input$rdIdioma) 
      
      if(input$rdIdioma == "Español"){
        plot_print = plot_print %>% ggimage::ggbackground("00_plantillas/00_IPS.pdf")
      } else {
        plot_print = plot_print %>% ggimage::ggbackground("00_plantillas/PLANTILLA_ENG.pdf")
      }
      
      ggsave(file, plot = plot_print, device = "png", 
             width = 23, height = 12, dpi = 200)
    }
  )
  
  output$tabla_peores <- DT::renderDT({
    gen_tabla_peores(input$selEstado2) %>% 
      DT::datatable(extensions = 'FixedColumns',
                    escape = FALSE,
                    rownames= FALSE,
                    filter = list(position = 'top', clear = FALSE),
                    options = list(
                      search = list(regex = TRUE, caseInsensitive = TRUE),
                      pageLength = 10,language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      autoWidth = FALSE,
                      scrollX = TRUE
                    ))
  })
  
  output$grafica_lineas <- plotly::renderPlotly({
    tryCatch({
      req(input$selIndicador, input$selEstado)
      datos <- traer_datos()
      
      # Verificar que los datos no estén vacíos
      if(nrow(datos) == 0) {
        empty_plotly <- plotly::plot_ly() %>%
          plotly::add_annotations(
            text = "No hay datos disponibles para este indicador",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 16, color = "#6551D0")
          ) %>%
          plotly::layout(
            title = "Sin datos disponibles",
            showlegend = FALSE,
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = "")
          )
        return(empty_plotly)
      }
      
      gen_grafica(edo_sel = input$selEstado, datos_sel = datos)
    }, error = function(e) {
      # En caso de error, mostrar un plotly con mensaje de error
      error_plotly <- plotly::plot_ly() %>%
        plotly::add_annotations(
          text = paste("Error al generar la gráfica:", e$message),
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 14, color = "red")
        ) %>%
        plotly::layout(
          title = "Error en la visualización",
          showlegend = FALSE,
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = "")
        )
      return(error_plotly)
    })
  })
  
  output$grafica_lineas2 <- renderPlot({
    tryCatch({
      req(input$selIndicador, input$selEstado)
      datos <- traer_datos()
      
      # Verificar que los datos no estén vacíos
      if(nrow(datos) == 0) {
        empty_plot <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No hay datos disponibles para este indicador", 
                   size = 6, color = "#6551D0") +
          labs(title = "Sin datos disponibles") +
          theme_void() +
          theme(text = element_text(family = "Ubuntu"),
                plot.title = element_text(hjust = 0.5, color = mcv_discrete[1], 
                                          size = 18, face = "bold"))
        return(empty_plot)
      }
      
      gen_grafica_ggplot(edo_sel = input$selEstado, datos_sel = datos)
    }, error = function(e) {
      # En caso de error, mostrar un ggplot con mensaje de error
      error_plot <- ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error al generar la gráfica:", e$message), 
                 size = 5, color = "red") +
        labs(title = "Error en la visualización") +
        theme_void() +
        theme(text = element_text(family = "Ubuntu"),
              plot.title = element_text(hjust = 0.5, color = "red", 
                                        size = 16, face = "bold"))
      return(error_plot)
    })
  })
  
  output$descargarGrafica2 <- downloadHandler(
    filename = function(){
      str_c(input$selEstado, "_",input$selIndicador,".png")
    },
    content = function(file){
      plot_print = gen_grafica_ggplot(edo_sel = input$selEstado,
                                      datos_sel = traer_datos()) %>%
        ggimage::ggbackground("00_plantillas/00_IPS.pdf")
      ggsave(file, plot = plot_print, device = "png", 
             width = 23, height = 12, dpi = 200)
    }
  )
  
  # Funciones para la pestaña de Correlación
  output$sld1 <- renderUI({
    anios = get_anios_variables(input$selV1)
    sliderTextInput("sld1", "Año del indicador 1", choices = anios, selected = max(anios), grid = TRUE)
  })
  
  output$sld2 <- renderUI({
    anios = get_anios_variables(input$selV2)
    sliderTextInput("sld2", "Año del indicador 2", choices = anios, selected = max(anios), grid = TRUE)
  })
  
  grafica_corr_reactiva <- reactive({
    req(input$selV1, input$selV2, input$sld1, input$sld2, input$rdTipo)
    tryCatch({
      gen_grafica_corr(v1 = input$selV1,
                       v2 = input$selV2, 
                       anio_1 = input$sld1, 
                       anio_2 = input$sld2, 
                       tipo = input$rdTipo)
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error al generar la gráfica:", e$message), 
                 size = 5, color = "red") +
        labs(title = "Error en correlación") +
        theme_void()
    })
  })
  
  output$grafica_corr <- renderPlot({
    grafica_corr_reactiva()
  })
  
  output$downGraficaCorr <- downloadHandler(
    filename = function(){
      str_c("Grafica_corr_", 
            input$selV1, "_", input$selV2, "_", 
            input$sld1, "_", input$sld2, "_", 
            input$rdTipo, 
            ".", input$rdFormatoCorr)
    },
    content = function(file){
      tryCatch({
        plot_print = grafica_corr_reactiva()
        if(file.exists("00_plantillas/00_IPS.pdf")) {
          plot_print = plot_print %>% ggimage::ggbackground("00_plantillas/00_IPS.pdf")
        }
        
        ggsave(file, plot = plot_print, device = input$rdFormatoCorr, 
               width = 23, height = 12, dpi = 200)
      }, error = function(e) {
        # Crear un archivo de error si falla
        cat("Error al generar archivo:", e$message, file = file)
      })
    }
  )
  
  # Funciones para la pestaña de Cambios Bivariados
  grafica_cambio_reactiva <- reactive({
    req(input$selV1_cambios, input$selV2_cambios, input$sldAnioInicio, input$sldAnioFin)
    tryCatch({
      gen_grafica_cambio(v1 = input$selV1_cambios, 
                         v2 = input$selV2_cambios, 
                         anio_inicio = input$sldAnioInicio, 
                         anio_fin = input$sldAnioFin, 
                         c_por_1 = input$rdTipoCambioV1, 
                         c_por_2 = input$rdTipoCambioV2)
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error al generar la gráfica:", e$message), 
                 size = 5, color = "red") +
        labs(title = "Error en cambios bivariados") +
        theme_void()
    })
  })
  
  output$grafica_cambio <- renderPlot({
    grafica_cambio_reactiva()
  })
  
  output$btnDescargaGraficaCambio <- downloadHandler(
    filename = function(){
      str_c("Grafica_cambio_", 
            input$selV1_cambios, "_", input$selV2_cambios, "_", 
            input$sldAnioInicio, "_a_", input$sldAnioFin, 
            ".", input$rdFormatoCambio)
    },
    content = function(file){
      tryCatch({
        plot_print = grafica_cambio_reactiva()
        if(file.exists("00_plantillas/00_IPS.pdf")) {
          plot_print = plot_print %>% ggimage::ggbackground("00_plantillas/00_IPS.pdf")
        }
        
        ggsave(file, plot = plot_print, device = input$rdFormatoCambio, 
               width = 20, height = 10, dpi = 200)
      }, error = function(e) {
        cat("Error al generar archivo:", e$message, file = file)
      })
    }
  )
  
  # Funciones para la pestaña de Regresión
  grafica_regresiones_reactiva <- reactive({
    req(input$selV1_reg)
    tryCatch({
      gen_regresiones(v1 = input$selV1_reg)
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error al generar la gráfica:", e$message), 
                 size = 5, color = "red") +
        labs(title = "Error en regresiones") +
        theme_void()
    })
  })
  
  output$grafica_regresiones <- renderPlot({
    grafica_regresiones_reactiva()
  })
  
  output$downGraficaRegMod <- downloadHandler(
    filename = function(){
      str_c("modelo_regresion_IPS_vs_", 
            input$selV1_reg, ".",
            input$rdFormatoRegresion)
    }, 
    content = function(file){
      tryCatch({
        plot_print = grafica_regresiones_reactiva()
        if(file.exists("00_plantillas/00_IPS.pdf")) {
          plot_print = plot_print %>% ggimage::ggbackground("00_plantillas/00_IPS.pdf")
        }
        
        ggsave(file, plot = plot_print,
               device = input$rdFormatoRegresion,
               width = 23, height = 12, dpi = 200)
      }, error = function(e) {
        cat("Error al generar archivo:", e$message, file = file)
      })
    }
  )
  
  # Funciones para la pestaña de Cambio Univariado
  output$grafica_cambio_simple <- renderPlot({
    req(input$selV0_cambios, input$sldAnioInicioSimple, input$sldAnioFinSimple)
    tryCatch({
      gen_grafica_simple_cambio(ind_sel = input$selV0_cambios,
                                anio_1 = input$sldAnioInicioSimple,
                                anio_2 = input$sldAnioFinSimple)
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste("Error al generar la gráfica:", e$message), 
                 size = 5, color = "red") +
        labs(title = "Error en cambio univariado") +
        theme_void()
    })
  })
  
}

shinyApp(ui, server)
