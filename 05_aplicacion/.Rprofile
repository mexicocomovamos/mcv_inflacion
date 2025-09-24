# Configuración para shinyapps.io
# Este archivo se ejecuta al inicio de la sesión R

# Configuración de locale para español
if (Sys.getenv("SHINY_SERVER_VERSION") != "") {
  # Estamos en shinyapps.io
  tryCatch({
    Sys.setlocale("LC_TIME", "es_ES.UTF-8")
    Sys.setlocale("LC_CTYPE", "es_ES.UTF-8")
    Sys.setlocale("LC_COLLATE", "es_ES.UTF-8")
    Sys.setlocale("LC_MONETARY", "es_ES.UTF-8")
  }, error = function(e) {
    message("No se pudo configurar locale en español en shinyapps.io")
  })
}

# Configuración adicional
options(encoding = "UTF-8")
options(scipen = 999)