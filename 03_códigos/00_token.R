#------------------------------------------------------------------------------#
#                     TOKEN PERSONAL DE LA API DEL INEGI                       #
#------------------------------------------------------------------------------#

# 0. Descripción ---------------------------------------------------------------

# Varios de los semáforos de México, ¿Cómo Vamos? se alimentan de información 
# proveniente del INEGI. Para automatizar el proceso de actualización de  los
# semáforos, se utiliza la API del INEGI. Para obtener información directo de
# la API, es necesario contar con un TOKEN personal. Es posible obtener    un 
# token personal directamente en la página de la API del INEGI (en la pestaña 
# de "Constructor de consultas"):
    
#       https://www.inegi.org.mx/servicios/api_indicadores.html

# Para utilizar los códigos de México, ¿Como Vamos?, es necesario que pongas 
# en este archivo el token personalizado que el INEGI haya mandado tu correo. 


# 1. Registro del TOKEN personal -----------------------------------------------

# Registra aquí tu token personal de la API del INEGI
v_token_inegi <- "682ad7f9-19fe-47f0-abec-e4c2ab2f2948"
dw_token <- "2G3RKs3H5y6dtkQC520BmkHHmkeCIggUqSyFZcQBuuQjXCR7GX8sIrwUk7NZHWoy"

# Token personal del SIE de Banxico (Sistema de Información Económica).
# Se usa en 01_03_07_componentes_conceptos_BMX.R para traer las series
# agregadas del INPC (subyacente/no subyacente + 9 conceptos), que INEGI retiró
# del API del BIE con la actualización 2024.
# Obtención: https://www.banxico.org.mx/SieAPIRest/service/v1/token
v_token_banxico <- "8f5bb376963606f4413db0ff2f6c68538d22249cbd94a5217de9206d71c877ee"

# FIN. -------------------------------------------------------------------------