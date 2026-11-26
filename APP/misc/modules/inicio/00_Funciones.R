# Funciones del módulo Inicio ----
.inicio_formatear_entero <- function(valor) {
  tags$span(formatC(valor, format = "f", digits = 0, big.mark = ".", decimal.mark = ","))
}

.inicio_datos_tabla <- function(solicitudes) {
  solicitudes %>%
    select(CodigoSolicitud, CodigoTercero, Responsable, Estado, CreadoEn, VenceEn,
           PorcentajeCompletitud)
}
