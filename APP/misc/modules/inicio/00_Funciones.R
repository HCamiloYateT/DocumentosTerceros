# Funciones del módulo Inicio ----
.inicio_formatear_porcentaje <- function(valor) {
  tags$span(paste0(formatC(valor, format = "f", digits = 1, decimal.mark = ","), "%"))
}

.inicio_porcentaje <- function(condicion) {
  if (!length(condicion)) return(0)
  round(100 * sum(condicion, na.rm = TRUE) / length(condicion), 1)
}

.inicio_datos_tabla <- function(solicitudes) {
  solicitudes %>%
    select(CodigoSolicitud, CodigoTercero, CreadoPor, CreadoEn, CanalPrincipal, EnviadoEn,
           AbiertoEn, TotalAperturas, Estado, VenceEn, PorcentajeCompletitud)
}

.inicio_cronologia_ui <- function(seleccion) {
  solicitud <- seleccion$data
  cronologia <- ListarCronologiaSolicitud(solicitud$CodigoSolicitud) %>%
    mutate(Fecha = format(Fecha, "%d/%m/%Y %I:%M:%S %p", tz = "America/Bogota"))
  canal <- solicitud$CanalPrincipal
  if (is.na(canal) || !nzchar(canal)) canal <- "SIN ENVÍO"
  racafeStack(
    racafeInfoGrid(
      racafeInfoItem("Creada por", solicitud$CreadoPor),
      racafeInfoItem("Canal principal", canal),
      racafeInfoItem("Aperturas", solicitud$TotalAperturas),
      racafeInfoItem("Estado", solicitud$Estado)
    ),
    TablaVancouverDatos(
      cronologia,
      col_labels = c(Fecha = "Fecha (Colombia)", RegistradoPor = "Registrado por")
    )
  )
}
