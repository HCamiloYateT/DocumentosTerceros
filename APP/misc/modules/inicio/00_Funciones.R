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

.inicio_resultado_compartir <- function(codigo_solicitud) {
  solicitudes <- ListarSolicitudes()
  solicitud <- solicitudes[match(codigo_solicitud, solicitudes$CodigoSolicitud), ]
  terceros <- ListarTerceros()
  tercero <- terceros[match(solicitud$CodigoTercero, terceros$CodigoTercero), ]
  requisitos <- ListarRequisitosSolicitudes(codigo_solicitud)
  vigencia_dias <- DOCUMENTOS_VIGENCIA_DIAS
  if (!is.na(solicitud$EnviadoEn) && !is.na(solicitud$VenceEn)) {
    vigencia_dias <- max(1L, as.integer(round(difftime(solicitud$VenceEn, solicitud$EnviadoEn, units = "days"))))
  }
  datos <- list(
    nombre = tercero$NombreRazonSocial, correo = tercero$CorreoContacto,
    documentos = requisitos$CodigoRequisito, vigencia_dias = vigencia_dias
  )
  enlace <- ConstruirEnlaceSolicitud(codigo_solicitud)
  vence <- if (is.na(solicitud$VenceEn)) {
    as.POSIXct(Sys.time(), tz = "UTC") + as.difftime(vigencia_dias, units = "days")
  } else {
    solicitud$VenceEn
  }
  vencimiento <- .sol_formatear_vencimiento(vence)
  list(
    codigo = codigo_solicitud, datos = datos, correo_destino = datos$correo, enlace = enlace,
    vencimiento = vencimiento, correo = .sol_render_correo(datos, enlace, vencimiento),
    qr = .sol_qr_data_uri(enlace), estado_correo = "PENDIENTE",
    detalle_correo = "El correo aún no se ha enviado."
  )
}

.inicio_acciones_compartir <- function(solicitud, ns) {
  if (solicitud$CanalPrincipal %in% c("CORREO", "QR")) return(NULL)
  resultado <- .inicio_resultado_compartir(solicitud$CodigoSolicitud)
  racafeDetailSection(
    "Compartir con el tercero",
    p("Esta solicitud todavía no se ha compartido por correo o código QR."),
    racafeInputReadOnly("inicio_enlace", "Enlace", value = resultado$enlace, ns = ns),
    racafeActionBar(
      left = tagList(
        racafeCopyButton("inicio_copiar_enlace", "Copiar enlace", text = resultado$enlace,
                         notify = FALSE, ns = ns),
        racafeCopyButton("inicio_copiar_qr", "Copiar QR", image = resultado$qr,
                         notify = FALSE, ns = ns)
      ),
      right = racafeButton("inicio_enviar_correo", "Enviar correo", icon = "paper-plane", ns = ns)
    )
  )
}

.inicio_cronologia_ui <- function(seleccion, ns) {
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
    ),
    .inicio_acciones_compartir(solicitud, ns)
  )
}
