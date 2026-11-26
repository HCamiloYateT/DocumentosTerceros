# Módulo de solicitudes ----
SolicitudesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    racafeForms::racafePanel(
      "Nueva solicitud",
      racafeForms::racafeForm(
        id = "nueva_solicitud", ns = ns, variant = "plain",
        racafeForms::racafeFormRow(
          columns = 4, responsive = "auto",
          racafeForms::racafeInputSelect("tipo_persona", "Tipo de persona",
                                         choices = c("Jurídica" = "JURIDICA", "Natural" = "NATURAL"),
                                         selected = "JURIDICA", empty = FALSE, ns = ns),
          racafeForms::racafeInputSelect("tipo_identificacion", "Tipo de identificación",
                                         choices = c("NIT", "CC", "CE", "PASAPORTE"), selected = "NIT",
                                         empty = FALSE, ns = ns),
          racafeForms::racafeInputText("numero_identificacion", "Número de identificación", required = TRUE,
                                       ns = ns),
          racafeForms::racafeInputText("digito_verificacion", "DV", ns = ns)
        ),
        racafeForms::racafeFormRow(
          columns = 3, responsive = "auto",
          racafeForms::racafeInputText("nombre_razon_social", "Nombre o razón social", required = TRUE, ns = ns),
          racafeForms::racafeInputSelect("tipo_tercero", "Tipo de tercero",
                                         choices = c("Proveedor", "Cliente", "Contratista", "Otro"),
                                         selected = "Proveedor", empty = FALSE, ns = ns),
          racafeForms::racafeInputText("responsable", "Responsable", required = TRUE, ns = ns)
        ),
        racafeForms::racafeFormRow(
          columns = 4, responsive = "auto",
          racafeForms::racafeInputText("nombre_contacto", "Nombre del contacto", required = TRUE, ns = ns),
          racafeForms::racafeInputText("correo_contacto", "Correo del contacto", type = "email",
                                       required = TRUE, ns = ns),
          racafeForms::racafeInputText("telefono", "Teléfono", type = "tel", ns = ns),
          racafeForms::racafeInputText("pais", "País", value = "CO", ns = ns)
        ),
        racafeForms::racafeFormRow(
          columns = 3, responsive = "auto",
          racafeForms::racafeInputText("ciudad", "Ciudad", ns = ns),
          racafeForms::racafeInputSelect("tipo_proceso", "Proceso",
                                         choices = c("Vinculación" = "VINCULACION",
                                                     "Actualización" = "ACTUALIZACION"),
                                         selected = "VINCULACION", empty = FALSE, ns = ns),
          racafeForms::racafeInputSelect(
            "requisitos", "Requisitos",
            choices = c("RUT" = "DOC_RUT", "CERL" = "DOC_CERL", "Habeas Data" = "FORM_HABEAS"),
            selected = .REQUISITOS_BASE$CodigoRequisito, multiple = TRUE, empty = FALSE, ns = ns,
            gender = "m"
          )
        ),
        racafeForms::racafeInputTextArea("observaciones", "Observaciones", rows = 2, ns = ns),
        racafeForms::racafeFormFooter(
          racafeShiny::Boton(ns("crear"), "Crear solicitud", icono = "folder-plus")
        )
      )
    ),
    racafeForms::racafePanel(
      "Registrar envío",
      racafeForms::racafeForm(
        id = "registrar_envio_form", ns = ns, variant = "plain",
        racafeForms::racafeFormRow(
          columns = 2, responsive = "auto",
          racafeForms::racafeInputSelect("solicitud_envio", "Solicitud", choices = character(), ns = ns),
          racafeForms::racafeInputNumeric("vigencia", "Vigencia en días", value = DOCUMENTOS_VIGENCIA_DIAS,
                                          format = "integer", min = 1, max = 30, ns = ns)
        ),
        racafeForms::racafeFormFooter(
          racafeShiny::Boton(ns("registrar_envio"), "Registrar envío", icono = "paper-plane")
        )
      )
    ),
    racafeForms::racafePanel("Solicitudes", reactable::reactableOutput(ns("tabla")))
  )
}

SolicitudesServer <- function(id, usuario, recarga) {
  shiny::moduleServer(id, function(input, output, session) {
    local_trigger <- shiny::reactiveVal(0L)

    solicitudes <- shiny::reactive({
      recarga()
      local_trigger()
      ListarSolicitudes()
    })

    shiny::observe({
      datos <- solicitudes()
      choices <- stats::setNames(datos$CodigoSolicitud,
                                 paste(datos$CodigoSolicitud, datos$Estado, sep = " · "))
      racafeForms::racafeUpdateSelect(session, "solicitud_envio", choices = choices)
    })

    shiny::observeEvent(input$crear, {
      cond <- c(
        "El número de identificación es obligatorio" = !nzchar(trimws(input$numero_identificacion)),
        "El nombre o razón social es obligatorio" = !nzchar(trimws(input$nombre_razon_social)),
        "El nombre del contacto es obligatorio" = !nzchar(trimws(input$nombre_contacto)),
        "El correo del contacto es obligatorio" = !nzchar(trimws(input$correo_contacto)),
        "El responsable es obligatorio" = !nzchar(trimws(input$responsable)),
        "Seleccione al menos un requisito" = !length(input$requisitos)
      )
      if (any(cond)) {
        sapply(names(cond), function(x) if (cond[[x]]) shiny::showNotification(x, duration = 4, type = "error"))
        return(invisible(NULL))
      }

      tryCatch({
        tercero <- CrearTercero(
          list(TipoPersona = input$tipo_persona, TipoIdentificacion = input$tipo_identificacion,
               NumeroIdentificacion = input$numero_identificacion, DigitoVerificacion = input$digito_verificacion,
               NombreRazonSocial = input$nombre_razon_social, NombreContacto = input$nombre_contacto,
               CorreoContacto = input$correo_contacto, Telefono = input$telefono,
               TipoTercero = input$tipo_tercero, Pais = input$pais, Ciudad = input$ciudad,
               Observaciones = input$observaciones),
          usuario()
        )
        codigo <- CrearSolicitud(tercero, input$tipo_proceso, input$responsable, usuario(), input$requisitos)
        local_trigger(local_trigger() + 1L)
        recarga(recarga() + 1L)
        racafeForms::racafeNotify(session, "Solicitud creada", codigo, type = "success")
      }, error = function(e) {
        shiny::showNotification(paste("No fue posible crear la solicitud:", e$message), duration = 6,
                                type = "error")
      })
    })

    shiny::observeEvent(input$registrar_envio, {
      if (is.null(input$solicitud_envio) || !nzchar(input$solicitud_envio)) return(invisible(NULL))
      tryCatch({
        RegistrarEnvioSolicitud(input$solicitud_envio, usuario(), input$vigencia)
        local_trigger(local_trigger() + 1L)
        recarga(recarga() + 1L)
        racafeForms::racafeNotify(session, "Envío registrado", input$solicitud_envio, type = "success")
      }, error = function(e) {
        shiny::showNotification(paste("No fue posible registrar el envío:", e$message), duration = 6,
                                type = "error")
      })
    })

    output$tabla <- reactable::renderReactable({
      datos <- solicitudes() %>%
        dplyr::select(CodigoSolicitud, CodigoTercero, TipoProceso, Responsable, CreadoPor, CreadoEn, EnviadoEn,
                      VenceEn, Estado, RequisitosCompletos, TotalRequisitos, PorcentajeCompletitud)
      reactable::reactable(
        datos, searchable = TRUE, filterable = TRUE, defaultPageSize = 10, highlight = TRUE,
        columns = list(
          CreadoEn = reactable::colDef(name = "Creada", format = reactable::colFormat(datetime = TRUE)),
          EnviadoEn = reactable::colDef(name = "Enviada", format = reactable::colFormat(datetime = TRUE)),
          VenceEn = reactable::colDef(name = "Vence", format = reactable::colFormat(datetime = TRUE)),
          PorcentajeCompletitud = reactable::colDef(
            name = "Completitud", format = reactable::colFormat(digits = 1, suffix = "%")
          )
        ), language = reactable::reactableLang(searchPlaceholder = "Buscar", noData = "No hay solicitudes")
      )
    })
  })
}
