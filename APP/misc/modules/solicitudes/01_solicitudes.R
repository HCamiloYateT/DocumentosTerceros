# Módulo de solicitudes ----
SolicitudesUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    racafeForms::racafePanel(
      "Nueva solicitud",
      shiny::fluidRow(
        shiny::column(2, shiny::selectInput(ns("tipo_persona"), "Tipo de persona",
                                            choices = c("Jurídica" = "JURIDICA", "Natural" = "NATURAL"))),
        shiny::column(2, shiny::selectInput(ns("tipo_identificacion"), "Tipo de identificación",
                                            choices = c("NIT", "CC", "CE", "PASAPORTE"))),
        shiny::column(2, shiny::textInput(ns("numero_identificacion"), "Número de identificación")),
        shiny::column(1, shiny::textInput(ns("digito_verificacion"), "DV")),
        shiny::column(5, shiny::textInput(ns("nombre_razon_social"), "Nombre o razón social"))
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::textInput(ns("nombre_contacto"), "Nombre del contacto")),
        shiny::column(3, shiny::textInput(ns("correo_contacto"), "Correo del contacto")),
        shiny::column(2, shiny::textInput(ns("telefono"), "Teléfono")),
        shiny::column(2, shiny::selectInput(ns("tipo_tercero"), "Tipo de tercero",
                                            choices = c("Proveedor", "Cliente", "Contratista", "Otro"))),
        shiny::column(2, shiny::textInput(ns("pais"), "País", value = "CO"))
      ),
      shiny::fluidRow(
        shiny::column(3, shiny::textInput(ns("ciudad"), "Ciudad")),
        shiny::column(3, shiny::selectInput(ns("tipo_proceso"), "Proceso",
                                            choices = c("Vinculación" = "VINCULACION",
                                                        "Actualización" = "ACTUALIZACION"))),
        shiny::column(3, shiny::textInput(ns("responsable"), "Responsable")),
        shiny::column(3, shiny::checkboxGroupInput(
          ns("requisitos"), "Requisitos",
          choices = c("RUT" = "DOC_RUT", "CERL" = "DOC_CERL", "Habeas Data" = "FORM_HABEAS"),
          selected = .REQUISITOS_BASE$CodigoRequisito
        ))
      ),
      shiny::fluidRow(
        shiny::column(9, shiny::textAreaInput(ns("observaciones"), "Observaciones", width = "100%", rows = 2)),
        shiny::column(3, class = "documentos-actions",
                      racafeShiny::Boton(ns("crear"), "Crear solicitud", icono = "folder-plus"))
      )
    ),
    racafeForms::racafePanel(
      "Registrar envío",
      shiny::fluidRow(
        shiny::column(5, shiny::selectInput(ns("solicitud_envio"), "Solicitud", choices = character())),
        shiny::column(3, shiny::numericInput(ns("vigencia"), "Vigencia en días",
                                             value = DOCUMENTOS_VIGENCIA_DIAS, min = 1, max = 30)),
        shiny::column(4, class = "documentos-actions",
                      racafeShiny::Boton(ns("registrar_envio"), "Registrar envío", icono = "paper-plane"))
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
      shiny::updateSelectInput(session, "solicitud_envio", choices = choices)
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
          PorcentajeCompletitud = reactable::colDef(name = "Completitud", suffix = "%")
        ), language = reactable::reactableLang(searchPlaceholder = "Buscar", noData = "No hay solicitudes")
      )
    })
  })
}
