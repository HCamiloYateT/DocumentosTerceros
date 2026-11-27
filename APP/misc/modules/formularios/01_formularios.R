# Módulo de administración de formularios PDF ----
.formularios_opciones <- function() {
  catalogo <- ListarCatalogoFormularios()
  stats::setNames(names(catalogo), vapply(catalogo, `[[`, character(1), "nombre"))
}

.formularios_contenido_ui <- function(ns) {
  tagList(
    tags$div(class = "documentos-cajas-row"),
    fluidRow(
      column(
        width = 5,
        racafePanel(
          "Plantilla PDF",
          racafeForm(
            id = "formulario_pdf", ns = ns, variant = "plain",
            racafeInputSelect("formulario", "Formulario", choices = .formularios_opciones(),
                              selected = "FORM_HABEAS", empty = FALSE, ns = ns),
            racafeActionBar(
              left = racafeButton("cargar_formulario", "Cargar", icon = "folder-open",
                                  variant = "secondary", ns = ns),
              right = racafeButton("nuevo_formulario", "Nuevo", icon = "plus",
                                   variant = "secondary", ns = ns)
            ),
            racafeFormRow(
              columns = 2, responsive = "auto",
              racafeInputText("codigo", "Código interno", value = "FORM_HABEAS", required = TRUE, ns = ns),
              racafeInputText("codigo_documento", "Código documental", value = "FT-GN-05",
                              required = TRUE, ns = ns)
            ),
            racafeInputText("nombre", "Nombre del formato", required = TRUE, ns = ns),
            racafeFormRow(
              columns = 2, responsive = "auto",
              racafeInputNumeric("version", "Versión", value = 7, min = 1, format = "integer", ns = ns),
              racafeInputText("fecha_formato", "Fecha del formato", value = "2025-01-28",
                              help = "Formato AAAA-MM-DD.", required = TRUE, ns = ns)
            ),
            racafeInputFile("archivo_pdf", "PDF del formato", accept = ".pdf",
                            max_size = .FOR_MAX_PDF, ns = ns),
            racafeInputSwitch("activo", "Disponible para diligenciamiento", value = TRUE, ns = ns),
            racafeActionBar(
              left = racafeButtonDownload("descargar_pdf", "Descargar PDF vigente",
                                          icon = "file-pdf", ns = ns),
              right = NULL
            ),
            racafeFormFooter(racafeButtonSave("guardar_formulario", "Guardar versión", ns = ns))
          )
        )
      ),
      column(
        width = 7,
        racafePanel(
          "Campos del PDF",
          p("El FT-GN-05 utiliza diez respuestas SI/NO y completa automáticamente nombre, identificación y fecha."),
          racafeInputFile("archivo_mapeo", "Modificar mapeo desde Excel", accept = ".xlsx",
                          max_size = 5 * 1024^2, ns = ns),
          racafeActionBar(
            left = racafeButtonDownload("plantilla_mapeo", "Descargar mapeo", icon = "file-excel", ns = ns),
            right = racafeButton("aplicar_mapeo", "Aplicar mapeo", icon = "upload",
                                 variant = "secondary", ns = ns)
          ),
          TablaVancouverUI(ns("tabla_campos"), descargable = TRUE)
        )
      )
    )
  )
}

FormulariosUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("contenido"))
}

FormulariosServer <- function(id, usuario, grupo, recarga) {
  moduleServer(id, function(input, output, session) {
    finalidades <- reactiveVal(.FOR_FINALIDADES_FT_GN_05)
    pendiente <- reactiveVal(NULL)
    autorizado <- reactive(any(grupo() %in% c("ANALÍTICA", "ANALITICA")))

    output$contenido <- renderUI({
      if (!autorizado()) {
        return(racafePanel(
          "Acceso restringido",
          p("La administración de formularios está disponible únicamente para el grupo ANALÍTICA.")
        ))
      }
      .formularios_contenido_ui(session$ns)
    })

    cargar_formulario <- function(codigo) {
      formulario <- ObtenerFormularioCatalogo(codigo)
      updateTextInput(session, "codigo", value = formulario$codigo)
      updateTextInput(session, "codigo_documento", value = formulario$codigo_documento)
      updateTextInput(session, "nombre", value = formulario$nombre)
      racafeUpdateNumeric(session, "version", value = formulario$version)
      updateTextInput(session, "fecha_formato", value = format(as.Date(formulario$fecha_formato), "%Y-%m-%d"))
      racafeUpdateSwitch(session, "activo", formulario$activo)
      finalidades(formulario$finalidades)
      invisible(formulario)
    }

    observeEvent(input$cargar_formulario, {
      tryCatch(cargar_formulario(input$formulario), error = function(e) {
        racafeNotify(session, "No fue posible cargar el formulario", conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$nuevo_formulario, {
      updateTextInput(session, "codigo", value = "FORM_")
      updateTextInput(session, "codigo_documento", value = "")
      updateTextInput(session, "nombre", value = "")
      racafeUpdateNumeric(session, "version", value = 1)
      updateTextInput(session, "fecha_formato", value = format(Sys.Date(), "%Y-%m-%d"))
      racafeUpdateSwitch(session, "activo", TRUE)
      finalidades(.FOR_FINALIDADES_FT_GN_05)
    })

    observeEvent(input$aplicar_mapeo, {
      tryCatch({
        finalidades(LeerMapeoFormularioExcel(input$archivo_mapeo))
        racafeNotify(session, "Mapeo aplicado", "Revise los campos antes de guardar la versión.", type = "success")
      }, error = function(e) {
        racafeNotify(session, "No fue posible aplicar el mapeo", conditionMessage(e), type = "error")
      })
    })

    output$plantilla_mapeo <- downloadHandler(
      filename = function() "mapeo_campos_FT-GN-05.xlsx",
      content = function(file) {
        datos <- dplyr::select(finalidades(), dplyr::all_of(.FOR_COLUMNAS_MAPEO))
        openxlsx::write.xlsx(datos, file, overwrite = TRUE)
      }
    )

    output$descargar_pdf <- downloadHandler(
      filename = function() {
        formulario <- ObtenerFormularioCatalogo(input$formulario %||% "FORM_HABEAS")
        paste0(formulario$codigo_documento, "_v", formulario$version, ".pdf")
      },
      content = function(file) {
        formulario <- ObtenerFormularioCatalogo(input$formulario %||% "FORM_HABEAS")
        origen <- .for_ruta_absoluta(formulario$archivo)
        if (!file.exists(origen) || !file.copy(origen, file, overwrite = TRUE)) {
          stop("El PDF vigente no está disponible.", call. = FALSE)
        }
      },
      contentType = "application/pdf"
    )

    TablaVancouver(
      "tabla_campos", data = reactive(finalidades()), page_size = 10,
      nombre_archivo = "mapeo_formulario_pdf",
      col_labels = c(Numero = "N.º", CodigoCampo = "Respuesta", CampoSi = "Campo SI", CampoNo = "Campo NO"),
      footer = reactive(paste(nrow(finalidades()), "finalidades configuradas"))
    )

    observeEvent(input$guardar_formulario, {
      formulario <- list(
        codigo = input$codigo %||% "", codigo_documento = input$codigo_documento %||% "",
        nombre = input$nombre %||% "", version = input$version,
        fecha_formato = input$fecha_formato %||% "", activo = isTRUE(input$activo),
        finalidades = finalidades()
      )
      pendiente(formulario)
      racafeConfirm(
        session, "confirmar_guardado", "Guardar plantilla PDF",
        "Se conservará la versión anterior y el portal utilizará la versión indicada.",
        confirm_label = "Guardar", ns = session$ns
      )
    })

    observeEvent(input$confirmar_guardado, {
      if (!autorizado() || !isTRUE(input$confirmar_guardado) || is.null(pendiente())) return(invisible(NULL))
      tryCatch({
        formulario <- GuardarFormularioCatalogo(pendiente(), input$archivo_pdf, usuario())
        pendiente(NULL)
        racafeUpdateSelect(session, "formulario", choices = .formularios_opciones(),
                           selected = formulario$codigo)
        cargar_formulario(formulario$codigo)
        recarga(recarga() + 1L)
        racafeNotify(
          session, "Plantilla publicada",
          paste(formulario$codigo_documento, "versión", formulario$version, "está disponible en el portal."),
          type = "success"
        )
      }, error = function(e) {
        racafeNotify(session, "No fue posible guardar la plantilla", conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(TRUE, cargar_formulario("FORM_HABEAS"), once = TRUE)
  })
}
