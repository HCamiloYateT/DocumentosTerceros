# Módulo de administración de formularios ----
.formularios_opciones <- function() {
  catalogo <- ListarCatalogoFormularios()
  stats::setNames(names(catalogo), vapply(catalogo, `[[`, character(1), "nombre"))
}

.formularios_campos_vacios <- function() .FOR_HABEAS_PREDETERMINADO$campos[0, , drop = FALSE]

FormulariosUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(class = "documentos-cajas-row"),
    fluidRow(
      column(
        width = 5,
        racafePanel(
          "Definición del formulario",
          racafeForm(
            id = "formulario_catalogo", ns = ns, variant = "plain",
            racafeInputSelect("formulario", "Formulario guardado", choices = .formularios_opciones(),
                              selected = "FORM_HABEAS", empty = FALSE, ns = ns),
            racafeActionBar(
              left = racafeButton("cargar_formulario", "Cargar", icon = "folder-open",
                                  variant = "secondary", ns = ns),
              right = racafeButton("nuevo_formulario", "Nuevo", icon = "plus",
                                   variant = "secondary", ns = ns)
            ),
            racafeInputText("codigo", "Código", value = "FORM_HABEAS", required = TRUE, ns = ns),
            racafeInputText("nombre", "Nombre", required = TRUE, ns = ns),
            racafeInputTextArea("descripcion", "Texto introductorio", rows = 6, required = TRUE, ns = ns),
            racafeInputSwitch("activo", "Disponible para diligenciamiento", value = TRUE, ns = ns),
            racafeFormFooter(
              racafeButtonSave("guardar_formulario", "Guardar formulario", ns = ns)
            )
          )
        )
      ),
      column(
        width = 7,
        racafePanel(
          "Campos para diligenciar",
          racafeFormRow(
            columns = 3, responsive = "auto",
            racafeInputNumeric("campo_orden", "Orden", value = 1, min = 1, format = "integer", ns = ns),
            racafeInputText("campo_codigo", "Código del campo", required = TRUE, ns = ns),
            racafeInputSelect("campo_tipo", "Tipo", choices = .FOR_TIPOS_CAMPO,
                              selected = "CASILLA", empty = FALSE, ns = ns)
          ),
          racafeInputTextArea("campo_etiqueta", "Etiqueta o pregunta", rows = 2, required = TRUE, ns = ns),
          racafeFormRow(
            columns = 2, responsive = "auto",
            racafeInputText("campo_opciones", "Opciones", help = "Separadas por | para campos de selección.", ns = ns),
            racafeInputText("campo_ayuda", "Ayuda", ns = ns)
          ),
          racafeInputSwitch("campo_obligatorio", "Campo obligatorio", value = TRUE, ns = ns),
          racafeActionBar(
            left = racafeButton("guardar_campo", "Agregar o actualizar", icon = "plus",
                                variant = "secondary", ns = ns),
            right = racafeButtonDelete("eliminar_campo", "Eliminar campo", ns = ns)
          ),
          racafeInputFile("archivo_campos", "Cargar definición desde Excel", accept = ".xlsx",
                          max_size = .FOR_MAX_ARCHIVO, ns = ns),
          racafeActionBar(
            left = racafeButtonDownload("plantilla_campos", "Descargar plantilla", icon = "file-excel", ns = ns),
            right = racafeButton("cargar_campos", "Reemplazar campos", icon = "upload",
                                 variant = "secondary", ns = ns)
          ),
          TablaVancouverUI(ns("tabla_campos"), descargable = TRUE)
        )
      )
    )
  )
}

FormulariosServer <- function(id, usuario, recarga) {
  moduleServer(id, function(input, output, session) {
    campos <- reactiveVal(.formularios_campos_vacios())
    pendiente_guardar <- reactiveVal(NULL)

    cargar_formulario <- function(codigo) {
      formulario <- ObtenerFormularioCatalogo(codigo)
      updateTextInput(session, "codigo", value = formulario$codigo)
      updateTextInput(session, "nombre", value = formulario$nombre)
      updateTextAreaInput(session, "descripcion", value = formulario$descripcion)
      racafeUpdateSwitch(session, "activo", formulario$activo)
      campos(formulario$campos)
      invisible(formulario)
    }

    limpiar_campo <- function() {
      updateNumericInput(session, "campo_orden", value = nrow(campos()) + 1L)
      updateTextInput(session, "campo_codigo", value = "")
      updateTextAreaInput(session, "campo_etiqueta", value = "")
      racafeUpdateSelect(session, "campo_tipo", selected = "CASILLA")
      updateTextInput(session, "campo_opciones", value = "")
      updateTextInput(session, "campo_ayuda", value = "")
      racafeUpdateSwitch(session, "campo_obligatorio", TRUE)
    }

    observeEvent(input$cargar_formulario, {
      tryCatch({
        cargar_formulario(input$formulario)
      }, error = function(e) {
        racafeNotify(session, "No fue posible cargar el formulario", conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$nuevo_formulario, {
      updateTextInput(session, "codigo", value = "FORM_")
      updateTextInput(session, "nombre", value = "")
      updateTextAreaInput(session, "descripcion", value = "")
      racafeUpdateSwitch(session, "activo", TRUE)
      campos(.formularios_campos_vacios())
      limpiar_campo()
    })

    observeEvent(input$guardar_campo, {
      fila <- tibble::tibble(
        Orden = suppressWarnings(as.integer(input$campo_orden)),
        CodigoCampo = toupper(trimws(input$campo_codigo %||% "")),
        Etiqueta = trimws(input$campo_etiqueta %||% ""),
        Tipo = input$campo_tipo %||% "",
        Obligatorio = isTRUE(input$campo_obligatorio),
        Opciones = trimws(input$campo_opciones %||% ""),
        Ayuda = trimws(input$campo_ayuda %||% "")
      )
      tryCatch({
        actual <- campos()
        actual <- actual[actual$CodigoCampo != fila$CodigoCampo, , drop = FALSE]
        campos(.for_normalizar_campos(dplyr::bind_rows(actual, fila)))
        limpiar_campo()
      }, error = function(e) {
        racafeNotify(session, "Campo no válido", conditionMessage(e), type = "error")
      })
    })

    observeEvent(input$eliminar_campo, {
      codigo <- toupper(trimws(input$campo_codigo %||% ""))
      if (!nzchar(codigo)) {
        racafeNotify(session, "Indique el campo", "Escriba el código del campo que desea eliminar.", type = "warning")
        return(invisible(NULL))
      }
      actual <- campos()
      campos(actual[actual$CodigoCampo != codigo, , drop = FALSE])
      limpiar_campo()
    })

    observeEvent(input$cargar_campos, {
      tryCatch({
        campos(LeerCamposFormularioExcel(input$archivo_campos))
        racafeNotify(session, "Campos cargados", "La definición del Excel reemplazó la edición actual.",
                     type = "success")
      }, error = function(e) {
        racafeNotify(session, "No fue posible cargar los campos", conditionMessage(e), type = "error")
      })
    })

    output$plantilla_campos <- downloadHandler(
      filename = function() "plantilla_campos_formulario.xlsx",
      content = function(file) {
        openxlsx::write.xlsx(.FOR_HABEAS_PREDETERMINADO$campos, file, overwrite = TRUE)
      }
    )

    TablaVancouver(
      "tabla_campos", data = reactive(campos()), page_size = 8,
      nombre_archivo = "campos_formulario",
      col_labels = c(CodigoCampo = "Código", Obligatorio = "Obligatorio"),
      footer = reactive(paste(nrow(campos()), "campos configurados"))
    )

    observeEvent(input$guardar_formulario, {
      formulario <- list(
        codigo = input$codigo,
        nombre = input$nombre,
        descripcion = input$descripcion,
        activo = isTRUE(input$activo),
        campos = campos()
      )
      cond <- c(
        "El código es obligatorio" = !nzchar(trimws(formulario$codigo %||% "")),
        "El nombre es obligatorio" = !nzchar(trimws(formulario$nombre %||% "")),
        "La descripción es obligatoria" = !nzchar(trimws(formulario$descripcion %||% "")),
        "Agregue al menos un campo" = !nrow(formulario$campos)
      )
      if (any(cond)) {
        sapply(names(cond)[cond], function(x) racafeNotify(session, x, type = "error"))
        return(invisible(NULL))
      }
      pendiente_guardar(formulario)
      racafeConfirm(session, "confirmar_guardado", "Guardar formulario",
                    "La nueva versión quedará disponible para las solicitudes que lo requieran.",
                    confirm_label = "Guardar", ns = session$ns)
    })

    observeEvent(input$confirmar_guardado, {
      if (!isTRUE(input$confirmar_guardado) || is.null(pendiente_guardar())) return(invisible(NULL))
      tryCatch({
        formulario <- GuardarFormularioCatalogo(pendiente_guardar(), usuario())
        pendiente_guardar(NULL)
        opciones <- .formularios_opciones()
        racafeUpdateSelect(session, "formulario", choices = opciones, selected = formulario$codigo)
        cargar_formulario(formulario$codigo)
        recarga(recarga() + 1L)
        racafeNotify(
          session, "Formulario guardado",
          paste("Se registró la versión", formulario$version, "y se actualizó el portal."),
          type = "success"
        )
      }, error = function(e) {
        racafeNotify(session, "No fue posible guardar el formulario", conditionMessage(e), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(TRUE, cargar_formulario("FORM_HABEAS"), once = TRUE)
  })
}
