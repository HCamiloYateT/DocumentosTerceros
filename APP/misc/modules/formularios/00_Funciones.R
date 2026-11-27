# Persistencia del catálogo ----
.for_ruta_rds <- function() file.path(.RUTAS_DOCUMENTOS$catalogos, "formularios.rds")
.for_ruta_json <- function() file.path(.RUTAS_DOCUMENTOS$catalogos, "formularios.json")

.for_catalogo_inicial <- function() {
  formularios <- list(.FOR_HABEAS_PREDETERMINADO)
  names(formularios) <- vapply(formularios, `[[`, character(1), "codigo")
  formularios
}

.for_formulario_json <- function(formulario) {
  list(
    codigo = formulario$codigo,
    nombre = formulario$nombre,
    descripcion = formulario$descripcion,
    version = as.integer(formulario$version),
    activo = isTRUE(formulario$activo),
    actualizado_en = format(formulario$actualizado_en, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    campos = lapply(seq_len(nrow(formulario$campos)), function(i) {
      campo <- formulario$campos[i, , drop = FALSE]
      list(
        orden = as.integer(campo$Orden),
        codigo = as.character(campo$CodigoCampo),
        etiqueta = as.character(campo$Etiqueta),
        tipo = as.character(campo$Tipo),
        obligatorio = isTRUE(campo$Obligatorio),
        opciones = as.character(campo$Opciones),
        ayuda = as.character(campo$Ayuda)
      )
    })
  )
}

.for_exportar_json <- function(catalogo) {
  contenido <- jsonlite::toJSON(
    list(formularios = unname(lapply(catalogo, .for_formulario_json))),
    auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE
  )
  ruta <- .for_ruta_json()
  temporal <- tempfile(pattern = ".formularios-", tmpdir = dirname(ruta), fileext = ".tmp")
  on.exit(if (file.exists(temporal)) unlink(temporal, force = TRUE), add = TRUE)
  writeLines(contenido, temporal, useBytes = TRUE)
  if (!file.rename(temporal, ruta)) stop("No fue posible actualizar el catálogo para el portal.", call. = FALSE)
  invisible(ruta)
}

InicializarCatalogoFormularios <- function() {
  dir.create(.RUTAS_DOCUMENTOS$catalogos, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(.for_ruta_rds())) {
    catalogo <- .for_catalogo_inicial()
    .EscribirRDSAtomico(catalogo, .for_ruta_rds())
    .for_exportar_json(catalogo)
  } else if (!file.exists(.for_ruta_json())) {
    .for_exportar_json(readRDS(.for_ruta_rds()))
  }
  invisible(.for_ruta_rds())
}

ListarCatalogoFormularios <- function() {
  InicializarCatalogoFormularios()
  catalogo <- readRDS(.for_ruta_rds())
  if (!is.list(catalogo)) stop("El catálogo de formularios no es válido.", call. = FALSE)
  catalogo
}

ListarRequisitosDisponibles <- function() {
  formularios <- ListarCatalogoFormularios()
  activos <- formularios[vapply(formularios, function(x) isTRUE(x$activo), logical(1))]
  catalogo_formularios <- if (length(activos)) {
    tibble::tibble(
      CodigoRequisito = vapply(activos, `[[`, character(1), "codigo"),
      TipoRequisito = "FORMULARIO",
      NombreRequisito = vapply(activos, `[[`, character(1), "nombre"),
      Obligatorio = TRUE,
      RequiereFirma = TRUE
    )
  } else {
    .REQUISITOS_BASE[0, , drop = FALSE]
  }
  dplyr::bind_rows(
    dplyr::filter(.REQUISITOS_BASE, TipoRequisito != "FORMULARIO"),
    catalogo_formularios
  )
}

ObtenerFormularioCatalogo <- function(codigo) {
  codigo <- .ValidarCodigoRuta(codigo, "código de formulario")
  formulario <- ListarCatalogoFormularios()[[codigo]]
  if (is.null(formulario)) stop("El formulario no existe.", call. = FALSE)
  formulario
}

.for_logico <- function(x) {
  toupper(trimws(as.character(x))) %in% c("TRUE", "VERDADERO", "SI", "SÍ", "1", "X")
}

.for_normalizar_campos <- function(campos) {
  faltantes <- setdiff(.FOR_COLUMNAS_EXCEL, names(campos))
  if (length(faltantes)) stop("Faltan columnas: ", paste(faltantes, collapse = ", "), call. = FALSE)
  campos <- tibble::as_tibble(campos[, .FOR_COLUMNAS_EXCEL, drop = FALSE])
  campos <- dplyr::mutate(
    campos,
    Orden = suppressWarnings(as.integer(Orden)),
    CodigoCampo = toupper(trimws(as.character(CodigoCampo))),
    Etiqueta = trimws(as.character(Etiqueta)),
    Tipo = toupper(trimws(as.character(Tipo))),
    Obligatorio = .for_logico(Obligatorio),
    Opciones = trimws(as.character(Opciones)),
    Ayuda = trimws(as.character(Ayuda))
  )
  cond <- c(
    "Cada campo debe tener un orden válido" = any(is.na(campos$Orden) | campos$Orden < 1L),
    "Cada campo debe tener un código válido" = any(!grepl("^[A-Z][A-Z0-9_]*$", campos$CodigoCampo)),
    "Los códigos de campo no pueden repetirse" = anyDuplicated(campos$CodigoCampo) > 0L,
    "Cada campo debe tener una etiqueta" = any(!nzchar(campos$Etiqueta)),
    "Hay tipos de campo no permitidos" = any(!campos$Tipo %in% unname(.FOR_TIPOS_CAMPO)),
    "Los campos de selección deben incluir opciones separadas por |" =
      any(campos$Tipo == "SELECCION" & !nzchar(campos$Opciones))
  )
  if (any(cond)) stop(paste(names(cond)[cond], collapse = ". "), call. = FALSE)
  dplyr::arrange(campos, Orden)
}

LeerCamposFormularioExcel <- function(archivo) {
  validacion <- racafeValidateInputFile(archivo, accept = ".xlsx", max_size = .FOR_MAX_ARCHIVO)
  if (!validacion$valid) stop(paste(validacion$errors, collapse = " "), call. = FALSE)
  datos <- openxlsx::read.xlsx(archivo$datapath, sheet = 1, check.names = FALSE)
  .for_normalizar_campos(datos)
}

GuardarFormularioCatalogo <- function(formulario, usuario) {
  codigo <- toupper(trimws(as.character(formulario$codigo)[1L]))
  if (!grepl("^FORM_[A-Z0-9_]+$", codigo)) {
    stop("El código debe iniciar por FORM_ y contener solo letras, números y guion bajo.", call. = FALSE)
  }
  nombre <- trimws(as.character(formulario$nombre)[1L])
  descripcion <- trimws(as.character(formulario$descripcion)[1L])
  if (!nzchar(nombre) || !nzchar(descripcion)) {
    stop("El nombre y la descripción son obligatorios.", call. = FALSE)
  }
  campos <- .for_normalizar_campos(formulario$campos)
  usuario <- trimws(as.character(usuario)[1L])

  .ConBloqueo("formularios", {
    catalogo <- ListarCatalogoFormularios()
    anterior <- catalogo[[codigo]]
    version <- if (is.null(anterior)) 1L else as.integer(anterior$version) + 1L
    catalogo[[codigo]] <- list(
      codigo = codigo,
      nombre = nombre,
      descripcion = descripcion,
      version = version,
      activo = isTRUE(formulario$activo),
      actualizado_en = as.POSIXct(Sys.time(), tz = "UTC"),
      actualizado_por = usuario,
      campos = campos
    )
    .EscribirRDSAtomico(catalogo, .for_ruta_rds())
    .for_exportar_json(catalogo)
    catalogo[[codigo]]
  })
}
