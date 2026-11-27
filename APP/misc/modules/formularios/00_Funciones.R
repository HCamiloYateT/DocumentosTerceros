# Persistencia del catálogo PDF ----
.for_ruta_rds <- function() file.path(.RUTAS_DOCUMENTOS$catalogos, "formularios.rds")
.for_ruta_json <- function() file.path(.RUTAS_DOCUMENTOS$catalogos, "formularios.json")
.for_ruta_historial <- function() file.path(.RUTAS_DOCUMENTOS$catalogos, "formularios_historial.rds")
.for_plantilla_inicial <- function() file.path(.ruta_app, "recursos", "formularios", "FT-GN-05_v7.pdf")

.for_ruta_absoluta <- function(ruta_relativa) {
  normalizePath(file.path(.RUTAS_DOCUMENTOS$raiz, ruta_relativa), winslash = "/", mustWork = FALSE)
}

.for_validar_pdf <- function(archivo) {
  validacion <- racafeValidateInputFile(archivo, accept = ".pdf", max_size = .FOR_MAX_PDF)
  if (!validacion$valid) stop(paste(validacion$errors, collapse = " "), call. = FALSE)
  cabecera <- readBin(archivo$datapath, what = "raw", n = 5L)
  if (!identical(rawToChar(cabecera), "%PDF-")) stop("El archivo no es un PDF válido.", call. = FALSE)
  invisible(archivo$datapath)
}

.for_copiar_pdf <- function(origen, destino) {
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  temporal <- tempfile(pattern = ".plantilla-", tmpdir = dirname(destino), fileext = ".pdf")
  on.exit(if (file.exists(temporal)) unlink(temporal, force = TRUE), add = TRUE)
  if (!file.copy(origen, temporal, overwrite = TRUE)) stop("No fue posible copiar el PDF.", call. = FALSE)
  if (file.exists(destino)) {
    respaldo <- file.path(dirname(destino), paste0("plantilla_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
    file.copy(destino, respaldo, overwrite = FALSE)
    unlink(destino, force = TRUE)
  }
  if (!file.rename(temporal, destino)) stop("No fue posible publicar el PDF.", call. = FALSE)
  invisible(destino)
}

.for_formulario_json <- function(formulario) {
  list(
    codigo = formulario$codigo, codigo_documento = formulario$codigo_documento,
    nombre = formulario$nombre, descripcion = formulario$descripcion, tipo = formulario$tipo,
    version = as.integer(formulario$version),
    fecha_formato = format(as.Date(formulario$fecha_formato), "%Y-%m-%d"),
    activo = isTRUE(formulario$activo), archivo = formulario$archivo,
    actualizado_en = format(formulario$actualizado_en, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    campos_texto = as.list(formulario$campos_texto),
    finalidades = lapply(seq_len(nrow(formulario$finalidades)), function(i) {
      fila <- formulario$finalidades[i, , drop = FALSE]
      list(
        numero = as.integer(fila$Numero), codigo = as.character(fila$CodigoCampo),
        etiqueta = as.character(fila$Etiqueta), campo_si = as.character(fila$CampoSi),
        campo_no = as.character(fila$CampoNo)
      )
    })
  )
}

.for_exportar_json <- function(catalogo) {
  contenido <- jsonlite::toJSON(
    list(formularios = unname(lapply(catalogo, .for_formulario_json))),
    auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE
  )
  temporal <- tempfile(pattern = ".formularios-", tmpdir = dirname(.for_ruta_json()), fileext = ".tmp")
  on.exit(if (file.exists(temporal)) unlink(temporal, force = TRUE), add = TRUE)
  writeLines(contenido, temporal, useBytes = TRUE)
  if (!file.rename(temporal, .for_ruta_json())) stop("No fue posible actualizar el catálogo.", call. = FALSE)
}

.for_catalogo_inicial <- function() {
  formulario <- .FOR_FT_GN_05_PREDETERMINADO
  destino <- .for_ruta_absoluta(formulario$archivo)
  if (!file.exists(destino) && file.exists(.for_plantilla_inicial())) {
    .for_copiar_pdf(.for_plantilla_inicial(), destino)
  }
  list(FORM_HABEAS = formulario)
}

InicializarCatalogoFormularios <- function() {
  dir.create(.RUTAS_DOCUMENTOS$catalogos, recursive = TRUE, showWarnings = FALSE)
  catalogo <- if (file.exists(.for_ruta_rds())) readRDS(.for_ruta_rds()) else list()
  migrar <- !length(catalogo) || is.null(catalogo$FORM_HABEAS$tipo) ||
    !identical(catalogo$FORM_HABEAS$tipo, "PDF_ACROFORM")
  if (migrar) {
    catalogo <- .for_catalogo_inicial()
    .EscribirRDSAtomico(catalogo, .for_ruta_rds())
  }
  formulario <- catalogo$FORM_HABEAS
  destino <- .for_ruta_absoluta(formulario$archivo)
  if (!file.exists(destino) && file.exists(.for_plantilla_inicial())) {
    .for_copiar_pdf(.for_plantilla_inicial(), destino)
  }
  .for_exportar_json(catalogo)
  if (!file.exists(.for_ruta_historial())) .EscribirRDSAtomico(tibble::tibble(), .for_ruta_historial())
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
  catalogo <- if (length(activos)) {
    tibble::tibble(
      CodigoRequisito = vapply(activos, `[[`, character(1), "codigo"), TipoRequisito = "FORMULARIO",
      NombreRequisito = vapply(activos, `[[`, character(1), "nombre"), Obligatorio = TRUE,
      RequiereFirma = TRUE
    )
  } else .REQUISITOS_BASE[0, , drop = FALSE]
  dplyr::bind_rows(dplyr::filter(.REQUISITOS_BASE, TipoRequisito != "FORMULARIO"), catalogo)
}

ObtenerFormularioCatalogo <- function(codigo) {
  codigo <- .ValidarCodigoRuta(codigo, "código de formulario")
  formulario <- ListarCatalogoFormularios()[[codigo]]
  if (is.null(formulario)) stop("El formulario no existe.", call. = FALSE)
  formulario
}

LeerMapeoFormularioExcel <- function(archivo) {
  if (is.null(archivo)) return(NULL)
  validacion <- racafeValidateInputFile(archivo, accept = ".xlsx", max_size = 5 * 1024^2)
  if (!validacion$valid) stop(paste(validacion$errors, collapse = " "), call. = FALSE)
  datos <- openxlsx::read.xlsx(archivo$datapath, sheet = 1, check.names = FALSE)
  faltantes <- setdiff(.FOR_COLUMNAS_MAPEO, names(datos))
  if (length(faltantes)) stop("Faltan columnas: ", paste(faltantes, collapse = ", "), call. = FALSE)
  datos <- tibble::as_tibble(datos[, .FOR_COLUMNAS_MAPEO, drop = FALSE])
  datos$Numero <- suppressWarnings(as.integer(datos$Numero))
  datos$CodigoCampo <- sprintf("FINALIDAD_%02d", datos$Numero)
  datos <- dplyr::select(datos, Numero, CodigoCampo, Etiqueta, CampoSi, CampoNo)
  if (any(is.na(datos$Numero)) || anyDuplicated(datos$Numero) || any(!nzchar(trimws(datos$Etiqueta))) ||
      any(!nzchar(trimws(datos$CampoSi))) || any(!nzchar(trimws(datos$CampoNo)))) {
    stop("El mapeo de campos no es válido.", call. = FALSE)
  }
  dplyr::arrange(datos, Numero)
}

GuardarFormularioCatalogo <- function(formulario, archivo_pdf, usuario) {
  codigo <- toupper(trimws(as.character(formulario$codigo)[1L]))
  if (!grepl("^FORM_[A-Z0-9_]+$", codigo)) stop("El código del formulario no es válido.", call. = FALSE)
  version <- suppressWarnings(as.integer(formulario$version))
  if (is.na(version) || version < 1L) stop("La versión debe ser un entero positivo.", call. = FALSE)
  catalogo <- ListarCatalogoFormularios()
  anterior <- catalogo[[codigo]]
  if (is.null(archivo_pdf) && is.null(anterior)) stop("Cargue el PDF del formulario.", call. = FALSE)
  ruta_relativa <- file.path("catalogos", "formularios", codigo, sprintf("v%03d", version), "plantilla.pdf")
  destino <- .for_ruta_absoluta(ruta_relativa)
  if (!is.null(archivo_pdf)) {
    .for_validar_pdf(archivo_pdf)
    .for_copiar_pdf(archivo_pdf$datapath, destino)
  } else if (!file.exists(destino) && !is.null(anterior) && file.exists(.for_ruta_absoluta(anterior$archivo))) {
    .for_copiar_pdf(.for_ruta_absoluta(anterior$archivo), destino)
  }
  finalidades <- formulario$finalidades %||% anterior$finalidades %||% .FOR_FINALIDADES_FT_GN_05
  nuevo <- list(
    codigo = codigo, codigo_documento = trimws(formulario$codigo_documento),
    nombre = trimws(formulario$nombre),
    descripcion = "Seleccione SI o NO para cada finalidad indicada en el formato oficial.",
    tipo = "PDF_ACROFORM", version = version,
    fecha_formato = as.Date(formulario$fecha_formato), activo = isTRUE(formulario$activo),
    archivo = ruta_relativa, actualizado_en = as.POSIXct(Sys.time(), tz = "UTC"),
    actualizado_por = trimws(usuario), campos_texto = c(nombre = "Texto1", identificacion = "Texto2",
                                                         fecha = "Texto3"),
    finalidades = finalidades
  )
  if (!nzchar(nuevo$codigo_documento) || !nzchar(nuevo$nombre) || is.na(nuevo$fecha_formato)) {
    stop("Código documental, nombre y fecha del formato son obligatorios.", call. = FALSE)
  }
  .ConBloqueo("formularios", {
    if (!is.null(anterior)) {
      historial <- readRDS(.for_ruta_historial())
      fila <- tibble::tibble(
        Codigo = anterior$codigo, CodigoDocumento = anterior$codigo_documento,
        Version = anterior$version, FechaFormato = as.Date(anterior$fecha_formato),
        ActualizadoEn = anterior$actualizado_en, ActualizadoPor = anterior$actualizado_por,
        Archivo = anterior$archivo
      )
      .EscribirRDSAtomico(dplyr::bind_rows(historial, fila), .for_ruta_historial())
    }
    catalogo[[codigo]] <- nuevo
    .EscribirRDSAtomico(catalogo, .for_ruta_rds())
    .for_exportar_json(catalogo)
  })
  nuevo
}
