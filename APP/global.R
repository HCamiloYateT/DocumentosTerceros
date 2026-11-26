# Configuraciones iniciales ----
verbose <- FALSE
debug <- FALSE
Sys.setenv(TZ = "America/Bogota", LANG = "es_CO.UTF-8", LANGUAGE = "es")

options(
  repos = c(CRAN = "https://cloud.r-project.org"), dplyr.summarise.inform = FALSE,
  lubridate.week.start = 1, encoding = "UTF-8", width = 120, scipen = 999,
  shiny.autoreload = FALSE, shiny.fullstacktrace = debug,
  shiny.error = if (debug) NULL else function() invisible(NULL), shiny.maxRequestSize = 25 * 1024^2
)

# Dependencias ----
.versiones_componentes <- c(racafeCore = "1.0.1", racafeForms = "0.8.0", racafeShiny = "1.5.0",
                             racafeModulos = "0.6.7")
.paquetes <- c("shiny", "bs4Dash", "dplyr", "tibble", "stringr", "reactable", "openxlsx", "digest", "htmltools",
               names(.versiones_componentes))
.faltantes <- .paquetes[!vapply(.paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(.faltantes)) stop("Instale las dependencias: ", paste(.faltantes, collapse = ", "), call. = FALSE)

for (.paquete in names(.versiones_componentes)) {
  if (utils::packageVersion(.paquete) < .versiones_componentes[[.paquete]]) {
    stop("Actualice ", .paquete, " a ", .versiones_componentes[[.paquete]], " o posterior.", call. = FALSE)
  }
}
rm(.paquete, .faltantes)
library("dplyr")

# Rutas y recursos ----
.resolver_ruta_app <- function(inicio = getwd()) {
  candidatas <- c(inicio, file.path(inicio, "APP"))
  validas <- vapply(candidatas, function(ruta) file.exists(file.path(ruta, "global.R")), logical(1))
  if (!any(validas)) stop("No se encontró APP. Inicie con shiny::runApp('APP').", call. = FALSE)
  normalizePath(candidatas[which(validas)[1L]], winslash = "/", mustWork = TRUE)
}

.ruta_app <- .resolver_ruta_app()
.url_compartido <- paste0("https://raw.githubusercontent.com/HCamiloYateT/Compartido/",
                          "6c29066361d10ae8330df96ede83586efdc864d6")
.logo_url <- paste0(.url_compartido, "/img/logo2.png")
.recursos_compartidos <- shiny::tagList(
  shiny::includeCSS(paste0(.url_compartido, "/Styles/style.css")),
  shiny::includeScript(paste0(.url_compartido, "/Scripts/scripts"))
)

# Fecha visible independiente del locale ----
.meses_abrev <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic")
format_fecha_es <- function(fecha = Sys.Date()) {
  fecha <- as.Date(fecha)
  sprintf("%02d %s %d", as.integer(format(fecha, "%d")), .meses_abrev[as.integer(format(fecha, "%m"))],
          as.integer(format(fecha, "%Y")))
}

# Impresiones ----
tit_app <- "Gestión de Terceros"
badge_estado <- "prototipo"

# Carga modular ----
for (.carpeta in c("config", "datos", "modules", "ui")) {
  .carga <- racafeCore::load_modules(path = file.path(.ruta_app, "misc", .carpeta), verbose = verbose)
  if (length(.carga$fallidos)) stop("No se pudieron cargar: ", paste(.carga$fallidos, collapse = ", "))
}
rm(.carpeta, .carga)
InicializarPersistenciaDocumentos()
