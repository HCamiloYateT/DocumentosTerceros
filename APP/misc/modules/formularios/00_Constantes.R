# Constantes del módulo Formularios ----
.FOR_TIPOS_CAMPO <- c("Texto" = "TEXTO", "Texto largo" = "TEXTO_LARGO",
                       "Selección" = "SELECCION", "Casilla" = "CASILLA")
.FOR_COLUMNAS_EXCEL <- c("Orden", "CodigoCampo", "Etiqueta", "Tipo", "Obligatorio", "Opciones", "Ayuda")
.FOR_MAX_ARCHIVO <- 5 * 1024^2

.FOR_HABEAS_PREDETERMINADO <- list(
  codigo = "FORM_HABEAS",
  nombre = "Autorización para el tratamiento de datos personales",
  descripcion = paste(
    "Autorizo de manera previa, expresa e informada a Racafé & Cía. S.C.A. para recolectar, almacenar,",
    "usar, circular, actualizar y suprimir mis datos personales conforme a su política de tratamiento",
    "de información y a las finalidades indicadas a continuación."
  ),
  version = 1L,
  activo = TRUE,
  actualizado_en = as.POSIXct(Sys.time(), tz = "UTC"),
  actualizado_por = "SISTEMA",
  campos = tibble::tribble(
    ~Orden, ~CodigoCampo, ~Etiqueta, ~Tipo, ~Obligatorio, ~Opciones, ~Ayuda,
    1L, "autoriza_creacion",
    "Autoriza el tratamiento de la información suministrada para la creación o actualización del tercero.",
    "CASILLA", TRUE, "", "",
    2L, "autoriza_validacion",
    "Autoriza la consulta de fuentes públicas, listas de control y demás validaciones aplicables.",
    "CASILLA", TRUE, "", "",
    3L, "autoriza_comunicaciones",
    "Autoriza recibir comunicaciones relacionadas con la relación comercial.",
    "CASILLA", FALSE, "", "Esta autorización es opcional."
  )
)
