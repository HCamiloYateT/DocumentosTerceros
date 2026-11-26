# Documentos — Gestión integral de terceros

Plataforma para vinculación y actualización documental de terceros de Racafé.

La solución integrará dos desarrollos existentes:

- el procesamiento de RUT y certificados de existencia y representación legal del proyecto OCR;
- el flujo de solicitudes, OTP, autorización, firma electrónica y evidencia del proyecto
  Autorización Habeas Data.

## Estado

Se completó el diagnóstico documental de la fase 0. La persistencia inicial será local mediante RDS y un expediente
de archivos por solicitud; no se utilizará MySQL. No se han modificado servicios de OEAPP, Nginx, systemd ni los
repositorios fuente. Tampoco se ha publicado esta rama.

Documentación inicial:

- [Persistencia local por solicitud](docs/PERSISTENCIA_LOCAL.md)
- [Fase 1 — Aplicación administrativa Shiny](docs/FASE_1_SHINY.md)

## Referencias obligatorias

- `HCamiloYateT/Libreria-Racafe`: paquetes corporativos.
- `HCamiloYateT/racafeModulos`: módulos reutilizables.
- `HCamiloYateT/Compartido`: estilos, scripts e imágenes versionados.
- `HCamiloYateT/PlantillaBs4Dash`: línea base visual y técnica.
- `HCamiloYateT/CRM_Nacional`: estructura de `APP`, carga modular y organización de UI.
