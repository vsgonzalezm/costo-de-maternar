# Flujo de trabajo con GitHub

La carpeta de trabajo debe estar fuera de Google Drive. GitHub sincroniza el
codigo y los resultados versionados; Drive conserva los datos y documentos que
no forman parte del repositorio.

## Antes de trabajar

Ejecutar `ACTUALIZAR_DESDE_GITHUB.bat`. El archivo descarga los ultimos cambios
solo cuando Git puede avanzar sin mezclar versiones.

## Despues de trabajar

Ejecutar `SUBIR_CAMBIOS_GITHUB.bat`. El archivo:

1. comprueba primero si existen cambios nuevos en GitHub;
2. muestra los archivos modificados;
3. solicita una descripcion breve;
4. muestra el resumen exacto que se incorporara;
5. pide confirmacion antes de crear el commit y hacer push.

No deben copiarse carpetas `.git` entre computadores. En un computador nuevo se
clona nuevamente el repositorio y se trabaja sobre la unica rama `main`.

Los datos sensibles o pesados que solo viven en Drive no deben agregarse
manualmente a GitHub.
