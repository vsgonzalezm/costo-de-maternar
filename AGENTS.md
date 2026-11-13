# Acuerdo de trabajo del repositorio

Este repositorio usa una sola rama de trabajo: `main`. La copia de trabajo debe ser un clon local fuera de las carpetas sincronizadas por Google Drive. Un acceso directo dentro de Drive puede apuntar al clon, pero no debe contener otra copia de `.git`.

## Antes de comenzar cualquier tarea

1. Abrir la terminal en la raiz de este repositorio.
2. Ejecutar `git status --short --branch` y revisar cualquier cambio local.
3. Ejecutar `git fetch --prune` y luego `git pull --ff-only origin main`.
4. Si el pull falla o aparecen cambios locales inesperados, detenerse y revisar su origen antes de editar. No descartarlos ni sobrescribirlos.
5. Trabajar directamente en `main`. No crear ramas salvo instruccion explicita de Valentina.

## Mientras se trabaja

- Conservar la estructura y los nombres del proyecto; no duplicar carpetas para resolver conflictos.
- No modificar ni borrar trabajo de otras personas sin verificar primero el historial y el estado del repositorio.
- Para `Paper_wos`, ejecutar los scripts desde la raiz del repositorio y mantener rutas reproducibles con `here()`.
- Versionar codigo, documentacion y tablas o figuras finales necesarias para los proyectos. No versionar perfiles de navegador, caches, archivos temporales, credenciales ni datos sensibles o intermedios sin autorizacion explicita.

## Antes de terminar

1. Ejecutar `git status --short` y revisar `git diff`.
2. Confirmar que solo se incluyan archivos correspondientes a la tarea.
3. Crear un commit con un mensaje breve y especifico.
4. Ejecutar `git pull --rebase origin main` para incorporar cambios remotos que hayan aparecido mientras se trabajaba.
5. Resolver cualquier conflicto con cuidado, sin elegir automaticamente una version completa ni perder cambios ajenos.
6. Ejecutar `git push origin main`.
7. Verificar que `git status --short --branch` quede limpio y sincronizado con `origin/main`.

Los archivos por lotes de la raiz sirven como accesos simples al flujo: `ACTUALIZAR_DESDE_GITHUB.bat` al comenzar y `SUBIR_CAMBIOS_GITHUB.bat` al terminar. La revision del estado y de las diferencias sigue siendo obligatoria.
