@echo off
setlocal
cd /d "%~dp0"

echo.
echo Estado antes de actualizar:
git status --short --branch
echo.

git pull --ff-only
if errorlevel 1 (
  echo.
  echo No se pudo actualizar. No se modifico ningun commit local.
  echo Revisa si hay cambios locales sin guardar o pide ayuda.
  pause
  exit /b 1
)

echo.
echo Proyecto actualizado correctamente.
pause
