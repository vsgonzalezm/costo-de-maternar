@echo off
setlocal
cd /d "%~dp0"

echo.
echo Primero se comprueba que GitHub no tenga cambios nuevos.
git pull --ff-only
if errorlevel 1 (
  echo.
  echo No se puede continuar hasta resolver la actualizacion.
  pause
  exit /b 1
)

echo.
echo Archivos modificados:
git status --short
echo.

set /p MENSAJE=Describe brevemente los cambios: 
if "%MENSAJE%"=="" (
  echo No se creo ningun commit.
  pause
  exit /b 1
)

git add -A
echo.
echo Resumen de lo que se subira:
git diff --cached --stat
echo.

choice /C SN /M "Confirmas que quieres crear el commit y subirlo"
if errorlevel 2 (
  git restore --staged .
  echo Operacion cancelada. Tus archivos no fueron modificados.
  pause
  exit /b 0
)

git commit -m "%MENSAJE%"
if errorlevel 1 (
  echo No se creo el commit. Puede que no existan cambios.
  pause
  exit /b 1
)

git push
echo.
echo Cambios subidos a GitHub.
pause
