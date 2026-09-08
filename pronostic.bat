@echo off
REM ===========================================================================
REM  Saligarda - pronostic diari automatic
REM  El crida la tasca programada de Windows cada vespre a les 19:00.
REM  Deixa pronostic.txt i pronostic.html a la carpeta, apunta la prediccio a
REM  derived\pronostics.csv i registra l'execucio al log.
REM  Per executar-lo a ma: doble clic, o des del terminal: pronostic.bat
REM ===========================================================================
set BASE=%~dp0
set RSCRIPT=C:\Program Files\R\R-4.3.3\bin\Rscript.exe

if not exist "%RSCRIPT%" (
  echo [%date% %time%] ERROR: no trobo Rscript a "%RSCRIPT%" >> "%BASE%derived\pronostic_execucions.log"
  exit /b 1
)

echo. >> "%BASE%derived\pronostic_execucions.log"
echo ===== %date% %time% ===== >> "%BASE%derived\pronostic_execucions.log"
"%RSCRIPT%" "%BASE%16_pronostic.R" >> "%BASE%derived\pronostic_execucions.log" 2>&1
if errorlevel 1 (
  echo [ERROR] 16_pronostic.R ha fallat; no es publica res >> "%BASE%derived\pronostic_execucions.log"
  exit /b 1
)

REM  Publicacio a X. Si no hi ha credencials a %%USERPROFILE%%\.saligarda_x.json
REM  nomes redacta el post i ho diu al log, sense enviar res.
"%RSCRIPT%" "%BASE%19_bot_x.R" >> "%BASE%derived\pronostic_execucions.log" 2>&1
exit /b %errorlevel%
