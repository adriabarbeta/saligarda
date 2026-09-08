@echo off
REM ===========================================================================
REM  Saligarda - assaig del post, SENSE ENVIAR MAI res
REM
REM  Sempre afegeix --prova, de manera que aquest fitxer no pot publicar a X
REM  encara que hi hagi credencials.
REM
REM  Us:
REM    prova.bat
REM    prova.bat --p=0.72 --u=14 --wc=-2 --nit=2026-01-15
REM ===========================================================================
set BASE=%~dp0
set RSCRIPT=C:\Program Files\R\R-4.3.3\bin\Rscript.exe

if not exist "%RSCRIPT%" (
  echo ERROR: no trobo Rscript a "%RSCRIPT%"
  echo Mira quina versio d'R tens instal.lada a C:\Program Files\R\ i corregeix
  echo la linia RSCRIPT d'aquest fitxer i la de pronostic.bat.
  exit /b 1
)

"%RSCRIPT%" "%BASE%19_bot_x.R" --prova %*
