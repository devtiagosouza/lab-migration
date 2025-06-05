@echo off
setlocal

:: Caminho para o BRCC32 - ajuste se necessário
set BRCC32="C:\Program Files (x86)\Embarcadero\Studio\17.0\bin\brcc32.exe"

:: Caminho do RC
set RCFILE=Project1Resource.rc

:: Verifica se o arquivo RC existe
if not exist %RCFILE% (
    echo ERRO: %RCFILE% não encontrado.
    exit /b 1
)

echo Compilando %RCFILE%...
%BRCC32% %RCFILE%

if %ERRORLEVEL% NEQ 0 (
    echo Houve um erro ao compilar o RC.
    exit /b %ERRORLEVEL%
)

echo Sucesso: arquivo RES gerado!
exit /b 0
