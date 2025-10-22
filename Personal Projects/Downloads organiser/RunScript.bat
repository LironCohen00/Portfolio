@echo off
REM =============================
REM  Run Downloads Organiser
REM =============================

cd /d "%~dp0"
echo Running Downloads Organiser...
java -jar "DownloadsOrganiser.jar"

echo.
echo Program finished.
pause
