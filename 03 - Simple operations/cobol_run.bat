@echo off
cobc -x main.cob
main.exe
del main.exe
pause