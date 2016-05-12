@echo off
set d=%~dp0
%d%wrk2il.exe %* | %d%il2fit.exe
