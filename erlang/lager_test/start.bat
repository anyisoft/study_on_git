@echo off

setlocal enabledelayedexpansion

set ebins=
for /d %%i in (deps/*) do @set ebins=!ebins! -pa deps\%%~ni\ebin
set ebins=%ebins% -pa ebin
set args= -s lager_test test

echo %ebins% %args%

if "%1"=="" (
    werl %ebins% %args%
) else (
    erl %ebins% %args% 
)
