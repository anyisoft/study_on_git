@echo off

setlocal enabledelayedexpansion

set ebins=
for /d %%i in (deps/*) do @set ebins=!ebins! -pa deps\%%~ni\ebin
set ebins=%ebins% -pa ebin
set args=-name dev@127.0.0.1 -setcookie tianshu -config etc/sys.config -s main start game

echo %ebins% %args%

if "%1"=="" (
    werl %ebins% %args%
) else (
    erl %ebins% %args% 
)
