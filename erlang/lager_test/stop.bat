@echo off

setlocal ENABLEEXTENSIONS ENABLEDELAYEDEXPANSION

"D:\Program Files\erl9.2\lib\erl_interface-3.10.1\bin\erl_call.exe" -a "main stop [game]" -c tianshu -n dev@127.0.0.1

endlocal
