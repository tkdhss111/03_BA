cd %~dp0
unlink makefile.win
unlink auto.exe
mklink makefile.win ..\..\tex\makefile_class
mklink auto.exe ..\..\tex\hss_auto_compiler\auto.exe
pause
