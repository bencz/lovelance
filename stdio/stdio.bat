
rem ====================================
rem generacio del fitxer objecte stdio.o
rem   -c indica que només compili
rem ====================================

gcc -c stdio.s

rem =========================================
rem generacio del fitxer objecte provastdio.o
rem   -c indica que només compili
rem =========================================

gcc -c provastdio.s


rem ==================================================================
rem muntatge de provastdio.o
rem    -o xxx especifica el nom que desitgem per al fitxer executable.
rem    -e xxx especifica l'etiqueta on es vol que comenci l'execució
rem ==================================================================

ld -o provastdio.exe -e _provastdio provastdio.o  stdio.o C:\GNAT\GAP_2005\lib\gcc\pentium-mingw32msv\3.4.5\*.a