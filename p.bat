@echo off
set arg1=skr

tasm.exe %arg1%
tlink.exe %arg1%
tasm.exe %arg1%
tlink.exe %arg1%
skr e.txt






