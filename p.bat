@echo off
set arg1=palindrome

tasm.exe %arg1%
tlink.exe %arg1%
tasm.exe %arg1%
tlink.exe %arg1%
palindrome e.txt






