@echo off
REM This makefile compiles the ps-print lisp files (Windows 9X/NT/2000).

REM Adapted by Klaus Berndl

if exist ps-compile-script-init del ps-compile-script-init
if exist ps-print.elc del *.el
echo (add-to-list 'load-path nil) > ps-compile-script-init
echo (setq debug-on-error t) >> ps-compile-script-init
emacs -batch -no-site-file -l ps-compile-script-init -f batch-byte-compile *.el
del ps-compile-script-init

REM End of make.bat
