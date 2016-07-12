@echo off

:: Copyright 1999-2016 Viktor Szakats (vszakats.net/harbour)

:: Xbase++
if "%1" == "xpp" set _CO=
if "%1" == "xpp" set _PO=
if "%1" == "xpp" set _LO=/out:hbtestxpp
if "%1" == "xpp" set _CX=rem
if "%1" == "xpp" set _PX=xpp
if "%1" == "xpp" set _LX=alink

:: Cl*pper 5.3
if  "%1" == "53" set _CO=/FPi
if  "%1" == "53" set _PO=/D_COMPAT_C53
if  "%1" == "53" set _LO=out hbtest53
if  "%1" == "53" set _CX=cl
if  "%1" == "53" set _PX=clipper
if  "%1" == "53" set _LX=exospace fi

:: Cl*pper 5.2 (default)
if "%_LX%" == "" set _CO=/FPa
if "%_LX%" == "" set _PO=
if "%_LX%" == "" set _LO=out hbtest52
if "%_LX%" == "" set _CX=cl
if "%_LX%" == "" set _PX=clipper
if "%_LX%" == "" set _LX=rtlink fi

::
%_CX% /c /AL /Zl /Oalt /Gs /W3 /G2 %_CO% rt_miscc.c
if exist rt_miscc.obj set _LO=%_LO% fi rt_miscc
if exist rt_miscc.obj set _PO=%_PO% /DRT_HAS_C

%_PX% hbtest /w /n %_PO%
%_LX% hbtest %_LO%
del *.obj
