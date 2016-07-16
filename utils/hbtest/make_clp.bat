@echo off

:: Xbase++
if "%1" == "xpp" set CO=
if "%1" == "xpp" set PO=
if "%1" == "xpp" set LO=/out:hbtestxpp
if "%1" == "xpp" set CX=rem
if "%1" == "xpp" set PX=xpp
if "%1" == "xpp" set LX=alink

:: Cl*pper 5.3
if "%1" == "53" set CO=/FPi
if "%1" == "53" set PO=/D_COMPAT_C53
if "%1" == "53" set LO=out hbtest53
if "%1" == "53" set CX=cl
if "%1" == "53" set PX=clipper
if "%1" == "53" set LX=exospace fi

:: Cl*pper 5.2 (default)
if "%LX%" == "" set CO=/FPa
if "%LX%" == "" set PO=
if "%LX%" == "" set LO=out hbtest52
if "%LX%" == "" set CX=cl
if "%LX%" == "" set PX=clipper
if "%LX%" == "" set LX=rtlink fi

::
%CX% /c /AL /Zl /Oalt /Gs /W3 /G2 %CO% hbtestc.c
if exist hbtestc.obj set LO=%LO% fi hbtestc
if exist hbtestc.obj set PO=%PO% /DRT_HAS_C

%PX% hbtest /w /n %PO%
%LX% hbtest %LO%
del *.obj
