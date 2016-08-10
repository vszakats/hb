@echo off

:: 5.2 (default)
set CX=cl
set CO=/FPa
set PX=clipper
set PO=
set LX=rtlink fi
set LO=out hbtest52

if "%1"=="xpp" goto XPP
if not "%1"=="53" goto OK

set CO=/FPi
set PO=/DHB_COMPAT_C53
set LX=exospace fi
set LO=out hbtest53
goto OK

:XPP
set CX=rem
set PX=xpp
set LX=alink
set LO=/out:hbtestxpp

:OK
%CX% /c /AL /Zl /Oalt /Gs /W3 /G2 %CO% hbtestc.c
if not exist hbtestc.obj goto SC
set PO=%PO% /DRT_HAS_C
set LO=%LO% fi hbtestc
:SC

%PX% hbtest /n/w %PO%
%LX% hbtest %LO%
del *.obj
