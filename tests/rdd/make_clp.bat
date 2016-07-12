@echo off

C=%1
if "%1" == "" set C=52

:: Cl*pper 5.3
if "%C%" == "53" set _P1=/d_TEST_SCOPE_
if "%C%" == "53" set _P2=/d_TEST_DESCEND_ /d_TEST_UNIQUE_
if "%C%" == "53" set _LX=exospace lib _dbfcdx, dbfcdx fi

:: Cl*pper 5.2 (default)
if "%C%" == "53" set _P1=
if "%C%" == "53" set _P2=
if "%C%" == "52" set _LX=rtlink lib dbfcdx fi

:: DBFCDX
clipper rddmktst /m/n/w/es2/d_TEST_CMPDIDX_ %_P1% %_P2%
%_LX% rddmktst
del *.obj
rddmktst cdxcl%C%.prg dbfcdx
clipper cdxcl%C% /m/n/w/es2
%_LX% cdxcl%C%

:: DBFNTX
clipper rddmktst /m/n/w/es2
%_LX% rddmktst
del *.obj
rddmktst ntxcl%C%.prg dbfntx
clipper ntxcl%C% /m/n/w/es2
%_LX% ntxcl%C%

:: DBFCDX for ADS RDD
clipper rddmktst /m/n/w/es2/d_TEST_ADS_ /d_TEST_CMPDIDX_ %_P1%
%_LX% rddmktst
del *.obj
rddmktst adscl%C%.prg dbfcdx
clipper adscl%C% /m/n/w/es2
%_LX% adscl%C%
