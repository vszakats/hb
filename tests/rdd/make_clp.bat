@echo off

C=%1
if "%1" == "" set C=52

:: Cl*pper 5.3
if "%C%" == "53" set P1=/d_TEST_SCOPE_
if "%C%" == "53" set P2=/d_TEST_DESCEND_ /d_TEST_UNIQUE_
if "%C%" == "53" set LX=exospace lib _dbfcdx, dbfcdx fi

:: Cl*pper 5.2 (default)
if "%C%" == "53" set P1=
if "%C%" == "53" set P2=
if "%C%" == "52" set LX=rtlink lib dbfcdx fi

:: DBFCDX
clipper rddmktst /m/n/w/es2/d_TEST_CMPDIDX_ %P1% %P2%
%LX% rddmktst
del *.obj
rddmktst cdxcl%C%.prg dbfcdx
clipper cdxcl%C% /m/n/w/es2
%LX% cdxcl%C%

:: DBFNTX
clipper rddmktst /m/n/w/es2
%LX% rddmktst
del *.obj
rddmktst ntxcl%C%.prg dbfntx
clipper ntxcl%C% /m/n/w/es2
%LX% ntxcl%C%

:: DBFCDX for ADS RDD
clipper rddmktst /m/n/w/es2/d_TEST_ADS_ /d_TEST_CMPDIDX_ %P1%
%LX% rddmktst
del *.obj
rddmktst adscl%C%.prg dbfcdx
clipper adscl%C% /m/n/w/es2
%LX% adscl%C%
