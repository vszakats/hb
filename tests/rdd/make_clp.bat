@echo off

:: 5.2 (default)
set C=52
set P1=
set P2=
set LX=rtlink lib dbfcdx fi

:: 5.3
if not "%1"=="53" goto DO
set C=53
set P1=/d_TEST_SCOPE_
set P2=/d_TEST_DESCEND_ /d_TEST_UNIQUE_
set LX=exospace lib _dbfcdx, dbfcdx fi
:DO

:: DBFCDX
clipper rddmktst /m/n/w/es2/d_TEST_CMPDIDX_ %P1% %P2%
%LX% rddmktst
rddmktst cdxcl%C%.prg dbfcdx
clipper cdxcl%C% /m/n/w/es2
%LX% cdxcl%C%

:: DBFNTX
clipper rddmktst /m/n/w/es2
%LX% rddmktst
rddmktst ntxcl%C%.prg dbfntx
clipper ntxcl%C% /m/n/w/es2
%LX% ntxcl%C%

:: DBFCDX for ADS RDD
clipper rddmktst /m/n/w/es2/d_TEST_ADS_ /d_TEST_CMPDIDX_ %P1%
%LX% rddmktst
rddmktst adscl%C%.prg dbfcdx
clipper adscl%C% /m/n/w/es2
%LX% adscl%C%

del *.obj
