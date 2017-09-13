#require "hbmisc"

PROCEDURE Main()

   LOCAL dDate := 0d20080616, tDate := hb_SToT( "20080616041121531" )

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? sql_sprintf( "Phi = %A", ( 1 + 5 ^ 0.5 ) / 2 )  // Phi = 1.618034  Expected: 0X1,9E3779B97F4A8P+0
   ? sql_sprintf( "Phi = %2$0*3$.*1$f", 4, ( 1 + 5 ^ 0.5 ) / 2, 7 ) // Phi = 01.6180
   ? sql_sprintf( "%s", dDate )                      // 2008-06-16
   ? sql_sprintf( "%s", tDate )                      // 2008-06-16 04:11:21.531
   ? sql_sprintf( "%{YYYYMMDD}s", tDate )            // 20080616
   ? sql_sprintf( "%{ hh:mm pp}s", tDate )           // 04:11 AM
   ? sql_sprintf( "%{VERDADERO,FALSO}s", .F. )       // FALSO
   ? sql_sprintf( "%{ONLY IF TRUE}s", .T. )          // ONLY IF TRUE

   RETURN
