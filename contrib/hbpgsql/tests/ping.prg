#require "hbpgsql"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL nVersion, cConnInfo

   LOCAL hostaddr := "127.0.0.1"
   LOCAL port := "5432"
   LOCAL connect_timeout := "10"

   CLS

   hb_default( @cHost, "localhost" )
   hb_default( @cDatabase, "postgres" )
   hb_default( @cUser, hb_UserName() )
   hb_default( @cPass, "" )

   ? "The function PQlibVersion() returns", ( nVersion := PQlibVersion() ); ?

   IF nVersion < 90100
      ? "Function PQping() not supported."
      QUIT
   ENDIF

   /* PQping() reports the status of the server.
      It accepts connection parameters identical to those of PQconnectdb(). It is not, however, necessary to
      supply correct user name, password, or database name values to obtain the server status. */
   HB_SYMBOL_UNUSED( cDatabase )
   HB_SYMBOL_UNUSED( cUser )
   HB_SYMBOL_UNUSED( cPass )

   /* the ConnInfo string can be empty to use ALL default parameters */
   cConnInfo := ""
   PingTest( cConnInfo )

   /* 'database' is not allowed parameter key, you can find the currently recognized parameter key words
      on https://www.postgresql.org/docs/9.1/static/libpq-connect.html */
   cConnInfo := "host = localhost database = test"
   PingTest( cConnInfo )

   /* the default port for Postgres is 5432, but we can try connect to port 3333 and see what is happen */
   cConnInfo := "host = " + cHost + " port = " + "3333"
   PingTest( cConnInfo )

   /* next attempt */
   cConnInfo := "host = " + cHost + " hostaddr = " + hostaddr + " port = " + port + " connect_timeout = " + connect_timeout
   PingTest( cConnInfo )

   RETURN

STATIC PROCEDURE PingTest( c )

   ? "cConnInfo is", '"' + c + '"'
   ? "PQPing( cConnInfo ) returns:", GetPingResult( PQPing( c ) ); ?

   RETURN

STATIC FUNCTION GetPingResult( n )

   LOCAL aMsg := { ;
      { "PQPING_OK"         , "Server is accepting connections" }, ;
      { "PQPING_REJECT"     , "Server is alive but rejecting connections" }, ;
      { "PQPING_NO_RESPONSE", "Could not establish connection" }, ;
      { "PQPING_NO_ATTEMPT" , "Connection not attempted (bad params)" } }

   IF n >= 0 .AND. n < Len( aMsg )
      RETURN aMsg[ n + 1 ][ 1 ] + " " + aMsg[ n + 1 ][ 2 ]
   ENDIF

   RETURN ""
