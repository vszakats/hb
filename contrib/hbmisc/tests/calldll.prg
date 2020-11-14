/* Copyright 2010 Viktor Szakats (vsz.me/hb) */

#require "hbmisc"

PROCEDURE Main()

#if defined( __PLATFORM__WINDOWS )

   LOCAL nFlags := -1

   ? CallDll( "InternetGetConnectedState", "wininet.dll", @nFlags, 0 )
   ? nFlags

#endif

   RETURN
