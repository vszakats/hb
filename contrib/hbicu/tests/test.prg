/* Copyright 2015 Viktor Szakats (vsz.me/hb) */

#require "hbicu"

PROCEDURE Main()

   ? u_errorName( 0 /* U_ZERO_ERROR */ )
   ? hb_u_getVersion()

   RETURN
