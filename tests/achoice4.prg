/* Copyright 2011 Viktor Szakats (vsz.me/hb) */

#include "inkey.ch"

PROCEDURE Main()

   CLS

   hb_keyPut( { K_DOWN, K_UP } )
   AChoice( 0, 0, 0, 0, { "1", "2" } )

   RETURN
