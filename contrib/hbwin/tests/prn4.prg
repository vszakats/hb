/* Copyright 2009 Viktor Szakats (vsz.me/hb) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL a := win_printerGetDefault()

   ? ">" + a + "<"

   ? win_printerSetDefault( a )

   RETURN
