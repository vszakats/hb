/* Copyright 2010 Viktor Szakats (vsz.me/hb) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nRPCStatus

   ? win_UuidCreateString( @nRPCStatus )
   ? nRPCStatus

   RETURN
