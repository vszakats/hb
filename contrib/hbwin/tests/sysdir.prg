/* Copyright 2011 Viktor Szakats (vsz.me/hb) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   ? ">" + wapi_GetWindowsDirectory() + "<"
   ? ">" + wapi_GetSystemDirectory() + "<"

   RETURN
