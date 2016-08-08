#!/usr/bin/env hbmk2

/* Copyright 2016 Viktor Szakats (vszakats.net/harbour) */

#include "hbver.ch"

PROCEDURE Main()

   LOCAL cURL := StrTran( hb_Version( HB_VERSION_URL_SOURCE ), "/commit/", "/archive/" ) + ".zip"
   LOCAL cDirBase := hb_FNameDir( hbshell_ScriptName() )

   IF hb_vfDirExists( cDirBase + hb_DirSepToOS( "../src/rtl" ) ) .AND. .F.
      OutStd( "! Error: This installation has the sources downloaded already." + hb_eol() )
      Inkey( 0 )
      ErrorLevel( 1 )
   ELSE
      OutStd( "! Download matching sources from URL:" + hb_eol() + "  " + cURL + hb_eol() )
   ENDIF

   RETURN
