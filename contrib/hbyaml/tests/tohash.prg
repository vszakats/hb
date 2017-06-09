/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

#include "simpleio.ch"

PROCEDURE Main( cFileName )

   ? hb_jsonEncode( hb_yaml_decode( ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "../../../.travis.yml" ) ) ) ), .T. )

   RETURN
