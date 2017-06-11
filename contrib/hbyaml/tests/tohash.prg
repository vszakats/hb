/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

PROCEDURE Main( cFileName )

   LOCAL hMeta

   OutStd( hb_jsonEncode( hb_yaml_decode( ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "../../../.travis.yml" ) ) ), @hMeta ), .T. ) )

   IF ! Empty( hMeta )
      OutErr( hb_jsonEncode( hMeta, .T. ) )
   ENDIF

   RETURN
