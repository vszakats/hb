/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

PROCEDURE Main( cFileName )

   LOCAL hMeta, xValue

   OutStd( hb_jsonEncode( xValue := hb_yaml_decode( ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "../../../.travis.yml" ) ) ), @hMeta ), .T. ) )

   IF ! Empty( hMeta )
      OutErr( hb_jsonEncode( hMeta, .T. ) )
   ENDIF

   OutStd( hb_yaml_encode( xValue ) )

   RETURN
