/* Copyright 2017-present Viktor Szakats */

#require "hbyaml"

PROCEDURE Main( cFileName )

   LOCAL hMeta, xValue

   OutStd( hb_jsonEncode( xValue := hb_yaml_decode( ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "test.yml" ) ) ), @hMeta ), .T. ) )

   IF ! Empty( hMeta )
      OutErr( hb_jsonEncode( hMeta, .T. ) )
   ENDIF

   OutStd( hb_yaml_encode( xValue ) )

   RETURN
