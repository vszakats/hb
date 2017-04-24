#!/usr/bin/env hbmk2

/* Copyright 2016 Viktor Szakats (vszakats.net/harbour) */

/* Two-way HBDOC <-> JSON converter */

#pragma -w3
#pragma -km+
#pragma -ko+

PROCEDURE Main( cFileName )

   IF hb_FileMatch( hb_FNameExt( cFileName ), ".json" )
      hb_MemoWrit( hb_FNameExtSet( cFileName, ".txt" ), __hbdoc_ToSource( hb_jsonDecode( MemoRead( cFileName ) ) ) )
   ELSE
      hb_MemoWrit( hb_FNameExtSet( cFileName, ".json" ), hb_jsonEncode( __hbdoc_FromSource( MemoRead( cFileName ) ), .T. ) )
   ENDIF

   RETURN
