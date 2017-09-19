/* Testing MIME type detection of a given file */

PROCEDURE Main( cFileName )

   IF ! HB_ISSTRING( cFileName )
      ? hb_StrFormat( "Usage: %1$s <file to test>", hb_ProgName() )
   ELSEIF hb_vfExists( cFileName )
      ? cFileName
      ?
      ? "hb_mimeStr()", hb_mimeStr( hb_MemoRead( cFileName ) )
      ? "hb_mimeFName()", hb_mimeFName( cFileName )
      ? "hb_mimeFile()", hb_mimeFile( cFileName )
   ELSE
      ? "File", cFileName, "cannot be found."
   ENDIF

   RETURN
