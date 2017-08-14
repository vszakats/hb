#require "hbmisc"

REQUEST FCOMMA

PROCEDURE Main()

   LOCAL cFileName := hb_FNameExtSet( __FILE__, ".csv" )

   hb_MemoWrit( cFileName, ;
      "hello,world,100" + hb_eol() + ;
      "foo,bar,-50" + hb_eol() )

   USE ( cFileName ) VIA "FCOMMA"

   dbGoTop()
   DO WHILE ! Eof()
      ? FieldGet( 1 )
      dbSkip()
   ENDDO

   dbCloseArea()

   hb_vfErase( cFileName )

   RETURN
