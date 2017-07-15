/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )

   LOCAL nRow := Int( MaxRow() / 2 )
   LOCAL nCol := Int( MaxCol() / 2 )

   LOCAL cDirection := SubStr( "udlr", hb_randInt( 1, 4 ), 1 )
   LOCAL cLine := iif( cDirection $ "ud", "│", "─" )
   LOCAL nColorLine := 0xf

   LOCAL nTime := hb_MilliSeconds()

   DO WHILE .T.

      hb_DispOutAtBox( nRow, nCol, hb_UTF8ToStr( cLine ), nColorLine )

      IF Inkey( 0.05 ) != 0
         EXIT
      ENDIF

      SWITCH cDirection
      CASE "u"
         cLine := "│"
         nRow := iif( nRow < 0, MaxRow(), nRow - 1 )
         EXIT
      CASE "d"
         cLine := "│"
         nRow := iif( nRow > MaxRow(), -1, nRow + 1 )
         EXIT
      CASE "l"
         cLine := "─"
         nCol := iif( nCol < 0, MaxCol(), nCol - 1 )
         EXIT
      CASE "r"
         cLine := "─"
         nCol := iif( nCol > MaxCol(), -1, nCol + 1 )
         EXIT
      ENDSWITCH

      IF hb_MilliSeconds() - nTime > hb_randInt( 100, 1000 )
         SWITCH cDirection
         CASE "u"
            SWITCH cDirection := SubStr( "udlr", hb_randInt( 3, 4 ), 1 )
            CASE "l" ; cLine := "┐" ; EXIT
            CASE "r" ; cLine := "┌" ; EXIT
            ENDSWITCH
            EXIT
         CASE "d"
            SWITCH cDirection := SubStr( "udlr", hb_randInt( 3, 4 ), 1 )
            CASE "l" ; cLine := "┘" ; EXIT
            CASE "r" ; cLine := "└" ; EXIT
            ENDSWITCH
            EXIT
         CASE "l"
            SWITCH cDirection := SubStr( "udlr", hb_randInt( 1, 2 ), 1 )
            CASE "u" ; cLine := "└" ; EXIT
            CASE "d" ; cLine := "┌" ; EXIT
            ENDSWITCH
            EXIT
         CASE "r"
            SWITCH cDirection := SubStr( "udlr", hb_randInt( 1, 2 ), 1 )
            CASE "u" ; cLine := "┘" ; EXIT
            CASE "d" ; cLine := "┐" ; EXIT
            ENDSWITCH
            EXIT
         ENDSWITCH
         nTime := hb_MilliSeconds()
      ENDIF

      IF ( nRow < 0 .OR. nRow > MaxRow() .OR. nCol < 0 .OR. nCol > MaxCol() ) .AND. ;
         ++nColorLine > 0xf
         nColorLine := 0x1
      ENDIF
   ENDDO

   SetCursor( nOldCursor )

   RETURN
