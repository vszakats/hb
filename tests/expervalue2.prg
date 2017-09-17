/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Experience Value 2 */

#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow, nCol, row, col
   LOCAL aArray
   LOCAL i, tmp, nRowOld, nColOld

   WelcomeScreen()

   FOR EACH row IN aArray := Array( MaxRow() + 1, MaxCol() + 1 )
      FOR EACH col IN row
         col := { hb_RandomInt( 1, 9 ), hb_RandomInt( 0x1, 0xf ) }
      NEXT
   NEXT

   nRow := Int( MaxRow() / 2 ) + 1
   nCol := Int( MaxCol() / 2 ) + 1

   aArray[ nRow ][ nCol ] := { 0, 0x0 }

   DO WHILE lContinue

      Show( nRow, nCol, aArray )

      SWITCH Inkey( 0, INKEY_ALL )
      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_UP

         IF nRow == 1
            hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
            lContinue := .F.
         ELSE
            tmp     := aArray[ nRow - 1 ][ nCol ][ 1 ]
            nRowOld := nRow
            nRow    -= aArray[ nRow - 1 ][ nCol ][ 1 ]
            IF nRow > 0
               FOR i := 1 TO tmp
                  aArray[ nRow + i ][ nCol ] := { 0, 0x0 }
               NEXT
            ELSE
               tmp := nRowOld
               nRow := 1
               FOR i := 1 TO tmp
                  aArray[ nRow + i ][ nCol ] := { 0, 0x0 }
               NEXT
               Show( nRow, nCol, aArray )
               hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
               Inkey( 0 )
               lContinue := .F.
            ENDIF
         ENDIF
         EXIT

      CASE K_DOWN

         IF nRow == MaxRow() + 1
            hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
            lContinue := .F.
         ELSE

            tmp  := aArray[ nRow + 1 ][ nCol ][ 1 ]
            nRow += aArray[ nRow + 1 ][ nCol ][ 1 ]

            IF nRow < MaxRow() + 2
               FOR i := 1 TO tmp
                  aArray[ nRow - i ][ nCol ] := { 0, 0x0 }
               NEXT
            ELSE
               nRow := MaxRow() + 1
               FOR i := 1 TO tmp
                  aArray[ nRow - i ][ nCol ] := { 0, 0x0 }
               NEXT
               Show( nRow, nCol, aArray )
               hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
               Inkey( 0 )
               lContinue := .F.
            ENDIF
         ENDIF
         EXIT

      CASE K_LEFT
         IF nCol == 1
            hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
            lContinue := .F.
         ELSE
            tmp  := aArray[ nRow ][ nCol - 1 ][ 1 ]
            nColOld := nCol
            nCol -= aArray[ nRow ][ nCol - 1 ][ 1 ]
            IF nCol > 0
               FOR i := 1 TO tmp
                  aArray[ nRow ][ nCol + i ] := { 0, 0x0 }
               NEXT
            ELSE
               tmp := nColOld
               nCol := 1
               FOR i := 1 TO tmp
                  aArray[ nRow ][ nCol + i ] := { 0, 0x0 }
               NEXT
               Show( nRow, nCol, aArray )
               hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
               Inkey( 0 )
               lContinue := .F.
            ENDIF
         ENDIF
         EXIT

      CASE K_RIGHT

         IF nCol == MaxCol() + 1
            hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
            lContinue := .F.
         ELSE
            tmp  := aArray[ nRow ][ nCol + 1 ][ 1 ]
            nCol += aArray[ nRow ][ nCol + 1 ][ 1 ]
            IF nCol < MaxCol() + 2
               FOR i := 1 TO tmp
                  aArray[ nRow ][ nCol - i ] := { 0, 0x0 }
               NEXT
            ELSE
               nCol := MaxCol() + 1
               FOR i := 1 TO tmp
                  aArray[ nRow ][ nCol - i ] := { 0, 0x0 }
               NEXT
               Show( nRow, nCol, aArray )
               hb_Alert( "You crossed the game board.; The End Experience Value", , 0xf0, 3 )
               Inkey( 0 )
               lContinue := .F.
            ENDIF
         ENDIF
         EXIT
      CASE K_TAB
         Score( aArray )
         EXIT

      ENDSWITCH

   ENDDO

   SetCursor( nOldCursor )

   RETURN

PROCEDURE WelcomeScreen()

   LOCAL aBanner := { ;
      "__________                          _____                        ", ;
      "___  ____/___  ________________________(_)______________________ ", ;
      "__  __/  __  |/_/__  __ \  _ \_  ___/_  /_  _ \_  __ \  ___/  _ \", ;
      "_  /___  __>  < __  /_/ /  __/  /   _  / /  __/  / / / /__ /  __/", ;
      "/_____/  /_/|_| _  .___/\___//_/    /_/  \___//_/ /_/\___/ \___/ ", ;
      "                /_/                                              ", ;
      "___    __      ______            ", ;
      "__ |  / /_____ ___  /___  ______ ", ;
      "__ | / /_  __ `/_  /_  / / /  _ \", ;
      "__ |/ / / /_/ /_  / / /_/ //  __/", ;
      "_____/  \__,_/ /_/  \__,_/ \___/ ", ;
      "                                    v0.0.0" }

   LOCAL nRow := Int( ( MaxRow() - Len( aBanner ) ) / 2 )
   LOCAL cLine

   DispBegin()

   FOR EACH cLine IN aBanner
      hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, PadC( cLine, MaxCol() + 1 ), 0xb )
   NEXT

   DispEnd()

   Inkey( 3 )

   RETURN

PROCEDURE Show( nRow, nCol, aArray )

   LOCAL row, col

   DispBegin()

   FOR EACH row IN aArray
      FOR EACH col IN row
         hb_DispOutAt( row:__enumIndex() - 1, col:__enumIndex() - 1, hb_ntos( col[ 1 ] ), col[ 2 ] )
      NEXT
   NEXT
   hb_DispOutAtBox( nRow - 1, nCol - 1, Chr( 2 ), aArray[ nRow ][ nCol ][ 2 ] )

   DispEnd()

   RETURN

PROCEDURE Score( aArray )

   LOCAL nRow := Int( ( MaxRow() - 1 ) / 2 )
   LOCAL row, col
   LOCAL nScore := 0

   CLS

   FOR EACH row IN aArray
      FOR EACH col IN row
         IF col[ 1 ] == 0
            nScore++
         ENDIF
      NEXT
   NEXT

   hb_DispOutAt( nRow, 0, hb_UPadC( ;
      "Experience Value 2 " + ;
      "  Score : " + " [ " + hb_ntos( nScore ) + " / " + "2000 ]", MaxCol() + 1 ), 0xf )

   Inkey( 3 )

   RETURN
