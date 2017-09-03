/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Experience Value */

#include "inkey.ch"
#include "setcurs.ch"

#pragma -w3
#pragma -km+
#pragma -ko+

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow, nCol, row, col
   LOCAL nUp, nRight, nDown, nLeft
   LOCAL aColors := { 0x2, 0x6, 0x3, 0x9, 0xb, 0xc, 0xd, 0xe, 0xf }
   LOCAL aMatrix
   LOCAL tmp

   WelcomeScreen()

   FOR EACH row IN aMatrix := Array( 7, 21 )
      FOR EACH col IN row
         tmp := hb_RandomInt( 1, 9 )
         col := { hb_ntos( tmp ), aColors[ tmp ] }
      NEXT
   NEXT

   DO WHILE lContinue

      DispBegin()

      FOR EACH row IN aMatrix
         FOR EACH col IN row
            hb_DispOutAt( row:__enumIndex() - 1, col:__enumIndex() - 1, col[ 1 ], col[ 2 ] )
         NEXT
      NEXT

      DispEnd()

      SWITCH Inkey( 0, INKEY_ALL )
      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_LBUTTONDOWN

         nRow := MRow() + 1
         nCol := MCol() + 1

         IF nRow < 2 .OR. nRow > Len( aMatrix ) - 1
            EXIT
         ENDIF
         IF nCol < 2 .OR. nCol > Len( aMatrix[ 1 ] ) - 1
            EXIT
         ENDIF

         nUp    := aMatrix[ nRow - 1 ][ nCol ]
         nRight := aMatrix[ nRow ][ nCol + 1 ]
         nDown  := aMatrix[ nRow + 1 ][ nCol ]
         nLeft  := aMatrix[ nRow ][ nCol - 1 ]

         aMatrix[ nRow - 1 ][ nCol ] := nLeft
         aMatrix[ nRow ][ nCol + 1 ] := nUp
         aMatrix[ nRow + 1 ][ nCol ] := nRight
         aMatrix[ nRow ][ nCol - 1 ] := nDown

         IsRowsEq( aMatrix, nRow - 1 )
         IsRowsEq( aMatrix, nRow )
         IsRowsEq( aMatrix, nRow + 1 )

         IsColumnEq( aMatrix, nCol - 1 )
         IsColumnEq( aMatrix, nCol )
         IsColumnEq( aMatrix, nCol + 1 )

         EXIT

      ENDSWITCH
   ENDDO

   SetCursor( nOldCursor )

   RETURN

STATIC PROCEDURE WelcomeScreen()

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
      "                                 v0.0.0" }

   LOCAL nRow := Int( ( MaxRow() - Len( aBanner ) ) / 2 )
   LOCAL cLine

   DispBegin()

   CLS
   FOR EACH cLine IN aBanner
      hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, PadC( cLine, MaxCol() + 1 ), 0xb )
   NEXT

   DispEnd()

   Inkey( 3 )

   RETURN

STATIC PROCEDURE IsColumnEq( aMatrix, nCol )

   LOCAL cValue := aMatrix[ 1 ][ nCol ][ 1 ]
   LOCAL row

   FOR EACH row IN aMatrix
      IF row[ nCol ][ 1 ] != cValue
         RETURN
      ENDIF
   NEXT

   FOR EACH row IN aMatrix
      ADel( row, nCol )
      row[ Len( row ) ] := { "0", 0 }
   NEXT

   RETURN

STATIC PROCEDURE IsRowsEq( aMatrix, nRow )

   LOCAL cValue := aMatrix[ nRow ][ 1 ][ 1 ]
   LOCAL col

   FOR EACH col IN aMatrix[ nRow ]
      IF col[ 1 ] != cValue
         RETURN
      ENDIF
   NEXT

   ADel( aMatrix, nRow )
   aMatrix[ Len( aMatrix ) ] := AFill( Array( Len( aMatrix[ 1 ] ) ), { "0", 0 } )

   RETURN
