/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Experience Value */
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

STATIC aArray := {}

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow, nCol
   LOCAL nUp, nRight, nDown, nLeft

   WelcomeScreen()

   SetMode( 6, 20 )

   FOR nRow := 1 TO MaxRow() + 1
      AAdd( aArray, Array( MaxCol() + 1 ) )
      FOR nCol := 1 TO MaxCol() + 1 
         aArray[ nRow, nCol ] := { hb_RandomInt( 1, 9 ), NIL }
         SWITCH aArray[ nRow, nCol, 1 ]
            CASE 1 ; aArray[ nRow, nCol, 2 ] := 0x2 ; EXIT
            CASE 2 ; aArray[ nRow, nCol, 2 ] := 0x6 ; EXIT
            CASE 3 ; aArray[ nRow, nCol, 2 ] := 0x3 ; EXIT
            CASE 4 ; aArray[ nRow, nCol, 2 ] := 0x9 ; EXIT
            CASE 5 ; aArray[ nRow, nCol, 2 ] := 0xb ; EXIT
            CASE 6 ; aArray[ nRow, nCol, 2 ] := 0xc ; EXIT
            CASE 7 ; aArray[ nRow, nCol, 2 ] := 0xd ; EXIT
            CASE 8 ; aArray[ nRow, nCol, 2 ] := 0xe ; EXIT
            CASE 9 ; aArray[ nRow, nCol, 2 ] := 0xf ; EXIT
         ENDSWITCH
      NEXT
   NEXT

   DO WHILE lContinue

      DispBegin()

      FOR nRow := 1 TO MaxRow() + 1
         FOR nCol := 1 TO MaxCol() + 1 
            hb_DispOutAt( nRow - 1, nCol - 1, hb_ntos( aArray[ nRow, nCol, 1 ] ) , aArray[ nRow, nCol, 2 ] )
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

         IF nRow < 2 .OR. nRow > MaxRow()
            EXIT
         ENDIF
         IF nCol < 2 .OR. nCol > MaxCol()
            EXIT
         ENDIF

         nUp    := aArray[ nRow - 1, nCol ]
         nRight := aArray[ nRow, nCol + 1 ]
         nDown  := aArray[ nRow + 1, nCol ]
         nLeft  := aArray[ nRow, nCol - 1 ]

         aArray[ nRow - 1, nCol ] := nLeft
         aArray[ nRow, nCol + 1 ] := nUp
         aArray[ nRow + 1, nCol ] := nRight
         aArray[ nRow, nCol - 1 ] := nDown

         IF IsRowsEq( nRow - 1 ) .OR. IsRowsEq( nRow ) .OR. IsRowsEq( nRow + 1 )
         ENDIF

         IF IsCololumnEq( nCol - 1 ) .OR. IsCololumnEq( nCol ) .OR. IsCololumnEq( nCol + 1 )
         ENDIF

         EXIT

      ENDSWITCH

   ENDDO

   SetCursor( nOldCursor )

   RETURN

PROCEDURE WelcomeScreen()

   LOCAL nRow := Int( MaxRow() / 2 )
   LOCAL nCol := MaxCol() + 1
   LOCAL nColor := 0xb
   LOCAL nMSeconds := hb_MilliSeconds()

   hb_gtInfo( HB_GTI_WINTITLE, "Experience Value" )

   CLS
   hb_DispOutAt( nRow - 6, 0, hb_UPadC( "__________                          _____                        ", nCol ), nColor )
   hb_DispOutAt( nRow - 5, 0, hb_UPadC( "___  ____/___  ________________________(_)______________________ ", nCol ), nColor )
   hb_DispOutAt( nRow - 4, 0, hb_UPadC( "__  __/  __  |/_/__  __ \  _ \_  ___/_  /_  _ \_  __ \  ___/  _ \", nCol ), nColor )
   hb_DispOutAt( nRow - 3, 0, hb_UPadC( "_  /___  __>  < __  /_/ /  __/  /   _  / /  __/  / / / /__ /  __/", nCol ), nColor )
   hb_DispOutAt( nRow - 2, 0, hb_UPadC( "/_____/  /_/|_| _  .___/\___//_/    /_/  \___//_/ /_/\___/ \___/ ", nCol ), nColor )
   hb_DispOutAt( nRow - 1, 0, hb_UPadC( "                /_/                                              ", nCol ), nColor )
   hb_DispOutAt( nRow + 1, 0, hb_UPadC( "___    __      ______            ", nCol ), nColor )
   hb_DispOutAt( nRow + 2, 0, hb_UPadC( "__ |  / /_____ ___  /___  ______ ", nCol ), nColor )
   hb_DispOutAt( nRow + 3, 0, hb_UPadC( "__ | / /_  __ `/_  /_  / / /  _ \", nCol ), nColor )
   hb_DispOutAt( nRow + 4, 0, hb_UPadC( "__ |/ / / /_/ /_  / / /_/ //  __/", nCol ), nColor )
   hb_DispOutAt( nRow + 5, 0, hb_UPadC( "_____/  \__,_/ /_/  \__,_/ \___/ ", nCol ), nColor )
   hb_DispOutAt( nRow + 6, 0, hb_UPadC( "                                 v 0.0.0", nCol ), nColor )
   DO WHILE hb_MilliSeconds() - nMSeconds < 3000
   ENDDO 

   RETURN

FUNCTION IsCololumnEq( nColumn )

   LOCAL i
   LOCAL lReturn := .T.
   LOCAL nValue := aArray[ 1, nColumn, 1 ]

   FOR i := 2 TO MaxRow() + 1
      IF aArray[ i, nColumn, 1 ] != nValue
         lReturn := .F.
         EXIT
      ENDIF
   NEXT

   IF lReturn
      FOR i := 1 TO MaxRow() + 1
         ADel( aArray[ i ], nColumn )
         aArray[ i, MaxCol() + 1 ] := { 0, 0 }
      NEXT
   ENDIF

   RETURN lReturn

FUNCTION IsRowsEq( nRows )

   LOCAL i
   LOCAL lReturn := .T.
   LOCAL nValue := aArray[ nRows, 1, 1 ]

   FOR i := 2 TO MaxCol() + 1
      IF aArray[ nRows, i, 1 ] != nValue
         lReturn := .F.
         EXIT
      ENDIF
   NEXT

   IF lReturn
      FOR i := 1 TO MaxCol() + 1
         ADel( aArray[ nRows ], i )
         aArray[ MaxRow() + 1, i ] := { 0, 0 }
      NEXT
   ENDIF

   RETURN lReturn
