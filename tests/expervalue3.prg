/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Experience Value 3 */

#include "inkey.ch"
#include "setcurs.ch"

#define CONSECUTIVE_CELLS  3

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow, nCol
   LOCAL selValue

   LOCAL aArray
   LOCAL aSelected := {}

   WelcomeScreen()

   FOR EACH nRow IN aArray := Array( MaxRow() + 1, MaxCol() + 1 )
      FOR EACH nCol IN nRow
         nCol := hb_RandomInt( 0x1, 0x4 )
      NEXT
   NEXT

   DO WHILE lContinue

      Show( aArray, aSelected )

      IF ScanConsecutiveCells( aArray )
         LOOP
      ENDIF

      SWITCH Inkey( 0, INKEY_ALL )
      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_LBUTTONDOWN

         nRow := MRow() + 1
         nCol := MCol() + 1

         IF nRow < 0 .OR. nRow > MaxRow() + 1
            EXIT
         ENDIF
         IF nCol < 0 .OR. nCol > MaxCol() + 1
            EXIT
         ENDIF

         SWITCH Len( aSelected )
         CASE 0
            AAdd( aSelected, { nRow, nCol } )
            EXIT
         CASE 1
            IF nRow == aSelected[ 1 ][ 1 ] .AND. ;
               nCol == aSelected[ 1 ][ 2 ]
               LOOP
            ENDIF
            IF Abs( nRow - aSelected[ 1 ][ 1 ] ) > 1 .OR. ;
               Abs( nCol - aSelected[ 1 ][ 2 ] ) > 1
               LOOP
            ENDIF
            selValue := aArray[ aSelected[ 1 ][ 1 ] ][ aSelected[ 1 ][ 2 ] ]
            aArray[ aSelected[ 1 ][ 1 ] ][ aSelected[ 1 ][ 2 ] ] := aArray[ nRow ][ nCol ]
            aArray[ nRow ][ nCol ] := selValue
            aSelected := {}
            EXIT

         ENDSWITCH

         EXIT

      ENDSWITCH

   ENDDO

   SetCursor( nOldCursor )

   RETURN

STATIC PROCEDURE Show( aArray, aSelected )

   LOCAL nRow
   LOCAL nCol
   LOCAL itm
   LOCAL nColor

   DispBegin()

   FOR EACH nRow IN aArray
      FOR EACH nCol IN nRow
         nColor := nCol
         FOR EACH itm IN aSelected
            IF nRow:__enumIndex == itm[ 1 ] .AND. ;
               nCol:__enumIndex == itm[ 2 ]
               nColor := 0xb
            ENDIF
         NEXT
         hb_DispOutAt( nRow:__enumIndex() - 1, nCol:__enumIndex() - 1, ;
            iif( nCol == 0, " ", hb_ntos( nCol ) ), nColor )
      NEXT
   NEXT

   DispEnd()

   RETURN

STATIC FUNCTION ScanConsecutiveCells( aArray )

   LOCAL row, col
   LOCAL nPrev
   LOCAL numConsec
   LOCAL rowChanged := .F.
   LOCAL loopRowAgain

   FOR EACH row IN aArray DESCEND
      numConsec := 0
      nPrev := NIL
      loopRowAgain := .F.
      FOR EACH col IN row
         IF col > 0 .AND. col == nPrev
            ++numConsec
         ELSE
            nPrev := col
            numConsec := 0
         ENDIF
         IF numConsec == CONSECUTIVE_CELLS - 1
            RemoveCells( aArray, row:__enumIndex, col:__enumIndex - CONSECUTIVE_CELLS + 1 )
            rowChanged := .T.
            loopRowAgain := .T.
            EXIT
         ENDIF
      NEXT
      IF loopRowAgain
         LOOP
      ENDIF
    NEXT

   RETURN rowChanged

STATIC PROCEDURE RemoveCells( aArray, nRow, nCol )

   LOCAL row, col

   FOR row := nRow TO 2 STEP -1
      FOR col := nCol TO nCol + CONSECUTIVE_CELLS - 1
         aArray[ row ][ col ] := aArray[ row - 1 ][ col ]
         aArray[ row - 1 ][ col ] := 0
      NEXT
   NEXT

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
      hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, hb_UPadC( cLine, MaxCol() + 1 ), 0xb )
   NEXT

   DispEnd()

   Inkey( 3 )

   RETURN
