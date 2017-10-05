/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Experience Value 3 */

#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow, nCol
   LOCAL selValue
   LOCAL nConsecutive_Cells := 3
   LOCAL nNumber_Range := 9

   LOCAL aArray
   LOCAL aSelected := {}

   WelcomeScreen()

   aArray := BuildArrayGame( aArray, nNumber_Range )

   DO WHILE lContinue

      Show( aArray, aSelected )

      IF ScanConsecutiveCells( aArray, nConsecutive_Cells )
         LOOP
      ENDIF

      SWITCH Inkey( 0, INKEY_ALL )
      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_SPACE
         aArray := BuildArrayGame( aArray, nNumber_Range )
         aSelected := {}
         LOOP

      CASE K_UP
         IF nNumber_Range < 9
            ++nNumber_Range
            aArray := BuildArrayGame( aArray, nNumber_Range )
            aSelected := {}
            LOOP
         ENDIF
         EXIT

      CASE K_DOWN
         IF nNumber_Range > 2
            --nNumber_Range
            aArray := BuildArrayGame( aArray, nNumber_Range )
            aSelected := {}
            LOOP
         ENDIF
         EXIT

      CASE K_LEFT
         IF nConsecutive_Cells > 2
            --nConsecutive_Cells
            aArray := BuildArrayGame( aArray, nNumber_Range )
            aSelected := {}
            LOOP
         ENDIF
         EXIT

      CASE K_RIGHT
         IF nConsecutive_Cells < MaxCol()
            ++nConsecutive_Cells
            aArray := BuildArrayGame( aArray, nNumber_Range )
            aSelected := {}
            LOOP
         ENDIF
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

         IF aArray[ nRow ][ nCol ] > 0
            SWITCH Len( aSelected )
            CASE 0
               AAdd( aSelected, { nRow, nCol } )
               EXIT
            CASE 1
               IF nRow == aSelected[ 1 ][ 1 ] .AND. ;
                  nCol == aSelected[ 1 ][ 2 ]
                  aSelected := {}
                  LOOP
               ENDIF
               IF Abs( nRow - aSelected[ 1 ][ 1 ] ) > 1 .OR. ;
                  Abs( nCol - aSelected[ 1 ][ 2 ] ) > 1
                  aSelected := {}
                  LOOP
               ENDIF
               selValue := aArray[ aSelected[ 1 ][ 1 ] ][ aSelected[ 1 ][ 2 ] ]
               aArray[ aSelected[ 1 ][ 1 ] ][ aSelected[ 1 ][ 2 ] ] := aArray[ nRow ][ nCol ]
               aArray[ nRow ][ nCol ] := selValue
               aSelected := {}
               EXIT

            ENDSWITCH
         ELSE
            aSelected := {}
         ENDIF

         EXIT

      CASE K_TAB
         ShowScoreboard( nNumber_Range, nConsecutive_Cells )
         EXIT

      ENDSWITCH

   ENDDO

   SetCursor( nOldCursor )

   RETURN

STATIC FUNCTION BuildArrayGame( aArray, nNumber_Range )

   LOCAL nRow
   LOCAL nCol

   FOR EACH nRow IN aArray := Array( MaxRow() + 1, MaxCol() + 1 )
      FOR EACH nCol IN nRow
         nCol := hb_RandomInt( 1, nNumber_Range )
      NEXT
   NEXT

   RETURN aArray

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

STATIC FUNCTION ScanConsecutiveCells( aArray, nConsecutive_Cells )

   LOCAL row, col
   LOCAL nPrev
   LOCAL numConsec
   LOCAL rowChanged := .F.
   LOCAL loopRowAgain

   row := Len( aArray )

   DO WHILE row > 0
      numConsec := 0
      nPrev := NIL
      loopRowAgain := .F.
      FOR EACH col IN aArray[ row ]
         IF col > 0 .AND. col == nPrev
            ++numConsec
         ELSE
            nPrev := col
            numConsec := 0
         ENDIF
         IF numConsec == nConsecutive_Cells - 1
            RemoveCells( aArray, row, col:__enumIndex - nConsecutive_Cells + 1, nConsecutive_Cells )
            rowChanged := .T.
            loopRowAgain := .T.
            EXIT
         ENDIF
      NEXT
      IF loopRowAgain
         LOOP
      ENDIF

      --row

   ENDDO

   RETURN rowChanged

STATIC PROCEDURE RemoveCells( aArray, nRow, nCol, nConsecutive_Cells )

   LOCAL row, col

   FOR row := nRow TO 1 STEP - 1
      FOR col := nCol TO nCol + nConsecutive_Cells - 1
         IF row > 1
            aArray[ row ][ col ] := aArray[ row - 1 ][ col ]
            aArray[ row - 1 ][ col ] := 0
         ELSE
            aArray[ row ][ col ] := 0
         ENDIF
      NEXT
   NEXT

   RETURN

STATIC PROCEDURE ShowScoreboard( nNumber_Range, nConsecutive_Cells )

   LOCAL nRow := Int( ( MaxRow() - 2 ) / 2 )

   CLS

   hb_DispOutAt( nRow, 0, hb_UPadC( "Experience Value 3 ", MaxCol() + 1 ), 0xf )
   hb_DispOutAt( ++nRow + 1, 0, hb_UPadR( "Range (up,down): 2.." + ;
      hb_ntos( nNumber_Range ) + ", Consecutive Cells (left,right): " + ;
      hb_ntos( nConsecutive_Cells ) + ". space: reset", MaxCol() + 1 ), 0xf0 )

   Inkey( 0 )

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
