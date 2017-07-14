/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )

   LOCAL nRow, nCol, c, nMod
   LOCAL lContinue := .T.

   DO WHILE lContinue

      FOR nMod := 0 TO 3
         DispBegin()
         FOR nRow := 0 TO MaxRow()
            FOR nCol := 0 TO MaxCol()
               DO CASE
               CASE nCol == 0        .AND. nRow == 0        ; c := "┌"
               CASE nCol == 0        .AND. nRow == MaxRow() ; c := "└"
               CASE nCol == 0                               ; c := "├"
               CASE nCol == MaxCol() .AND. nRow == 0        ; c := "┐"
               CASE nCol == MaxCol() .AND. nRow == MaxRow() ; c := "┘"
               CASE nCol == MaxCol()                        ; c := "┤"
               CASE                        nRow == 0        ; c := "┬"
               CASE                        nRow == MaxRow() ; c := "┴"
               OTHERWISE                                    ; c := "┼"
               ENDCASE
               hb_DispOutAtBox( nRow, nCol, hb_UTF8ToStr( c ), ;
                  Color( nRow + nMod, nCol ) )
            NEXT
         NEXT
         DispEnd()

         IF Inkey( 1 ) != 0
            lContinue := .F.
            EXIT
         ENDIF
      NEXT
   ENDDO

   SetCursor( nOldCursor )

   RETURN

STATIC FUNCTION Color( nRow, nCol )

   SWITCH Int( nRow % 4 * 10 + nCol % 4 )
   CASE  2
   CASE 11
   CASE 20
   CASE 33 ; RETURN 0xf
   CASE  0
   CASE 13
   CASE 22
   CASE 31 ; RETURN 0x2
   ENDSWITCH

   RETURN 0xb
