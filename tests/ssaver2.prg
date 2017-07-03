/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

PROCEDURE Main()

   LOCAL nRow, nCol, c

   DispBegin()
   FOR nRow := 0 TO MaxRow()
      FOR nCol := 0 TO MaxCol()
         DO CASE
         CASE nCol == 0        .AND. nRow == 0        ; c := hb_UTF8ToStr( "┌" )
         CASE nCol == 0        .AND. nRow == MaxRow() ; c := hb_UTF8ToStr( "└" )
         CASE nCol == 0                               ; c := hb_UTF8ToStr( "├" )
         CASE nCol == MaxCol() .AND. nRow == 0        ; c := hb_UTF8ToStr( "┐" )
         CASE nCol == MaxCol() .AND. nRow == MaxRow() ; c := hb_UTF8ToStr( "┘" )
         CASE nCol == MaxCol()                        ; c := hb_UTF8ToStr( "┤" )
         CASE                        nRow == 0        ; c := hb_UTF8ToStr( "┬" )
         CASE                        nRow == MaxRow() ; c := hb_UTF8ToStr( "┴" )
         OTHERWISE                                    ; c := hb_UTF8ToStr( "┼" )
         ENDCASE
         hb_DispOutAtBox( nRow, nCol, c, Color( nRow, nCol ) )
      NEXT
   NEXT
   DispEnd()

   RETURN

STATIC FUNCTION Color( nRow, nCol )

   DO CASE
   CASE nRow % 4 == 1
      DO CASE
      CASE nCol % 4 == 1 ; RETURN 0xf
      CASE nCol % 4 == 3 ; RETURN 0x2
      OTHERWISE          ; RETURN 0xb
      ENDCASE
   CASE nRow % 4 == 2
      DO CASE
      CASE nCol % 4 == 2 ; RETURN 0x2
      CASE nCol % 4 == 0 ; RETURN 0xf
      OTHERWISE          ; RETURN 0xb
      ENDCASE
   CASE nRow % 4 == 3
      DO CASE
      CASE nCol % 4 == 1 ; RETURN 0x2
      CASE nCol % 4 == 3 ; RETURN 0xf
      OTHERWISE          ; RETURN 0xb
      ENDCASE
   OTHERWISE
      DO CASE
      CASE nCol % 4 == 2 ; RETURN 0xf
      CASE nCol % 4 == 0 ; RETURN 0x2
      OTHERWISE          ; RETURN 0xb
      ENDCASE
   ENDCASE

   RETURN 0
