/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Snake game */

#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

#define DIRECTION_UP     1
#define DIRECTION_DOWN   2
#define DIRECTION_RIGHT  3
#define DIRECTION_LEFT   4

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL oSnake := Snake():New()

   oSnake:InitFood()

   DO WHILE lContinue

      oSnake:Refresh()

      SWITCH Inkey( 1 )
      CASE K_ESC
         lContinue := .F.
         EXIT

      CASE K_UP
         oSnake:Up()
         EXIT

      CASE K_DOWN
         oSnake:Down()
         EXIT

      CASE K_LEFT
         oSnake:Left()
         EXIT

      CASE K_RIGHT
         oSnake:Right()
         EXIT

      OTHERWISE
         oSnake:Move()
      ENDSWITCH

      IF oSnake:nRow == oSnake:nRowFood .AND. ;
         oSnake:nCol == oSnake:nColFood

         oSnake:nLength++
         AAdd( oSnake:aTail, { 0, 0 } )
         oSnake:InitFood()
      ENDIF
   ENDDO

   oSnake:PrintResults()

   SetCursor( nOldCursor )

   RETURN

CREATE CLASS Snake

   DATA nStartGame INIT hb_MilliSeconds()

   DATA nRow       INIT Int( MaxRow() / 2 )
   DATA nCol       INIT Int( MaxCol() / 2 )

   DATA nRowFood
   DATA nColFood

   DATA nDirection INIT DIRECTION_RIGHT
   DATA nLength    INIT 1

   DATA aTail      INIT { { Int( MaxRow() / 2 ), Int( MaxCol() / 2 ) - 1 } }

   METHOD New()
   METHOD Up()
   METHOD Down()
   METHOD Left()
   METHOD Right()
   METHOD Refresh()
   METHOD Snake()
   METHOD Move()
   METHOD Food()
   METHOD InitFood()
   METHOD PrintResults()

ENDCLASS

METHOD New() CLASS Snake
   RETURN Self

METHOD Up() CLASS Snake

   IF ::nRow < 0
      ::nRow := MaxRow() + 1
   ENDIF

   IF ::nDirection == DIRECTION_DOWN
      ::nRow++
   ELSE
      ::nRow--
      ::nDirection := DIRECTION_UP
   ENDIF

   RETURN Self

METHOD Down() CLASS Snake

   IF ::nRow > MaxRow()
      ::nRow := -1
   ENDIF

   IF ::nDirection == DIRECTION_UP
      ::nRow--
   ELSE
      ::nRow++
      ::nDirection := DIRECTION_DOWN
   ENDIF

   RETURN Self

METHOD Left() CLASS Snake

   IF ::nCol < 0
      ::nCol := MaxCol() + 1
   ENDIF

   IF ::nDirection == DIRECTION_RIGHT
      ::nCol++
   ELSE
      ::nCol--
      ::nDirection := DIRECTION_LEFT
   ENDIF

   RETURN Self

METHOD Right() CLASS Snake

   IF ::nCol > MaxCol()
      ::nCol := -1
   ENDIF

   IF ::nDirection == DIRECTION_LEFT
      ::nCol--
   ELSE
      ::nCol++
      ::nDirection := DIRECTION_RIGHT
   ENDIF

   RETURN Self

METHOD Refresh() CLASS Snake

   DispBegin()

   Scroll()

   ::Snake()
   ::Food()

   DispEnd()

   RETURN Self

METHOD Snake() CLASS Snake

   LOCAL tmp

   SWITCH ::nDirection
   CASE DIRECTION_UP    ; tmp := { ::nRow + 1, ::nCol } ; EXIT
   CASE DIRECTION_DOWN  ; tmp := { ::nRow - 1, ::nCol } ; EXIT
   CASE DIRECTION_LEFT  ; tmp := { ::nRow, ::nCol + 1 } ; EXIT
   CASE DIRECTION_RIGHT ; tmp := { ::nRow, ::nCol - 1 } ; EXIT
   ENDSWITCH

   hb_AIns( ::aTail, 1, tmp )

   FOR EACH tmp IN ::aTail
      hb_DispOutAt( tmp[ 1 ], tmp[ 2 ], "*", "R/N" )
   NEXT

   hb_DispOutAt( ::nRow, ::nCol, Chr( 2 ) /* LOW-ASCII "☺" */, "R/N" )

   RETURN Self

METHOD Move() CLASS Snake

   SWITCH ::nDirection
   CASE DIRECTION_UP    ; ::Up()    ; EXIT
   CASE DIRECTION_DOWN  ; ::Down()  ; EXIT
   CASE DIRECTION_LEFT  ; ::Left()  ; EXIT
   CASE DIRECTION_RIGHT ; ::Right() ; EXIT
   ENDSWITCH

   RETURN Self

METHOD Food() CLASS Snake

   hb_DispOutAt( ::nRowFood, ::nColFood, "*", "GR+/N" )

   RETURN Self

METHOD InitFood() CLASS Snake

   ::nRowFood := hb_randInt( 1, MaxRow() - 1 )
   ::nColFood := hb_randInt( 1, MaxCol() - 1 )

   RETURN Self

METHOD PrintResults() CLASS Snake

   CLS

   ? "Results:"
   ?
   ? "Duration of the game - ", hb_ntos( Int( ( ( hb_MilliSeconds() - ::nStartGame ) / 1000 ) ) ), "second(s)"
   ? "Length of the snake  - ", hb_ntos( ::nLength ), "element(s)"

   Inkey( 0 )

   RETURN Self
