/*
Snake game

MIT License

Copyright (c) 2017 Rafał Jopek ( rafaljopek at hotmail com )

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "box.ch"

#define DIRECTION_UP    1
#define DIRECTION_DOWN  2
#define DIRECTION_RIGHT 3
#define DIRECTION_LEFT  4

FUNCTION Main()

   LOCAL oSnake
   LOCAL lExecute := .T.
   LOCAL nKey
   LOCAL nOldCursor := SetCursor( SC_NONE )

   SetMode( 25, 80 )

   oSnake := Snake():New()
   oSnake:InitFood()

   DO WHILE ( lExecute )

      oSnake:Refresh()

      nKey := InKey( 1 )

      SWITCH nKey
         CASE K_ESC
            lExecute := .F.

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
           EXIT

      ENDSWITCH

      IF oSnake:nRow == oSnake:nRowFood .AND. oSnake:nCol == oSnake:nColFood
         oSnake:nLength++
         AADD( oSnake:aTail, { 0, 0 } )
         oSnake:InitFood()
      ENDIF

   ENDDO

   oSnake:PrintResults()

   SetCursor( nOldCursor )

RETURN ( NIL )

CLASS Snake

   DATA nStartGame INIT Hb_MilliSeconds()

   DATA nRow INIT Int( MaxRow() / 2 )
   DATA nCol INIT Int( MaxCol() / 2 )

   DATA nRowFood
   DATA nColFood

   DATA nDirection INIT DIRECTION_RIGHT
   DATA nLength    INIT 1

   DATA aTail INIT { { Int( MaxRow() / 2 ), Int( MaxCol() / 2 ) - 1 } }

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
   METHOD Frame()
   METHOD PrintResults()

ENDCLASS

/*
   New()
*/
METHOD New() CLASS Snake

RETURN ( Self )

/*
   Up()
*/
METHOD Up() CLASS Snake

   IF ::nRow < 0
      ::nRow := MaxRow() + 1
   ENDIF

   IF ::nDirection == DIRECTION_DOWN
      ::nRow++
      RETURN ( Self )   
   ELSE
      ::nRow--
      ::nDirection := DIRECTION_UP
   ENDIF

RETURN ( Self )

/*
   Down()
*/
METHOD Down() CLASS Snake

   IF ::nRow > MaxRow() 
      ::nRow := 0 - 1
   ENDIF

   IF ::nDirection == DIRECTION_UP
      ::nRow--
      RETURN ( Self )
   ELSE
      ::nRow++
      ::nDirection := DIRECTION_DOWN
   ENDIF 

RETURN ( Self )

/*
   Left()
*/
METHOD Left() CLASS Snake

   IF ::nCol < 0
      ::nCol := MaxCol() + 1
   ENDIF

   IF ::nDirection == DIRECTION_RIGHT
      ::nCol++
      RETURN ( Self )
   ELSE 
      ::nCol--
      ::nDirection := DIRECTION_LEFT
   ENDIF

RETURN ( Self )

/*
   Right()
*/
METHOD Right() CLASS Snake

   IF ::nCol > MaxCol()
      ::nCol := 0 - 1
   ENDIF

   IF ::nDirection == DIRECTION_LEFT
      ::nCol--
      RETURN ( Self )
   ELSE 
      ::nCol++
      ::nDirection := DIRECTION_RIGHT
   ENDIF

RETURN ( Self )

/*
   Refresh()
*/
METHOD Refresh() CLASS Snake

   DispBegin()

   Scroll()

   ::Snake() 

   ::Food()

   DispEnd()
   
RETURN ( Self )

/*
   Snake()
*/
METHOD Snake() CLASS Snake

   LOCAL i

   FOR i := Len( ::aTail ) - 1 TO 1 STEP - 1
      ::aTail[ i + 1, 1 ] := ::aTail[ i, 1 ]
      ::aTail[ i + 1, 2 ] := ::aTail[ i, 2 ]

      Hb_DispOutAt( ::aTail[ i + 1, 1 ], ::aTail[ i + 1, 2 ], "*", "R/N" )

   NEXT

   SWITCH ::nDirection
      CASE DIRECTION_UP
         ::aTail[ 1, 1 ] := ::nRow + 1
         ::aTail[ 1, 2 ] := ::nCol
         EXIT

      CASE DIRECTION_DOWN
         ::aTail[ 1, 1 ] := ::nRow - 1
         ::aTail[ 1, 2 ] := ::nCol
         EXIT

      CASE DIRECTION_LEFT 
         ::aTail[ 1, 1 ] := ::nRow
         ::aTail[ 1, 2 ] := ::nCol + 1
         EXIT

      CASE DIRECTION_RIGHT
         ::aTail[ 1, 1 ] := ::nRow
         ::aTail[ 1, 2 ] := ::nCol - 1 
         EXIT

   ENDSWITCH

   Hb_DispOutAt( ::aTail[ 1, 1 ], ::aTail[ 1, 2 ], "*", "R/N" )

   Hb_DispOutAt( ::nRow, ::nCol, Chr( 2 ), "R/N" )

RETURN ( Self )

/*
   Move()
*/
METHOD Move() CLASS Snake

   SWITCH ::nDirection
      CASE DIRECTION_UP
         ::Up()
         EXIT

      CASE DIRECTION_DOWN
         ::Down()
         EXIT

      CASE DIRECTION_LEFT 
         ::Left()
         EXIT

      CASE DIRECTION_RIGHT
         ::Right()
         EXIT

   ENDSWITCH

RETURN ( Self )

/*
   Food()
*/
METHOD Food() CLASS Snake

   Hb_DispOutAt( ::nRowFood, ::nColFood, "*", "GR+/N" )

RETURN ( Self )

/*
   InitFood()
*/
METHOD InitFood() CLASS Snake

   ::nRowFood := Hb_RandInt( 1, MaxRow() - 1 )
   ::nColFood := Hb_RandInt( 1, MaxCol() - 1 )

RETURN ( Self )

/*
   Frame()
*/
METHOD Frame() CLASS Snake

   Hb_DispBox( 0, 0, MaxRow(), MaxCol(), HB_B_SINGLE_UNI + " ", "R/N" )

RETURN ( Self )

/*
   PrintResults()
*/
METHOD PrintResults() CLASS Snake

   CLS

   ? "Results:"
   ? 
   ? "Duration of the game    - ", LTRIM( STR( Int( ( ( Hb_MilliSeconds() - ::nStartGame ) / 1000 ) ) ) ), "seconds"
   ? "The length of the snake - ", LTRIM( STR( ::nLength ) ), "elements"

   InKey( 0 )

RETURN ( Self )
