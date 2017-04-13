/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Sliding puzzle game */

#include "hbclass.ch"
#include "setcurs.ch"
#include "box.ch"
#include "inkey.ch"

REQUEST HB_CODEPAGE_UTF8

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL oSlidingPuzzle
   LOCAL i

   SetMode( 17, 29 )

   hb_cdpSelect( "UTF8" )

   oSlidingPuzzle := SlidingPuzzle():New()
   oSlidingPuzzle:Frame()
   oSlidingPuzzle:RandomValue()

   DO WHILE lContinue

      oSlidingPuzzle:Refresh()

      SWITCH Inkey( 0, INKEY_ALL )

         CASE K_ESC
            lContinue := .F.
            EXIT

         CASE K_UP
            oSlidingPuzzle:Up()
            EXIT

         CASE K_DOWN
            oSlidingPuzzle:Down()
            EXIT

         CASE K_LEFT
            oSlidingPuzzle:Left()
            EXIT

         CASE K_RIGHT
            oSlidingPuzzle:Right()
            EXIT

         CASE K_LBUTTONDOWN
            oSlidingPuzzle:LButtonDown()
            EXIT 
    
      ENDSWITCH

      ++oSlidingPuzzle:nNumberMoves

      i := 1
      IF hb_AScan( oSlidingPuzzle:aArray, { |x| x <> i++ }, 1, 15 ) == 0
         lContinue := .F.
      ENDIF

   ENDDO

   oSlidingPuzzle:PrintResults()

   SetCursor( nOldCursor )

RETURN

/*
   Class Sliding Puzzle
*/
CREATE CLASS SlidingPuzzle

   DATA nStartGame   INIT hb_MilliSeconds()
   DATA nNumberMoves INIT 0

   DATA nRow INIT 0
   DATA nCol INIT 0
   DATA nPos INIT 0

   DATA aArray INIT {}

   METHOD New()
   METHOD Up()
   METHOD Down()
   METHOD Left()
   METHOD Right()
   METHOD LButtonDown()
   METHOD Calculate()
   METHOD Show()
   METHOD Refresh()
   METHOD Frame()
   METHOD RandomValue()
   METHOD PrintResults()

ENDCLASS

METHOD New() CLASS SlidingPuzzle
RETURN Self

METHOD Up() CLASS SlidingPuzzle

   ::Calculate()

   IF ::nRow < 4
      ::aArray[ ::nPos ] := ::aArray[ ::nPos + 4 ]
      ::aArray[ ::nPos + 4 ] := 0
      ::nPos += 4
   ENDIF

RETURN Self

METHOD Down() CLASS SlidingPuzzle

   ::Calculate()

   IF ::nRow > 1
      ::aArray[ ::nPos ] := ::aArray[ ::nPos - 4 ]
      ::aArray[ ::nPos - 4 ] := 0
      ::nPos -= 4
   ENDIF

RETURN Self

METHOD Left() CLASS SlidingPuzzle

   ::Calculate()

   IF ::nCol < 4
      ::aArray[ ::nPos ] := ::aArray[ ::nPos + 1 ]
      ::aArray[ ::nPos + 1 ] := 0
      ::nPos++
   ENDIF

RETURN Self

METHOD Right() CLASS SlidingPuzzle

   ::Calculate()

   IF ::nCol > 1
      ::aArray[ ::nPos ] := ::aArray[ ::nPos - 1 ]
      ::aArray[ ::nPos - 1 ] := 0
      ::nPos--
   ENDIF

RETURN Self

METHOD LButtonDown() CLASS SlidingPuzzle

   LOCAL nMRow := MRow()
   LOCAL nMCol := MCol()
   LOCAL nMPos

   IF ( nMRow  % 4 ) == 0
      RETURN Self
   ENDIF 

   nMPos := ( Int( ( nMRow - 1 ) / 4 ) ) * 4 + Int( ( nMCol - 1 ) / 7 ) + 1
   
   ::Calculate()

   DO CASE  
      CASE nMPos == ::nPos + 4
         ::Up()

      CASE nMPos == ::nPos - 4
         ::Down()

      CASE nMPos == ::nPos + 1
         ::Left()

      CASE nMPos == ::nPos - 1 
         ::Right()

   ENDCASE

RETURN Self

METHOD Calculate() CLASS SlidingPuzzle

   LOCAL i

   FOR i := 1 TO 16
      IF ::aArray[ i ] == 0 
         ::nPos := i
         ::nRow := 1 + Int( ( ::nPos - 1 ) / 4 )
         ::nCol := 1 + Int( ( ::nPos - 1 ) % 4 )
         EXIT
      ENDIF
   NEXT

RETURN Self

METHOD Show() CLASS SlidingPuzzle

   LOCAL i
   LOCAL tmp := 0
   LOCAL nRowPos := 1, nColPos := 3

   FOR i := 1 TO 16

      tmp++

      IF ::aArray[ i ] == IIF( i == 16, 0, i )
         hb_DispBox( nRowPos, nColPos - 2, ( nRowPos - 1 ) + 3, ( nColPos - 3 ) + 6, HB_B_SINGLE_UNI + " ", 0x20 )
         hb_DispOutAt( nRowPos + 1, nColPos, hb_ntos( ::aArray[ i ], 2 ), 0x20 )
      ELSE
         hb_DispOutAt( nRowPos + 1, nColPos, hb_ntos( ::aArray[ i ], 2 ), 0xf1 )
      ENDIF

      IF ::aArray[ i ] == 0
         hb_DispBox( nRowPos, nColPos - 2, ( nRowPos - 1 ) + 3, ( nColPos - 3 ) + 6, , 0x11 )
      ENDIF
      
      nColPos += 7
      
      IF tmp  >= 4
         nRowPos += 4
         nColPos := 3
         tmp  := 0
      ENDIF
   NEXT

RETURN Self

METHOD Refresh() CLASS SlidingPuzzle

   DispBegin()

   hb_Scroll()
   ::Frame()
   ::Show()

   DispEnd()

RETURN Self

METHOD Frame() CLASS SlidingPuzzle

   LOCAL nColor := 0xf0

   hb_DispOutAt(  0, 0, "┌──────┬──────┬──────┬──────┐", nColor )
   hb_DispOutAt(  1, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  2, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  3, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  4, 0, "├──────┼──────┼──────┼──────┤", nColor )
   hb_DispOutAt(  5, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  6, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  7, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt(  8, 0, "├──────┼──────┼──────┼──────┤", nColor )
   hb_DispOutAt(  9, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 10, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 11, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 12, 0, "├──────┼──────┼──────┼──────┤", nColor )
   hb_DispOutAt( 13, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 14, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 15, 0, "│      │      │      │      │", nColor )
   hb_DispOutAt( 16, 0, "└──────┴──────┴──────┴──────┘", nColor )

RETURN Self

METHOD RandomValue() CLASS SlidingPuzzle

   LOCAL i
   LOCAL nRandomValue := hb_randInt( 0, 15 )

   FOR i := 1 TO 16
      AAdd( ::aArray, nRandomValue-- )
      IF nRandomValue == - 1
         ::nPos := i
      ENDIF
      nRandomValue := IF( nRandomValue == - 1, 15, nRandomValue )
   NEXT 

RETURN Self

METHOD PrintResults() CLASS SlidingPuzzle

   CLS

   ? "Results:"
   ?
   ? "Game duration    - ", hb_ntos( Int( ( ( hb_MilliSeconds() - ::nStartGame ) / 1000 ) ) )
   ? "second(s)"
   ? 
   ? "Number of moves  - ", hb_ntos( ::nNumberMoves - 1 )
   Inkey( 0 )

RETURN Self
