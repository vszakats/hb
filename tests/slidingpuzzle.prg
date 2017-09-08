/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

/* Sliding puzzle game */

#include "box.ch"
#include "hbclass.ch"
#include "inkey.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL oSlidingPuzzle
   LOCAL tmp

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

      tmp := 1
      IF AScan( oSlidingPuzzle:aArray, {| x | x != tmp++ }, 1, 15 ) == 0
         lContinue := .F.
      ENDIF
   ENDDO

   oSlidingPuzzle:PrintResults()

   SetCursor( nOldCursor )

   RETURN

CREATE CLASS SlidingPuzzle

   VAR nStartGame   INIT hb_MilliSeconds()
   VAR nNumberMoves INIT 0

   VAR nRow INIT 0
   VAR nCol INIT 0
   VAR nPos INIT 0

   VAR aArray INIT {}

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

   IF ( nMRow % 4 ) == 0
      RETURN Self
   ENDIF

   IF ( nMCol % 7 ) == 0
      RETURN Self
   ENDIF

   ::Calculate()

   nMPos := ( Int( ( nMRow - 1 ) / 4 ) ) * 4 + Int( ( nMCol - 1 ) / 7 ) + 1

   DO CASE
   CASE nMPos == ::nPos + 4 ; ::Up()
   CASE nMPos == ::nPos - 4 ; ::Down()
   CASE nMPos == ::nPos + 1 ; ::Left()
   CASE nMPos == ::nPos - 1 ; ::Right()
   ENDCASE

   RETURN Self

METHOD Calculate() CLASS SlidingPuzzle

   LOCAL tmp

   FOR tmp := 1 TO 16
      IF ::aArray[ tmp ] == 0
         ::nPos := tmp
         ::nRow := 1 + Int( ( ::nPos - 1 ) / 4 )
         ::nCol := 1 + Int( ( ::nPos - 1 ) % 4 )
         EXIT
      ENDIF
   NEXT

   RETURN Self

METHOD Show() CLASS SlidingPuzzle

   LOCAL nRowPos := 1, nColPos := 3
   LOCAL tmp := 0
   LOCAL i

   FOR i := 1 TO 16

      ++tmp

      IF ::aArray[ i ] == iif( i == 16, 0, i )
         hb_DispBox( nRowPos, nColPos - 2, ( nRowPos - 1 ) + 3, ( nColPos - 3 ) + 6, HB_B_SINGLE_UNI + " ", 0x20 )
         hb_DispOutAt( nRowPos + 1, nColPos, hb_ntos( ::aArray[ i ] ), 0x20 )
      ELSE
         hb_DispOutAt( nRowPos + 1, nColPos, hb_ntos( ::aArray[ i ] ), 0xf1 )
      ENDIF

      IF ::aArray[ i ] == 0
         hb_DispBox( nRowPos, nColPos - 2, ( nRowPos - 1 ) + 3, ( nColPos - 3 ) + 6,, 0x11 )
      ENDIF

      nColPos += 7

      IF tmp >= 4
         nRowPos += 4
         nColPos := 3
         tmp := 0
      ENDIF
   NEXT

   RETURN Self

METHOD Refresh() CLASS SlidingPuzzle

   DispBegin()

   Scroll()
   ::Frame()
   ::Show()

   DispEnd()

   RETURN Self

METHOD Frame() CLASS SlidingPuzzle

   LOCAL cLine

   FOR EACH cLine IN { ;
      "┌──────┬──────┬──────┬──────┐", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "├──────┼──────┼──────┼──────┤", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "├──────┼──────┼──────┼──────┤", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "├──────┼──────┼──────┼──────┤", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "│      │      │      │      │", ;
      "└──────┴──────┴──────┴──────┘" }
      hb_DispOutAt( cLine:__enumIndex() - 1, 0, hb_UTF8ToStr( cLine ), 0xf0 )
   NEXT

   RETURN Self

METHOD RandomValue() CLASS SlidingPuzzle

   LOCAL nRandomValue := hb_randInt( 0, 15 )
   LOCAL tmp

   FOR tmp := 1 TO 16
      AAdd( ::aArray, nRandomValue-- )
      IF nRandomValue == -1
         ::nPos := tmp
      ENDIF
      nRandomValue := iif( nRandomValue == -1, 15, nRandomValue )
   NEXT

   RETURN Self

METHOD PrintResults() CLASS SlidingPuzzle

   CLS

   ? "Results:"
   ?
   ? "Game duration   -", hb_ntos( Int( ( ( hb_MilliSeconds() - ::nStartGame ) / 1000 ) ) ), "second(s)"
   ?
   ? "Number of moves -", hb_ntos( ::nNumberMoves - 1 )

   Inkey( 0 )

   RETURN Self
