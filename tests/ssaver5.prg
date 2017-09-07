/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow := Int( ( MaxRow() - 6 ) / 2 )
   LOCAL cLine
   LOCAL i
   LOCAL aArray
   LOCAL nColor

   LOCAL aS := { ; 
      "  _________ __                 __    ", ;
      " /   _____//  |______ ________/  |_  ", ;
      " \_____  \\   __\__  \\_  __ \   __\ ", ;
      " /        \|  |  / __ \|  | \/|  |   ", ;
      "/_______  /|__| (____  /__|   |__|   ", ;
      "        \/           \/              " }

   LOCAL a0 := { ;
      "_______   ", ;
      "\   _  \  ", ;
      "/  /_\  \ ", ;
      "\  \_/   \", ;
      " \_____  /", ;
      "       \/ " }

   LOCAL a1 := { ;
      " ____ ", ;
      "/_   |", ;
      " |   |", ;
      " |   |", ;
      " |___|" }

   LOCAL a2 := { ;
      "________  ", ;
      "\_____  \ ", ;
      " /  ____/ ", ;
      "/       \ ", ;
      "\_______ \", ;
      "        \/" }

   LOCAL a3 := { ;
      "________  ", ;
      "\_____  \ ", ;
      "  _(__  < ", ;
      " /       \", ;
      "/______  /", ;
      "       \/ " } 
   LOCAL a4 := { ;
      "   _____  ", ;
      "  /  |  | ", ;
      " /   |  |_", ;
      "/    ^   /", ;
      "\____   | ", ;
      "     |__| " }

   LOCAL a5 := { ;
      " .________", ;
      " |   ____/", ;
      " |____  \ ", ;
      " /       \", ;
      "/______  /", ;
      "       \/ " }

   LOCAL a6 := { ;
      "  ________", ;
      " /  _____/", ;
      "/   __  \ ", ;
      "\  |__\  \", ;
      " \_____  /", ;
      "       \/ " }

   LOCAL a7 := { ;
      "_________ ", ;
      "\______  \", ;
      "    /    /", ;
      "   /    / ", ;
      "  /____/  ", ;
      "          " }

   LOCAL a8 := { ;
      "  ______  ", ;
      " /  __  \ ", ;
      " >      < ", ;
      "/   --   \", ;
      "\______  /", ;
      "       \/ " }

   LOCAL a9 := { ;
      " ________ ", ;
      "/   __   \", ;
      "\____    /", ;
      "   /    / ", ;
      "  /____/  " }

   aArray := { aS, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 }

   DO WHILE lContinue

      nColor := hb_RandomInt( 0x1, 0xf )

      FOR i := Len( aArray ) TO 1 STEP -1
         hb_SCroll()
         FOR EACH cLine IN aArray[ i ]
            hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, PadC( cLine, MaxCol() + 1 ), nColor )
         NEXT
         IF Inkey( 1 ) != 0
            lContinue := .F.
            EXIT
         ENDIF
      NEXT
   ENDDO
   
   SetCursor( nOldCursor )

   RETURN
