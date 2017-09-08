/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL lContinue := .T.
   LOCAL nRow
   LOCAL cLine
   LOCAL item
   LOCAL nColor

   LOCAL aArray := { { ;
      "  _________ __                 __    ", ;
      " /   _____//  |______ ________/  |_  ", ;
      " \_____  \\   __\__  \\_  __ \   __\ ", ;
      " /        \|  |  / __ \|  | \/|  |   ", ;
      "/_______  /|__| (____  /__|   |__|   ", ;
      "        \/           \/              " }, { ;
      "_______   ", ;
      "\   _  \  ", ;
      "/  /_\  \ ", ;
      "\  \_/   \", ;
      " \_____  /", ;
      "       \/ " }, { ;
      " ____ ", ;
      "/_   |", ;
      " |   |", ;
      " |   |", ;
      " |___|" }, { ;
      "________  ", ;
      "\_____  \ ", ;
      " /  ____/ ", ;
      "/       \ ", ;
      "\_______ \", ;
      "        \/" }, { ;
      "________  ", ;
      "\_____  \ ", ;
      "  _(__  < ", ;
      " /       \", ;
      "/______  /", ;
      "       \/ " }, { ;
      "   _____  ", ;
      "  /  |  | ", ;
      " /   |  |_", ;
      "/    ^   /", ;
      "\____   | ", ;
      "     |__| " }, { ;
      " .________", ;
      " |   ____/", ;
      " |____  \ ", ;
      " /       \", ;
      "/______  /", ;
      "       \/ " }, { ;
      "  ________", ;
      " /  _____/", ;
      "/   __  \ ", ;
      "\  |__\  \", ;
      " \_____  /", ;
      "       \/ " }, { ;
      "_________ ", ;
      "\______  \", ;
      "    /    /", ;
      "   /    / ", ;
      "  /____/  ", ;
      "          " }, { ;
      "  ______  ", ;
      " /  __  \ ", ;
      " >      < ", ;
      "/   --   \", ;
      "\______  /", ;
      "       \/ " }, { ;
      " ________ ", ;
      "/   __   \", ;
      "\____    /", ;
      "   /    / ", ;
      "  /____/  " } }

   DO WHILE lContinue

      nColor := hb_RandomInt( 0x1, 0xf )

      FOR EACH item IN aArray DESCEND
         nRow := Int( ( MaxRow() - Len( item ) ) / 2 )
         DispBegin()
         Scroll()
         FOR EACH cLine IN item
            hb_DispOutAt( nRow + cLine:__enumIndex() - 1, 0, PadC( cLine, MaxCol() + 1 ), nColor )
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
