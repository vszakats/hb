/* Copyright 2017 Rafał Jopek ( rafaljopek at hotmail com ) */

#include "hbclass.ch"
#include "setcurs.ch"

PROCEDURE Main()

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL aSnow := {}, tmp
   LOCAL nSeconds := hb_MilliSeconds()

   DO WHILE .T.

      DispBegin()

      Scroll()

      tmp := 1
      DO WHILE tmp <= Len( aSnow )
         aSnow[ tmp ]:Show()
         IF aSnow[ tmp ]:nRow > MaxRow()
            hb_ADel( aSnow, tmp, .T. )
         ELSE
            ++tmp
         ENDIF
      ENDDO

      IF hb_MilliSeconds() - nSeconds > 5000
         AAdd( aSnow, Snow():New():Show() )

         IF aSnow[ tmp ]:nRow > MaxRow()
            hb_ADel( aSnow, tmp, .T. )
         ENDIF

         nSeconds := hb_MilliSeconds()
      ENDIF

      DispEnd()

      IF Inkey( 0.5 ) != 0
         EXIT
      ENDIF
   ENDDO

   SetCursor( nOldCursor )

   RETURN

CREATE CLASS Snow

   VAR nRow INIT 0
   VAR nCol
   VAR nSnowflake
   VAR nSnowColor

   METHOD New()
   METHOD Show()

   PROTECTED:

   CLASS VAR aFLake SHARED INIT { ;
      { ;
         "*" }, ;
      { ;
         "   *  .  *   ", ;
         " . _\/ \/_ . ", ;
         "  \  \ /  /  ", ;
         "-==>: X :<==-", ;
         "  / _/ \_ \  ", ;
         " '  /\ /\  ' ", ;
         "   *  '  *   " }, ;
      { ;
         "   ..    ..   ", ;
         "   '\    /'   ", ;
         "     \\//     ", ;
         "_.__\\\///__._", ;
         " '  ///\\\  ' ", ;
         "     //\\     ", ;
         "   ./    \.   ", ;
         "   ''    ''   " }, ;
      { ;
         "   .      .   ", ;
         "   _\/  \/_   ", ;
         "    _\/\/_    ", ;
         "_\_\_\/\/_/_/_", ;
         " / /_/\/\_\ \ ", ;
         "    _/\/\_    ", ;
         "    /\  /\    ", ;
         "   '      '   " }, ;
      { ;
         "     .:.     ", ;
         "..   \o/   ..", ;
         ":o|   |   |o:", ;
         " ~ '. ' .' ~ ", ;
         "     >O<     ", ;
         " _ .' . '. _ ", ;
         ":o|   |   |o:", ;
         "''   /o\   ''", ;
         "     ':'     " }, ;
      { ;
         "   ._    _.   ", ;
         "   (_)  (_)   ", ;
         "    .\::/.    ", ;
         "_.=._\\//_.=._", ;
         " '=' //\\ '=' ", ;
         "    '/::\'    ", ;
         "   (_)  (_)   ", ;
         "   '      '   " }, ;
      { ;
         "   <> \  / <>   ", ;
         "   \_\/  \/_/   ", ;
         "      \\//      ", ;
         "_<>_\_\<>/_/_<>_", ;
         " <> / /<>\ \ <> ", ;
         "    _ //\\ _    ", ;
         "   / /\  /\ \   ", ;
         "   <> /  \ <>   " }, ;
      { ;
         "   __  __   ", ;
         "  /_/  \_\  ", ;
         "    \\//    ", ;
         "/\_\\><//_/\", ;
         "\/ //><\\ \/", ;
         "  __//\\__  ", ;
         "  \_\  /_/  " }, ;
      { ;
         "     O     ", ;
         "O    :    O", ;
         "  '.\'/,'  ", ;
         "  :->o<-:  ", ;
         "  .'/.\'.  ", ;
         "O    :    O", ;
         "     O     " }, ;
      { ;
         "  '.|  |.'  ", ;
         ". ~~\  /~~ .", ;
         "_\_._\/_._/_", ;
         " / ' /\ ' \ ", ;
         "' __/  \__ '", ;
         "' .'|  |'. '" }, ;
      { ;
         "      \o/      ", ;
         "  _o/.:|:.\o_  ", ;
         "    .\:|:/.    ", ;
         "-=>>::>o<::<<=-", ;
         "  _ '/:|:\' _  ", ;
         "   o\':|:'/o   ", ;
         "      /o\      " }, ;
      { ;
         "      .      ", ;
         "      :      ", ;
         "'.___/*\___.'", ;
         "  \* \ / */  ", ;
         "   >--X--<   ", ;
         "  /*_/ \_*\  ", ;
         ".'   \*/   '.", ;
         "      :      ", ;
         "      '      " } }

END CLASS

METHOD New() CLASS Snow

   ::nCol       := hb_RandomInt( 5, MaxCol() - 5 )
   ::nSnowflake := hb_RandomInt( 1, Len( ::aFlake ) )
   ::nSnowColor := hb_RandomInt( 0x1, 0xf )

   RETURN Self

METHOD Show() CLASS Snow

   LOCAL nRow := ::nRow - Len( ::aFlake[ ::nSnowflake ] ), cRow

   FOR EACH cRow IN ::aFlake[ ::nSnowflake ]
      hb_DispOutAt( nRow++, ::nCol, cRow, ::nSnowColor )
   NEXT

   ++::nRow

   RETURN Self
