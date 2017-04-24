#include "hbgtinfo.ch"
#include "inkey.ch"

#if defined( __HBSCRIPT__HBSHELL )
INIT PROCEDURE gtinit()
   #if defined( __PLATFORM__WINDOWS )
      hbshell_gtSelect( "GTWVT" )
   #elif defined( __PLATFORM__UNIX )
      hbshell_gtSelect( "GTXWC" )
   #endif
   RETURN
#else
   #if defined( __PLATFORM__WINDOWS )
      REQUEST HB_GT_WVT_DEFAULT
      REQUEST HB_GT_WVT
   #elif defined( __PLATFORM__UNIX )
      REQUEST HB_GT_XWC_DEFAULT
      REQUEST HB_GT_XWC
   #endif
#endif

PROCEDURE Main()

   LOCAL nFontHeight         := 17
   LOCAL nFontWidth          := 7
   LOCAL cFontName           := "Consolas"
   LOCAL nFontWeight         := HB_GTI_FONTW_BOLD
   LOCAL nScreenHeightChars  := 40
   LOCAL nScreenWidthChars   := 120

   LOCAL nDeskTopHeightPx    := hb_gtInfo( HB_GTI_DESKTOPHEIGHT )
   LOCAL nDeskTopWidthPx     := hb_gtInfo( HB_GTI_DESKTOPWIDTH )
   LOCAL nScreenHeightPx
   LOCAL nScreenWidthPx
   LOCAL nViewMaxHeightChars
   LOCAL nViewMaxWidthChars

   LOCAL nWeightFactor       := iif( nFontWeight > 2, 1, 0 )

   LOCAL GetList := {}

   SetMode( nScreenHeightChars, nScreenWidthChars )

   nFontHeight := Int( ( ( nDeskTopHeightPx - 30 ) * 0.9 ) / nScreenHeightChars )
   nFontWidth  := Int( ( nDeskTopWidthPx * 0.9 ) / nScreenWidthChars )
   hb_gtInfo( HB_GTI_SCREENWIDTH,  ( nFontWidth + nWeightFactor )  * nScreenWidthChars )
   hb_gtInfo( HB_GTI_SCREENHEIGHT, nFontHeight * nScreenHeightChars )
   hb_gtInfo( HB_GTI_FONTNAME,     cFontName )
   hb_gtInfo( HB_GTI_FONTWEIGHT,   nFontWeight )
   hb_gtInfo( HB_GTI_FONTWIDTH,    nFontWidth )
   hb_gtInfo( HB_GTI_FONTSIZE,     nFontHeight )
   hb_gtInfo( HB_GTI_FONTWEIGHT,   nFontWeight )

   SetColor( "W/B, W/R+,,, N/W" )

   CLS

   DO WHILE LastKey() != K_ESC

      cFontName     := AllTrim( cFontName )
      nWeightFactor := iif( nFontWeight > 2, 1, 0 )

      SetMode( nScreenHeightChars, nScreenWidthChars )

      hb_gtInfo( HB_GTI_SCREENWIDTH,  ( nFontWidth + nWeightFactor )  * nScreenWidthChars )
      hb_gtInfo( HB_GTI_SCREENHEIGHT, nFontHeight * nScreenHeightChars )
      hb_gtInfo( HB_GTI_FONTNAME,     cFontName )
      hb_gtInfo( HB_GTI_FONTWEIGHT,   nFontWeight )
      hb_gtInfo( HB_GTI_FONTWIDTH,    nFontWidth )
      hb_gtInfo( HB_GTI_FONTSIZE,     nFontHeight )
      hb_gtInfo( HB_GTI_FONTWEIGHT,   nFontWeight )
      CLS

      cFontName := PadR( cFontName, 50 )

      TotalPixels( nScreenHeightChars, nFontHeight, 0, 19, 20 )
      TotalPixels( nScreenWidthChars, nFontWidth, nWeightFactor, 20, 20 )

      nScreenHeightPx     := hb_gtInfo( HB_GTI_SCREENHEIGHT )
      nScreenWidthPx      := hb_gtInfo( HB_GTI_SCREENWIDTH )
      nViewMaxHeightChars := hb_gtInfo( HB_GTI_VIEWMAXHEIGHT )
      nViewMaxWidthChars  := hb_gtInfo( HB_GTI_VIEWMAXWIDTH )

      @  0,  0 SAY PadL( "hello world", MaxCol() + 1 )
      @  0,  0 SAY "hello world"
      @ MaxRow(),  0 SAY PadL( "hello world", MaxCol() + 1 )
      @ MaxRow(),  0 SAY "hello world"

      @  2,  2 SAY "Codepage          " + hb_ValToExp( hb_gtInfo( HB_GTI_CODEPAGE ) )
      @  4,  2 SAY "DeskTopHeightPx   " + Str( nDeskTopHeightPx, 4 )
      @  5,  2 SAY "DeskTopWidthPx    " + Str( nDeskTopWidthPx, 4 )

      @  4, 29 SAY "ScreenHeightPx " + Str( nScreenHeightPx, 4 )
      @  5, 29 SAY "ScreenWidthPx  " + Str( nScreenWidthPx, 4 )

      @  4, 54 SAY "ViewMaxHeightChars " + Str( nViewMaxHeightChars, 4 )
      @  5, 54 SAY "ViewMaxWidthChars  " + Str( nViewMaxWidthChars, 4 )

      @  7,  2 SAY "MaxRow()          " + Str( MaxRow(), 4 )
      @  8,  2 SAY "MaxCol()          " + Str( MaxCol(), 4 )

      @ 10,  2 SAY "ScreenHeightChars " GET nScreenHeightChars PICTURE "999" ;
         VALID TotalPixels( nScreenHeightChars, nFontHeight, 0, 19, 20 )
      @ 11,  2 SAY "ScreenWidthChars  " GET nScreenWidthChars  PICTURE "999" ;
         VALID TotalPixels( nScreenWidthChars, nFontWidth, nWeightFactor, 20, 20 )

      @ 13,  2 SAY "FontHeight        " GET nFontHeight   PICTURE "999" ;
         VALID TotalPixels( nScreenHeightChars, nFontHeight, 0, 19, 20 )
      @ 14,  2 SAY "FontWidth         " GET nFontWidth    PICTURE "999" ;
         VALID TotalPixels( nScreenWidthChars, nFontWidth, nWeightFactor, 20, 20 )

      @ 16,  2 SAY "FontName          " GET cFontName
      @ 17,  2 SAY "FontWeight        " GET nFontWeight   PICTURE "999"

      @ 19,  2 SAY "TotalHeight "
      @ 20,  2 SAY "TotalWidt   "

      READ
   ENDDO

   RETURN

FUNCTION TotalPixels( nScreen, nFont, nWeight, nRow, nCol )

   @ nRow, nCol SAY Int( nScreen * ( nFont + nWeight ) ) PICTURE "9999"

   RETURN .T.
