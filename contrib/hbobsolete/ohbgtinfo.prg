#ifndef __XHARBOUR__
#include "hbgtinfo.ch"

FUNCTION wvt_SetTitle( cTitle )
   RETURN hb_gtInfo( HB_GTI_WINTITLE, cTitle )

FUNCTION wvt_GetTitle()
   RETURN hb_gtInfo( HB_GTI_WINTITLE )

FUNCTION wvt_SetCodepage( nCodePage )
   RETURN hb_gtInfo( HB_GTI_CODEPAGE, nCodePage )

FUNCTION wvt_GetPalette()
   RETURN hb_gtInfo( HB_GTI_PALETTE )

FUNCTION wvt_SetPalette( aRGB )
   RETURN hb_gtInfo( HB_GTI_PALETTE, aRGB )

FUNCTION wvt_GetRGBColor( nIndex )
   RETURN hb_gtInfo( HB_GTI_PALETTE, nIndex )

FUNCTION wvt_GetWindowHandle()
   RETURN hb_gtInfo( HB_GTI_WINHANDLE )

FUNCTION wvt_GetClipboard()
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION wvt_SetClipboard( cText )
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA, cText )

FUNCTION wvt_SetAltF4Close( lSetClose )
   RETURN hb_gtInfo( HB_GTI_CLOSABLE, lSetClose )

FUNCTION wvt_GetScreenWidth()
   RETURN hb_gtInfo( HB_GTI_DESKTOPWIDTH )

FUNCTION wvt_GetScreenHeight()
   RETURN hb_gtInfo( HB_GTI_DESKTOPHEIGHT )

#endif
