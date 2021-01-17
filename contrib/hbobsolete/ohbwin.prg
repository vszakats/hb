#ifndef __XHARBOUR__
#include "hbwin.ch"

FUNCTION wvg_PrepareBitmapFromResourceId( nID, nExpWidth, nExpHeight, lMap3Dcolors )
   RETURN wapi_LoadImage( wapi_GetModuleHandle(), nID, WIN_IMAGE_BITMAP, ;
      nExpWidth, nExpHeight, ;
      iif( hb_defaultValue( lMap3Dcolors, .F. ), WIN_LR_LOADMAP3DCOLORS, WIN_LR_DEFAULTCOLOR ) )

FUNCTION wvg_PrepareBitmapFromResourceName( cName, nExpWidth, nExpHeight, lMap3Dcolors )
   RETURN wapi_LoadImage( wapi_GetModuleHandle(), cName, WIN_IMAGE_BITMAP, ;
      nExpWidth, nExpHeight, ;
      iif( hb_defaultValue( lMap3Dcolors, .F. ), WIN_LR_LOADMAP3DCOLORS, WIN_LR_DEFAULTCOLOR ) )

#endif
