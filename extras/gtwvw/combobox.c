/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw combobox functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbgtwvw.h"

static long hb_gt_wvw_GetFontDialogUnits( HWND h, HFONT f )
{
   const TCHAR tmp[] = TEXT( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" );

   /* get the hdc to the main window */
   HDC hDc = GetDC( h );

   /* with the current font attributes, select the font */
   HFONT hFont    = f; /* GetStockObject( ANSI_VAR_FONT ); */
   HFONT hFontOld = ( HFONT ) SelectObject( hDc, &hFont );

   SIZE sz;

   /* get its length */
   GetTextExtentPoint32( hDc, tmp, HB_SIZEOFARRAY( tmp ), &sz );

   /* re-select the previous font & delete the hDc */
   SelectObject( hDc, hFontOld );
   DeleteObject( hFont );
   ReleaseDC( h, hDc );

   /* calculate the average character width */
   return sz.cx / HB_SIZEOFARRAY( tmp );
}

/* wvw_cbCreate( [nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
 *                          nReserved, nKbdType, aOffset, hControl )
 * create combobox (drop-down list, no editbox) for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nWidth: width of combobox (in character unit)
 * aText: array of drop-down list members, default = {"empty"}
 *      eg. {"yes","no"}
 * bBlock: codeblock to execute on these events:
 *        event=CBN_SELCHANGE(1): user changes selection
 *                      (not executed if selection
 *                      is changed programmatically)
 *         event=CBN_SETFOCUS
 *         event=CBN_KILLFOCUS
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCBid  : combobox id
 *         nType  : event type (CBN_SELCHANGE/CBN_SETFOCUS/CBN_KILLFOCUS supported)
 *         nIndex : index of the selected list item (0 based)
 * nListLines: number of lines for list items, default = 3
 *            (will be automatically truncated if it's > Len(aText))
 * nReserved: reserved for future (this parm is now ignored)
 *
 * nKbdType: WVW_CB_KBD_STANDARD (0): similar to standard windows convention
 *            ENTER/ESC: will kill focus from combobox
 *          WVW_CB_KBD_CLIPPER (1):
 *            ENTER: drop (show) the list box
 *            UP/DOWN/TAB/SHIFTTAB/ESC: kill focus
 * default is WVW_CB_KBD_STANDARD (0)
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of combobox.
 *         defaults: {-2,-2,+2,+2}
 *         NOTES: the third element (y2) is actually ignored.
 *                height of combobox is always 1 char height
 *                (see also wvw_cbSetFont())
 *
 * returns control id of newly created combobox of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC( WVW_CBCREATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      HWND hWndParent = wvw_win->hWnd;
      HWND hWnd;

      HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, 10, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      int   iOffTop, iOffLeft, iOffBottom, iOffRight;

      int    nCtrlId;
      USHORT usWidth      = ( USHORT ) hb_parni( 4 );
      USHORT usTop        = ( USHORT ) hb_parni( 2 ),
             usLeft       = ( USHORT ) hb_parni( 3 ),
             usBottom     = usTop,
             usRight      = usLeft + usWidth - 1;
      USHORT usNumElement = ( USHORT ) ( HB_ISARRAY( 5 ) ? hb_arrayLen( hb_param( 5, HB_IT_ARRAY ) ) : 0 );
      USHORT usListLines  = ( USHORT ) hb_parnidef( 7, 3 );
      BYTE   byCharHeight = hb_gt_wvw_LineHeight( wvw_win );

      /* in the future combobox type might be selectable by 8th parameter */
      int     iStyle   = CBS_DROPDOWNLIST | WS_VSCROLL;
      HB_BYTE bKbdType = ( HB_BYTE ) hb_parnidef( 9, WVW_CB_KBD_STANDARD );

      if( wvw_win->hCBfont == NULL )
      {
         wvw_win->hCBfont = CreateFontIndirect( &wvw->lfCB );
         if( wvw_win->hCBfont == NULL )
         {
            HB_STOREHANDLE( NULL, 11 );
            hb_retnl( 0 );
            return;
         }
      }

      iOffTop  = hb_parvni( 10, 1 );
      iOffLeft = hb_parvni( 10, 2 );

      iOffBottom = usListLines;
      iOffRight  = hb_parvni( 10, 4 );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + ( iOffBottom * byCharHeight );
      iRight  = xy.x - 1 + iOffRight;

      nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_COMBOBOX );
      if( nCtrlId == 0 )
         nCtrlId = WVW_ID_BASE_COMBOBOX;
      else
         nCtrlId++;

      InitCommonControls();

      hWnd = CreateWindowEx(
         0,
         TEXT( "COMBOBOX" ),
         NULL,
         WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
         iLeft,
         iTop,
         iRight - iLeft + 1,
         iBottom - iTop + 1,
         hWndParent,
         ( HMENU ) ( HB_PTRDIFF ) nCtrlId,
         wvw->hInstance,
         NULL );

      if( hWnd )
      {
         long LongComboWidth = 0, NewLongComboWidth;

         RECT    rXB, rOffXB;
         WNDPROC OldProc;
         USHORT  i;

         SendMessage( hWnd, WM_SETREDRAW, ( WPARAM ) TRUE, 0 );

         if( usNumElement == 0 )
         {
            if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) TEXT( "empty" ) ) < 0 )
            {
               /* ignore failure */
            }
         }
         else
         {
            for( i = 1; i <= usNumElement; i++ )
            {
               void * hText;

               if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) HB_PARASTR( 5, i, &hText, NULL ) ) < 0 )
               {
                  /* ignore failure */
               }
               else
               {
                  long numofchars = ( int ) SendMessage( hWnd, CB_GETLBTEXTLEN, i - 1, 0 );
                  if( numofchars > LongComboWidth )
                     LongComboWidth = numofchars;
               }

               hb_strfree( hText );
            }
         }

         SendMessage( hWnd, CB_SETCURSEL, 0, 0 );
         SendMessage( hWnd, CB_SETEXTENDEDUI, ( WPARAM ) TRUE, 0 );

         NewLongComboWidth = ( LongComboWidth - 2 ) * hb_gt_wvw_GetFontDialogUnits( hWndParent, hFont );
         SendMessage( hWnd, CB_SETDROPPEDWIDTH, ( WPARAM ) NewLongComboWidth + 100 /* LongComboWidth + 100 */, 0 );

         rXB.top    = usTop;
         rXB.left   = usLeft;
         rXB.bottom = usBottom;
         rXB.right  = usRight;

         rOffXB.top    = iOffTop;
         rOffXB.left   = iOffLeft;
         rOffXB.bottom = iOffBottom;
         rOffXB.right  = iOffRight;

         hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hWnd, nCtrlId, hb_param( 6, HB_IT_EVALITEM ), rXB, rOffXB, bKbdType );

         OldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_CBProc );

         hb_gt_wvw_StoreControlProc( wvw_win, WVW_CONTROL_COMBOBOX, hWnd, OldProc );

         SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hCBfont, ( LPARAM ) TRUE );

         HB_STOREHANDLE( hWnd, 11 );
         hb_retnl( nCtrlId );
         return;
      }
   }

   HB_STOREHANDLE( NULL, 11 );
   hb_retnl( 0 );
}

/* wvw_cbDestroy( [nWinNum], nCBid )
 * destroy combobox nCBid for window nWinNum
 */
HB_FUNC( WVW_CBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int      nCtrlId     = hb_parni( 2 );
      PWVW_CTL wvw_ctl     = wvw_win->ctlList;
      PWVW_CTL wvw_ctlPrev = NULL;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX && wvw_ctl->nId == nCtrlId )
            break;

         wvw_ctlPrev = wvw_ctl;
         wvw_ctl     = wvw_ctl->pNext;
      }

      if( wvw_ctl )
      {
         DestroyWindow( wvw_ctl->hWnd );

         if( wvw_ctlPrev )
            wvw_ctlPrev->pNext = wvw_ctl->pNext;
         else
            wvw_win->ctlList = wvw_ctl->pNext;

         if( wvw_ctl->pBlock )
            hb_itemRelease( wvw_ctl->pBlock );

         hb_xfree( wvw_ctl );
      }
   }
}

/* wvw_cbSetFocus( [nWinNum], nComboId )
 * set the focus to combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBSETFOCUS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_cbIsFocused( [nWinNum], nComboId )
 * returns .T. if the focus is on combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBISFOCUSED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && GetFocus() == hWnd );
}

/* wvw_cbEnable( [nWinNum], nComboId, [lEnable] )
 * enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .T., ie. enabling the combobox)
 * return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      HB_BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWnd, ( BOOL ) bEnable ) == 0 );

      if( ! bEnable )
         SetFocus( wvw_win->hWnd );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetCodeblock( [nWinNum], nCBid, bBlock )
 * assign (new) codeblock bBlock to combobox nCBid for window nWinNum
 *
 * return .T. if successful
 */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );
   PHB_ITEM pBlock  = hb_param( 3, HB_IT_EVALITEM );

   if( pBlock && wvw_ctl && ! wvw_ctl->fBusy )
   {
      PWVW_GLO wvw         = hb_gt_wvw();
      HB_BOOL  fOldSetting = wvw->fRecurseCBlock;

      wvw->fRecurseCBlock = HB_FALSE;
      wvw_ctl->fBusy      = HB_TRUE;

      if( wvw_ctl->pBlock )
         hb_itemRelease( wvw_ctl->pBlock );

      wvw_ctl->pBlock = hb_itemNew( pBlock );

      wvw_ctl->fBusy      = HB_FALSE;
      wvw->fRecurseCBlock = fOldSetting;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL comboboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_CBSETFONT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      HB_BOOL fResult = HB_TRUE;

      wvw->lfCB.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
      wvw->lfCB.lfWidth          = hb_parnldef( 4, wvw->lfCB.lfWidth );
      wvw->lfCB.lfEscapement     = 0;
      wvw->lfCB.lfOrientation    = 0;
      wvw->lfCB.lfWeight         = hb_parnldef( 5, wvw->lfCB.lfWeight );
      wvw->lfCB.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfCB.lfQuality );
      wvw->lfCB.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfCB.lfItalic );
      wvw->lfCB.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfCB.lfUnderline );
      wvw->lfCB.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfCB.lfStrikeOut );
      wvw->lfCB.lfCharSet        = DEFAULT_CHARSET;
      wvw->lfCB.lfPitchAndFamily = FF_DONTCARE;

      if( HB_ISCHAR( 2 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfCB.lfFaceName, HB_SIZEOFARRAY( wvw->lfCB.lfFaceName ) );
         wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfCB.lfFaceName ) - 1 ] = TEXT( '\0' );
      }

      if( wvw_win->hCBfont )
      {
         HFONT hOldFont = wvw_win->hCBfont;
         HFONT hFont    = CreateFontIndirect( &wvw->lfCB );
         if( hFont )
         {
            PWVW_CTL wvw_ctl = wvw_win->ctlList;

            while( wvw_ctl )
            {
               if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX &&
                   ( HFONT ) SendMessage( wvw_ctl->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
                  SendMessage( wvw_ctl->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

               wvw_ctl = wvw_ctl->pNext;
            }

            wvw_win->hCBfont = hFont;
            DeleteObject( hOldFont );
         }
         else
            fResult = HB_FALSE;
      }

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetIndex( [nWinNum], nCBid, nIndex )
 *  set current selection of combobox nCBid in window nWinNum to nIndex
 *  (nIndex is 0 based)
 *  returns .T. if successful.
 *
 * NOTE: the better name to this function should be wvw_CBSetCurSel()
 *      but that name is already used.
 *      (wvw_CBSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );
   int      iIndex  = hb_parni( 3 );

   if( wvw_ctl && iIndex >= 0 )
      hb_retl( ( HB_BOOL ) SendMessage( wvw_ctl->hWnd, CB_SETCURSEL, ( WPARAM ) iIndex, 0 ) == iIndex );
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbGetIndex( [nWinNum], nCBid )
 *  get current selection of combobox nCBid in window nWinNum
 *  return nIndex (0 based)
 *  returns CB_ERR (-1) if none selected
 *
 * NOTE: the better name to this function should be WVW_CBgetCurSel()
 *      but that name is potentially misleading to WVW_CBsetCursel
 *      which is not our family of WVW_CB* functions
 *      (wvw_CBSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBGETINDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
      hb_retni( ( int ) SendMessage( wvw_ctl->hWnd, CB_GETCURSEL, 0, 0 ) );
   else
      hb_retni( CB_ERR );
}

/* wvw_cbFindString( [nWinNum], nCBid, cString )
 *  find index of cString in combobox nCBid in window nWinNum
 *  returns index of cString (0 based)
 *  returns CB_ERR (-1) if string not found
 *
 * NOTE:case insensitive
 */
HB_FUNC( WVW_CBFINDSTRING )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      void * hStr;
      hb_retni( ( int ) SendMessage( wvw_ctl->hWnd, CB_FINDSTRING, ( WPARAM ) -1, ( LPARAM ) HB_PARSTRDEF( 3, &hStr, NULL ) ) );
      hb_strfree( hStr );
   }
   else
      hb_retni( CB_ERR );
}

/* wvw_cbGetCurText( [nWinNum], nCBid )
   get current selected cString in combobox nCBid in window nWinNum
   returns "" if none selected */
HB_FUNC( WVW_CBGETCURTEXT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      int iCurSel  = ( int ) SendMessage( wvw_ctl->hWnd, CB_GETCURSEL, 0, 0 );
      int iTextLen = ( int ) SendMessage( wvw_ctl->hWnd, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel, 0 );
      if( iTextLen == CB_ERR )
         hb_retc_null();
      else
      {
         LPTSTR lptstr = ( LPTSTR ) hb_xgrab( ( iTextLen + 1 ) * sizeof( TCHAR ) );

         if( SendMessage( wvw_ctl->hWnd, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstr ) == CB_ERR )
            hb_retc_null();
         else
            HB_RETSTR( lptstr );

         hb_xfree( lptstr );
      }
   }
   else
      hb_retc_null();
}

/* wvw_cbIsDropped( [nWinNum], nCBid )
   get current dropped state of combobox nCBid in window nWinNum
   returns .T. if listbox is being shown, otherwise .F.
   Also returns .F. if nCBid not valid */
HB_FUNC( WVW_CBISDROPPED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
      hb_retl( ( HB_BOOL ) SendMessage( wvw_ctl->hWnd, CB_GETDROPPEDSTATE, 0, 0 ) );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_CBVISIBLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
}