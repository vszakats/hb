/*
 * Windows API functions (shellapi.h - shell32.dll)
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwapi.h"
#if defined( HB_OS_WIN_CE )
   #include "hbwince.h"
#endif

#include <shellapi.h>

HB_FUNC( WAPI_SHELLEXECUTE )
{
#if defined( HB_OS_WIN_CE )
   hb_retnint( -1 );
#else
   void * hOperation;
   void * hFile;
   void * hParameters;
   void * hDirectory;

   hb_retnint( ( HB_PTRDIFF ) ShellExecute( hbwapi_par_raw_HWND( 1 ),
                                            HB_PARSTR( 2, &hOperation, NULL ), /* edit, explore, open, print, play?, properties? */
                                            HB_PARSTRDEF( 3, &hFile, NULL ),
                                            HB_PARSTR( 4, &hParameters, NULL ),
                                            HB_PARSTR( 5, &hDirectory, NULL ),
                                            hb_parnidef( 6, SW_SHOWNORMAL ) /* nShowCmd */ ) );

   hb_strfree( hOperation );
   hb_strfree( hFile );
   hb_strfree( hParameters );
   hb_strfree( hDirectory );
#endif
}

HB_FUNC( WAPI_ISUSERANADMIN )
{
   BOOL bResult = FALSE;

   HMODULE hLib = hbwapi_LoadLibrarySystem( TEXT( "shell32.dll" ) );

   if( hLib )
   {
      typedef int ( WINAPI * ISUSERANADMIN )( void );
      ISUSERANADMIN pIsUserAnAdmin = ( ISUSERANADMIN )
                                     HB_WINAPI_GETPROCADDRESS( hLib, "IsUserAnAdmin" );
      if( pIsUserAnAdmin )
         bResult = ( pIsUserAnAdmin )();

      FreeLibrary( hLib );
   }

   hb_retl( bResult );
}

/*
   Function wapi_ShellExecuteEx( aShExecInfoStruct )
   Performs operation(s) on a specified file.
   Usage: nResult := wapi_ShellExecuteEx( {[<hparentWindow>] [, <"operation">] , <"filename"> ;
                                          [, <"parameters">] [, <"workdir">] [, <nShowmode>] ;
                                          [, <nfMask>]} )
   commonly used verbs: "open", "print", "edit", "explore", "find", "properties", "openas" et.c.
                        (The set of available verbs depends on the particular file or folder.
                        Generally, the actions available from an object's shortcut menu are (on paper ;-))
                        available verbs.)
                        - "openas" may not work on older win vers
                        - to get "properties", nfMask must be passed with SEE_MASK_INVOKEIDLIST (0x0000000C) value
   Returns: numeric: > 32 on sucess; <= 32 on failure!
   More about ShellExecuteEx and error codes at:
   <https://msdn.microsoft.com/en-us/library/windows/desktop/bb759784%28v=vs.85%29.aspx>
   (Pete D. - 2014/11/05)
 */
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
HB_FUNC( WAPI_SHELLEXECUTEEX )
{
   SHELLEXECUTEINFO ShExecInfo;

   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if ( ! pArray )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      hb_retni( 0 );
      return;
   }

   memset( &ShExecInfo, 0, sizeof( ShExecInfo ) ); /* initialize struct to avoid unpredictable behavior..*/

   ShExecInfo.cbSize       = sizeof( ShExecInfo );
   ShExecInfo.hwnd         = (HWND) hb_arrayGetNL( pArray, 1 ); /* parent window. usually NIL */
   ShExecInfo.lpVerb       = hb_arrayGetC( pArray, 2 ); /* operation requested */
   ShExecInfo.lpFile       = hb_arrayGetC( pArray, 3 ); /* cfilename on which the requested operation will be performed */
   ShExecInfo.lpParameters = hb_arrayGetC( pArray, 4 ); /* additional exec parameters */
   ShExecInfo.lpDirectory  = hb_arrayGetC( pArray, 5 ); /* working dir. NULL=current dir */
   ShExecInfo.nShow        = hb_arrayGetNI( pArray, 6 ); /* Show modes, default:SW_SHOWNORMAL */
   ShExecInfo.fMask        = hb_arrayGetNL( pArray, 7 ); /* needs to be assigned with proper value for some operations */

   if ( ! ShellExecuteEx( &ShExecInfo ) )
      hbwapi_SetLastError( GetLastError() );

   LocalFree( pArray );

   hb_retni( (int) ShExecInfo.hInstApp );
}
