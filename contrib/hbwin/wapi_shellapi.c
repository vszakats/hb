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

HB_FUNC( WAPI_SHELLEXECUTEEX ){
/*
Performs operation(s) on a specified file.
Usage: nResult := wapi_ShellExecuteEx( [<hparentWindow>] [, <"operation">], <"filename"> ;
                               [, <"parameters">] [, <"workdir">] [, <nShowmode>] [, <nfMask>] )
Returns: numeric : > 32 on sucess;
                   <= 32 on error! (in which case, the returned value indicates the cause of the failure).
                   More about error codes at: <http://msdn.microsoft.com/en-us/library/windows/desktop/bb759784%28v=vs.85%29.aspx> 
(c) Pete D. <pete_westg@yahoo.gr> 29/10/2014                   
*/
   void * hOperation;
   void * hFile;
   void * hParameters;
   void * hDirectory;
   SHELLEXECUTEINFO ShExecInfo;
	
   memset( &ShExecInfo, 0, sizeof(SHELLEXECUTEINFO) );           // initialize struct to avoid unpredictable behavior..
   
   ShExecInfo.cbSize       = sizeof( SHELLEXECUTEINFO );
   ShExecInfo.fMask        = hb_parnldef(7, SEE_MASK_DEFAULT);   
   ShExecInfo.hwnd         = ( HWND ) hb_parptr( 1 );            // parent window. usually 0
   ShExecInfo.lpVerb       = HB_PARSTR( 2, &hOperation, NULL );  // "open", "print", "edit", "find", "properties", "openas" ("openas" may not work on older win vers)
                                                                 // note: to get "properties", must used the SEE_MASK_INVOKEIDLIST flag (tip: pass nfMask=12 when calling)

   ShExecInfo.lpFile       = HB_PARSTR( 3, &hFile, NULL );       // cfilename on which the requeste operation will be performed
   ShExecInfo.lpParameters = HB_PARSTR( 4, &hParameters, NULL ); // additional exec parameters
   ShExecInfo.lpDirectory  = HB_PARSTR( 5, &hDirectory, NULL );  // workdir. NULL=current dir
   ShExecInfo.nShow        = hb_parnidef( 6, SW_SHOWNORMAL );    // Show modes, default:SW_SHOWNORMAL
   ShExecInfo.hInstApp     = NULL;

   ShellExecuteEx( &ShExecInfo );

   hb_strfree( hOperation );
   hb_strfree( hFile );
   hb_strfree( hParameters );
   hb_strfree( hDirectory );

   hb_retnint( (int) ShExecInfo.hInstApp );
}
