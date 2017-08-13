/*
 * Display build information
 *
 * Copyright 1999-2017 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbapi.h"
#include "hbmemory.ch"

void hb_verBuildInfoCB( PHB_OUT_FUNC pOutFunc )
{
   ( pOutFunc )( "Harbour Build Info", 0 );
   ( pOutFunc )( hb_conNewLine(), 0 );
   ( pOutFunc )( "---------------------------", 0 );
   ( pOutFunc )( hb_conNewLine(), 0 );

   {
      char * pszVersion = hb_verHarbour();
      ( pOutFunc )( "Version: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verCompiler();
      ( pOutFunc )( "Compiler: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszVersion = hb_verPlatform();
      ( pOutFunc )( "Platform: ", 0 );
      ( pOutFunc )( pszVersion, 0 );
      ( pOutFunc )( hb_conNewLine(), 0 );
      hb_xfree( pszVersion );
   }

   {
      char * pszPCode = hb_verPCode();
      ( pOutFunc )( pszPCode, 0 );
      ( pOutFunc )( hb_conNewLine(), 0 );
      hb_xfree( pszPCode );
   }

   ( pOutFunc )( "Commit info: ", 0 );
   ( pOutFunc )( hb_verCommitInfo(), 0 );
   ( pOutFunc )( hb_conNewLine(), 0 );

   ( pOutFunc )( "Commit ID: ", 0 );
   ( pOutFunc )( hb_verCommitID(), 0 );
   ( pOutFunc )( hb_conNewLine(), 0 );

   {
      const char * pszFlags = hb_verFlagsPRG();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra Harbour compiler options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsC();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra C compiler options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( hb_conNewLine(), 0 );
      }
   }

   {
      const char * pszFlags = hb_verFlagsL();
      if( pszFlags && *pszFlags )
      {
         ( pOutFunc )( "Extra linker options: ", 0 );
         ( pOutFunc )( pszFlags, 0 );
         ( pOutFunc )( hb_conNewLine(), 0 );
      }
   }

   ( pOutFunc )( "Build options:", 0 );
   if( hb_xquery( HB_MEM_BLOCKS ) != 0 )
      ( pOutFunc )( " (memory tracking)", 0 );
#if defined( HB_TR_INFO ) && ( HB_TR_LEVEL == HB_TR_INFO || HB_TR_LEVEL == HB_TR_DEBUG )
   ( pOutFunc )( " (tracing)", 0 );
#endif
#if ! defined( HB_NO_PROFILER )
   ( pOutFunc )( " (profiler)", 0 );
#endif
#if defined( __cplusplus )
   ( pOutFunc )( " (C++ mode)", 0 );
#endif
#if ! defined( HB_COMPAT_C53 )
   ( pOutFunc )( " (no Clipper 5.3b)", 0 );
#endif
#if ! defined( HB_CLP_UNDOC )
   ( pOutFunc )( " (no Clipper 5.x undoc)", 0 );
#endif
#if defined( HB_CLP_STRICT )
   ( pOutFunc )( " (Clipper 5.x strict)", 0 );
#endif
   ( pOutFunc )( hb_conNewLine(), 0 );

   ( pOutFunc )( "---------------------------", 0 );
   ( pOutFunc )( hb_conNewLine(), 0 );
}

/* deprecated */
void hb_verBuildInfo( void )
{
   hb_verBuildInfoCB( hb_conOutErr );
}
