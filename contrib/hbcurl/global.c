/*
 * curl_global_*() - Global initialization/de-initialization
 *
 * Copyright 2008-present Viktor Szakats
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

#ifndef CURL_STRICTER
#define CURL_STRICTER
#endif

#include <curl/curl.h>

#include "hbapi.h"
#include "hbapiitm.h"

#include "hbcurl.ch"

static void * hb_curl_xgrab( size_t size )
{
   return size > 0 ? hb_xgrab( size ) : NULL;
}

static void hb_curl_xfree( void * p )
{
   if( p )
      hb_xfree( p );
}

static void * hb_curl_xrealloc( void * p, size_t size )
{
   return size > 0 ? ( p ? hb_xrealloc( p, size ) : hb_xgrab( size ) ) : NULL;
}

static char * hb_curl_strdup( const char * s )
{
   return hb_strdup( s );
}

static void * hb_curl_calloc( size_t nelem, size_t elsize )
{
   size_t size = nelem * elsize;

   return size > 0 ? hb_xgrabz( size ) : NULL;
}

HB_FUNC( CURL_GLOBAL_SSLSET )
{
#if LIBCURL_VERSION_NUM >= 0x073800
   const curl_ssl_backend ** avail = NULL;
   int tmp;
   hb_retni( tmp = curl_global_sslset(
      ( curl_sslbackend ) hb_parni( 1 ),
      hb_parc( 2 ),
      &avail ) );
   if( HB_ISBYREF( 3 ) )
   {
      PHB_ITEM pAvail = hb_hashNew( NULL );
      if( avail )
      {
         PHB_ITEM pKey = hb_itemNew( NULL );
         PHB_ITEM pVal = hb_itemNew( NULL );
         HB_SIZE nLen = 0;
         for( nLen = 0; avail[ nLen ]; ++nLen )
            hb_hashAdd( pAvail,
               hb_itemPutNI( pKey, ( int ) avail[ nLen ]->id ),
               hb_itemPutCConst( pVal, avail[ nLen ]->name ) );
         hb_itemRelease( pVal );
         hb_itemRelease( pKey );
      }
      if( ! hb_itemParamStoreRelease( 3, pAvail ) )
         hb_itemRelease( pAvail );
   }
#else
   hb_retni( HB_CURLSSLSET_NOT_IMPLEMENTED );
   if( HB_ISBYREF( 3 ) )
   {
      PHB_ITEM pAvail = hb_hashNew( NULL );
      if( ! hb_itemParamStoreRelease( 3, pAvail ) )
         hb_itemRelease( pAvail );
   }
#endif
}

HB_FUNC( CURL_GLOBAL_INIT )
{
#if LIBCURL_VERSION_NUM >= 0x070C00
   hb_retnl( ( long ) curl_global_init_mem( hb_parnldef( 1, CURL_GLOBAL_ALL ),
                                            hb_curl_xgrab,
                                            hb_curl_xfree,
                                            hb_curl_xrealloc,
                                            hb_curl_strdup,
                                            hb_curl_calloc ) );
#else
   hb_retnl( ( long ) curl_global_init( hb_parnldef( 1, CURL_GLOBAL_ALL ) ) );
#endif
}

HB_FUNC( CURL_GLOBAL_CLEANUP )
{
   curl_global_cleanup();
}
