/*
 * libcurl URL API - Harbour interface.
 *
 * Copyright 2019-present Viktor Szakats
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
#include "hbapierr.h"

#include "hbcurl.ch"

#if LIBCURL_VERSION_NUM >= 0x073E00

/* Constructor/Destructor */

static HB_GARBAGE_FUNC( hb_CURLU_release )
{
   CURLU ** ph = ( CURLU ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( *ph )
   {
      /* Destroy the object */
      curl_url_cleanup( *ph );
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcCURLUFuncs =
{
   hb_CURLU_release,
   NULL
};

static void hb_CURLU_ret( CURLU * from )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( CURLU * ), &s_gcCURLUFuncs );

   *ph = from ? curl_url_dup( from ) : curl_url();

   hb_retptrGC( ph );
}

static void * hb_CURLU_is( int iParam )
{
   return hb_parptrGC( &s_gcCURLUFuncs, iParam );
}

static CURLU * hb_CURLU_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLUFuncs, iParam );

   return ph ? ( CURLU * ) *ph : NULL;
}

#endif

/* Harbour interface */

HB_FUNC( CURL_URL )
{
#if LIBCURL_VERSION_NUM >= 0x073E00
   hb_CURLU_ret( NULL );
#else
   hb_ret();
#endif
}

HB_FUNC( CURL_URL_DUP )
{
#if LIBCURL_VERSION_NUM >= 0x073E00
   if( hb_CURLU_is( 1 ) )
      hb_CURLU_ret( hb_CURLU_par( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_ret();
#endif
}

HB_FUNC( CURL_URL_SET )
{
   CURLUcode res = ( CURLUcode ) HB_CURLUE_ERROR;

#if LIBCURL_VERSION_NUM >= 0x073E00
   if( hb_CURLU_is( 1 ) )
   {
      CURLU * url = hb_CURLU_par( 1 );

      if( url )
      {
         #ifdef HB_CURL_UTF8
         void * hValue;
         res = curl_url_set( url, ( CURLUPart ) hb_parni( 2 ), hb_parstr_utf8( 3, &hValue, NULL ), hb_parnint( 4 ) );
         hb_strfree( hValue );
         #else
         res = curl_url_set( url, ( CURLUPart ) hb_parni( 2 ), hb_parc( 3 ), hb_parnint( 4 ) );
         #endif
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif

   hb_retnl( ( long ) res );
}

HB_FUNC( CURL_URL_GET )
{
   CURLUcode res = ( CURLUcode ) HB_CURLUE_ERROR;
   char * part = NULL;

#if LIBCURL_VERSION_NUM >= 0x073E00
   if( hb_CURLU_is( 1 ) )
   {
      CURLU * url = hb_CURLU_par( 1 );

      if( url )
         res = curl_url_get( url, ( CURLUPart ) hb_parni( 2 ), &part, hb_parnint( 4 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif

   if( HB_ISBYREF( 3 ) )
   {
      #ifdef HB_CURL_UTF8
      hb_storstr_utf8( part, 3 );
      #else
      hb_storc( part, 3 );
      #endif
      hb_retnl( ( long ) res );
   }
   else
      hb_retc( part );

   curl_free( part );
}

HB_FUNC( CURL_URL_STRERROR )
{
#if LIBCURL_VERSION_NUM >= 0x075000
   hb_retc_const( curl_url_strerror( ( CURLUcode ) hb_parnl( 1 ) ) );
#else
   hb_retc_null();
#endif
}
