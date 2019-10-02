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

typedef struct _HB_CURLU
{
   CURLU * curlu;
} HB_CURLU, * PHB_CURLU;

/* Constructor/Destructor */

static void PHB_CURLU_free( PHB_CURLU hb_curlu, HB_BOOL bFree )
{
   if( bFree )
   {
      curl_url_cleanup( hb_curlu->curlu );
      hb_xfree( hb_curlu );
   }
}

/* NOTE: Will create a new one. If 'from' is specified, the new one
         will be based on the 'from' one. */

static PHB_CURLU PHB_CURLU_create( PHB_CURLU from )
{
   CURLU * curlu = from && from->curlu ? curl_url_dup( from->curlu ) : curl_url();

   if( curlu )
   {
      PHB_CURLU hb_curlu = ( PHB_CURLU ) hb_xgrabz( sizeof( HB_CURLU ) );

      hb_curlu->curlu = curlu;

      return hb_curlu;
   }
   else
      return NULL;
}

static HB_GARBAGE_FUNC( PHB_CURLU_release )
{
   PHB_CURLU * hb_curlu_ptr = ( PHB_CURLU * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( hb_curlu_ptr && *hb_curlu_ptr )
   {
      /* Destroy the object */
      PHB_CURLU_free( *hb_curlu_ptr, HB_TRUE );
      *hb_curlu_ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcCURLUFuncs =
{
   PHB_CURLU_release,
   NULL
};

static void PHB_CURLU_ret( PHB_CURLU from )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_CURLU ), &s_gcCURLUFuncs );

   *ph = PHB_CURLU_create( from );

   hb_retptrGC( ph );
}

static void * PHB_CURLU_is( int iParam )
{
   return hb_parptrGC( &s_gcCURLUFuncs, iParam );
}

static PHB_CURLU PHB_CURLU_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLUFuncs, iParam );

   return ph ? ( PHB_CURLU ) *ph : NULL;
}

#endif

/* Harbour interface */

HB_FUNC( CURL_URL )
{
#if LIBCURL_VERSION_NUM >= 0x073E00
   PHB_CURLU_ret( NULL );
#else
   hb_ret();
#endif
}

HB_FUNC( CURL_URL_DUP )
{
#if LIBCURL_VERSION_NUM >= 0x073E00
   if( PHB_CURLU_is( 1 ) )
      PHB_CURLU_ret( PHB_CURLU_par( 1 ) );
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
   if( PHB_CURLU_is( 1 ) )
   {
      PHB_CURLU hb_curlu = PHB_CURLU_par( 1 );

      if( hb_curlu && hb_curlu->curlu )
         res = curl_url_set( hb_curlu->curlu, ( CURLUPart ) hb_parni( 2 ), hb_parc( 3 ), hb_parnint( 4 ) );
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
   if( PHB_CURLU_is( 1 ) )
   {
      PHB_CURLU hb_curlu = PHB_CURLU_par( 1 );

      if( hb_curlu && hb_curlu->curlu )
         res = curl_url_get( hb_curlu->curlu, ( CURLUPart ) hb_parni( 2 ), &part, hb_parnint( 4 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif

   if( HB_ISBYREF( 3 ) )
   {
      hb_storc( part, 3 );
      hb_retnl( ( long ) res );
   }
   else
      hb_retc( part );

   curl_free( part );
}
