/*
 * curl_version()/curl_version_info()
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
#if LIBCURL_VERSION_NUM < 0x070C00
   #include <curl/types.h>
#endif

#include "hbapi.h"
#include "hbapiitm.h"

#include "hbcurl.ch"

HB_FUNC( CURL_VERSION )
{
   hb_retc( curl_version() );
}

HB_FUNC( CURL_VERSION_INFO )
{
   curl_version_info_data * data = curl_version_info( CURLVERSION_NOW );

   if( data )
   {
      PHB_ITEM pArray = hb_itemArrayNew( HB_CURLVERINFO_LEN );

      hb_arraySetC(  pArray, HB_CURLVERINFO_VERSION       , data->version );                      /* LIBCURL_VERSION */
      hb_arraySetNI( pArray, HB_CURLVERINFO_VERSION_NUM   , data->version_num );                  /* LIBCURL_VERSION_NUM */
      hb_arraySetC(  pArray, HB_CURLVERINFO_HOST          , data->host );                         /* OS/host/cpu/machine when configured */
      hb_arraySetNI( pArray, HB_CURLVERINFO_FEATURES      , data->features );                     /* bitmask, see defines below */
      hb_arraySetC(  pArray, HB_CURLVERINFO_SSLVERSION    , data->ssl_version );                  /* human readable string */
      hb_arraySetNI( pArray, HB_CURLVERINFO_SSLVERSION_NUM, data->ssl_version_num );              /* not used anymore, always 0 */
      hb_arraySetC(  pArray, HB_CURLVERINFO_LIBZ_VERSION  , data->libz_version );                 /* human readable string */
#if defined( CURLVERSION_SECOND )
      hb_arraySetC(  pArray, HB_CURLVERINFO_ARES          , data->age >= CURLVERSION_SECOND ? data->ares : NULL );
      hb_arraySetNI( pArray, HB_CURLVERINFO_ARES_NUM      , data->age >= CURLVERSION_SECOND ? data->ares_num : 0 );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_ARES          , NULL );
      hb_arraySetNI( pArray, HB_CURLVERINFO_ARES_NUM      , 0 );
#endif
#if defined( CURLVERSION_THIRD )
      hb_arraySetC(  pArray, HB_CURLVERINFO_LIBIDN        , data->age >= CURLVERSION_THIRD ? data->libidn : NULL );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_LIBIDN        , NULL );
#endif
#if defined( CURLVERSION_FOURTH )
      hb_arraySetNI( pArray, HB_CURLVERINFO_ICONV_VER_NUM , data->age >= CURLVERSION_FOURTH ? data->iconv_ver_num : 0 ); /* Same as '_libiconv_version' if built with HAVE_ICONV */
#else
      hb_arraySetNI( pArray, HB_CURLVERINFO_ICONV_VER_NUM , 0 );
#endif
#if defined( CURLVERSION_FOURTH ) && LIBCURL_VERSION_NUM >= 0x071001
      hb_arraySetC(  pArray, HB_CURLVERINFO_LIBSSH_VERSION, data->age >= CURLVERSION_FOURTH ? data->libssh_version : NULL ); /* human readable string */
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_LIBSSH_VERSION, NULL );
#endif
#if defined( CURLVERSION_FIFTH )
      hb_arraySetNI( pArray, HB_CURLVERINFO_BROTLI_VER_NUM, data->age >= CURLVERSION_FIFTH ? data->brotli_ver_num : 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_BROTLI_VERSION, data->age >= CURLVERSION_FIFTH ? data->brotli_version : NULL );
#else
      hb_arraySetNI( pArray, HB_CURLVERINFO_BROTLI_VER_NUM, 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_BROTLI_VERSION, NULL );
#endif
#if defined( CURLVERSION_SIXTH )
      hb_arraySetNI( pArray, HB_CURLVERINFO_NGHTTP2_VER_NUM, data->age >= CURLVERSION_SIXTH ? data->nghttp2_ver_num : 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_NGHTTP2_VERSION, data->age >= CURLVERSION_SIXTH ? data->nghttp2_version : NULL );
      hb_arraySetC(  pArray, HB_CURLVERINFO_QUIC_VERSION, data->age >= CURLVERSION_SIXTH ? data->quic_version : NULL );
#else
      hb_arraySetNI( pArray, HB_CURLVERINFO_NGHTTP2_VER_NUM, 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_NGHTTP2_VERSION, NULL );
      hb_arraySetC(  pArray, HB_CURLVERINFO_QUIC_VERSION, NULL );
#endif
#if defined( CURLVERSION_SEVENTH )
      hb_arraySetC(  pArray, HB_CURLVERINFO_CAINFO, data->age >= CURLVERSION_SEVENTH ? data->cainfo : NULL );
      hb_arraySetC(  pArray, HB_CURLVERINFO_CAPATH, data->age >= CURLVERSION_SEVENTH ? data->capath : NULL );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_CAINFO, NULL );
      hb_arraySetC(  pArray, HB_CURLVERINFO_CAPATH, NULL );
#endif
#if defined( CURLVERSION_EIGHTH )
      hb_arraySetNI( pArray, HB_CURLVERINFO_ZSTD_VER_NUM, data->age >= CURLVERSION_EIGHTH ? data->zstd_ver_num : 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_ZSTD_VERSION, data->age >= CURLVERSION_EIGHTH ? data->zstd_version : NULL );
#else
      hb_arraySetNI( pArray, HB_CURLVERINFO_ZSTD_VER_NUM, 0 );
      hb_arraySetC(  pArray, HB_CURLVERINFO_ZSTD_VERSION, NULL );
#endif
#if defined( CURLVERSION_NINTH )
      hb_arraySetC(  pArray, HB_CURLVERINFO_HYPER_VERSION, data->age >= CURLVERSION_NINETH ? data->hyper_version : NULL );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_HYPER_VERSION, NULL );
#endif
#if defined( CURLVERSION_TENTH )
      hb_arraySetC(  pArray, HB_CURLVERINFO_GSASL_VERSION, data->age >= CURLVERSION_TENTH ? data->gsasl_version : NULL );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_GSASL_VERSION, NULL );
#endif
#if defined( CURLVERSION_ELEVENTH )
      if( data->age >= CURLVERSION_ELEVENTH )
      {
         PHB_ITEM pList;
         int      nCount = 0;
         const char * const * item = data->feature_names;

         while( *( item++ ) )
            nCount++;

         pList = hb_arrayGetItemPtr( pArray, HB_CURLVERINFO_FEATURE_NAMES );
         hb_arrayNew( pList, nCount );

         for( item = data->feature_names, nCount = 1; *item; item++ )
            hb_arraySetC( pList, nCount++, *item );
      }
#endif
#if defined( CURLVERSION_TWELFTH )
      hb_arraySetC(  pArray, HB_CURLVERINFO_RTMP_VERSION, data->age >= CURLVERSION_TWELFTH ? data->rtmp_version : NULL );
#else
      hb_arraySetC(  pArray, HB_CURLVERINFO_RTMP_VERSION, NULL );
#endif
      {
         PHB_ITEM pList;
         int      nCount = 0;
         const char * const * item = data->protocols;

         while( *( item++ ) )
            nCount++;

         pList = hb_arrayGetItemPtr( pArray, HB_CURLVERINFO_PROTOCOLS );
         hb_arrayNew( pList, nCount );

         for( item = data->protocols, nCount = 1; *item; item++ )
            hb_arraySetC( pList, nCount++, *item );
      }

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}
