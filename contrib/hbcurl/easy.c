/*
 * libcurl 'easy' API - Harbour interface.
 *
 * Copyright 2008-present Viktor Szakats
 * originally based on:
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
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
#if LIBCURL_VERSION_NUM < 0x070A03
   #include <curl/easy.h>
#endif
#if LIBCURL_VERSION_NUM < 0x070C00
   #include <curl/types.h>
#endif

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbhash.h"

#include "hbcurl.ch"

#define HB_CURL_OPT_BOOL( n )      ( HB_ISLOG( n ) ? ( long ) hb_parl( n ) : hb_parnldef( n, 1 ) )
#define HB_CURL_OPT_LARGENUM( n )  ( ( curl_off_t ) hb_parnint( n ) )

/* NOTE: Harbour requires libcurl 7.17.0 or upper.
         make copies of passed strings, which we currently require.
         [vszakats] */

#if LIBCURL_VERSION_NUM < 0x071100
   #error "libcurl 7.17.0 or upper required"
#endif

/* Fall back to return simple error if special abort signal is not available. */
#if ! defined( CURL_READFUNC_ABORT ) /* Introduced in LIBCURL_VERSION_NUM >= 0x070C01 */
   #define CURL_READFUNC_ABORT  ( ( size_t ) -1 )
#endif

#define HB_CURL_ONFREE_NOOP     0
#define HB_CURL_ONFREE_CLOSE    1
#define HB_CURL_ONFREE_DETACH   2

typedef struct _HB_CURL
{
   CURL * curl;

   struct curl_httppost * pHTTPPOST_First;
   struct curl_httppost * pHTTPPOST_Last;
   struct curl_slist *    pHTTPHEADER;
   struct curl_slist *    pHTTP200ALIASES;
   struct curl_slist *    pQUOTE;
   struct curl_slist *    pPOSTQUOTE;
   struct curl_slist *    pPREQUOTE;
   struct curl_slist *    pTELNETOPTIONS;
   struct curl_slist *    pMAIL_RCPT;
   struct curl_slist *    pRESOLVE;
   struct curl_slist *    pPROXYHEADER;
   struct curl_slist *    pCONNECT_TO;
#if LIBCURL_VERSION_NUM >= 0x073800
   curl_mime *            mime;
#endif

   char *   ul_name;
   PHB_FILE ul_file;
   int      ul_onfree;

   char *   dl_name;
   PHB_FILE dl_file;
   int      dl_onfree;

   unsigned char * ul_ptr;
   size_t          ul_len;
   size_t          ul_pos;

   unsigned char * dl_ptr;
   size_t          dl_len;
   size_t          dl_pos;

   PHB_ITEM pXferInfoCallback;
   PHB_ITEM pDebugCallback;

   PHB_HASH_TABLE pHash;

} HB_CURL, * PHB_CURL;

/* Callbacks */

static size_t hb_curl_read_dummy_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   HB_SYMBOL_UNUSED( buffer );
   HB_SYMBOL_UNUSED( size );
   HB_SYMBOL_UNUSED( nmemb );
   HB_SYMBOL_UNUSED( Cargo );

   return 0;
}

static size_t hb_curl_read_file_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      if( hb_curl->ul_file == NULL && hb_curl->ul_name )
      {
         hb_curl->ul_file = hb_fileExtOpen( hb_curl->ul_name, NULL,
                                            FO_READ | FO_SHARED | FO_PRIVATE |
                                            FXO_SHARELOCK | FXO_NOSEEKPOS,
                                            NULL, NULL );

         if( hb_curl->ul_file == NULL )
            return ( size_t ) -1;

         hb_curl->ul_onfree = HB_CURL_ONFREE_CLOSE;
      }

      if( hb_curl->ul_file )
      {
         size_t ret = ( size_t ) hb_fileRead( hb_curl->ul_file, buffer, size * nmemb, -1 );

         return ( ret == ( size_t ) FS_ERROR || hb_fsError() ) ? CURL_READFUNC_ABORT : ret;
      }
   }

   return ( size_t ) -1;
}

static size_t hb_curl_read_buff_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      size_t nTodo = size * nmemb;
      size_t nLeft = hb_curl->ul_len - hb_curl->ul_pos;

      if( nTodo > nLeft )
         nTodo = nLeft;

      hb_xmemcpy( buffer, hb_curl->ul_ptr + hb_curl->ul_pos, nTodo );

      hb_curl->ul_pos += nTodo;

      return nTodo;
   }

   return ( size_t ) -1;
}

static size_t hb_curl_write_file_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      if( hb_curl->dl_file == NULL && hb_curl->dl_name )
      {
         hb_curl->dl_file = hb_fileExtOpen( hb_curl->dl_name, NULL,
                                            FO_WRITE | FO_EXCLUSIVE | FO_PRIVATE |
                                            FXO_TRUNCATE | FXO_SHARELOCK | FXO_NOSEEKPOS,
                                            NULL, NULL );

         if( hb_curl->dl_file == NULL )
            return ( size_t ) -1;

         hb_curl->dl_onfree = HB_CURL_ONFREE_CLOSE;
      }

      if( hb_curl->dl_file )
      {
         HB_SIZE nDone;
         if( ( nDone = hb_fileWrite( hb_curl->dl_file, buffer, size * nmemb, -1 ) ) != ( HB_SIZE ) FS_ERROR )
            return ( size_t ) nDone;
      }
   }

   return ( size_t ) -1;
}

#define HB_CURL_DL_BUFF_SIZE_INIT  ( CURL_MAX_WRITE_SIZE * 4 )
#define HB_CURL_DL_BUFF_SIZE_INCR  ( CURL_MAX_WRITE_SIZE * 4 )

static size_t hb_curl_write_buff_callback( void * buffer, size_t size, size_t nmemb, void * Cargo )
{
   PHB_CURL hb_curl = ( PHB_CURL ) Cargo;

   if( hb_curl )
   {
      size_t nTodo = size * nmemb;
      size_t nLeft = hb_curl->dl_len - hb_curl->dl_pos;

      if( nTodo > nLeft )
      {
         hb_curl->dl_len += HB_CURL_DL_BUFF_SIZE_INCR;
         hb_curl->dl_ptr  = ( unsigned char * ) hb_xrealloc( hb_curl->dl_ptr, hb_curl->dl_len );
      }

      hb_xmemcpy( hb_curl->dl_ptr + hb_curl->dl_pos, buffer, nTodo );

      hb_curl->dl_pos += nTodo;

      return nTodo;
   }

   return ( size_t ) -1;
}

#if LIBCURL_VERSION_NUM >= 0x072000
static int hb_curl_xferinfo_callback( void * Cargo, curl_off_t dltotal, curl_off_t dlnow, curl_off_t ultotal, curl_off_t ulnow )
{
   int result = 0;

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushNumInt( ( HB_FOFFSET ) ( ulnow > 0 ? ulnow : dlnow ) );
         hb_vmPushNumInt( ( HB_FOFFSET ) ( ultotal > 0 ? ultotal : dltotal ) );
         hb_vmSend( 2 );

         if( hb_parl( -1 ) )
            result = 1;  /* Abort */

         hb_vmRequestRestore();
      }
   }

   return result;
}
#else
static int hb_curl_progress_callback( void * Cargo, double dltotal, double dlnow, double ultotal, double ulnow )
{
   int result = 0;

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushDouble( ulnow > 0 ? ulnow : dlnow, HB_DEFAULT_DECIMALS );
         hb_vmPushDouble( ultotal > 0 ? ultotal : dltotal, HB_DEFAULT_DECIMALS );
         hb_vmSend( 2 );

         if( hb_parl( -1 ) )
            result = 1;  /* Abort */

         hb_vmRequestRestore();
      }
   }

   return result;
}
#endif

#if LIBCURL_VERSION_NUM >= 0x070906
static int hb_curl_debug_callback( CURL * curl, curl_infotype type, char * data, size_t size, void * Cargo )
{
   HB_SYMBOL_UNUSED( curl );

   if( Cargo )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( ( PHB_ITEM ) Cargo );
         hb_vmPushInteger( ( int ) type );
         hb_vmPushString( data, ( HB_SIZE ) size );
         hb_vmSend( 2 );

         hb_vmRequestRestore();
      }
   }

   return 0;
}
#endif

/* Helpers */

static void hb_curl_form_free( struct curl_httppost ** ptr )
{
   if( ptr && *ptr )
   {
      curl_formfree( *ptr );
      *ptr = NULL;
   }
}

static void hb_curl_slist_free( struct curl_slist ** ptr )
{
   if( ptr && *ptr )
   {
      curl_slist_free_all( *ptr );
      *ptr = NULL;
   }
}

static void hb_curl_ul_free( PHB_CURL hb_curl )
{
   if( hb_curl )
   {
      if( hb_curl->ul_name )
      {
         hb_xfree( hb_curl->ul_name );
         hb_curl->ul_name = NULL;
      }

      if( hb_curl->ul_file )
      {
         if( hb_curl->ul_onfree == HB_CURL_ONFREE_CLOSE )
            hb_fileClose( hb_curl->ul_file );
         else if( hb_curl->ul_onfree == HB_CURL_ONFREE_DETACH )
            hb_fileDetach( hb_curl->ul_file );

         hb_curl->ul_file = NULL;
      }

      hb_curl->ul_onfree = HB_CURL_ONFREE_NOOP;

      if( hb_curl->ul_ptr )
      {
         hb_xfree( hb_curl->ul_ptr );
         hb_curl->ul_ptr = NULL;
         hb_curl->ul_len = 0;
         hb_curl->ul_pos = 0;
      }
   }
}

static void hb_curl_dl_free( PHB_CURL hb_curl )
{
   if( hb_curl )
   {
      if( hb_curl->dl_name )
      {
         hb_xfree( hb_curl->dl_name );
         hb_curl->dl_name = NULL;
      }

      if( hb_curl->dl_file )
      {
         if( hb_curl->dl_onfree == HB_CURL_ONFREE_CLOSE )
            hb_fileClose( hb_curl->dl_file );
         else if( hb_curl->dl_onfree == HB_CURL_ONFREE_DETACH )
            hb_fileDetach( hb_curl->dl_file );

         hb_curl->dl_file = NULL;
      }

      hb_curl->dl_onfree = HB_CURL_ONFREE_NOOP;

      if( hb_curl->dl_ptr )
      {
         hb_xfree( hb_curl->dl_ptr );
         hb_curl->dl_ptr = NULL;
         hb_curl->dl_len = 0;
         hb_curl->dl_pos = 0;
      }
   }
}

/* Constructor/Destructor */

static void PHB_CURL_free( PHB_CURL hb_curl, HB_BOOL bFree )
{
   curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );

   /* Some extra safety. Set these to NULL, before freeing their pointers. */
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, NULL );
   curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, NULL );
#if LIBCURL_VERSION_NUM >= 0x071400
   curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x071503
   curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x072500
   curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
   curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, NULL );
#endif
#if LIBCURL_VERSION_NUM >= 0x073800
   curl_easy_setopt( hb_curl->curl, CURLOPT_MIMEPOST, NULL );
#endif

   hb_curl_form_free( &hb_curl->pHTTPPOST_First );
   hb_curl->pHTTPPOST_Last = NULL;
   hb_curl_slist_free( &hb_curl->pHTTPHEADER );
   hb_curl_slist_free( &hb_curl->pHTTP200ALIASES );
   hb_curl_slist_free( &hb_curl->pQUOTE );
   hb_curl_slist_free( &hb_curl->pPOSTQUOTE );
   hb_curl_slist_free( &hb_curl->pPREQUOTE );
   hb_curl_slist_free( &hb_curl->pTELNETOPTIONS );
   hb_curl_slist_free( &hb_curl->pMAIL_RCPT );
   hb_curl_slist_free( &hb_curl->pRESOLVE );
   hb_curl_slist_free( &hb_curl->pPROXYHEADER );
   hb_curl_slist_free( &hb_curl->pCONNECT_TO );
#if LIBCURL_VERSION_NUM >= 0x073800
   if( hb_curl->mime )
      curl_mime_free( hb_curl->mime );
#endif

   hb_curl_ul_free( hb_curl );
   hb_curl_dl_free( hb_curl );

   if( hb_curl->pXferInfoCallback )
   {
      hb_itemRelease( hb_curl->pXferInfoCallback );
      hb_curl->pXferInfoCallback = NULL;
   }

   if( hb_curl->pDebugCallback )
   {
      hb_itemRelease( hb_curl->pDebugCallback );
      hb_curl->pDebugCallback = NULL;
   }

   if( hb_curl->pHash )
   {
      hb_hashTableKill( hb_curl->pHash );
      hb_curl->pHash = NULL;
   }

   if( bFree )
   {
      curl_easy_cleanup( hb_curl->curl );
      hb_xfree( hb_curl );
   }
#if LIBCURL_VERSION_NUM >= 0x070C01
   else
      curl_easy_reset( hb_curl->curl );
#endif
}

/* NOTE: Will create a new one. If 'from' is specified, the new one
         will be based on the 'from' one. */

static PHB_CURL PHB_CURL_create( PHB_CURL from )
{
   CURL * curl = from && from->curl ? curl_easy_duphandle( from->curl ) : curl_easy_init();

   if( curl )
   {
      PHB_CURL hb_curl = ( PHB_CURL ) hb_xgrabz( sizeof( HB_CURL ) );

      hb_curl->curl = curl;

      return hb_curl;
   }
   else
      return NULL;
}

static HB_GARBAGE_FUNC( PHB_CURL_release )
{
   PHB_CURL * hb_curl_ptr = ( PHB_CURL * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( hb_curl_ptr && *hb_curl_ptr )
   {
      /* Destroy the object */
      PHB_CURL_free( *hb_curl_ptr, HB_TRUE );
      *hb_curl_ptr = NULL;
   }
}

static HB_GARBAGE_FUNC( PHB_CURL_mark )
{
   PHB_CURL * hb_curl_ptr = ( PHB_CURL * ) Cargo;

   if( hb_curl_ptr && *hb_curl_ptr )
   {
      PHB_CURL hb_curl = *hb_curl_ptr;

      if( hb_curl->pXferInfoCallback )
         hb_gcMark( hb_curl->pXferInfoCallback );

      if( hb_curl->pDebugCallback )
         hb_gcMark( hb_curl->pDebugCallback );
   }
}

static const HB_GC_FUNCS s_gcCURLFuncs =
{
   PHB_CURL_release,
   PHB_CURL_mark
};


static void PHB_CURL_ret( PHB_CURL from )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_CURL ), &s_gcCURLFuncs );

   *ph = PHB_CURL_create( from );

   hb_retptrGC( ph );
}

static void * PHB_CURL_is( int iParam )
{
   return hb_parptrGC( &s_gcCURLFuncs, iParam );
}

static PHB_CURL PHB_CURL_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLFuncs, iParam );

   return ph ? ( PHB_CURL ) *ph : NULL;
}

/* Harbour interface */

HB_FUNC( CURL_EASY_INIT )
{
   PHB_CURL_ret( NULL );
}

HB_FUNC( CURL_EASY_DUPLICATE )
{
   if( PHB_CURL_is( 1 ) )
      PHB_CURL_ret( PHB_CURL_par( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_CLEANUP )
{
   if( PHB_CURL_is( 1 ) )
   {
      void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLFuncs, 1 );

      if( ph && *ph )
      {
         /* Destroy the object */
         PHB_CURL_free( ( PHB_CURL ) *ph, HB_TRUE );
         *ph = NULL;
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_RESET )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         PHB_CURL_free( hb_curl, HB_FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_PAUSE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x071200
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      hb_retnl( hb_curl ? ( long ) curl_easy_pause( hb_curl->curl, hb_parni( 2 ) ) : HB_CURLE_ERROR );
#else
      hb_retnl( HB_CURLE_ERROR );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_PERFORM )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      hb_retnl( hb_curl ? ( long ) curl_easy_perform( hb_curl->curl ) : HB_CURLE_ERROR );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: curl_easy_send( curl, cBuffer, @nSentBytes ) --> nResult */
HB_FUNC( CURL_EASY_SEND )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res = ( CURLcode ) HB_CURLE_ERROR;
#if LIBCURL_VERSION_NUM >= 0x071202
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         size_t size = 0;

         res = curl_easy_send( hb_curl->curl, hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ), &size );

         hb_storns( size, 3 );
      }
#endif
      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: curl_easy_recv( curl, @cBuffer ) --> nResult */
HB_FUNC( CURL_EASY_RECV )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res = ( CURLcode ) HB_CURLE_ERROR;
#if LIBCURL_VERSION_NUM >= 0x071202
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
      {
         size_t size = ( size_t ) hb_parclen( 2 );
         void * buffer;

         if( size < 1024 )
            size = 1024;

         buffer = hb_xgrab( size + 1 );

         res = curl_easy_recv( hb_curl->curl, buffer, size, &size );

         if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
            hb_xfree( buffer );
      }
#endif
      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: curl_easy_upkeep( curl ) --> nResult */
HB_FUNC( CURL_EASY_UPKEEP )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res = ( CURLcode ) HB_CURLE_ERROR;
#if LIBCURL_VERSION_NUM >= 0x073E00
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         res = curl_easy_upkeep( hb_curl->curl );
#endif
      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if LIBCURL_VERSION_NUM >= 0x073800

/* #define HB_CURL_MIME_TRACE */

/* Convert string array to curl_slist */
static struct curl_slist * hb_curl_opt_mime_header( PHB_ITEM pArray )
{
   struct curl_slist * headers = NULL;

   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE nPos, nLen = hb_arrayLen( pArray );

      for( nPos = 0; nPos < nLen; ++nPos )
      {
         struct curl_slist * temp = curl_slist_append( headers, hb_arrayGetCPtr( pArray, nPos + 1 ) );

         if( temp == NULL )
         {
            hb_curl_slist_free( &headers );
            break;
         }
         else
            headers = temp;
      }
   }

   return headers;
}

static curl_mime * hb_curl_opt_mime( CURL * curl, PHB_ITEM pItem );

/* Convert a Harbour hash describing a MIME part to a curl MIME part
   and add it to the MIME part list */
static void hb_curl_opt_mime_addpart( CURL * curl, curl_mime * mime, PHB_ITEM pHash )
{
   if( pHash && HB_IS_HASH( pHash ) )
   {
      HB_SIZE nLen = hb_hashLen( pHash );

      if( nLen > 0 )
      {
         curl_mimepart * part = curl_mime_addpart( mime );
         HB_SIZE nPos;

         #ifdef HB_CURL_MIME_TRACE
         printf( "curl_mime_addpart( %p )\n", ( void * ) mime );
         #endif

         for( nPos = 0; nPos < nLen; ++nPos )
         {
            PHB_ITEM pKey = hb_hashGetKeyAt( pHash, nPos + 1 );
            PHB_ITEM pVal = hb_hashGetValueAt( pHash, nPos + 1 );

            if( pKey && pVal )
            {
               const char * szKey = hb_itemGetCPtr( pKey );

               if( szKey )
               {
                  if(      hb_stricmp( szKey, "subparts" ) == 0 )
                     curl_mime_subparts( part, hb_curl_opt_mime( curl, pVal ) );
                  else if( hb_stricmp( szKey, "headers" ) == 0 )
                     curl_mime_headers( part, hb_curl_opt_mime_header( pVal ), 1 /* take ownership */ );
                  else if( ! HB_IS_STRING( pVal ) )
                     continue;
                  else if( hb_stricmp( szKey, "name" ) == 0 )
                     curl_mime_name( part, hb_itemGetCPtr( pVal ) );  /* cp:? */
                  else if( hb_stricmp( szKey, "filename" ) == 0 )
                     curl_mime_filename( part, hb_itemGetCPtr( pVal ) );  /* cp:UTF-8 */
                  else if( hb_stricmp( szKey, "type" ) == 0 )
                     curl_mime_type( part, hb_itemGetCPtr( pVal ) );  /* cp:ASCII */
                  else if( hb_stricmp( szKey, "encoder" ) == 0 )
                     curl_mime_encoder( part, hb_itemGetCPtr( pVal ) );  /* cp:ASCII */
                  else if( hb_stricmp( szKey, "filedata" ) == 0 )
                     curl_mime_filedata( part, hb_itemGetCPtr( pVal ) );  /* cp:ASCII? */
                  else if( hb_stricmp( szKey, "data" ) == 0 )
                     curl_mime_data( part, hb_itemGetCPtr( pVal ), ( size_t ) hb_itemGetCLen( pVal ) );  /* cp:binary */
                  else if( hb_stricmp( szKey, "datablock" ) == 0 )
                  {
                     #if 0  /* TODO: Implement callback-based data input */
                     curl_mime_data_cb( part,
                                        curl_off_t datasize,
                                        curl_read_callback readfunc,
                                        curl_seek_callback seekfunc,
                                        curl_free_callback freefunc,
                                        void * arg );
                     #endif
                  }

                  #ifdef HB_CURL_MIME_TRACE
                  printf( "curl_mime_%s( \"%s\" )\n", szKey, hb_itemGetCPtr( pVal ) );
                  #endif
               }
            }
         }
      }
   }
}

/* Convert an array of MIME parts to curl MIME structure.
   If it's not an array, but a hash, consider it a single MIME part. */
static curl_mime * hb_curl_opt_mime( CURL * curl, PHB_ITEM pItem )
{
   curl_mime * mime = NULL;

   if( pItem )
   {
      if( HB_IS_ARRAY( pItem ) )
      {
         HB_SIZE nLen = hb_arrayLen( pItem );

         if( nLen > 0 )
         {
            HB_SIZE nPos;

            mime = curl_mime_init( curl );

            #ifdef HB_CURL_MIME_TRACE
            printf( "curl_mime_init()\n" );
            #endif

            for( nPos = 0; nPos < nLen; ++nPos )
               hb_curl_opt_mime_addpart( curl, mime, hb_arrayGetItemPtr( pItem, nPos + 1 ) );
         }
      }
      else if( HB_IS_HASH( pItem ) )
      {
         mime = curl_mime_init( curl );

         #ifdef HB_CURL_MIME_TRACE
         printf( "curl_mime_init()\n" );
         #endif

         hb_curl_opt_mime_addpart( curl, mime, pItem );
      }
   }

   return mime;
}
#endif

#if LIBCURL_VERSION_NUM >= 0x074700
CURLcode hb_curl_easy_setopt_blob( CURL * curl, CURLoption option, PHB_ITEM item )
{
   if( item )
   {
      struct curl_blob blob;

      blob.data  = HB_UNCONST( hb_itemGetCPtr( item ) );
      blob.len   = hb_itemGetCLen( item );
      blob.flags = CURL_BLOB_COPY;

      return curl_easy_setopt( curl, option, &blob );
   }

   return curl_easy_setopt( curl, option, NULL );
}
#endif

HB_FUNC( CURL_EASY_SETOPT )
{
   if( PHB_CURL_is( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      CURLcode res     = ( CURLcode ) HB_CURLE_ERROR;

      if( hb_curl )
      {
         switch( hb_parni( 2 ) )
         {
            /* Behavior */

            case HB_CURLOPT_VERBOSE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_VERBOSE, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HEADER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HEADER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_NOPROGRESS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOPROGRESS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_NOSIGNAL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOSIGNAL, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071500
            case HB_CURLOPT_WILDCARDMATCH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_WILDCARDMATCH, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x075600
            case HB_CURLOPT_WS_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_WS_OPTIONS, hb_parnl( 3 ) );
               break;
#endif

            /* Callback */

            /* These are hidden on the Harbour level: */
            /* HB_CURLOPT_WRITEFUNCTION */
            /* HB_CURLOPT_WRITEDATA */
            /* HB_CURLOPT_READFUNCTION */
            /* HB_CURLOPT_READDATA */
#if LIBCURL_VERSION_NUM >= 0x070C03
            /* HB_CURLOPT_IOCTLFUNCTION */
            /* HB_CURLOPT_IOCTLDATA */
#endif
            /* HB_CURLOPT_SEEKFUNCTION */
            /* HB_CURLOPT_SEEKDATA */
            /* HB_CURLOPT_SOCKOPTFUNCTION */
            /* HB_CURLOPT_SOCKOPTDATA */
            /* HB_CURLOPT_OPENSOCKETFUNCTION */
            /* HB_CURLOPT_OPENSOCKETDATA */
            /* HB_CURLOPT_PROGRESSFUNCTION */
            /* HB_CURLOPT_PROGRESSDATA */
            /* HB_CURLOPT_HEADERFUNCTION */
            /* HB_CURLOPT_HEADERDATA / CURLOPT_WRITEHEADER */
#if LIBCURL_VERSION_NUM >= 0x070906
            /* HB_CURLOPT_DEBUGFUNCTION */
            /* HB_CURLOPT_DEBUGDATA */
#endif
#if LIBCURL_VERSION_NUM >= 0x070B00
            /* HB_CURLOPT_SSL_CTX_FUNCTION */
            /* HB_CURLOPT_SSL_CTX_DATA */
#endif
            /* HB_CURLOPT_CONV_TO_NETWORK_FUNCTION */
            /* HB_CURLOPT_CONV_FROM_NETWORK_FUNCTION */
            /* HB_CURLOPT_CONV_FROM_UTF8_FUNCTION */

            /* Error */

            /* HB_CURLOPT_ERRORBUFFER */
            /* HB_CURLOPT_STDERR */

            case HB_CURLOPT_FAILONERROR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FAILONERROR, HB_CURL_OPT_BOOL( 3 ) );
               break;

#if LIBCURL_VERSION_NUM >= 0x073300
            case HB_CURLOPT_KEEP_SENDING_ON_ERROR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KEEP_SENDING_ON_ERROR, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif

            /* Network */

            /* This is the only option that must be set before curl_easy_perform() is called. */
            case HB_CURLOPT_URL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_URL, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_PROXY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_PROXYPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYPORT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_PROXYTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYTYPE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073600
            case HB_CURLOPT_SUPPRESS_CONNECT_HEADERS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SUPPRESS_CONNECT_HEADERS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_HTTPPROXYTUNNEL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPROXYTUNNEL, HB_CURL_OPT_BOOL( 3 ) );
               break;

            case HB_CURLOPT_INTERFACE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INTERFACE, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
#if LIBCURL_VERSION_NUM >= 0x070F02
            case HB_CURLOPT_LOCALPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOCALPORT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_LOCALPORTRANGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOCALPORTRANGE, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_DNS_CACHE_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_CACHE_TIMEOUT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x075700
            case HB_CURLOPT_CA_CACHE_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CA_CACHE_TIMEOUT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_QUICK_EXIT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_QUICK_EXIT, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A00
            case HB_CURLOPT_BUFFERSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_BUFFERSIZE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073E00
            case HB_CURLOPT_UPLOAD_BUFFERSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD_BUFFERSIZE, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_PORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PORT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070B02
            case HB_CURLOPT_TCP_NODELAY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_NODELAY, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_TCP_FASTOPEN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_FASTOPEN, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071300
            case HB_CURLOPT_ADDRESS_SCOPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ADDRESS_SCOPE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072D00
            case HB_CURLOPT_DEFAULT_PROTOCOL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DEFAULT_PROTOCOL, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x075500
            case HB_CURLOPT_PROTOCOLS_STR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROTOCOLS_STR, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_REDIR_PROTOCOLS_STR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REDIR_PROTOCOLS_STR, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071304
            case HB_CURLOPT_PROTOCOLS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROTOCOLS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_REDIR_PROTOCOLS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REDIR_PROTOCOLS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_NOPROXY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOPROXY, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_SOCKS5_GSSAPI_NEC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_NEC, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM < 0x073100
            case HB_CURLOPT_SOCKS5_GSSAPI_SERVICE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SOCKS5_GSSAPI_SERVICE, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074001
            case HB_CURLOPT_ALTSVC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ALTSVC, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_ALTSVC_CTRL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ALTSVC_CTRL, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074100
            case HB_CURLOPT_MAXAGE_CONN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXAGE_CONN, hb_parnl( 3 ) );
               break;
#endif
#if 0
            case HB_CURLOPT_H3:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_H3, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074200
            case HB_CURLOPT_SASL_AUTHZID:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SASL_AUTHZID, hb_parc( 3 ) );  /* cp:? */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072B00
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_SOCKS5_GSSAPI_SERVICE:
#endif
            case HB_CURLOPT_PROXY_SERVICE_NAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SERVICE_NAME, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_SERVICE_NAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SERVICE_NAME, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_TCP_KEEPALIVE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPALIVE, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TCP_KEEPIDLE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPIDLE, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TCP_KEEPINTVL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPINTVL, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x080900
            case HB_CURLOPT_TCP_KEEPCNT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TCP_KEEPCNT, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072800
            case HB_CURLOPT_UNIX_SOCKET_PATH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UNIX_SOCKET_PATH, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073500
            case HB_CURLOPT_ABSTRACT_UNIX_SOCKET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ABSTRACT_UNIX_SOCKET, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072A00
            case HB_CURLOPT_PATH_AS_IS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PATH_AS_IS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072B00
            case HB_CURLOPT_PIPEWAIT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PIPEWAIT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x075000
            case HB_CURLOPT_MAXLIFETIME_CONN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXLIFETIME_CONN, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x080800
            case HB_CURLOPT_ECH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ECH, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif

            /* Names and passwords options (Authentication) */

            case HB_CURLOPT_NETRC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A09
            case HB_CURLOPT_NETRC_FILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NETRC_FILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
            case HB_CURLOPT_USERPWD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERPWD, hb_parc( 3 ) );  /* cp:? */
               break;
#if LIBCURL_VERSION_NUM >= 0x073D00
            case HB_CURLOPT_DISALLOW_USERNAME_IN_URL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DISALLOW_USERNAME_IN_URL, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_USERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERNAME, hb_parc( 3 ) );  /* cp:? */
               break;
            case HB_CURLOPT_PASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PASSWORD, hb_parc( 3 ) );  /* cp:? */
               break;
#endif
            case HB_CURLOPT_PROXYUSERPWD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYUSERPWD, hb_parc( 3 ) );  /* cp:? */
               break;
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_PROXYUSERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYUSERNAME, hb_parc( 3 ) );  /* cp:? */
               break;
            case HB_CURLOPT_PROXYPASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYPASSWORD, hb_parc( 3 ) );  /* cp:? */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A06
            case HB_CURLOPT_HTTPAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
            case HB_CURLOPT_PROXYAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073700
            case HB_CURLOPT_SOCKS5_AUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SOCKS5_AUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072100
            case HB_CURLOPT_XOAUTH2_BEARER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_XOAUTH2_BEARER, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072200
            case HB_CURLOPT_LOGIN_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOGIN_OPTIONS, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
#endif

            /* HTTP options */

            case HB_CURLOPT_AUTOREFERER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_AUTOREFERER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_ACCEPT_ENCODING:
#if LIBCURL_VERSION_NUM >= 0x071506
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ACCEPT_ENCODING, hb_parc( 3 ) );  /* cp:ASCII */
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ENCODING, hb_parc( 3 ) );  /* cp:ASCII */
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x071506
            case HB_CURLOPT_TRANSFER_ENCODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TRANSFER_ENCODING, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073700
            case HB_CURLOPT_REQUEST_TARGET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REQUEST_TARGET, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
            case HB_CURLOPT_FOLLOWLOCATION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FOLLOWLOCATION, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_UNRESTRICTED_AUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UNRESTRICTED_AUTH, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_MAXREDIRS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXREDIRS, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_POSTREDIR:
#if LIBCURL_VERSION_NUM >= 0x071301
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTREDIR, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POST301, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
#endif
            case HB_CURLOPT_PUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PUT, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_POST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POST, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_POSTFIELDS:
            case HB_CURLOPT_COPYPOSTFIELDS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COPYPOSTFIELDS, hb_parc( 3 ) );  /* cp:raw? */
               break;
#endif
            case HB_CURLOPT_POSTFIELDSIZE_LARGE:
#if LIBCURL_VERSION_NUM >= 0x070B01
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTFIELDSIZE, hb_parnl( 3 ) );
#endif
               break;
            case HB_CURLOPT_HTTPPOST:  /* deprecated */
            {
               PHB_ITEM pList = hb_param( 3, HB_IT_ARRAY );

               if( pList )
               {
                  HB_BOOL fHash = HB_IS_HASH( pList );
                  HB_SIZE nPos, nLen = fHash ? hb_hashLen( pList ) : hb_arrayLen( pList );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     PHB_ITEM pKey, pVal;

                     if( fHash )
                     {
                        pKey = hb_hashGetKeyAt( pList, nPos + 1 );
                        pVal = hb_hashGetValueAt( pList, nPos + 1 );
                     }
                     else
                     {
                        PHB_ITEM pSubArray = hb_arrayGetItemPtr( pList, nPos + 1 );

                        if( pSubArray )
                        {
                           pKey = hb_arrayGetItemPtr( pSubArray, 1 );
                           pVal = hb_arrayGetItemPtr( pSubArray, 2 );
                        }
                        else
                           pKey = pVal = NULL;
                     }

                     if( pKey && pVal )
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_itemGetCPtr( pKey ),  /* cp:UTF-8? */
                                      CURLFORM_NAMELENGTH, ( long ) hb_itemGetCLen( pKey ),
                                      CURLFORM_FILE, hb_itemGetCPtr( pVal ),  /* cp:UTF-8? */
                                      CURLFORM_END );
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
            case HB_CURLOPT_HTTPPOST_CONTENT:  /* deprecated */
            {
               PHB_ITEM pList = hb_param( 3, HB_IT_ARRAY | HB_IT_HASH );

               if( pList )
               {
                  HB_BOOL fHash = HB_IS_HASH( pList );
                  HB_SIZE nPos, nLen = fHash ? hb_hashLen( pList ) : hb_arrayLen( pList );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     PHB_ITEM pKey, pVal;

                     if( fHash )
                     {
                        pKey = hb_hashGetKeyAt( pList, nPos + 1 );
                        pVal = hb_hashGetValueAt( pList, nPos + 1 );
                     }
                     else
                     {
                        PHB_ITEM pSubArray = hb_arrayGetItemPtr( pList, nPos + 1 );

                        if( pSubArray )
                        {
                           pKey = hb_arrayGetItemPtr( pSubArray, 1 );
                           pVal = hb_arrayGetItemPtr( pSubArray, 2 );
                        }
                        else
                           pKey = pVal = NULL;
                     }

                     if( pKey && pVal )
#if defined( CURLFORM_CONTENTLEN )
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_itemGetCPtr( pKey ),  /* cp:UTF-8? */
                                      CURLFORM_NAMELENGTH, ( long ) hb_itemGetCLen( pKey ),
                                      CURLFORM_COPYCONTENTS, hb_itemGetCPtr( pVal ),  /* cp:binary */
                                      CURLFORM_CONTENTLEN, ( curl_off_t ) hb_itemGetCLen( pVal ),
                                      CURLFORM_END );
#else
                        curl_formadd( &hb_curl->pHTTPPOST_First,
                                      &hb_curl->pHTTPPOST_Last,
                                      CURLFORM_COPYNAME, hb_itemGetCPtr( pKey ),  /* cp:UTF-8? */
                                      CURLFORM_NAMELENGTH, ( long ) hb_itemGetCLen( pKey ),
                                      CURLFORM_COPYCONTENTS, hb_itemGetCPtr( pVal ),  /* cp:binary */
                                      CURLFORM_CONTENTSLENGTH, ( long ) hb_itemGetCLen( pVal ),
                                      CURLFORM_END );
#endif
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
            case HB_CURLOPT_HTTPPOST_FORM:  /* deprecated */
            {
               PHB_ITEM pList = hb_param( 3, HB_IT_ARRAY );

               if( pList )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pList );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     PHB_ITEM pSubArray = hb_arrayGetItemPtr( pList, nPos + 1 );

                     if( pSubArray && HB_IS_ARRAY( pSubArray ) && hb_arrayLen( pSubArray ) >= 3 )
                     {
                        switch( hb_arrayGetNI( pSubArray, 1 ) )
                        {
                           case HB_CURLOPT_HTTPPOST_FORM_CONTENT:
#if defined( CURLFORM_CONTENTLEN )
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 3 ),
                                            CURLFORM_CONTENTLEN, ( curl_off_t ) hb_arrayGetCLen( pSubArray, 3 ),
                                            CURLFORM_END );
#else
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_COPYCONTENTS, hb_arrayGetCPtr( pSubArray, 3 ),
                                            CURLFORM_CONTENTSLENGTH, ( long ) hb_arrayGetCLen( pSubArray, 3 ),
                                            CURLFORM_END );
#endif

                              break;
                           case HB_CURLOPT_HTTPPOST_FORM_FILE:
                              curl_formadd( &hb_curl->pHTTPPOST_First,
                                            &hb_curl->pHTTPPOST_Last,
                                            CURLFORM_COPYNAME, hb_arrayGetCPtr( pSubArray, 2 ),
                                            CURLFORM_NAMELENGTH, ( long ) hb_arrayGetCLen( pSubArray, 2 ),
                                            CURLFORM_FILE, hb_arrayGetCPtr( pSubArray, 3 ),
                                            CURLFORM_END );
                              break;
                        }
                     }
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPPOST, hb_curl->pHTTPPOST_First );
               }
               break;
            }
#if LIBCURL_VERSION_NUM >= 0x073800
            case HB_CURLOPT_MIMEPOST:
            {
               curl_mime * mime = hb_curl_opt_mime( hb_curl->curl, hb_param( 3, HB_IT_ANY ) );

               if( mime )
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_MIMEPOST, hb_curl->mime = mime );

               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x075100
            case HB_CURLOPT_MIME_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MIME_OPTIONS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074B00
            case HB_CURLOPT_AWS_SIGV4:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_AWS_SIGV4, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
#endif
            case HB_CURLOPT_REFERER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_REFERER, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_USERAGENT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USERAGENT, hb_parc( 3 ) );  /* cp:raw? */
               break;
            case HB_CURLOPT_HTTPHEADER:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, NULL );
               hb_curl_slist_free( &hb_curl->pHTTPHEADER );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pHTTPHEADER, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pHTTPHEADER );
                        break;
                     }
                     else
                        hb_curl->pHTTPHEADER = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPHEADER, hb_curl->pHTTPHEADER );
               }
               break;
            }
#if LIBCURL_VERSION_NUM >= 0x072500
            case HB_CURLOPT_HEADEROPT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HEADEROPT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_PROXYHEADER:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, NULL );
               hb_curl_slist_free( &hb_curl->pPROXYHEADER );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pPROXYHEADER, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pPROXYHEADER );
                        break;
                     }
                     else
                        hb_curl->pPROXYHEADER = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXYHEADER, hb_curl->pPROXYHEADER );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x073100
            case HB_CURLOPT_CONNECT_TO:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, NULL );
               hb_curl_slist_free( &hb_curl->pCONNECT_TO );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pCONNECT_TO, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pCONNECT_TO );
                        break;
                     }
                     else
                        hb_curl->pCONNECT_TO = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_TO, hb_curl->pCONNECT_TO );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x070A03
            case HB_CURLOPT_HTTP200ALIASES:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, NULL );
               hb_curl_slist_free( &hb_curl->pHTTP200ALIASES );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pHTTP200ALIASES, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pHTTP200ALIASES );
                        break;
                     }
                     else
                        hb_curl->pHTTP200ALIASES = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP200ALIASES, hb_curl->pHTTP200ALIASES );
               }
               break;
            }
#endif
            case HB_CURLOPT_COOKIE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIE, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
            case HB_CURLOPT_COOKIEFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEFILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_COOKIEJAR:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIEJAR, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_COOKIESESSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIESESSION, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070E01
            case HB_CURLOPT_COOKIELIST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_COOKIELIST, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
#endif
            case HB_CURLOPT_HTTPGET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTPGET, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HTTP_VERSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_VERSION, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x074000
            case HB_CURLOPT_HTTP09_ALLOWED:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP09_ALLOWED, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070E01
            case HB_CURLOPT_IGNORE_CONTENT_LENGTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_IGNORE_CONTENT_LENGTH, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_HTTP_CONTENT_DECODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_CONTENT_DECODING, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_HTTP_TRANSFER_DECODING:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HTTP_TRANSFER_DECODING, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072400
            case HB_CURLOPT_EXPECT_100_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_EXPECT_100_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073E00
            case HB_CURLOPT_UPKEEP_INTERVAL_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPKEEP_INTERVAL_MS, hb_parnl( 3 ) );
               break;
#endif

               /* SMTP options */

#if LIBCURL_VERSION_NUM >= 0x074500
            case HB_CURLOPT_MAIL_RCPT_ALLOWFAILS:
#if LIBCURL_VERSION_NUM >= 0x080200
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT_ALLOWFAILS, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT_ALLLOWFAILS, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_MAIL_FROM:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_FROM, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
            case HB_CURLOPT_MAIL_RCPT:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, NULL );
               hb_curl_slist_free( &hb_curl->pMAIL_RCPT );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pMAIL_RCPT, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pMAIL_RCPT );
                        break;
                     }
                     else
                        hb_curl->pMAIL_RCPT = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_RCPT, hb_curl->pMAIL_RCPT );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_MAIL_AUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAIL_AUTH, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
#endif

               /* TFTP options */

#if LIBCURL_VERSION_NUM >= 0x071304
            case HB_CURLOPT_TFTP_BLKSIZE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TFTP_BLKSIZE, hb_parnl( 3 ) );
               break;
#endif

            /* FTP options */

            case HB_CURLOPT_FTPPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPPORT, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_QUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pQUOTE );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pQUOTE, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pQUOTE );
                        break;
                     }
                     else
                        hb_curl->pQUOTE = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_QUOTE, hb_curl->pQUOTE );
               }
               break;
            }
            case HB_CURLOPT_POSTQUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pPOSTQUOTE );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pPOSTQUOTE, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pPOSTQUOTE );
                        break;
                     }
                     else
                        hb_curl->pPOSTQUOTE = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_POSTQUOTE, hb_curl->pPOSTQUOTE );
               }
               break;
            }
            case HB_CURLOPT_PREQUOTE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, NULL );
               hb_curl_slist_free( &hb_curl->pPREQUOTE );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pPREQUOTE, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pPREQUOTE );
                        break;
                     }
                     else
                        hb_curl->pPREQUOTE = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PREQUOTE, hb_curl->pPREQUOTE );
               }
               break;
            }
            case HB_CURLOPT_DIRLISTONLY: /* HB_CURLOPT_FTPLISTONLY */
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DIRLISTONLY, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPLISTONLY, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
            case HB_CURLOPT_APPEND: /* HB_CURLOPT_FTPAPPEND */
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_APPEND, HB_CURL_OPT_BOOL( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPAPPEND, HB_CURL_OPT_BOOL( 3 ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x070A05
            case HB_CURLOPT_FTP_USE_EPRT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPRT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_FTP_USE_EPSV:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_EPSV, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_FTP_USE_PRET:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_USE_PRET, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A07
            case HB_CURLOPT_FTP_CREATE_MISSING_DIRS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_CREATE_MISSING_DIRS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_SERVER_RESPONSE_TIMEOUT:
#if LIBCURL_VERSION_NUM >= 0x071400
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SERVER_RESPONSE_TIMEOUT, hb_parnl( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_RESPONSE_TIMEOUT, hb_parnl( 3 ) );
#endif
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x080600
            case HB_CURLOPT_SERVER_RESPONSE_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SERVER_RESPONSE_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F05
            case HB_CURLOPT_FTP_ALTERNATIVE_TO_USER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_ALTERNATIVE_TO_USER, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070E02
            case HB_CURLOPT_FTP_SKIP_PASV_IP:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SKIP_PASV_IP, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_USE_SSL:
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_USE_SSL, hb_parnl( 3 ) );
#elif LIBCURL_VERSION_NUM >= 0x070B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SSL, hb_parnl( 3 ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x070C02
            case HB_CURLOPT_FTPSSLAUTH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTPSSLAUTH, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_FTP_SSL_CCC:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_SSL_CCC, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070D00
            case HB_CURLOPT_FTP_ACCOUNT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_ACCOUNT, hb_parc( 3 ) );  /* cp:raw? */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F01
            case HB_CURLOPT_FTP_FILEMETHOD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FTP_FILEMETHOD, hb_parnl( 3 ) );
               break;
#endif

               /* RTSP */

#if LIBCURL_VERSION_NUM >= 0x071400
            case HB_CURLOPT_RTSP_REQUEST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_REQUEST, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_RTSP_SESSION_ID:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_SESSION_ID, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
            case HB_CURLOPT_RTSP_STREAM_URI:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_STREAM_URI, hb_parc( 3 ) );  /* cp:UTF-8? */
               break;
            case HB_CURLOPT_RTSP_TRANSPORT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_TRANSPORT, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
            case HB_CURLOPT_RTSP_CLIENT_CSEQ:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_CLIENT_CSEQ, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_RTSP_SERVER_CSEQ:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RTSP_SERVER_CSEQ, hb_parnl( 3 ) );
               break;
#endif

            /* Protocol */

            case HB_CURLOPT_TRANSFERTEXT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TRANSFERTEXT, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071200
            case HB_CURLOPT_PROXY_TRANSFER_MODE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TRANSFER_MODE, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
            case HB_CURLOPT_CRLF:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CRLF, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_RANGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RANGE, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
            case HB_CURLOPT_RESUME_FROM_LARGE:
#if LIBCURL_VERSION_NUM >= 0x070B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESUME_FROM, hb_parnl( 3 ) );
#endif
               break;
            case HB_CURLOPT_CUSTOMREQUEST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CUSTOMREQUEST, hb_parc( 3 ) );  /* cp:ASCII? */
               break;
            case HB_CURLOPT_FILETIME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FILETIME, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_NOBODY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NOBODY, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_INFILESIZE_LARGE:
#if LIBCURL_VERSION_NUM >= 0x070B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_INFILESIZE, hb_parnl( 3 ) );
#endif
               break;
            case HB_CURLOPT_UPLOAD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_DOWNLOAD: /* Harbour extension */
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_UPLOAD, ! HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_MAXFILESIZE_LARGE:
#if LIBCURL_VERSION_NUM >= 0x070B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXFILESIZE, hb_parnl( 3 ) );
#endif
               break;
#endif
            case HB_CURLOPT_TIMECONDITION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMECONDITION, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_TIMEVALUE_LARGE:
#if LIBCURL_VERSION_NUM >= 0x073B00
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEVALUE_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEVALUE, hb_parnl( 3 ) );
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x073C00
            case HB_CURLOPT_HAPROXYPROTOCOL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HAPROXYPROTOCOL, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_DNS_SHUFFLE_ADDRESSES:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_SHUFFLE_ADDRESSES, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif

            /* Connection */

            case HB_CURLOPT_TIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEOUT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_LOW_SPEED_LIMIT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOW_SPEED_LIMIT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_LOW_SPEED_TIME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_LOW_SPEED_TIME, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x070F05
            case HB_CURLOPT_MAX_SEND_SPEED_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAX_SEND_SPEED_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
            case HB_CURLOPT_MAX_RECV_SPEED_LARGE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAX_RECV_SPEED_LARGE, HB_CURL_OPT_LARGENUM( 3 ) );
               break;
#endif
            case HB_CURLOPT_MAXCONNECTS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_MAXCONNECTS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_CLOSEPOLICY: /* OBSOLETE, does nothing. */
               break;
            case HB_CURLOPT_FRESH_CONNECT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FRESH_CONNECT, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_FORBID_REUSE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_FORBID_REUSE, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_CONNECTTIMEOUT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECTTIMEOUT, hb_parnl( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x071002
            case HB_CURLOPT_CONNECTTIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECTTIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070A08
            case HB_CURLOPT_IPRESOLVE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_IPRESOLVE, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073E00
            case HB_CURLOPT_DOH_URL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DOH_URL, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074C00
            case HB_CURLOPT_DOH_SSL_VERIFYHOST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DOH_SSL_VERIFYHOST, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_DOH_SSL_VERIFYPEER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DOH_SSL_VERIFYPEER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_DOH_SSL_VERIFYSTATUS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DOH_SSL_VERIFYSTATUS, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070F02
            case HB_CURLOPT_CONNECT_ONLY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CONNECT_ONLY, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071503
            case HB_CURLOPT_RESOLVE:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, NULL );
               hb_curl_slist_free( &hb_curl->pRESOLVE );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pRESOLVE, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pRESOLVE );
                        break;
                     }
                     else
                        hb_curl->pRESOLVE = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_RESOLVE, hb_curl->pRESOLVE );
               }
               break;
            }
#endif
#if LIBCURL_VERSION_NUM >= 0x071800
            case HB_CURLOPT_DNS_SERVERS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_DNS_SERVERS, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_ACCEPTTIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ACCEPTTIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073B00
            case HB_CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS, hb_parnl( 3 ) );
               break;
#endif

            /* SSL and Security */

            case HB_CURLOPT_SSLCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERT, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#if LIBCURL_VERSION_NUM >= 0x074700
            case HB_CURLOPT_SSLCERT_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_SSLCERT_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x070903
            case HB_CURLOPT_SSLCERTTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERTTYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
            case HB_CURLOPT_SSLKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEY, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#if LIBCURL_VERSION_NUM >= 0x074700
            case HB_CURLOPT_SSLKEY_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_SSLKEY_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
            case HB_CURLOPT_SSLKEYTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYTYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_KEYPASSWD:
#if LIBCURL_VERSION_NUM > 0x071004
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KEYPASSWD, hb_parc( 3 ) );  /* cp:raw? */
#elif LIBCURL_VERSION_NUM > 0x070902
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLKEYPASSWD, hb_parc( 3 ) );  /* cp:raw? */
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLCERTPASSWD, hb_parc( 3 ) );  /* cp:raw? */
#endif
               break;
            case HB_CURLOPT_SSLENGINE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_SSLENGINE_DEFAULT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLENGINE_DEFAULT, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSLVERSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSLVERSION, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSL_VERIFYPEER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYPEER, HB_CURL_OPT_BOOL( 3 ) );
               break;
#if LIBCURL_VERSION_NUM >= 0x074900
            case HB_CURLOPT_SSL_EC_CURVES:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_EC_CURVES, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
            case HB_CURLOPT_CAINFO:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAINFO, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#if LIBCURL_VERSION_NUM >= 0x074D00
            case HB_CURLOPT_CAINFO_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_CAINFO_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
            case HB_CURLOPT_CAPATH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CAPATH, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_SSL_VERIFYHOST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYHOST, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_SSL_CIPHER_LIST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_CIPHER_LIST, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#if LIBCURL_VERSION_NUM >= 0x073D00
            case HB_CURLOPT_TLS13_CIPHERS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TLS13_CIPHERS, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
            case HB_CURLOPT_SSL_SESSIONID_CACHE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_SESSIONID_CACHE, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_KRBLEVEL: /* HB_CURLOPT_KRB4LEVEL */
#if LIBCURL_VERSION_NUM > 0x071003
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KRBLEVEL, hb_parc( 3 ) );  /* cp:ASCII */
#else
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_KRB4LEVEL, hb_parc( 3 ) );  /* cp:ASCII */
#endif
               break;
#if LIBCURL_VERSION_NUM >= 0x071300
            case HB_CURLOPT_CRLFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CRLFILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_ISSUERCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_ISSUERCERT, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074700
            case HB_CURLOPT_ISSUERCERT_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_ISSUERCERT_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071301
            case HB_CURLOPT_CERTINFO:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_CERTINFO, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071600
            case HB_CURLOPT_GSSAPI_DELEGATION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_GSSAPI_DELEGATION, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071900
            case HB_CURLOPT_SSL_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_OPTIONS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072400
            case HB_CURLOPT_SSL_ENABLE_NPN:
#if LIBCURL_VERSION_NUM <= 0x075500
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_ENABLE_NPN, hb_parnl( 3 ) );
               break;
#endif
            case HB_CURLOPT_SSL_ENABLE_ALPN:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_ENABLE_ALPN, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072700
            case HB_CURLOPT_PINNEDPUBLICKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PINNEDPUBLICKEY, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072900
            case HB_CURLOPT_SSL_VERIFYSTATUS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_VERIFYSTATUS, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x072A00
            case HB_CURLOPT_SSL_FALSESTART:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSL_FALSESTART, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x074A00
            case HB_CURLOPT_HSTS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HSTS, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_HSTS_CTRL:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_HSTS_CTRL, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071504
            case HB_CURLOPT_TLSAUTH_PASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TLSAUTH_PASSWORD, hb_parc( 3 ) );  /* cp:raw? */
               break;
            case HB_CURLOPT_TLSAUTH_TYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TLSAUTH_TYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_TLSAUTH_USERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_TLSAUTH_USERNAME, hb_parc( 3 ) );  /* cp:raw? */
               break;
#endif

            /* SSL proxy */

#if LIBCURL_VERSION_NUM >= 0x073400
            case HB_CURLOPT_PROXY_SSLCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSLCERT, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#if LIBCURL_VERSION_NUM >= 0x074700
            case HB_CURLOPT_PROXY_SSLCERT_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_PROXY_SSLCERT_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
            case HB_CURLOPT_PROXY_ISSUERCERT:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_ISSUERCERT, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_PROXY_ISSUERCERT_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_PROXY_ISSUERCERT_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
            case HB_CURLOPT_PROXY_SSLCERTTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSLCERTTYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_PROXY_SSLKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSLKEY, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#if LIBCURL_VERSION_NUM >= 0x074700
            case HB_CURLOPT_PROXY_SSLKEY_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_PROXY_SSLKEY_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
            case HB_CURLOPT_PROXY_SSLKEYTYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSLKEYTYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_PROXY_KEYPASSWD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_KEYPASSWD, hb_parc( 3 ) );  /* cp:raw? */
               break;
            case HB_CURLOPT_PROXY_SSLVERSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSLVERSION, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_PROXY_SSL_VERIFYPEER:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSL_VERIFYPEER, HB_CURL_OPT_BOOL( 3 ) );
               break;
            case HB_CURLOPT_PROXY_CAINFO:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_CAINFO, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#if LIBCURL_VERSION_NUM >= 0x074D00
            case HB_CURLOPT_PROXY_CAINFO_BLOB:
               res = hb_curl_easy_setopt_blob( hb_curl->curl, CURLOPT_PROXY_CAINFO_BLOB, hb_param( 3, HB_IT_STRING ) );  /* cp:binary */
               break;
#endif
            case HB_CURLOPT_PROXY_CAPATH:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_CAPATH, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_PROXY_SSL_VERIFYHOST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSL_VERIFYHOST, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_PROXY_SSL_CIPHER_LIST:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSL_CIPHER_LIST, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#if LIBCURL_VERSION_NUM >= 0x073D00
            case HB_CURLOPT_PROXY_TLS13_CIPHERS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TLS13_CIPHERS, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
            case HB_CURLOPT_PROXY_CRLFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_CRLFILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_PROXY_SSL_OPTIONS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_SSL_OPTIONS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_PROXY_PINNEDPUBLICKEY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_PINNEDPUBLICKEY, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_PROXY_TLSAUTH_PASSWORD:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TLSAUTH_PASSWORD, hb_parc( 3 ) );  /* cp:raw? */
               break;
            case HB_CURLOPT_PROXY_TLSAUTH_TYPE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TLSAUTH_TYPE, hb_parc( 3 ) );  /* cp:ASCII */
               break;
            case HB_CURLOPT_PROXY_TLSAUTH_USERNAME:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROXY_TLSAUTH_USERNAME, hb_parc( 3 ) );  /* cp:raw? */
               break;
            case HB_CURLOPT_PRE_PROXY:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PRE_PROXY, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif

               /* SSH options */

#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_SSH_AUTH_TYPES:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_AUTH_TYPES, hb_parnl( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x073800
            case HB_CURLOPT_SSH_COMPRESSION:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_COMPRESSION, HB_CURL_OPT_BOOL( 3 ) );
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071101
            case HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_HOST_PUBLIC_KEY_MD5, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x075000
            case HB_CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256, hb_parc( 3 ) );  /* cp:ASCII */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071001
            case HB_CURLOPT_SSH_PUBLIC_KEYFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_PUBLIC_KEYFILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
            case HB_CURLOPT_SSH_PRIVATE_KEYFILE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_PRIVATE_KEYFILE, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif
#if LIBCURL_VERSION_NUM >= 0x071306
            case HB_CURLOPT_SSH_KNOWNHOSTS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_SSH_KNOWNHOSTS, hb_parc( 3 ) );  /* cp:UTF-8 */
               break;
#endif

               /* Other options */

#if LIBCURL_VERSION_NUM >= 0x070A03
            case HB_CURLOPT_PRIVATE:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_PRIVATE, hb_parptr( 3 ) );
               break;
#endif

               /* HB_CURLOPT_SHARE */

#if LIBCURL_VERSION_NUM >= 0x071004
            case HB_CURLOPT_NEW_FILE_PERMS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NEW_FILE_PERMS, hb_parnl( 3 ) );
               break;
            case HB_CURLOPT_NEW_DIRECTORY_PERMS:
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_NEW_DIRECTORY_PERMS, hb_parnl( 3 ) );
               break;
#endif

            /* Telnet options */

            case HB_CURLOPT_TELNETOPTIONS:
            {
               PHB_ITEM pArray = hb_param( 3, HB_IT_ARRAY );

               curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, NULL );
               hb_curl_slist_free( &hb_curl->pTELNETOPTIONS );

               if( pArray )
               {
                  HB_SIZE nPos, nLen = hb_arrayLen( pArray );

                  for( nPos = 0; nPos < nLen; ++nPos )
                  {
                     struct curl_slist * temp = curl_slist_append( hb_curl->pTELNETOPTIONS, hb_arrayGetCPtr( pArray, nPos + 1 ) );

                     if( temp == NULL )
                     {
                        hb_curl_slist_free( &hb_curl->pTELNETOPTIONS );
                        break;
                     }
                     else
                        hb_curl->pTELNETOPTIONS = temp;
                  }

                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_TELNETOPTIONS, hb_curl->pTELNETOPTIONS );
               }
               break;
            }

            /* Undocumented */

            /* HB_CURLOPT_WRITEINFO */

            /* Harbour specials */

            case HB_CURLOPT_XFERINFOBLOCK:
            {
               PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

               if( hb_curl->pXferInfoCallback )
               {
#if LIBCURL_VERSION_NUM >= 0x072000
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFOFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFODATA, NULL );
#else
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, NULL );
#endif

                  hb_itemRelease( hb_curl->pXferInfoCallback );
                  hb_curl->pXferInfoCallback = NULL;
               }

               if( pCallback )
               {
                  hb_curl->pXferInfoCallback = hb_itemNew( pCallback );
                  /* unlock the item so GC will not mark them as used */
                  hb_gcUnlock( hb_curl->pXferInfoCallback );

#if LIBCURL_VERSION_NUM >= 0x072000
                  curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFOFUNCTION, hb_curl_xferinfo_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_XFERINFODATA, hb_curl->pXferInfoCallback );
#else
                  curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSFUNCTION, hb_curl_progress_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_PROGRESSDATA, hb_curl->pXferInfoCallback );
#endif
               }
               break;
            }

            case HB_CURLOPT_DEBUGBLOCK:
            {
#if LIBCURL_VERSION_NUM >= 0x070906
               PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

               if( hb_curl->pDebugCallback )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGFUNCTION, NULL );
                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGDATA, NULL );

                  hb_itemRelease( hb_curl->pDebugCallback );
                  hb_curl->pDebugCallback = NULL;
               }

               if( pCallback )
               {
                  hb_curl->pDebugCallback = hb_itemNew( pCallback );
                  /* unlock the item so GC will not mark them as used */
                  hb_gcUnlock( hb_curl->pDebugCallback );

                  curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGFUNCTION, hb_curl_debug_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_DEBUGDATA, hb_curl->pDebugCallback );
               }
#endif
               break;
            }

            case HB_CURLOPT_UL_FILE_SETUP:
            {
               HB_BOOL lSuccess = HB_TRUE;

               hb_curl_ul_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
                  hb_curl->ul_name = hb_strdup( hb_parc( 3 ) );
               else if( hb_fileParamGet( 3 ) )
                  hb_curl->ul_file = hb_fileParamGet( 3 );
               else if( HB_ISNUM( 3 ) )
               {
                  hb_curl->ul_file   = hb_fileFromHandle( hb_numToHandle( hb_parnint( 3 ) ) );
                  hb_curl->ul_onfree = HB_CURL_ONFREE_DETACH;
               }
               else
                  lSuccess = HB_FALSE;

               if( lSuccess )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_file_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               }
               break;
            }
            case HB_CURLOPT_UL_FILE_CLOSE:
               hb_curl_ul_free( hb_curl );
               res = CURLE_OK;
               break;

            case HB_CURLOPT_DL_FILE_SETUP:
            {
               HB_BOOL lSuccess = HB_TRUE;

               hb_curl_dl_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
                  hb_curl->dl_name = hb_strdup( hb_parc( 3 ) );
               else if( hb_fileParamGet( 3 ) )
                  hb_curl->dl_file = hb_fileParamGet( 3 );
               else if( HB_ISNUM( 3 ) )
               {
                  hb_curl->dl_file   = hb_fileFromHandle( hb_numToHandle( hb_parnint( 3 ) ) );
                  hb_curl->dl_onfree = HB_CURL_ONFREE_DETACH;
               }
               else
                  lSuccess = HB_FALSE;

               if( lSuccess )
               {
                  curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_file_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, hb_curl );
               }
               break;
            }
            case HB_CURLOPT_DL_FILE_CLOSE:
               hb_curl_dl_free( hb_curl );
               res = CURLE_OK;
               break;

            case HB_CURLOPT_UL_BUFF_SETUP:
               hb_curl_ul_free( hb_curl );

               if( HB_ISCHAR( 3 ) )
               {
                  hb_curl->ul_pos = 0;
                  hb_curl->ul_len = hb_parclen( 3 );
                  hb_curl->ul_ptr = ( unsigned char * ) hb_xgrab( hb_curl->ul_len + 1 );

                  hb_xmemcpy( hb_curl->ul_ptr, hb_parc( 3 ), hb_curl->ul_len );

                  curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_buff_callback );
                  res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               }
               break;

            case HB_CURLOPT_DL_BUFF_SETUP:
               hb_curl_dl_free( hb_curl );

               hb_curl->dl_pos = 0;
               hb_curl->dl_len = hb_parnldef( 3, HB_CURL_DL_BUFF_SIZE_INIT );
               hb_curl->dl_ptr = ( unsigned char * ) hb_xgrab( hb_curl->dl_len + 1 );

               curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEFUNCTION, hb_curl_write_buff_callback );
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_WRITEDATA, hb_curl );
               break;

            case HB_CURLOPT_DL_BUFF_GET:
               hb_storclen( ( char * ) hb_curl->dl_ptr, hb_curl->dl_pos, 3 );
               if( hb_curl->dl_ptr )
                  res = CURLE_OK;
               break;

            case HB_CURLOPT_UL_NULL_SETUP:
               hb_curl_ul_free( hb_curl );

               curl_easy_setopt( hb_curl->curl, CURLOPT_READFUNCTION, hb_curl_read_dummy_callback );
               res = curl_easy_setopt( hb_curl->curl, CURLOPT_READDATA, hb_curl );
               break;
         }
      }

      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Harbour extension. */
HB_FUNC( CURL_EASY_DL_BUFF_GET )
{
   if( PHB_CURL_is( 1 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         hb_retclen( ( char * ) hb_curl->dl_ptr, hb_curl->dl_pos );
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#define HB_CURL_INFO_TYPE_INVALID    0
#define HB_CURL_INFO_TYPE_STR        1
#define HB_CURL_INFO_TYPE_PTR        2
#define HB_CURL_INFO_TYPE_PTR_SLIST  3
#define HB_CURL_INFO_TYPE_LONG       4
#define HB_CURL_INFO_TYPE_DOUBLE     5
#define HB_CURL_INFO_TYPE_SLIST      6
#define HB_CURL_INFO_TYPE_SOCKET     7
#define HB_CURL_INFO_TYPE_CERTINFO   8
#define HB_CURL_INFO_TYPE_OFFSET     9

#define HB_CURL_EASY_GETINFO( hb_curl, n, p )  ( hb_curl ? curl_easy_getinfo( hb_curl->curl, n, p ) : ( CURLcode ) HB_CURLE_ERROR )

/* NOTE: curl_easy_getinfo( curl, x, @nError ) --> xValue */
HB_FUNC( CURL_EASY_GETINFO )
{
   if( PHB_CURL_is( 1 ) && HB_ISNUM( 2 ) )
   {
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      CURLcode res     = ( CURLcode ) HB_CURLE_ERROR;

      int type = HB_CURL_INFO_TYPE_INVALID;

      char * ret_string = NULL;
      char * ret_ptr    = NULL;
      long   ret_long   = 0;
      struct curl_slist * ret_slist = NULL;
      double ret_double = 0.0;
      curl_socket_t ret_socket = CURL_SOCKET_BAD;
      struct curl_certinfo * ret_certinfo = NULL;
#if LIBCURL_VERSION_NUM >= 0x073700
      curl_off_t ret_offset = 0;
#endif

      switch( hb_parni( 2 ) )
      {
         case HB_CURLINFO_EFFECTIVE_URL:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_EFFECTIVE_URL, &ret_string );  /* cp:UTF-8 */
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_RESPONSE_CODE:
#if LIBCURL_VERSION_NUM > 0x070A07
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RESPONSE_CODE, &ret_long );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_CODE, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_HTTP_CONNECTCODE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_CONNECTCODE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_FILETIME_T:
#if LIBCURL_VERSION_NUM >= 0x073B00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_FILETIME_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#elif LIBCURL_VERSION_NUM >= 0x070500
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_FILETIME, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
#else
            type = HB_CURL_INFO_TYPE_LONG;
#endif
            break;
         case HB_CURLINFO_TOTAL_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_TOTAL_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_NAMELOOKUP_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NAMELOOKUP_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CONNECT_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONNECT_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_PRETRANSFER_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRETRANSFER_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_STARTTRANSFER_TIME:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_STARTTRANSFER_TIME, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_REDIRECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x070907
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_TIME, &ret_double );
#endif
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_TOTAL_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_TOTAL_TIME_T, &ret_offset );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_TOTAL_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_NAMELOOKUP_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NAMELOOKUP_TIME_T, &ret_offset );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NAMELOOKUP_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_CONNECT_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONNECT_TIME_T, &ret_offset );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONNECT_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_PRETRANSFER_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRETRANSFER_TIME_T, &ret_offset );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRETRANSFER_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_STARTTRANSFER_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_STARTTRANSFER_TIME_T, &ret_offset );
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_STARTTRANSFER_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_REDIRECT_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_TIME_T, &ret_offset );
#elif LIBCURL_VERSION_NUM >= 0x070907
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_APPCONNECT_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x073D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_APPCONNECT_TIME_T, &ret_offset );
#elif LIBCURL_VERSION_NUM >= 0x071300
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_APPCONNECT_TIME, &ret_double );
            ret_offset = ( curl_off_t ) ( ret_double * 1000000.0 );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_QUEUE_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x080600
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_QUEUE_TIME_T, &ret_offset );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_POSTTRANSFER_TIME_T:
#if LIBCURL_VERSION_NUM >= 0x080a00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_POSTTRANSFER_TIME_T, &ret_offset );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_REDIRECT_COUNT:
#if LIBCURL_VERSION_NUM >= 0x070907
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_COUNT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_REDIRECT_URL:
#if LIBCURL_VERSION_NUM >= 0x071202
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REDIRECT_URL, &ret_string );  /* cp:UTF-8 */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_REFERER:
#if LIBCURL_VERSION_NUM >= 0x074C00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REFERER, &ret_string );  /* cp:UTF-8 */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_SIZE_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_UPLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_SIZE_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_DOWNLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SIZE_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_SPEED_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_DOWNLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_SPEED_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_UPLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SPEED_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_HEADER_SIZE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HEADER_SIZE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_REQUEST_SIZE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_REQUEST_SIZE, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_SSL_VERIFYRESULT:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SSL_VERIFYRESULT, &ret_long );
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PROXY_SSL_VERIFYRESULT:
#if LIBCURL_VERSION_NUM >= 0x073400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PROXY_SSL_VERIFYRESULT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_SSL_ENGINES:
#if LIBCURL_VERSION_NUM >= 0x071203
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SSL_ENGINES, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_SLIST;
            break;
         case HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_CONTENT_LENGTH_UPLOAD_T:
#if LIBCURL_VERSION_NUM >= 0x073700
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_UPLOAD_T, &ret_offset );
            type = HB_CURL_INFO_TYPE_OFFSET;
#else
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_LENGTH_UPLOAD, &ret_double );
            type = HB_CURL_INFO_TYPE_DOUBLE;
#endif
            break;
         case HB_CURLINFO_CONTENT_TYPE:
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONTENT_TYPE, &ret_string );  /* cp:ASCII? */
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_PRIVATE:
#if LIBCURL_VERSION_NUM >= 0x070A03
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIVATE, &ret_ptr );
#endif
            type = HB_CURL_INFO_TYPE_PTR;
            break;
         case HB_CURLINFO_HTTPAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTPAUTH_AVAIL, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PROXYAUTH_AVAIL:
#if LIBCURL_VERSION_NUM >= 0x070A08
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PROXYAUTH_AVAIL, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_OS_ERRNO:
#if LIBCURL_VERSION_NUM >= 0x070C02
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_OS_ERRNO, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_NUM_CONNECTS:
#if LIBCURL_VERSION_NUM >= 0x070C03
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_NUM_CONNECTS, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_COOKIELIST:
#if LIBCURL_VERSION_NUM >= 0x070E01
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_COOKIELIST, &ret_slist );
#endif
            type = HB_CURL_INFO_TYPE_SLIST;
            break;
         case HB_CURLINFO_ACTIVESOCKET:
#if LIBCURL_VERSION_NUM >= 0x072D00
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_ACTIVESOCKET, &ret_socket );
#endif
            type = HB_CURL_INFO_TYPE_SOCKET;
            break;
         case HB_CURLINFO_LASTSOCKET:  /* NOTE: Not compatible with 64-bit Windows builds */
#if LIBCURL_VERSION_NUM >= 0x070F02
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LASTSOCKET, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_FTP_ENTRY_PATH:
#if LIBCURL_VERSION_NUM >= 0x070F04
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_FTP_ENTRY_PATH, &ret_string );  /* cp:UTF-8? */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_PRIMARY_IP:
#if LIBCURL_VERSION_NUM >= 0x071300
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIMARY_IP, &ret_string );  /* cp:ASCII */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_APPCONNECT_TIME:
#if LIBCURL_VERSION_NUM >= 0x071300
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_APPCONNECT_TIME, &ret_double );
#endif
            type = HB_CURL_INFO_TYPE_DOUBLE;
            break;
         case HB_CURLINFO_CERTINFO:
#if LIBCURL_VERSION_NUM >= 0x071301
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CERTINFO, &ret_certinfo );
#endif
            type = HB_CURL_INFO_TYPE_CERTINFO;
            break;
         case HB_CURLINFO_CAINFO:
#if LIBCURL_VERSION_NUM >= 0x075400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CAINFO, &ret_string );  /* cp:UTF-8 */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_CAPATH:
#if LIBCURL_VERSION_NUM >= 0x075400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CAPATH, &ret_string );  /* cp:UTF-8 */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_CONDITION_UNMET:
#if LIBCURL_VERSION_NUM >= 0x071304
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONDITION_UNMET, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_SESSION_ID:
#if LIBCURL_VERSION_NUM >= 0x071400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_SESSION_ID, &ret_string );  /* cp:ASCII? */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_RTSP_CLIENT_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_CLIENT_CSEQ, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_SERVER_CSEQ:
#if LIBCURL_VERSION_NUM >= 0x071400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_SERVER_CSEQ, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_RTSP_CSEQ_RECV:
#if LIBCURL_VERSION_NUM >= 0x071400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RTSP_CSEQ_RECV, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PRIMARY_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PRIMARY_PORT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_LOCAL_IP:
#if LIBCURL_VERSION_NUM >= 0x071500
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LOCAL_IP, &ret_string );  /* cp:ASCII */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_LOCAL_PORT:
#if LIBCURL_VERSION_NUM >= 0x071500
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_LOCAL_PORT, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_HTTP_VERSION:
#if LIBCURL_VERSION_NUM >= 0x073200
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_HTTP_VERSION, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_PROTOCOL:
#if LIBCURL_VERSION_NUM >= 0x073400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PROTOCOL, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_SCHEME:
#if LIBCURL_VERSION_NUM >= 0x073400
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_SCHEME, &ret_string );  /* cp:ASCII */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_RETRY_AFTER:
#if LIBCURL_VERSION_NUM >= 0x074200
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_RETRY_AFTER, &ret_offset );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_EFFECTIVE_METHOD:
#if LIBCURL_VERSION_NUM >= 0x074800
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_EFFECTIVE_METHOD, &ret_string );  /* cp:ASCII */
#endif
            type = HB_CURL_INFO_TYPE_STR;
            break;
         case HB_CURLINFO_PROXY_ERROR:
#if LIBCURL_VERSION_NUM >= 0x074900
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_PROXY_ERROR, &ret_long );
#endif
            type = HB_CURL_INFO_TYPE_LONG;
            break;
         case HB_CURLINFO_CONN_ID:
#if LIBCURL_VERSION_NUM >= 0x080200
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_CONN_ID, &ret_offset );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
         case HB_CURLINFO_XFER_ID:
#if LIBCURL_VERSION_NUM >= 0x080200
            res  = HB_CURL_EASY_GETINFO( hb_curl, CURLINFO_XFER_ID, &ret_offset );
#endif
            type = HB_CURL_INFO_TYPE_OFFSET;
            break;
      }

      switch( type )
      {
         case HB_CURL_INFO_TYPE_STR:
            hb_retc( ret_string );  /* FIXME: convert from UTF-8 when necessary */
            break;
         case HB_CURL_INFO_TYPE_PTR:
            hb_retptr( ret_ptr );
            break;
         case HB_CURL_INFO_TYPE_PTR_SLIST:
            hb_retptr( ret_slist );
            break;
         case HB_CURL_INFO_TYPE_LONG:
            hb_retnl( ret_long );
            break;
         case HB_CURL_INFO_TYPE_DOUBLE:
            hb_retnd( ret_double );
            break;
         case HB_CURL_INFO_TYPE_SLIST:
            if( ret_slist )
            {
               PHB_ITEM pArray;
               int      nCount;
               struct curl_slist * walk_ret_slist;

               /* Count */
               for( walk_ret_slist = ret_slist, nCount = 0; walk_ret_slist->next; nCount++ )
                  walk_ret_slist = walk_ret_slist->next;

               /* Fill */
               pArray = hb_itemArrayNew( nCount );
               for( walk_ret_slist = ret_slist, nCount = 1; walk_ret_slist->next; )
               {
                  hb_arraySetC( pArray, nCount++, walk_ret_slist->data );
                  walk_ret_slist = walk_ret_slist->next;
               }
               hb_itemReturnRelease( pArray );

               curl_slist_free_all( ret_slist );
            }
            else
               hb_reta( 0 );
            break;
         case HB_CURL_INFO_TYPE_SOCKET:
            hb_retnint( ret_socket );
            break;
         case HB_CURL_INFO_TYPE_CERTINFO:
         {
            int nCerts = ret_certinfo != NULL ? ret_certinfo->num_of_certs : 0;

            PHB_ITEM pCerts = hb_itemArrayNew( nCerts );
            int nPos = 0;

            for( nPos = 0; nPos < nCerts; ++nPos )
            {
               PHB_ITEM pArray;
               int      nCount;
               struct curl_slist * walk_ret_slist;

               /* Count */
               for( walk_ret_slist = ret_certinfo->certinfo[ nPos ], nCount = 0; walk_ret_slist->next; nCount++ )
                  walk_ret_slist = walk_ret_slist->next;

               /* Fill */
               pArray = hb_itemArrayNew( nCount );
               for( walk_ret_slist = ret_certinfo->certinfo[ nPos ], nCount = 1; walk_ret_slist->next; )
               {
                  hb_arraySetC( pArray, nCount++, walk_ret_slist->data );
                  walk_ret_slist = walk_ret_slist->next;
               }
               hb_arraySetForward( pCerts, nPos + 1, pArray );
               hb_itemRelease( pArray );

               curl_slist_free_all( ret_slist );
            }

            hb_itemReturnRelease( pCerts );
            break;
         }
#if LIBCURL_VERSION_NUM >= 0x073700
         case HB_CURL_INFO_TYPE_OFFSET:
            hb_retnint( ( HB_MAXINT ) ret_offset );
            break;
#endif
      }

      hb_stornl( ( long ) res, 3 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_ESCAPE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x070F04
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      int nSrcLen;

      if( hb_curl && ( nSrcLen = ( int ) hb_parclen( 2 ) ) > 0 )
      {
         char * buffer = curl_easy_escape( hb_curl->curl, hb_parcx( 2 ), nSrcLen );
         hb_retc( buffer );
         curl_free( buffer );
      }
      else
#endif
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_UNESCAPE )
{
   if( PHB_CURL_is( 1 ) )
   {
#if LIBCURL_VERSION_NUM >= 0x070F04
      PHB_CURL hb_curl = PHB_CURL_par( 1 );
      int nSrcLen;

      if( hb_curl && ( nSrcLen = ( int ) hb_parclen( 2 ) ) > 0 )
      {
         int    nLen   = 0;
         char * buffer = curl_easy_unescape( hb_curl->curl, hb_parcx( 2 ), nSrcLen, &nLen );
         hb_retclen( buffer, nLen );
         curl_free( buffer );
      }
      else
#endif
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_EASY_STRERROR )
{
   if( HB_ISNUM( 1 ) )
#if LIBCURL_VERSION_NUM >= 0x070C00
      hb_retc( curl_easy_strerror( ( CURLcode ) hb_parnl( 1 ) ) );
#else
      hb_retc_null();
#endif
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* CURLcode curl_ws_send(struct Curl_easy *data,
                         const void *buffer, size_t buflen,
                         size_t *sent,
                         curl_off_t framesize,
                         unsigned int sendflags) */
HB_FUNC( CURL_WS_SEND )
{
   if( PHB_CURL_is( 1 ) )
   {
      CURLcode res  = HB_CURLE_ERROR;
      size_t   sent = 0;

#if LIBCURL_VERSION_NUM >= 0x075600
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      if( hb_curl )
         res = curl_ws_send( hb_curl->curl,
                             ( const void * ) hb_parc( 2 ), ( size_t ) hb_parclen( 2 ),
                             &sent,
                             ( curl_off_t ) hb_parnint( 4 ),
                             ( unsigned int ) hb_parnl( 5 ) );
#endif

      hb_storns( ( HB_SIZE ) sent, 3 );
      hb_retnl( res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* CURLcode curl_ws_recv(struct Curl_easy *data,
                         void *buffer, size_t buflen,
                         size_t *recv, struct curl_ws_frame **meta) */
HB_FUNC( CURL_WS_RECV )
{
   if( PHB_CURL_is( 1 ) && HB_ISBYREF( 2 ) )
   {
      CURLcode res  = HB_CURLE_ERROR;
      size_t   recv = 0;

#if LIBCURL_VERSION_NUM >= 0x075600
      PHB_CURL hb_curl = PHB_CURL_par( 1 );

      const struct curl_ws_frame * meta = NULL;

      PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
      char *   buffer;
      HB_SIZE  buflen;

      if( hb_itemGetWriteCL( pBuffer, &buffer, &buflen ) )
         res = curl_ws_recv( hb_curl->curl,
                             ( void * ) buffer, ( size_t ) buflen,
                             &recv,
                             &meta );

      hb_stornl( meta ? meta->flags : 0, 4 );
      hb_stornint( meta ? meta->offset: 0, 5 );
      hb_stornint( meta ? meta->bytesleft: 0, 6 );
#else
      hb_stornl( 0, 4 );
      hb_stornint( 0, 5 );
      hb_stornint( 0, 6 );
#endif

      hb_storns( ( HB_SIZE ) recv, 3 );
      hb_retnl( res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Multi interface */

typedef struct _HB_CURLM
{
   CURLM * curlm;
} HB_CURLM, *PHB_CURLM;

/* Constructor/Destructor */

static void PHB_CURLM_free( PHB_CURLM hb_curlm )
{
   curl_multi_cleanup( hb_curlm->curlm );
   hb_xfree( hb_curlm );
}

static PHB_CURLM PHB_CURLM_create( void )
{
   CURLM * curlm = curl_multi_init();

   if( curlm )
   {
      PHB_CURLM hb_curlm = ( PHB_CURLM ) hb_xgrab( sizeof( HB_CURLM ) );

      memset( hb_curlm, 0, sizeof( HB_CURLM ) );
      hb_curlm->curlm = curlm;

      return hb_curlm;
   }
   else
      return NULL;
}

static HB_GARBAGE_FUNC( PHB_CURLM_release )
{
   PHB_CURLM * hb_curlm_ptr = ( PHB_CURLM * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( hb_curlm_ptr && *hb_curlm_ptr )
   {
      /* Destroy the object */
      PHB_CURLM_free( *hb_curlm_ptr );
      *hb_curlm_ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcCURLMFuncs =
{
   PHB_CURLM_release,
   hb_gcDummyMark
};

static void PHB_CURLM_ret()
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( PHB_CURLM ), &s_gcCURLMFuncs );

   *ph = PHB_CURLM_create();

   hb_retptrGC( ph );
}

static void * PHB_CURLM_is( int iParam )
{
   return hb_parptrGC( &s_gcCURLMFuncs, iParam );
}

static PHB_CURLM PHB_CURLM_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLMFuncs, iParam );

   return ph ? ( PHB_CURLM ) *ph : NULL;
}

/* Harbour wrappers */

HB_FUNC( CURL_MULTI_INIT )
{
   PHB_CURLM_ret();
}

HB_FUNC( CURL_MULTI_CLEANUP )
{
   if( PHB_CURLM_is( 1 ) )
   {
      void ** ph = ( void ** ) hb_parptrGC( &s_gcCURLMFuncs, 1 );

      if( ph && *ph )
      {
         /* Destroy the object */
         PHB_CURLM_free( ( PHB_CURLM ) *ph );
         *ph = NULL;
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_MULTI_ADD_HANDLE )
{
   if( PHB_CURLM_is( 1 ) && PHB_CURL_is( 2 ) )
   {
      PHB_CURLM hb_curlm = PHB_CURLM_par( 1 );
      PHB_CURL  hb_curl  = PHB_CURL_par( 2 );

      hb_retnl( hb_curlm && hb_curl ? ( long ) curl_multi_add_handle( hb_curlm->curlm, hb_curl->curl ) : HB_CURLM_ERROR );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_MULTI_REMOVE_HANDLE )
{
   if( PHB_CURLM_is( 1 ) && PHB_CURL_is( 2 ) )
   {
      PHB_CURLM hb_curlm = PHB_CURLM_par( 1 );
      PHB_CURL  hb_curl  = PHB_CURL_par( 2 );

      hb_retnl( hb_curlm && hb_curl ? ( long ) curl_multi_remove_handle( hb_curlm->curlm, hb_curl->curl ) : HB_CURLM_ERROR );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_MULTI_PERFORM )
{
   if( PHB_CURLM_is( 1 ) && HB_ISBYREF( 2 ) )
   {
      CURLMcode res      = ( CURLMcode ) HB_CURLM_ERROR;
      PHB_CURLM hb_curlm = PHB_CURLM_par( 1 );

      if( hb_curlm )
      {
         int running_handles = 0;
         res = curl_multi_perform( hb_curlm->curlm, &running_handles );
         hb_storni( running_handles, 2 );
      }

      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_MULTI_POLL )
{
   if( PHB_CURLM_is( 1 ) && HB_ISNUM( 2 ) )
   {
      CURLMcode res      = ( CURLMcode ) HB_CURLM_ERROR;
      PHB_CURLM hb_curlm = PHB_CURLM_par( 1 );

      if( hb_curlm )
         res = curl_multi_poll( hb_curlm->curlm,
                                NULL,
                                0,
                                hb_parni( 2 ),
                                NULL );

      hb_retnl( ( long ) res );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CURL_MULTI_INFO_READ )
{
   if( PHB_CURLM_is( 1 ) )
   {
      PHB_CURLM hb_curlm = PHB_CURLM_par( 1 );

      if( hb_curlm )
      {
         int msgs_in_queue    = 0;
         struct CURLMsg * msg = curl_multi_info_read( hb_curlm->curlm, &msgs_in_queue );

         if( msg )
         {
            PHB_ITEM pReturn;
            long     response_code = 0;

            ( void ) curl_easy_getinfo( msg->easy_handle, CURLINFO_RESPONSE_CODE, &response_code );

            pReturn = hb_itemArrayNew( HB_CURLMSG_RESP_LAST );

            hb_arraySetNI( pReturn, HB_CURLMSG_RESP_LEN, msgs_in_queue );
            hb_arraySetNL( pReturn, HB_CURLMSG_RESP_RESPONSE_CODE, response_code );
            hb_arraySetNL( pReturn, HB_CURLMSG_RESP_MSG, ( long ) msg->msg );
            hb_arraySetNL( pReturn, HB_CURLMSG_RESP_RESULT, ( long ) msg->data.result );

            hb_itemReturnRelease( pReturn );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
