/*
 * libyaml API - Harbour interface
 *
 * Copyright 2017 Viktor Szakats (vszakats.net/harbour)
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

#include "hbyaml.h"

typedef struct _HB_EMITTER
{
   yaml_emitter_t * emitter;
   unsigned char *  buffer;
   size_t           size;
   size_t           len;
} HB_EMITTER, * PHB_EMITTER;

/* Constructor/Destructor */

static HB_GARBAGE_FUNC( emitter_release )
{
   PHB_EMITTER ** ptr = ( PHB_EMITTER ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ptr && *ptr )
   {
      PHB_EMITTER hbemitter = ( PHB_EMITTER ) *ptr;

      yaml_emitter_delete( hbemitter->emitter );

      if( hbemitter->buffer != NULL )
         hb_xfree( hbemitter->buffer );

      /* Destroy the object */
      hb_xfree( hbemitter->emitter );
      hb_xfree( *ptr );
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gc_emitter_funcs =
{
   emitter_release,
   NULL
};

static yaml_emitter_t * hb_yaml_par_emitter( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_emitter_funcs, iParam );

   return ptr ? ( ( PHB_EMITTER ) *ptr )->emitter : NULL;
}

static PHB_EMITTER hb_yaml_par_hbemitter( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_emitter_funcs, iParam );

   return ptr ? ( PHB_EMITTER ) *ptr : NULL;
}

/* Harbour interface */

HB_FUNC( YAML_EMITTER_INITIALIZE )
{
   yaml_emitter_t * emitter = ( yaml_emitter_t * ) hb_xgrabz( sizeof( yaml_emitter_t ) );

   if( yaml_emitter_initialize( emitter ) )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( yaml_emitter_t * ), &s_gc_emitter_funcs );

      PHB_EMITTER hbemitter = ( PHB_EMITTER ) hb_xgrabz( sizeof( HB_EMITTER ) );

      hbemitter->emitter = emitter;
      *ph = hbemitter;

      hb_retptrGC( ph );
   }
   else
   {
      hb_xfree( emitter );
      hb_retptr( NULL );
   }
}

HB_FUNC( HB_YAML_EMITTER_GET_OUTPUT )
{
   PHB_EMITTER hbemitter = hb_yaml_par_hbemitter( 1 );

   if( hbemitter )
      hb_retclen( ( char * ) hbemitter->buffer, ( HB_SIZE ) hbemitter->len );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_OUTPUT_STRING )
{
   PHB_EMITTER hbemitter = hb_yaml_par_hbemitter( 1 );
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( hbemitter && emitter )
   {
      if( hbemitter->buffer != NULL )
         hb_xfree( hbemitter->buffer );

      hbemitter->size = ( size_t ) hb_parns( 2 );
      hbemitter->buffer = ( unsigned char * ) hb_xgrab( hbemitter->size );
      hbemitter->len = 0;

      yaml_emitter_set_output_string( emitter,
         hbemitter->buffer,
         hbemitter->size,
         &hbemitter->len );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_ENCODING )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_encoding( emitter, ( yaml_encoding_t ) hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_CANONICAL )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_canonical( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_INDENT )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_indent( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_WIDTH )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_width( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_UNICODE )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_unicode( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_BREAK )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      yaml_emitter_set_break( emitter, ( yaml_break_t ) hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Constructor/Destructor */

static HB_GARBAGE_FUNC( document_release )
{
   yaml_document_t ** ptr = ( yaml_document_t ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ptr && *ptr )
   {
      yaml_document_delete( ( yaml_document_t * ) *ptr );

      /* Destroy the object */
      hb_xfree( *ptr );
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gc_document_funcs =
{
   document_release,
   NULL
};

static yaml_document_t * hb_yaml_par_document( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_document_funcs, iParam );

   return ptr ? ( yaml_document_t * ) *ptr : NULL;
}

static void hb_yaml_drop_document( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_document_funcs, iParam );

   if( ptr )
      *ptr = NULL;
}

/* Harbour interface */

HB_FUNC( YAML_DOCUMENT_INITIALIZE )
{
   yaml_document_t * document = ( yaml_document_t * ) hb_xgrabz( sizeof( yaml_document_t ) );

   PHB_ITEM pParam = hb_param( 1, HB_IT_HASH );

   HB_BOOL fVer = HB_FALSE;
   yaml_version_directive_t version_directive;
   yaml_tag_directive_t * tag_directives_start = NULL;
   yaml_tag_directive_t * tag_directives_end = NULL;
   void ** hConvert = NULL;
   HB_SIZE nLen = 0;
   int start_implicit = 0;
   int end_implicit = 0;

   if( pParam )
   {
      if( hb_hashGetCItemPtr( pParam, "minor" ) ||
          hb_hashGetCItemPtr( pParam, "major" ) )
      {
         fVer = HB_TRUE;
         version_directive.minor = hb_itemGetNI( hb_hashGetCItemPtr( pParam, "minor" ) );
         version_directive.major = hb_itemGetNI( hb_hashGetCItemPtr( pParam, "major" ) );
      }

      {
         PHB_ITEM pTags = hb_hashGetCItemPtr( pParam, "tags" );

         if( pTags && ( nLen = hb_hashLen( pTags ) ) > 0 )
         {
            HB_SIZE nItem;

            tag_directives_start = tag_directives_end =
               ( yaml_tag_directive_t * ) hb_xgrab( sizeof( yaml_tag_directive_t ) * nLen );
            hConvert = ( void ** ) hb_xgrab( sizeof( void * ) * nLen * 2 );

            for( nItem = 0; nItem < nLen; ++nItem )
            {
               tag_directives_end->handle = ( yaml_char_t * ) HB_UNCONST( hb_strnull( hb_itemGetStrUTF8( hb_hashGetKeyAt( pTags, nItem + 1 )  , &hConvert[   nItem * 2       ], NULL ) ) );
               tag_directives_end->prefix = ( yaml_char_t * ) HB_UNCONST( hb_strnull( hb_itemGetStrUTF8( hb_hashGetValueAt( pTags, nItem + 1 ), &hConvert[ ( nItem * 2 ) + 1 ], NULL ) ) );
               ++tag_directives_end;
            }
         }
      }

      start_implicit = hb_itemGetNI( hb_hashGetCItemPtr( pParam, "start_implicit" ) );
      end_implicit = hb_itemGetNI( hb_hashGetCItemPtr( pParam, "end_implicit" ) );
   }

   if( yaml_document_initialize( document,
          fVer ? &version_directive : NULL,
          tag_directives_start,
          tag_directives_end,
          start_implicit,
          end_implicit ) )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( yaml_document_t * ), &s_gc_document_funcs );
      *ph = document;
      hb_retptrGC( ph );
   }
   else
   {
      hb_xfree( document );
      hb_retptr( NULL );
   }

   if( tag_directives_start )
      hb_xfree( tag_directives_start );

   if( hConvert )
   {
      HB_SIZE nItem;

      nLen <<= 1;

      for( nItem = 0; nItem < nLen; ++nItem )
         hb_strfree( hConvert[ nItem ] );

      hb_xfree( hConvert );
   }
}

HB_FUNC( YAML_DOCUMENT_ADD_SCALAR )
{
   yaml_document_t * document = hb_yaml_par_document( 1 );

   if( document && HB_ISCHAR( 3 ) )
   {
      void * hTag;
      void * hValue;
      HB_SIZE nValueLen;
      const char * pValue = hb_parstr_utf8( 3, &hValue, &nValueLen );

      hb_retni( yaml_document_add_scalar( document,
         ( yaml_char_t * ) HB_UNCONST( hb_parstr_utf8( 2, &hTag, NULL ) ),
         ( yaml_char_t * ) HB_UNCONST( pValue ),
         ( int ) nValueLen,
         ( yaml_scalar_style_t ) hb_parni( 4 ) ) );

      hb_strfree( hTag );
      hb_strfree( hValue );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_DOCUMENT_ADD_SEQUENCE )
{
   yaml_document_t * document = hb_yaml_par_document( 1 );

   if( document )
   {
      void * hTag;

      hb_retni( yaml_document_add_sequence( document,
         ( yaml_char_t * ) HB_UNCONST( hb_parstr_utf8( 2, &hTag, NULL ) ),
         ( yaml_sequence_style_t ) hb_parni( 3 ) ) );

      hb_strfree( hTag );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_DOCUMENT_ADD_MAPPING )
{
   yaml_document_t * document = hb_yaml_par_document( 1 );

   if( document )
   {
      void * hTag;

      hb_retni( yaml_document_add_mapping( document,
         ( yaml_char_t * ) HB_UNCONST( hb_parstr_utf8( 2, &hTag, NULL ) ),
         ( yaml_mapping_style_t ) hb_parni( 3 ) ) );

      hb_strfree( hTag );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_DOCUMENT_APPEND_SEQUENCE_ITEM )
{
   yaml_document_t * document = hb_yaml_par_document( 1 );

   if( document )
      hb_retni( yaml_document_append_sequence_item( document,
         hb_parni( 2 ) /* sequence */,
         hb_parni( 3 ) /* item */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_DOCUMENT_APPEND_MAPPING_PAIR )
{
   yaml_document_t * document = hb_yaml_par_document( 1 );

   if( document )
      hb_retni( yaml_document_append_mapping_pair( document,
         hb_parni( 2 ) /* mapping */,
         hb_parni( 3 ) /* key */,
         hb_parni( 4 ) /* value */ ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_OPEN )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      hb_retni( yaml_emitter_open( emitter ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_DUMP )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );
   yaml_document_t * document = hb_yaml_par_document( 2 );

   if( emitter && document )
   {
      hb_retni( yaml_emitter_dump( emitter, document ) );
      /* 'document' object is handled by the emitter object at this point
         and will be released automatically on yaml_emitter_delete(). Thus
         we need to make sure we don't delete it when deleting the document
         object, by NULL-ing it in the Harbour level object. [vszakats] */
      hb_yaml_drop_document( 2 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_CLOSE )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      hb_retni( yaml_emitter_close( emitter ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_FLUSH )
{
   yaml_emitter_t * emitter = hb_yaml_par_emitter( 1 );

   if( emitter )
      hb_retni( yaml_emitter_flush( emitter ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_PARSER_LOAD )
{
   yaml_parser_t * parser = hb_yaml_par_parser( 1 );

   if( parser )
   {
      yaml_document_t * document = ( yaml_document_t * ) hb_xgrabz( sizeof( yaml_document_t ) );

      if( yaml_parser_load( parser, document ) )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( yaml_document_t * ), &s_gc_document_funcs );
         *ph = document;
         hb_retptrGC( ph );
      }
      else
      {
         hb_xfree( document );
         hb_retptr( NULL );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
yaml_stream_start_event_initialize(yaml_event_t *event,
yaml_stream_end_event_initialize(yaml_event_t *event);
yaml_document_start_event_initialize(yaml_event_t *event,
yaml_document_end_event_initialize(yaml_event_t *event, int implicit);
yaml_alias_event_initialize(yaml_event_t *event, yaml_char_t *anchor);
yaml_scalar_event_initialize(yaml_event_t *event,
yaml_sequence_start_event_initialize(yaml_event_t *event,
yaml_sequence_end_event_initialize(yaml_event_t *event);
yaml_mapping_start_event_initialize(yaml_event_t *event,
yaml_mapping_end_event_initialize(yaml_event_t *event);

yaml_document_get_node(yaml_document_t *document, int index);
yaml_document_get_root_node(yaml_document_t *document);
yaml_emitter_emit(yaml_emitter_t *emitter, yaml_event_t *event);
*/
