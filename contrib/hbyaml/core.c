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

/* Constructor/Destructor */

static HB_GARBAGE_FUNC( parser_release )
{
   yaml_parser_t ** ptr = ( yaml_parser_t ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ptr && *ptr )
   {
      yaml_parser_delete( ( yaml_parser_t * ) *ptr );

      /* Destroy the object */
      hb_xfree( *ptr );
      *ptr = NULL;
   }
}

static const HB_GC_FUNCS s_gc_parser_funcs =
{
   parser_release,
   NULL
};

yaml_parser_t * hb_yaml_par_parser( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_parser_funcs, iParam );

   return ptr ? ( yaml_parser_t * ) *ptr : NULL;
}

/* Harbour interface */

HB_FUNC( YAML_PARSER_INITIALIZE )
{
   yaml_parser_t * parser = ( yaml_parser_t * ) hb_xgrabz( sizeof( yaml_parser_t ) );

   if( yaml_parser_initialize( parser ) )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( yaml_parser_t * ), &s_gc_parser_funcs );
      *ph = parser;
      hb_retptrGC( ph );
   }
   else
   {
      hb_xfree( parser );
      hb_retptr( NULL );
   }
}

HB_FUNC( YAML_PARSER_SET_INPUT_STRING )
{
   yaml_parser_t * parser = hb_yaml_par_parser( 1 );

   if( parser )
      yaml_parser_set_input_string( parser, ( const unsigned char * ) hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static void s_token_ret( yaml_token_t * token )
{
   PHB_ITEM hReturn = hb_hashNew( NULL );

   PHB_ITEM pKey = hb_itemNew( NULL );
   PHB_ITEM pVal = hb_itemNew( NULL );

   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "type" ), hb_itemPutNI( pVal, ( int ) token->type ) );

   switch( token->type )
   {
      case YAML_NO_TOKEN:
      case YAML_STREAM_END_TOKEN:
      case YAML_DOCUMENT_START_TOKEN:
      case YAML_DOCUMENT_END_TOKEN:
      case YAML_BLOCK_SEQUENCE_START_TOKEN:
      case YAML_BLOCK_MAPPING_START_TOKEN:
      case YAML_BLOCK_END_TOKEN:
      case YAML_FLOW_SEQUENCE_START_TOKEN:
      case YAML_FLOW_SEQUENCE_END_TOKEN:
      case YAML_FLOW_MAPPING_START_TOKEN:
      case YAML_FLOW_MAPPING_END_TOKEN:
      case YAML_BLOCK_ENTRY_TOKEN:
      case YAML_FLOW_ENTRY_TOKEN:
      case YAML_KEY_TOKEN:
      case YAML_VALUE_TOKEN:
         break;
      case YAML_STREAM_START_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "encoding" ), hb_itemPutNI( pVal, ( int ) token->data.stream_start.encoding ) );
         break;
      case YAML_VERSION_DIRECTIVE_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "major" ), hb_itemPutNI( pVal, token->data.version_directive.major ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "minor" ), hb_itemPutNI( pVal, token->data.version_directive.minor ) );
         break;
      case YAML_TAG_DIRECTIVE_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "handle" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.tag_directive.handle ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "prefix" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.tag_directive.prefix ) );
         break;
      case YAML_ALIAS_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "value" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.alias.value ) );
         break;
      case YAML_ANCHOR_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "value" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.anchor.value ) );
         break;
      case YAML_TAG_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "handle" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.tag.handle ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "suffix" ), hb_itemPutStrUTF8( pVal, ( const char * ) token->data.tag.suffix ) );
         break;
      case YAML_SCALAR_TOKEN:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "scalar" ), hb_itemPutStrLenUTF8( pVal, ( const char * ) token->data.scalar.value, token->data.scalar.length ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "style" ), hb_itemPutNI( pVal, ( int ) token->data.scalar.style ) );
         break;
   }

   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "start_line" ), hb_itemPutNS( pVal, ( HB_SIZE ) token->start_mark.line ) );
   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "start_column" ), hb_itemPutNS( pVal, ( HB_SIZE ) token->start_mark.column ) );

   hb_itemReturnRelease( hReturn );

   hb_itemRelease( pVal );
   hb_itemRelease( pKey );
}

HB_FUNC( YAML_PARSER_SCAN )
{
   yaml_parser_t * parser = hb_yaml_par_parser( 1 );

   if( parser )
   {
      yaml_token_t token;
      if( yaml_parser_scan( parser, &token ) == 1 )
         s_token_ret( &token );
      yaml_token_delete( &token );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_PARSER_SET_ENCODING )
{
   yaml_parser_t * parser = hb_yaml_par_parser( 1 );

   if( parser )
      yaml_parser_set_encoding( parser, ( yaml_encoding_t ) hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_GET_VERSION_STRING )
{
   hb_retc_const( yaml_get_version_string() );
}

HB_FUNC( YAML_GET_VERSION )
{
   int major, minor, patch;

   yaml_get_version( &major, &minor, &patch );

   hb_storni( major, 1 );
   hb_storni( minor, 2 );
   hb_storni( patch, 3 );
}

static void s_event_ret( yaml_event_t * event )
{
   PHB_ITEM hReturn = hb_hashNew( NULL );

   PHB_ITEM pKey = hb_itemNew( NULL );
   PHB_ITEM pVal = hb_itemNew( NULL );

   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "type" ), hb_itemPutNI( pVal, ( int ) event->type ) );

   switch( event->type )
   {
      case YAML_NO_EVENT:
      case YAML_STREAM_END_EVENT:
      case YAML_SEQUENCE_END_EVENT:
      case YAML_MAPPING_END_EVENT:
         break;
      case YAML_STREAM_START_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "encoding" ), hb_itemPutNI( pVal, ( int ) event->data.stream_start.encoding ) );
         break;
      case YAML_DOCUMENT_START_EVENT:
         if( event->data.document_start.version_directive != NULL )
         {
            hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "major" ), hb_itemPutNI( pVal, event->data.document_start.version_directive->major ) );
            hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "minor" ), hb_itemPutNI( pVal, event->data.document_start.version_directive->minor ) );
         }
         if( event->data.document_start.tag_directives.start != NULL &&
             event->data.document_start.tag_directives.end != NULL )
         {
            yaml_tag_directive_t * tag_directive;
            PHB_ITEM pTags = hb_hashNew( NULL );

            for( tag_directive = event->data.document_start.tag_directives.start;
                 tag_directive != event->data.document_start.tag_directives.end;
                 ++tag_directive )
            {
               hb_hashAdd( pTags,
                  hb_itemPutStrUTF8( pKey, ( const char * ) tag_directive->handle ),
                  hb_itemPutStrUTF8( pVal, ( const char * ) tag_directive->prefix ) );
            }

            hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "tag_directives" ), pTags );

            hb_itemRelease( pTags );
         }
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "implicit" ), hb_itemPutNI( pVal, event->data.document_start.implicit ) );
         break;
      case YAML_DOCUMENT_END_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "implicit" ), hb_itemPutNI( pVal, event->data.document_start.implicit ) );
         break;
      case YAML_ALIAS_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "anchor" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.alias.anchor ) );
         break;
      case YAML_SCALAR_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "anchor" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.scalar.anchor ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "tag" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.scalar.tag ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "value" ), hb_itemPutStrLenUTF8( pVal, ( const char * ) event->data.scalar.value, event->data.scalar.length ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "plain_implicit" ), hb_itemPutNI( pVal, event->data.scalar.plain_implicit ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "quoted_implicit" ), hb_itemPutNI( pVal, event->data.scalar.quoted_implicit ) );
         break;
      case YAML_SEQUENCE_START_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "anchor" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.sequence_start.anchor ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "tag" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.sequence_start.tag ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "implicit" ), hb_itemPutNI( pVal, event->data.sequence_start.implicit ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "style" ), hb_itemPutNI( pVal, ( int ) event->data.sequence_start.style ) );
         break;
      case YAML_MAPPING_START_EVENT:
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "anchor" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.mapping_start.anchor ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "tag" ), hb_itemPutStrUTF8( pVal, ( const char * ) event->data.mapping_start.tag ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "implicit" ), hb_itemPutNI( pVal, event->data.mapping_start.implicit ) );
         hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "style" ), hb_itemPutNI( pVal, ( int ) event->data.mapping_start.style ) );
         break;
   }

   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "start_line" ), hb_itemPutNS( pVal, ( HB_SIZE ) event->start_mark.line ) );
   hb_hashAdd( hReturn, hb_itemPutCConst( pKey, "start_column" ), hb_itemPutNS( pVal, ( HB_SIZE ) event->start_mark.column ) );

   hb_itemReturnRelease( hReturn );

   hb_itemRelease( pVal );
   hb_itemRelease( pKey );
}

HB_FUNC( YAML_PARSER_PARSE )
{
   yaml_parser_t * parser = hb_yaml_par_parser( 1 );

   if( parser )
   {
      yaml_event_t event;
      if( yaml_parser_parse( parser, &event ) == 1 )
         s_event_ret( &event );
      yaml_event_delete( &event );
   }
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
