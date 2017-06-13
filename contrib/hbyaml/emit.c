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

#include "yaml.h"

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#include "hbyaml.ch"

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

static yaml_emitter_t * emitter_par( int iParam )
{
   void ** ptr = ( void ** ) hb_parptrGC( &s_gc_emitter_funcs, iParam );

   return ptr ? ( ( PHB_EMITTER ) *ptr )->emitter : NULL;
}

static PHB_EMITTER emitter_par_hb( int iParam )
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
   PHB_EMITTER hbemitter = emitter_par_hb( 1 );

   if( hbemitter )
      hb_retclen( ( char * ) hbemitter->buffer, ( HB_SIZE ) hbemitter->len );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_OUTPUT_STRING )
{
   PHB_EMITTER hbemitter = emitter_par_hb( 1 );
   yaml_emitter_t * emitter = emitter_par( 1 );

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
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_encoding( emitter, ( yaml_encoding_t ) hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_CANONICAL )
{
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_canonical( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_INDENT )
{
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_indent( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_WIDTH )
{
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_width( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_UNICODE )
{
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_unicode( emitter, hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( YAML_EMITTER_SET_BREAK )
{
   yaml_emitter_t * emitter = emitter_par( 1 );

   if( emitter )
      yaml_emitter_set_break( emitter, ( yaml_break_t ) hb_parni( 2 ) );
   else
      hb_errRT_BASE( EG_ARG, 2040, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
