/*
 * libyaml API - Harbour interface (high-level)
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

#include "hbyaml.ch"

#ifdef HBYAML_TRACE
#xtranslate _TRACE( [<x,...>] ) => OutStd( <x> ); OutStd( hb_eol() )
#else
#xtranslate _TRACE( [<x,...>] ) =>
#endif

FUNCTION hb_yaml_decode( cFile, /* @ */ meta )

   LOCAL parser, token, root, parents, anchors, tags
   LOCAL key, val, node, exp, level, type, tmp
   LOCAL anchor_stack := {}

   meta := { => }

   IF Empty( parser := yaml_parser_initialize() )
      s_log_msg( meta, "error", "failed to initialize" )
   ELSE
      yaml_parser_set_input_string( parser, cFile )

      level := 0
      parents := {}
      exp := 0
      anchors := { => }
      tags := { => }

      DO WHILE HB_ISHASH( token := yaml_parser_scan( parser ) )

         type := token[ "type" ]

         _TRACE( Space( level * 3 ) + s_yaml_token_str( type ), hb_ValToExp( token ) )

         SWITCH type
         CASE YAML_VALUE_TOKEN
         CASE YAML_BLOCK_ENTRY_TOKEN
         CASE YAML_FLOW_ENTRY_TOKEN
            s_add( token, meta, val, key, type )  /* pre-fill with null */
            /* fallthrough */
         CASE YAML_KEY_TOKEN
            exp := type  /* expected type in next YAML_SCALAR_TOKEN */
            EXIT

         CASE YAML_SCALAR_TOKEN
            SWITCH exp
            CASE YAML_KEY_TOKEN
               key := token[ "scalar" ]
               EXIT
            CASE YAML_VALUE_TOKEN
            CASE YAML_FLOW_ENTRY_TOKEN
            CASE YAML_BLOCK_ENTRY_TOKEN
               IF Len( anchor_stack ) > 0 .AND. ATail( anchor_stack )[ 2 ] == level
                  anchors[ ATail( anchor_stack )[ 1 ] ] := token[ "scalar" ]
                  ASize( anchor_stack, Len( anchor_stack ) - 1 )
               ENDIF
               s_add( token, meta, val, key, exp, token[ "scalar" ] )
               EXIT
            ENDSWITCH
            EXIT

         CASE YAML_TAG_TOKEN
            s_log_msg( meta, "warning", ;
               hb_StrFormat( "line: %1$d, column: %2$d: tag token not supported", ;  /* TODO */
                  token[ "start_line" ], token[ "start_column" ] ) )
            EXIT

         CASE YAML_ALIAS_TOKEN
            IF token[ "value" ] $ anchors
               node := anchors[ token[ "value" ] ]
               DO CASE
               CASE HB_ISHASH( node )
                  node := hb_HValueAt( node, 1 )
                  DO CASE
                  CASE HB_ISHASH( node )
                     s_add( token, meta, val, val, exp, node )
                  CASE HB_ISARRAY( node )
                     FOR EACH tmp IN node
                        s_add( token, meta, val, val, exp, tmp )
                     NEXT
                  ENDCASE
               CASE HB_ISSTRING( node )
                  s_add( token, meta, val, key, exp, node )
               ENDCASE
            ENDIF
            EXIT

         CASE YAML_FLOW_SEQUENCE_START_TOKEN
         CASE YAML_FLOW_MAPPING_START_TOKEN
         CASE YAML_BLOCK_SEQUENCE_START_TOKEN
         CASE YAML_BLOCK_MAPPING_START_TOKEN
            ++level
            AAdd( parents, { key, val, exp } )
            val := iif( type == YAML_BLOCK_SEQUENCE_START_TOKEN .OR. ;
                        type == YAML_FLOW_SEQUENCE_START_TOKEN, {}, { => } )
            key := NIL
            IF root == NIL
               root := val
            ENDIF
            EXIT

         CASE YAML_FLOW_SEQUENCE_END_TOKEN
         CASE YAML_FLOW_MAPPING_END_TOKEN
         CASE YAML_BLOCK_END_TOKEN
            node := val
            IF Len( anchor_stack ) > 0 .AND. ATail( anchor_stack )[ 2 ] == level
               anchors[ ATail( anchor_stack )[ 1 ] ] := hb_HClone( val )
               ASize( anchor_stack, Len( anchor_stack ) - 1 )
            ENDIF
            key := ATail( parents )[ 1 ]
            val := ATail( parents )[ 2 ]
            exp := ATail( parents )[ 3 ]
            ASize( parents, Len( parents ) - 1 )
            s_add( token, meta, val, key, exp, node )
            --level
            EXIT

         CASE YAML_ANCHOR_TOKEN
            AAdd( anchor_stack, { token[ "value" ], level } )
            EXIT

         CASE YAML_TAG_DIRECTIVE_TOKEN
            tags[ token[ "handle" ] ] := token[ "prefix" ]
            EXIT

         CASE YAML_VERSION_DIRECTIVE_TOKEN
            meta[ "major" ] := token[ "major" ]
            meta[ "minor" ] := token[ "minor" ]
            EXIT

         ENDSWITCH

         IF type == YAML_STREAM_END_TOKEN
            EXIT
         ENDIF
      ENDDO
   ENDIF

   _TRACE( hb_ValToExp( anchors ) )
   _TRACE( hb_ValToExp( tags ) )
   _TRACE( hb_ValToExp( meta ) )

   RETURN root

STATIC PROCEDURE s_add( token, meta, val, key, type, new )

   LOCAL final := PCount() > 5  /* false when pre-filling with null */

   _TRACE( "s_add", hb_ValToExp( hb_AParams() ) )

   DO CASE
   CASE HB_ISARRAY( val )

      IF final .AND. ATail( val ) == NIL .AND. Len( val ) > 0
         val[ Len( val ) ] := new  /* replace pre-filled array tail */
      ELSE
         AAdd( val, new )
      ENDIF

   CASE HB_ISHASH( val )

      IF HB_ISSTRING( key )
         IF type == YAML_FLOW_ENTRY_TOKEN .OR. ;
            type == YAML_BLOCK_ENTRY_TOKEN

            IF ! key $ val .OR. ;
               val[ key ] == NIL  /* replace pre-filled hash element */
               val[ key ] := {}
            ELSEIF ! HB_ISARRAY( val[ key ] )
               RETURN
            ENDIF

            val := val[ key ]
            IF final .AND. ATail( val ) == NIL .AND. Len( val ) > 0
               val[ Len( val ) ] := new  /* replace pre-filled array tail */
            ELSE
               AAdd( val, new )
            ENDIF
         ELSE
            val[ key ] := new
         ENDIF
      ELSE
         s_log_msg( meta, "error", ;
            hb_StrFormat( "line: %1$d, column: %2$d: key expected", ;
               token[ "start_line" ], token[ "start_column" ] ) )
      ENDIF

   ENDCASE

   RETURN

STATIC PROCEDURE s_log_msg( meta, cat, cMessage )

   IF ! cat $ meta
      meta[ cat ] := {}
   ENDIF

   AAdd( meta[ cat ], cMessage )

   RETURN

#ifdef HBYAML_TRACE
STATIC FUNCTION s_yaml_token_str( nType )

   IF ! HB_ISNUMERIC( nType )
      RETURN "HB_YAML_TOKEN_INVALID"
   ENDIF

   SWITCH nType
   CASE YAML_NO_TOKEN                   ; RETURN "YAML_NO_TOKEN"
   CASE YAML_STREAM_START_TOKEN         ; RETURN "YAML_STREAM_START_TOKEN"
   CASE YAML_STREAM_END_TOKEN           ; RETURN "YAML_STREAM_END_TOKEN"
   CASE YAML_VERSION_DIRECTIVE_TOKEN    ; RETURN "YAML_VERSION_DIRECTIVE_TOKEN"
   CASE YAML_TAG_DIRECTIVE_TOKEN        ; RETURN "YAML_TAG_DIRECTIVE_TOKEN"
   CASE YAML_DOCUMENT_START_TOKEN       ; RETURN "YAML_DOCUMENT_START_TOKEN"
   CASE YAML_DOCUMENT_END_TOKEN         ; RETURN "YAML_DOCUMENT_END_TOKEN"
   CASE YAML_BLOCK_SEQUENCE_START_TOKEN ; RETURN "YAML_BLOCK_SEQUENCE_START_TOKEN"
   CASE YAML_BLOCK_MAPPING_START_TOKEN  ; RETURN "YAML_BLOCK_MAPPING_START_TOKEN"
   CASE YAML_BLOCK_END_TOKEN            ; RETURN "YAML_BLOCK_END_TOKEN"
   CASE YAML_FLOW_SEQUENCE_START_TOKEN  ; RETURN "YAML_FLOW_SEQUENCE_START_TOKEN"
   CASE YAML_FLOW_SEQUENCE_END_TOKEN    ; RETURN "YAML_FLOW_SEQUENCE_END_TOKEN"
   CASE YAML_FLOW_MAPPING_START_TOKEN   ; RETURN "YAML_FLOW_MAPPING_START_TOKEN"
   CASE YAML_FLOW_MAPPING_END_TOKEN     ; RETURN "YAML_FLOW_MAPPING_END_TOKEN"
   CASE YAML_BLOCK_ENTRY_TOKEN          ; RETURN "YAML_BLOCK_ENTRY_TOKEN"
   CASE YAML_FLOW_ENTRY_TOKEN           ; RETURN "YAML_FLOW_ENTRY_TOKEN"
   CASE YAML_KEY_TOKEN                  ; RETURN "YAML_KEY_TOKEN"
   CASE YAML_VALUE_TOKEN                ; RETURN "YAML_VALUE_TOKEN"
   CASE YAML_ALIAS_TOKEN                ; RETURN "YAML_ALIAS_TOKEN"
   CASE YAML_ANCHOR_TOKEN               ; RETURN "YAML_ANCHOR_TOKEN"
   CASE YAML_TAG_TOKEN                  ; RETURN "YAML_TAG_TOKEN"
   CASE YAML_SCALAR_TOKEN               ; RETURN "YAML_SCALAR_TOKEN"
   ENDSWITCH

   RETURN "HB_YAML_TOKEN_UNRECOGNIZED_" + hb_ntos( nType )
#endif
