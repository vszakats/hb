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

FUNCTION hb_yaml_encode( xValue, nMaxSize )

   LOCAL emitter, document

   IF Empty( emitter := yaml_emitter_initialize() )
      RETURN ""
   ENDIF

   yaml_emitter_set_output_string( emitter, hb_defaultValue( nMaxSize, 0x20000 ) )

   IF yaml_emitter_open( emitter ) == 0
      RETURN ""
   ENDIF

   IF Empty( document := yaml_document_initialize( { ;
         "start_implicit" => 1, ;
         "end_implicit" => 1 } ) )
      RETURN ""
   ENDIF

   s_emit_value( document, xValue )

   IF yaml_emitter_dump( emitter, document ) == 0 .OR. ;
      yaml_emitter_flush( emitter ) == 0 .OR. ;
      yaml_emitter_close( emitter ) == 0
      RETURN ""
   ENDIF

   RETURN hb_yaml_emitter_get_output( emitter )

STATIC FUNCTION s_emit_value( document, value )

   LOCAL node, item

   SWITCH ValType( value )
   CASE "H"
      node := yaml_document_add_mapping( document )
      FOR EACH item IN value
         yaml_document_append_mapping_pair( document, node, ;
            yaml_document_add_scalar( document,, item:__enumKey() ), ;
            s_emit_value( document, item ) )
      NEXT
      EXIT
   CASE "A"
      node := yaml_document_add_sequence( document )
      FOR EACH item IN value
         yaml_document_append_sequence_item( document, node, ;
            s_emit_value( document, item ) )
      NEXT
      EXIT
   OTHERWISE
      SWITCH ValType( value )
      CASE "M" /* fallthrough */
      CASE "C" ; EXIT
      CASE "N" ; value := hb_ntos( value ) ; EXIT
      CASE "L" ; value := iif( value, "1", "0" ) ; EXIT
      CASE "D" ; value := DToS( value ) ; EXIT
      CASE "T" ; value := hb_TToS( value ) ; EXIT
      OTHERWISE ; value := "null"
      ENDSWITCH
      node := yaml_document_add_scalar( document,, value )
   ENDSWITCH

   RETURN node
