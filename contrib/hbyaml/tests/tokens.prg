/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

#include "simpleio.ch"

PROCEDURE Main( cFileName )

   LOCAL parser, token
   LOCAL v1, v2, v3

   ? yaml_get_version_string()
   yaml_get_version( @v1, @v2, @v3 )
   ? hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 )

   IF Empty( parser := yaml_parser_initialize() )
      RETURN
   ENDIF

   yaml_parser_set_input_string( parser, ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "../../../.travis.yml" ) ) ) )

   DO WHILE HB_ISHASH( token := yaml_parser_scan( parser ) )

      ? hb_ValToExp( token )

      SWITCH token[ "type" ]
      /* Stream start/end */
      CASE YAML_STREAM_START_TOKEN
         ? "STREAM START"
         EXIT
      CASE YAML_STREAM_END_TOKEN
         ? "STREAM END"
         EXIT

      /* Token types (read before actual token) */
      CASE YAML_KEY_TOKEN
         ? "(Key token)"
         EXIT
      CASE YAML_VALUE_TOKEN
         ? "(Value token)"
         EXIT

      /* Block delimiters */
      CASE YAML_BLOCK_SEQUENCE_START_TOKEN
         ? "<b>Start Block (Sequence)</b>"
         EXIT
      CASE YAML_BLOCK_ENTRY_TOKEN
         ? "<b>Start Block (Entry)</b>"
         EXIT
      CASE YAML_BLOCK_END_TOKEN
         ? "<b>End block</b>"
         EXIT

      /* Data */
      CASE YAML_BLOCK_MAPPING_START_TOKEN
         ? "[Block mapping]"
         EXIT
      CASE YAML_SCALAR_TOKEN
         ? "scalar", token[ "scalar" ]
         EXIT

      /* Others */
      OTHERWISE
         ? "Got token of type", token[ "type" ]
      ENDSWITCH

      IF token[ "type" ] == YAML_STREAM_END_TOKEN
         EXIT
      ENDIF
   ENDDO

   RETURN
