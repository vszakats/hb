/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

#include "simpleio.ch"

PROCEDURE Main( cFileName )

   LOCAL parser, event

   IF ! Empty( parser := yaml_parser_initialize() )

      yaml_parser_set_input_string( parser, ;
         hb_MemoRead( hb_defaultValue( cFileName, ;
            hb_DirSepToOS( "../../../.travis.yml" ) ) ) )

      DO WHILE HB_ISHASH( event := yaml_parser_parse( parser ) )

         event[ "type_str" ] := s_yaml_event_str( event[ "type" ] )

         ? hb_ValToExp( event )

         IF event[ "type" ] == YAML_STREAM_END_EVENT
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN

STATIC FUNCTION s_yaml_event_str( nType )

   IF ! HB_ISNUMERIC( nType )
      RETURN "HB_YAML_EVENT_INVALID"
   ENDIF

   SWITCH nType
   CASE YAML_NO_EVENT             ; RETURN "YAML_NO_EVENT"
   CASE YAML_STREAM_START_EVENT   ; RETURN "YAML_STREAM_START_EVENT"
   CASE YAML_STREAM_END_EVENT     ; RETURN "YAML_STREAM_END_EVENT"
   CASE YAML_DOCUMENT_START_EVENT ; RETURN "YAML_DOCUMENT_START_EVENT"
   CASE YAML_DOCUMENT_END_EVENT   ; RETURN "YAML_DOCUMENT_END_EVENT"
   CASE YAML_ALIAS_EVENT          ; RETURN "YAML_ALIAS_EVENT"
   CASE YAML_SCALAR_EVENT         ; RETURN "YAML_SCALAR_EVENT"
   CASE YAML_SEQUENCE_START_EVENT ; RETURN "YAML_SEQUENCE_START_EVENT"
   CASE YAML_SEQUENCE_END_EVENT   ; RETURN "YAML_SEQUENCE_END_EVENT"
   CASE YAML_MAPPING_START_EVENT  ; RETURN "YAML_MAPPING_START_EVENT"
   CASE YAML_MAPPING_END_EVENT    ; RETURN "YAML_MAPPING_END_EVENT"
   ENDSWITCH

   RETURN "HB_YAML_EVENT_UNRECOGNIZED_" + hb_ntos( nType )
