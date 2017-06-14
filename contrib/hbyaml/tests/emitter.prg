/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */

#require "hbyaml"

#include "simpleio.ch"

PROCEDURE Main( cFileName )

   LOCAL parser, emitter, document

   IF Empty( parser := yaml_parser_initialize() )
      RETURN
   ENDIF

   yaml_parser_set_input_string( parser, ;
      hb_MemoRead( hb_defaultValue( cFileName, ;
         hb_DirSepToOS( "../../../.travis.yml" ) ) ) )

   IF Empty( emitter := yaml_emitter_initialize() )
      RETURN
   ENDIF

   yaml_emitter_set_output_string( emitter, 0x20000 )
   yaml_emitter_set_indent( emitter, 4 )

   ? document := yaml_parser_load( parser )

   ? yaml_emitter_open( emitter )
   ? yaml_emitter_dump( emitter, document )
   ? yaml_emitter_close( emitter )
   ? yaml_emitter_flush( emitter )

   ? "|" + hb_yaml_emitter_get_output( emitter ) + "|"

#if 0
   ? document := yaml_document_initialize( { ;
      "major" => 1, ;
      "minor" => 0 } )
#endif

   RETURN
