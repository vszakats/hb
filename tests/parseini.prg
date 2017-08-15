/* Test for .ini file reading/writing by Giancarlo Niccolai */

PROCEDURE Main( cName )

   LOCAL cIni, hIni, aSect, cValue
   LOCAL nRow

   SetColor( "W+/B" )
   CLS

   nRow := 1
   @ nRow++, 20 SAY "Harbour - .ini file parser test"
   @ nRow++,  5 SAY "Call from command-line using a .ini file name as the only parameter"
   nRow++

   IF Empty( cName )
      cName := "parseini.ini"
      @ nRow++, 5 SAY "Using default parseini.ini file"
   ENDIF

   @ nRow, 0

   ? "Content of", cName

   IF Empty( hIni := hb_iniRead( cName ) )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH aSect IN hIni
         ?
         ? "Section [" + aSect:__enumKey() + "]"

         FOR EACH cValue IN aSect
            ? cValue:__enumKey(), "=", cValue
         NEXT
      NEXT
   ENDIF

   ?
   ? "Adding section 'Added', with key NEW := new"
   hIni[ "Added" ] := { "NEW" => "new" }

   ? "Writing output to parseini_out.ini"
   IF hb_iniWrite( "parseini_out.ini", hIni, "#Generated file; don't touch", "#End of file" )
      ? "File written"
   ELSE
      ? "Could not write file"
   ENDIF
   ?
   WAIT

   @ 3, 0 CLEAR
   ?
   ? "REPEATING TESTS WITHOUT AUTOMATIC MAIN SECTION"
   ?
   ? "Content of", cName

   IF Empty( hIni := hb_iniRead( cName, ;
                                 /* default case */, ;
                                 /* default key indicators */, .F. ) )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH aSect IN hIni

         /* Now (without automatic main), top-level options may be in the root hash */
         IF HB_ISHASH( aSect )
            /* It's a section */
            ?
            ? "Section [" + aSect:__enumKey() + "]"

            FOR EACH cValue IN aSect
               ? cValue:__enumKey(), "=", cValue
            NEXT
         ELSE
            /* It's a top-level option */
            ? "Top-level option:", aSect:__enumKey(), "=", aSect
         ENDIF
      NEXT
   ENDIF

   ?
   ? "Adding section 'Added', with key NEW := new"
   hIni[ "Added" ] := { "NEW" => "new" }

   ? "Writing output to parseini_out1.ini"
   IF hb_iniWrite( "parseini_out1.ini", hIni, ;
                   "#Generated file without main auto section; don't touch", "#End of file", ;
                   .F. )
      ? "File written"
   ELSE
      ? "Could not write file"
   ENDIF
   ?
   WAIT

   @ 3, 0 CLEAR
   ?
   ? "WRITING .ini TO A STRING"
   ?

   cIni := hb_iniWriteStr( hIni )

   ? "Content of hIni:"
   ?
   ? cIni
   ?
   WAIT

   @ 3, 0 CLEAR
   ?
   ? "READING .ini FILE FROM A STRING"
   ?
   ? "Content:"

   IF Empty( hIni := hb_iniReadStr( cIni, ;
                                    /* default case */, ;
                                    /* default key indicators */, .F. ) )
      ? "Not a valid .ini file!"
   ELSE
      FOR EACH aSect IN hIni

         /* Now (without automatic main), top-level options may be in the root hash */
         IF HB_ISHASH( aSect )
            /* It's a section */
            ?
            ? "Section [" + aSect:__enumKey() + "]"

            FOR EACH cValue IN aSect
               ? cValue:__enumKey(), "=", cValue
            NEXT
         ELSE
            /* It's a top-level option */
            ? "Top-level option:", aSect:__enumKey(), "=", aSect
         ENDIF
      NEXT
   ENDIF

   ?
   WAIT

   @ 3, 0 CLEAR
   ?
   ? "WRITING .ini FILE TO A STRING"
   ?

   ? "Content of", cName
   ?
   ? hb_iniWriteStr( hb_iniRead( cName ) )
   ?
   WAIT

   RETURN
