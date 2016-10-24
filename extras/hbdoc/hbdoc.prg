/*
 * Document generator
 *
 * Copyright 2009 April White <bright.tigra gmail.com>
 * Copyright 1999-2003 Luiz Rafael Culik <culikr@uol.com.br> (Portions of this project are based on hbdoc)
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

/* TODO:
   - handle preformatted text / code / etc
   - include links back to index
   - include jumps to 'top'
   - 'coverage' to have links to corresponding file
   - 'Filename' must return the same file name all of the time for the same source
      - one method to retrieve, one to add?
      - key-value pair [hash table?]

   TODO: - treat '<fixed>' / </fixed> as an non-conformance condition
   ntf: this may be okay for EXAMPLES and TESTS but this is also used
        within other sections, much like <table>

   TODO: - look for embedded 'fixed'

   done - recognize and accept </par>; see macro.txt output esp. hb_SetMacro()
   done - list 'compliance' and 'platforms' within help
   done - list 'category' and 'subcategory' types on help screen
   done - load into memory (class and) method template
   done - minimize these to the barest
   done - build a list of 'categories' and validate against; see what 'classdoc' uses
   done - validate sources against these templates
*/

#include "directry.ch"
#include "hbclass.ch"
#include "hbver.ch"

#include "hbdoc.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST HB_CODEPAGE_UTF8EX

#define BASE_DIR        ".." + hb_ps() + ".." + hb_ps()

#define OnOrOff( b )    iif( b, "excluded", "included" )
#define YesOrNo( b )    iif( b, "yes", "no" )
#define IsDefault( b )  iif( b, "; default", "" )

STATIC sc_aExclusions := { "class_tp.txt", "hdr_tpl.txt" }
STATIC sc_hConstraint
STATIC s_hSwitches

PROCEDURE Main( ... )

   LOCAL aArgs := hb_AParams()
   LOCAL idx, idx2, item, item4
   LOCAL arg
   LOCAL cArgName
   LOCAL cFormat
   LOCAL oDocument, oIndex
   LOCAL aContent

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   init_Templates()

   s_hSwitches := { ;
      /* configuration settings, values, etc */ ;
      "basedir"             => BASE_DIR, ;
      "lang"                => "en", ;
      "doc"                 => .T., ;
      "source"              => .F., ;
      "contribs"            => .T., ;
      "format"              => {}, ;
      "output"              => "category", ;
      "include-doc-source"  => .F., ;
      "include-doc-version" => .F., ;
      "immediate-errors"    => .F., ;
      /* internal settings, values, etc */ ;
      "DELIMITER"           => "$", ;
      "format-list"         => { "text", "ascii", "html", "xml", "rtf", "hpc", "ngi", "os2", "chm", "ch2", "pdf", "trf", "doc", "dbf", "all" }, ;
      "hHBX"                => {}, ;
      "in hbextern"         => {}, ;
      "not in hbextern"     => {}, ;
      "<eol>"               => NIL }

   /* remove formats that have not been implemented yet */
   FOR EACH item IN s_hSwitches[ "format-list" ] DESCEND
      IF item == "all"
      ELSEIF ! hb_IsFunction( "Generate" + item )
         hb_ADel( item:__enumBase(), item:__enumIndex(), .T. )
      ENDIF
   NEXT

   IF Len( aArgs ) == 0 .OR. aArgs[ 1 ] == "-?" .OR. aArgs[ 1 ] == "/?" .OR. aArgs[ 1 ] == "--help"
      ShowHelp( , aArgs )
      RETURN
   ENDIF

   FOR EACH arg IN aArgs
      IF ! Empty( arg )
         IF ( idx := At( "=", arg ) ) == 0
            cArgName := arg
            arg := ""
         ELSE
            cArgName := Left( arg, idx - 1 )
            arg := SubStr( arg, idx + 1 )
         ENDIF

         DO CASE
         CASE cArgName == "-source" ; s_hSwitches[ "basedir" ] := hb_DirSepAdd( arg )
         CASE cArgName == "-lang" ; s_hSwitches[ "lang" ] := Lower( arg )
         CASE cArgName == "-format"
            IF arg == "" .OR. hb_AScan( s_hSwitches[ "format-list" ], arg, , , .T. ) == 0
               ShowHelp( "Unrecognized format option '" + arg + "'" )
               RETURN
            ELSEIF arg == "all"
               s_hSwitches[ "format" ] := s_hSwitches[ "format-list" ]
            ELSE
               AAdd( s_hSwitches[ "format" ], arg )
            ENDIF
         CASE cArgName == "-output-single" ;          s_hSwitches[ "output" ] := "single"
         CASE cArgName == "-output-category" ;        s_hSwitches[ "output" ] := "category"
         CASE cArgName == "-output-entry" ;           s_hSwitches[ "output" ] := "entry"
         CASE cArgName == "-include-doc-source" ;     s_hSwitches[ "include-doc-source" ] := .T.
         CASE cArgName == "-include-doc-version" ;    s_hSwitches[ "include-doc-version" ] := .T.
         OTHERWISE
            IF hb_AScan( s_hSwitches[ "format-list" ], SubStr( cArgName, 2 ), , , .T. ) > 0
               IF SubStr( cArgName, 2 ) == "all"
                  s_hSwitches[ "format" ] := s_hSwitches[ "format-list" ]
               ELSE
                  AAdd( s_hSwitches[ "format" ], SubStr( cArgName, 2 ) )
               ENDIF
            ELSE
               ShowHelp( "Unrecognized option:" + cArgName + iif( Len( arg ) > 0, "=" + arg, "" ) )
               RETURN
            ENDIF
         ENDCASE
      ENDIF
   NEXT

   s_hSwitches[ "hHBX" ] := { => }
   hb_HCaseMatch( s_hSwitches[ "hHBX" ], .F. )
   aContent := ProcessDirs( s_hSwitches[ "hHBX" ] )

#if 0
   hb_MemoWrit( "hbx.json", hb_jsonEncode( s_hSwitches[ "hHBX" ], .T. ) )
#endif

   OutStd( hb_ntos( Len( aContent ) ), "items found" + hb_eol() )
   OutStd( hb_eol() )

   ASort( aContent, , , {| oL, oR | ;
      hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + Chr( 1 ) + oL:Name + " " ;
      <= ;
      hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + Chr( 1 ) + oR:Name + " " ;
      } )

   /* TODO: what is this for?  it is sorting the category sub-arrays and removing empty (?) sub-arrays, but why? */
   FOR EACH item IN sc_hConstraint[ "categories" ]
      IF ! Empty( item )
         IF Len( item ) == 4  /* category, list of subcategory, list of entries, handle */
            FOR idx2 := Len( item[ 3 ] ) TO 1 STEP -1
               IF HB_ISARRAY( item[ 3 ][ idx2 ] )
                  ASort( item[ 3 ][ idx2 ], , , ;
                     {| oL, oR | ;
                        hb_ntos( oL:CategoryIndex( oL:Category ) ) + " " + hb_ntos( oL:SubcategoryIndex( oL:Category, oL:Subcategory ) ) + " " + oL:Name ;
                        <= ;
                        hb_ntos( oR:CategoryIndex( oR:Category ) ) + " " + hb_ntos( oR:SubcategoryIndex( oR:Category, oR:Subcategory ) ) + " " + oR:Name ;
                        } )
               ELSE
                  hb_ADel( item[ 2 ], idx2, .T. )
                  hb_ADel( item[ 3 ], idx2, .T. )
               ENDIF
            NEXT
         ELSE
            OutStd( "Index", item:__enumIndex(), "is not length 4 but rather", Len( item ), hb_eol() )
         ENDIF
      ENDIF
   NEXT

   IF Len( s_hSwitches[ "format" ] ) == 0
      s_hSwitches[ "format" ] := { "text" }
   ENDIF

   FOR EACH cFormat IN s_hSwitches[ "format" ]
      IF !( cFormat == "all" )
         OutStd( "Output as", cFormat + hb_eol() )

         DO CASE
         CASE s_hSwitches[ "output" ] == "single"

            oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, "harbour", "Harbour Reference Guide" )

            FOR EACH item IN aContent
               IF Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  oDocument:AddEntry( item )
                  EXIT
               ENDIF
            NEXT

            FOR EACH item IN aContent
               IF !( Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt" )
                  oDocument:AddEntry( item )
               ENDIF
            NEXT

            oDocument:Generate()
            oDocument := NIL

         CASE s_hSwitches[ "output" ] == "component"

            // TODO

         CASE s_hSwitches[ "output" ] == "category"

            oIndex := &( "Generate" + cFormat + "()" ):NewIndex( cFormat, "harbour", "Harbour Reference Guide" )

            FOR EACH item IN aContent
               IF Right( item:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt"
                  IF oIndex != NIL
                     oIndex:AddEntry( item )
                  ENDIF
                  EXIT
               ENDIF
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]
               IF ! Empty( item )
                  item[ 4 ] := Filename( item[ 1 ] )
#if 0
                  oIndex:BeginSection( item[ 1 ], item[ 4 ] )
                  oIndex:EndSection( item[ 1 ], item[ 4 ] )
#endif
               ENDIF
            NEXT

            FOR EACH item IN sc_hConstraint[ "categories" ]
               IF ! Empty( item )
                  oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, item[ 4 ], "Harbour Reference Guide - " + item[ 1 ] )

                  IF oIndex != NIL
                     oIndex:BeginSection( item[ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:BeginSection( item[ 1 ], oDocument:cFilename )

                  FOR idx := 1 TO Len( item[ 3 ] )
                     IF ! Empty( item[ 3 ][ idx ] )
                        ASort( item[ 3 ][ idx ], , , {| oL, oR | oL:Name <= oR:Name } )
                        IF Len( item[ 2 ][ idx ] ) > 1 .OR. Len( item[ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:BeginSection( item[ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:BeginSection( item[ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                        FOR EACH item4 IN item[ 3 ][ idx ]
                           IF ! Empty( item4 )
                              IF !( Right( item4:sourcefile_, Len( "1stread.txt" ) ) == "1stread.txt" )
                                 IF oIndex != NIL
                                    oIndex:AddReference( item4 )
                                 ENDIF
                                 oDocument:AddEntry( item4 )
                                 IF oIndex != NIL
                                    oDocument:AddReference( "Index", oIndex:cFilename )
                                    /* this kind of works; the reference is outputed but it is not what I meant */
                                    oDocument:AddReference( item[ 1 ], oIndex:cFilename, item[ 4 ] )
                                 ENDIF
                              ENDIF
                           ENDIF
                        NEXT
                        IF Len( item[ 2 ][ idx ] ) > 1 .OR. Len( item[ 2 ][ idx ] ) > 0
                           IF oIndex != NIL
                              oIndex:EndSection( item[ 2 ][ idx ], oDocument:cFilename )
                           ENDIF
                           oDocument:EndSection( item[ 2 ][ idx ], oDocument:cFilename )
                        ENDIF
                     ENDIF
                  NEXT
                  IF oIndex != NIL
                     oIndex:EndSection( item[ 1 ], oDocument:cFilename )
                  ENDIF
                  oDocument:EndSection( item[ 1 ], oDocument:cFilename )
                  oDocument:Generate()
               ENDIF
            NEXT

         CASE s_hSwitches[ "output" ] == "entry"

            FOR EACH item IN aContent
               oDocument := &( "Generate" + cFormat + "()" ):NewDocument( cFormat, item:filename, "Harbour Reference Guide" )
               IF oIndex != NIL
                  oIndex:AddEntry( item )
               ENDIF
               oDocument:AddEntry( item )
               oDocument:Generate()
            NEXT

         ENDCASE

         oDocument := NIL

         IF oIndex != NIL
            oIndex:Generate()
            oIndex := NIL
         ENDIF

      ENDIF
   NEXT

   OutStd( hb_eol() )

   RETURN

STATIC FUNCTION ProcessDirs( hAll )

   LOCAL aContent := {}
   LOCAL cDir
   LOCAL file

   DirLoadHBX( s_hSwitches[ "basedir" ] + "include", hAll )

   ProcessDocDir( s_hSwitches[ "basedir" ], "harbour", @aContent )

   IF s_hSwitches[ "contribs" ]

      cDir := s_hSwitches[ "basedir" ] + "contrib"

      FOR EACH file IN hb_DirScan( cDir,, "D" )
         IF file[ F_ATTR ] == "D" .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == "." ) .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == ".." )

            DirLoadHBX( cDir + hb_ps() + file[ F_NAME ], hAll )

            IF ! ProcessDocDir( cDir + hb_ps() + file[ F_NAME ], hb_FNameName( file[ F_NAME ] ), @aContent )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN aContent

STATIC FUNCTION ProcessDocDir( cDir, cComponent, aContent )

   LOCAL aErrMsg := {}
   LOCAL aEntry := __hbdoc_LoadDir( cDir, cComponent, aErrMsg )

   LOCAL hEntry
   LOCAL nOldContentLen, tmp

   FOR EACH tmp IN aErrMsg
      AddErrorCondition( cDir, tmp )
   NEXT

   IF ! Empty( aEntry )

#if 0
      hb_MemoWrit( "_" + aEntry[ 1 ][ "_COMPONENT" ] + ".json", hb_jsonEncode( aEntry, .t. ) )
#endif

      nOldContentLen := Len( aContent )

      FOR EACH hEntry IN aEntry
         IF Lower( hEntry[ "_LANG" ] ) == s_hSwitches[ "lang" ]
            ProcessBlock( hEntry, aContent )
         ENDIF
      NEXT

      IF Len( aContent ) > nOldContentLen
         OutStd( ">", cDir, "(" + hb_ntos( Len( aContent ) - nOldContentLen ), "items)" + hb_eol() )
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION NewLineVoodoo( cSectionIn, lPreformatted )

   LOCAL cSection := ""
   LOCAL lLastPreformatted := lPreformatted
   LOCAL nLastIndent := -1

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( cSectionIn, .T. )

      IF Len( AllTrim( cLine ) ) == 0
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
            cSection += hb_eol()
         ENDIF
         nLastIndent := -1
      ELSEIF AllTrim( cLine ) == "<table>"
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += "<table>" // + hb_eol()
         lLastPreformatted := lPreformatted
         lPreformatted := .T.
      ELSEIF AllTrim( cLine ) == "</table>"
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += "</table>" + hb_eol()
         lPreformatted := lLastPreformatted
      ELSEIF nLastIndent != ( Len( cLine ) - Len( LTrim( cLine ) ) ) .OR. lPreformatted .OR. Right( cLine, Len( "</par>" ) ) == "</par>"
         IF Right( cLine, Len( "</par>" ) ) == "</par>"
            cLine := hb_StrShrink( cLine, Len( "</par>" ) )
         ENDIF
         nLastIndent := Len( cLine ) - Len( LTrim( cLine ) )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
            cSection += hb_eol()
         ENDIF
         cSection += iif( lPreformatted, cLine, AllTrim( cLine ) )
      ELSE
         cSection += " " + AllTrim( cLine )
      ENDIF
   NEXT

   IF hb_LeftEq( cSection, hb_eol() )
      cSection := SubStr( cSection, Len( hb_eol() ) + 1 )
   ENDIF
   IF Right( cSection, Len( hb_eol() ) ) == hb_eol()
      cSection := hb_StrShrink( cSection, Len( hb_eol() ) )
   ENDIF

   RETURN cSection

STATIC PROCEDURE ProcessBlock( hEntry, aContent )

   LOCAL cFile := hEntry[ "_DOCSOURCE" ]
   LOCAL cType := hEntry[ "_COMPONENT" ]

   LOCAL cSectionName
   LOCAL cSection
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := -1
   LOCAL idxSubCategory := -1
   LOCAL item

   LOCAL cSourceFile := StrTran( ".." + hb_ps() + cFile, iif( hb_ps() == "\", "/", "\" ), hb_ps() )

   LOCAL o := Entry():New( "Template" )

   o:type_ := cType
   o:sourcefile_ := cSourceFile
   o:sourcefileversion_ := ""
   o:module_ := hEntry[ "_COMPONENT" ]
   o:Name := "?NAME?"
   o:SetTemplate( "Function" )

   FOR EACH item IN hEntry

      cSectionName := item:__enumKey()

      cSection := StrTran( item, Chr( 13 ) + Chr( 10 ), hb_eol() )

      IF !( cSectionName == "EXAMPLES" ) .AND. ;
         !( cSectionName == "TESTS" )
         cSection := NewLineVoodoo( cSection, o:IsPreformatted( cSectionName ) )
      ENDIF

      cSection := StrTran( cSection, hb_eol(), Chr( 10 ) )

      IF hb_LeftEq( cSectionName, "_" )

         /* do nothing */

      ELSEIF cSectionName == "TEMPLATE"
         IF o:IsTemplate( cSection )
            o:SetTemplate( cSection )
         ELSE
            AddErrorCondition( cFile, "Unrecognized TEMPLATE '" + cSection + "'", .T. )
            lAccepted := .F.
            EXIT
         ENDIF

      ELSEIF o:IsField( cSectionName )

         DO CASE
         CASE o:IsField( cSectionName, TPL_START )

            AddErrorCondition( cFile, "Encountered another section '" + cSection, .T. )
            lAccepted := .F.
            EXIT

         CASE o:IsField( cSectionName, TPL_END )

            EXIT

         CASE ! Empty( o:&cSectionName )

            AddErrorCondition( cFile, "Duplicate " + cSectionName, .T. )
            lAccepted := .F.

         CASE cSectionName == "CATEGORY"

            IF ( idxCategory := AScan( sc_hConstraint[ "categories" ], {| c | ! Empty( c ) .AND. iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) } ) ) == 0
               AddErrorCondition( cFile, "Unrecognized CATEGORY '" + cSection + "' for template '" + o:Template )
            ENDIF

         CASE cSectionName == "SUBCATEGORY" .AND. o:IsField( "SUBCATEGORY" )

            IF idxCategory <= 0 .OR. o:Category == ""
               AddErrorCondition( cFile, "SUBCATEGORY '" + cSection + "' defined before CATEGORY" )
            ELSEIF ( idxSubCategory := AScan( sc_hConstraint[ "categories" ][ idxCategory ][ 2 ], {| c | c != NIL .AND. iif( HB_ISCHAR( c ), Lower( c ) == Lower( cSection ), Lower( c[ 1 ] ) == Lower( cSection ) ) } ) ) == 0
               AddErrorCondition( cFile, "Unrecognized SUBCATEGORY '" + sc_hConstraint[ "categories" ][ idxCategory ][ 1 ] + "-" + cSection )
            ENDIF

         CASE o:IsField( "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
               Empty( cSection ) .OR. ;
               Lower( cSection ) == "nil" .OR. ;
               Lower( cSection ) == "none" .OR. ;
               Lower( cSection ) == "none." )

            AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + cSection + ")" )

         CASE ! o:IsConstraint( cSectionName, cSection )

            cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: "
#if 0
            cSource := hb_HKeyAt( hsTemplate, idx ) + " should be one of: "
#endif
            AEval( sc_hConstraint[ cSectionName ], {| c, n | cSource += iif( n == 1, "", "," ) + c } )
            AddErrorCondition( cFile, cSource )

         OTHERWISE

         ENDCASE

         IF lAccepted
            o:&cSectionName := Decode( cSectionName, , cSection )
         ENDIF
      ELSE
         AddErrorCondition( cFile, "Using template '" + o:Template + "' encountered an unexpected section '" + cSectionName + "'", .T. )
         lAccepted := .F.
      ENDIF
   NEXT

   /* Verify entry-wide constraints */
   IF lAccepted

      DO CASE
      CASE ! o:IsComplete( @cSource )
         AddErrorCondition( cFile, "Missing sections: '" + cSource + "'" )
#if 0
         lAccepted := .F.
#endif
      CASE o:Template == "Function" .AND. ( ;
         Empty( o:Returns ) .OR. ;
         Lower( o:Returns ) == "nil" .OR. ;
         Lower( o:Returns ) == "none" .OR. ;
         Lower( o:Returns ) == "none." )

         AddErrorCondition( cFile, "'" + o:Name + "' is identified as template " + o:Template + " but has no RETURNS value (" + o:Returns + ")" )
#if 0
         lAccepted := .F.
#endif
      ENDCASE
   ENDIF

   IF lAccepted

      IF !( Lower( hEntry[ "CATEGORY" ] ) == "document" )
         cSectionName := Parse( o:Name, "(" )
         IF ! cSectionName $ s_hSwitches[ "hHBX" ]
            AddErrorCondition( cFile, "Not found in HBX: " + cSectionName + " " + cType )
         ENDIF
      ENDIF

      IF s_hSwitches[ "include-doc-source" ]
         o:Files += hb_eol() + o:sourcefile_ + iif( s_hSwitches[ "include-doc-version" ], " (" + o:sourcefileversion_ + ")", "" )
      ENDIF

      o:filename := Filename( o:Name )

      AAdd( aContent, o )

      IF idxSubCategory == -1 .AND. ( ! o:IsField( "SUBCATEGORY" ) .OR. ! o:IsRequired( "SUBCATEGORY" ) ) // .AND. idxSubCategory == -1
         idxSubCategory := o:SubcategoryIndex( o:Category, "" )
      ENDIF

      IF idxCategory > 0 .AND. idxSubCategory > 0
         IF ! HB_ISARRAY( sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ] )
            sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ] := {}
         ENDIF
         AAdd( sc_hConstraint[ "categories" ][ idxCategory ][ 3 ][ idxSubCategory ], o )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION Decode( cType, hsBlock, cKey )

   LOCAL cCode
   LOCAL cResult
   LOCAL idx

   IF cKey != NIL .AND. HB_ISHASH( hsBlock ) .AND. cKey $ hsBlock
      cCode := hsBlock[ cKey ]
   ELSE
      cCode := cKey
   ENDIF

   SWITCH cType
   CASE "STATUS"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "status" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "status" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "status" ][ idx ][ 2 ]
      ELSEIF Len( cCode ) > 1
         RETURN cCode
      ELSEIF ! HB_ISNULL( cCode )
         RETURN "Unrecognized 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN ATail( sc_hConstraint[ "status" ] )[ 2 ]
      ENDIF

   CASE "PLATFORMS"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "platforms" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "platforms" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "platforms" ][ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF

   CASE "COMPLIANCE"
      IF "," $ cCode .AND. hb_AScan( sc_hConstraint[ "compliance" ], Parse( cCode, "," ), , , .T. ) > 0
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + Decode( cType, hsBlock, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF ( idx := AScan( sc_hConstraint[ "compliance" ], {| a | a[ 1 ] == cCode } ) ) > 0
         RETURN sc_hConstraint[ "compliance" ][ idx ][ 2 ]
      ELSE
         RETURN cCode
      ENDIF

      SWITCH cCode
      CASE "C" ;        RETURN "This is CA-Cl*pper v5.2 compliant"
      CASE "C(array)" ; RETURN "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements"
      CASE "C(menu)" ;  RETURN "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements"
      CASE "C52U" ;     RETURN "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_CLP_UNDOC flag"
      CASE "C52S" ;     RETURN "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_CLP_STRICT flag"
      CASE "C53" ;      RETURN "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag"
      CASE "H" ;        RETURN "This is Harbour specific"
      CASE "NA" ;       RETURN "Not applicable"
      OTHERWISE ;       RETURN cCode
      ENDSWITCH

   CASE "NAME"
      IF hsBlock == NIL
         RETURN cCode
      ELSEIF !( "RETURNS" $ hsBlock )
         RETURN hsBlock[ "NAME" ]
      ELSEIF Empty( hsBlock[ "RETURNS" ] ) .OR. ;
         Upper( hsBlock[ "RETURNS" ] ) == "NIL" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none" .OR. ;
         Lower( hsBlock[ "RETURNS" ] ) == "none."

         hsBlock[ "RETURNS" ] := ""

         DO CASE
         CASE Lower( hsBlock[ "CATEGORY" ] ) == "document"
            RETURN hsBlock[ "NAME" ]
         CASE Lower( hsBlock[ "TEMPLATE" ] ) == "function" .OR. ;
              Lower( hsBlock[ "TEMPLATE" ] ) == "procedure"
            RETURN "Procedure " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN LTrim( hsBlock[ "SUBCATEGORY" ] + " " ) + hsBlock[ "CATEGORY" ] + " " + hsBlock[ "NAME" ]
         ENDCASE
      ELSE
         DO CASE
         CASE ! Empty( hsBlock[ "NAME" ] )
            RETURN "Function " + hsBlock[ "NAME" ]
         OTHERWISE
            RETURN "Unrecognized 'CATEGORY': " + hsBlock[ "CATEGORY" ]
         ENDCASE
      ENDIF

   ENDSWITCH

   RETURN /* cType + "=" + */ cCode

STATIC PROCEDURE ShowSubHelp( xLine, /* @ */ nMode, nIndent, n )

   DO CASE
   CASE xLine == NIL
   CASE HB_ISNUMERIC( xLine )
      nMode := xLine
   CASE HB_ISEVALITEM( xLine )
      Eval( xLine )
   CASE HB_ISARRAY( xLine )
      IF nMode == 2
         OutStd( Space( nIndent ) + Space( 2 ) )
      ENDIF
      AEval( xLine, {| x, n | ShowSubHelp( x, @nMode, nIndent + 2, n ) } )
      IF nMode == 2
         OutStd( hb_eol() )
      ENDIF
   OTHERWISE
      DO CASE
      CASE nMode == 1 ; OutStd( Space( nIndent ) + xLine + hb_eol() )
      CASE nMode == 2 ; OutStd( iif( n > 1, ", ", "" ) + xLine )
      OTHERWISE       ; OutStd( "(" + hb_ntos( nMode ) + ") " + xLine + hb_eol() )
      ENDCASE
   ENDCASE

   RETURN

STATIC FUNCTION HBRawVersion()
   RETURN hb_StrFormat( "%d.%d.%d%s (%s) (%s)", ;
      hb_Version( HB_VERSION_MAJOR ), ;
      hb_Version( HB_VERSION_MINOR ), ;
      hb_Version( HB_VERSION_RELEASE ), ;
      hb_Version( HB_VERSION_STATUS ), ;
      hb_Version( HB_VERSION_ID ), ;
      "20" + Transform( hb_Version( HB_VERSION_REVISION ), "99-99-99 99:99" ) )

STATIC PROCEDURE ShowHelp( cExtraMessage, aArgs )

   LOCAL nMode := 1

   LOCAL aHelp

   DO CASE
   CASE Empty( aArgs ) .OR. Len( aArgs ) <= 1 .OR. Empty( aArgs[ 1 ] )
      aHelp := { ;
         cExtraMessage, ;
         "Harbour Document Compiler (hbdoc) " + HBRawVersion(), ;
         "Copyright (c) 1999-2016, " + hb_Version( HB_VERSION_URL_BASE ), ;
         "", ;
         "Syntax:", ;
         "", ;
         { "hbdoc [options]" }, ;
         "", ;
         "Options:", ;
         { ;
            "-? or --help                    this screen", ;
            "-? <option> or --help <option>  help on <option>, <option> is one of:", ;
            2, ;
            { "Categories", "Templates", "Compliance", "Platforms" }, ;
            1, ;
            "-[format=]<type>                output type, default is text, or one of:", ;
            2, ;
            s_hSwitches[ "format-list" ], ;
            1, ;
            "-output-single                  output is one file" + IsDefault( s_hSwitches[ "output" ] == "single" ), ;
            "-output-category                output is one file per category" + IsDefault( s_hSwitches[ "output" ] == "category" ), ;
            "-output-entry                   output is one file per entry (function, command, etc)" + IsDefault( s_hSwitches[ "output" ] == "entry" ), ;
            "-source=<directory>             source directory, default is .." + hb_ps() + "..", ;
            "-include-doc-source             output is to indicate the document source file name", ;
            "-include-doc-version            output is to indicate the document source file version" ;
         } }

   CASE aArgs[ 2 ] == "Categories"
      aHelp := { ;
         "Defined categories and sub-categories are:", ;
         sc_hConstraint[ "categories" ] }

   CASE aArgs[ 2 ] == "Templates"
      aHelp := { ;
         iif( Len( aArgs ) >= 3, aArgs[ 3 ] + " template is:", "Defined templates are:" ), ;
         "", ;
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ), s_hSwitches[ "DELIMITER" ] ) } }

   CASE aArgs[ 2 ] == "Compliance"
      aHelp := { ;
         "Defined 'COMPLIANCE' are:", ;
         "", ;
         {|| ShowComplianceHelp() } }

   CASE aArgs[ 2 ] == "Platforms"
      aHelp := { ;
         "Defined 'PLATFORMS' are:", ;
         "", ;
         {|| ShowPlatformsHelp() } }

   OTHERWISE

      ShowHelp( "Unrecognized help option" )
      RETURN

   ENDCASE

   /* using hbmk2 style */
   AEval( aHelp, {| x | ShowSubHelp( x, @nMode, 0 ) } )

   RETURN

FUNCTION Parse( /* @ */ cVar, xDelimiter )

   LOCAL cResult
   LOCAL idx

   IF ( idx := At( xDelimiter, cVar ) ) > 0
      cResult := Left( cVar, idx - 1 )
      cVar := SubStr( cVar, idx + Len( xDelimiter ) )
   ELSE
      cResult := cVar
      cVar := ""
   ENDIF

   RETURN cResult

STATIC FUNCTION Join( aVar, cDelimiter )

   LOCAL cResult := ""

   AEval( aVar, {| c, n | cResult += iif( n > 1, cDelimiter, "" ) + c } )

   RETURN cResult

STATIC PROCEDURE AddErrorCondition( cFile, cMessage, lFatal )

   IF s_hSwitches[ "immediate-errors" ] .OR. hb_defaultValue( lFatal, .F. )
      OutStd( cFile + ":", cMessage + hb_eol() )
   ENDIF

   RETURN

FUNCTION Indent( cText, nLeftMargin, nWidth, lRaw, lForceRaw )

   LOCAL cResult := ""
   LOCAL idx
   LOCAL cLine

   LOCAL aText := hb_ATokens( cText, .T. )

   hb_default( @lRaw, .F. )
   hb_default( @lForceRaw, .F. )

   IF nWidth == 0 .OR. lRaw
      idx := 99999
      AEval( aText, {| c | iif( Empty( c ), , idx := Min( idx, Len( c ) - Len( LTrim( c ) ) ) ) } )
      AEval( aText, {| c, n | aText[ n ] := Space( nLeftMargin ) + SubStr( c, idx + 1 ) } )
      cResult := Join( aText, hb_eol() ) + hb_eol() + hb_eol()
   ELSE
      FOR EACH cLine IN aText
         IF cLine == "<table>"
            lRaw := .T.
         ELSEIF cLine == "</table>"
            cResult += hb_eol()
            lRaw := .F.
         ELSEIF lRaw .OR. lForceRaw
            cResult += Space( nLeftMargin ) + LTrim( cLine ) + hb_eol()
         ELSE
            DO WHILE Len( cLine ) > nWidth
               idx := nWidth + 1
               DO WHILE idx > 0
                  idx--
                  DO CASE
                  CASE ! SubStr( cLine, idx, 1 ) $ " ,;.!?"
                     /* do nothing */
                  CASE Upper( SubStr( cLine, idx, 3 ) ) == ".T." .OR. Upper( SubStr( cLine, idx, 3 ) ) == ".F."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 2, 3 ) ) == ".T." .OR. Upper( SubStr( cLine, idx - 1, 3 ) ) == ".F."
                     idx -= 3
                  CASE Upper( SubStr( cLine, idx, 5 ) ) == ".AND." .OR. Upper( SubStr( cLine, idx, 5 ) ) == ".NOT."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 4, 5 ) ) == ".AND." .OR. Upper( SubStr( cLine, idx - 4, 5 ) ) == ".NOT."
                     idx -= 5
                  CASE Upper( SubStr( cLine, idx, 4 ) ) == ".OR."
                     idx--
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == ".OR."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 4 ) ) == "i.e."
                     idx -= 2
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == "i.e."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 4 ) ) == "e.g."
                     idx -= 2
                  CASE Upper( SubStr( cLine, idx - 3, 4 ) ) == "e.g."
                     idx -= 4
                  CASE Upper( SubStr( cLine, idx - 1, 2 ) ) == "*."
                     idx -= 2
                  OTHERWISE
                     EXIT
                  ENDCASE
               ENDDO
               IF idx <= 0
                  idx := nWidth
               ENDIF

               cResult += Space( nLeftMargin ) + Left( cLine, idx - iif( SubStr( cLine, idx, 1 ) == " ", 1, 0 ) ) + hb_eol()
               cLine := LTrim( SubStr( cLine, idx + 1 ) )
            ENDDO

            IF ! HB_ISNULL( cLine )
               cResult += Space( nLeftMargin ) + cLine + hb_eol()
            ENDIF

            cResult += hb_eol()
         ENDIF
      NEXT
   ENDIF

   RETURN cResult

STATIC FUNCTION Filename( cFile, cFormat, nLength )

   STATIC s_aFiles := {}

   LOCAL cResult := ""
   LOCAL idx
   LOCAL char

   hb_default( @nLength, 0 )

   IF Lower( hb_defaultValue( cFormat, "alnum" ) ) == "alnum"
      FOR idx := 1 TO Len( cFile )
         char := SubStr( cFile, idx, 1 )
         IF hb_asciiIsDigit( char ) .OR. hb_asciiIsAlpha( char ) .OR. char == "_"
            cResult += Lower( char )
            IF nLength > 0 .AND. Len( cResult ) == nLength
               EXIT
            ENDIF
         ENDIF
      NEXT
   ELSE
      cResult := cFile
   ENDIF

   IF hb_AScan( s_aFiles, cResult, , , .T. ) == 0
      AAdd( s_aFiles, cResult )
   ELSE
      idx := 0
      DO WHILE hb_AScan( s_aFiles, cResult + StrZero( ++idx, 3 ), , , .T. ) > 0
      ENDDO
      cResult += StrZero( idx, 3 )
      AAdd( s_aFiles, cResult )
   ENDIF

   RETURN cResult

/* a class that will hold one entry */
CREATE CLASS Entry

   EXPORTED:

   CLASS VAR Fields AS ARRAY INIT { ;
      { "DOC",          "Doc" }, ;
      { "TEMPLATE",     "Template" }, ;
      { "NAME",         "" }, ;
      { "CATEGORY",     "Category" }, ;
      { "SUBCATEGORY",  "Sub category" }, ;
      { "ONELINER",     "" }, ;
      { "SYNTAX",       "Syntax" }, ;
      { "ARGUMENTS",    "Argument(s)" }, ;
      { "RETURNS",      "Returns" }, ;
      { "DESCRIPTION",  "Description" }, ;
      { "DATALINK",     "Data link" }, ;
      { "DATANOLINK",   "Data no link" }, ;
      { "METHODSLINK",  "Methods link" }, ;
      { "METHODSNOLINK","Methods no link" }, ;
      { "EXAMPLES",     "Example(s)" }, ;
      { "TESTS",        "Test(s)" }, ;
      { "STATUS",       "Status" }, ;      /* sc_hConstraint[ "status" ] is the constraint list */
      { "COMPLIANCE",   "Compliance" }, ;  /* sc_hConstraint[ "compliance" ] is the constraint list */
      { "PLATFORMS",    "Platform(s)" }, ; /* sc_hConstraint[ "platforms" ] is the constraint list */
      { "FILES",        "File(s)" }, ;
      { "SEEALSO",      "See also" }, ;
      { "END",          "End" } }

   #define _S TPL_START
   #define _E TPL_END
   #define _T TPL_TEMPLATE
   #define _R TPL_REQUIRED
   #define _O TPL_OPTIONAL
   #define _P TPL_PREFORMATTED
   #define _U TPL_OUTPUT

   /* the columns of this array correspond to the elements of Fields */
   CLASS VAR Templates AS ARRAY INIT { ;
      { "Template"      , { _S, _T,  0+_U,  0, _O   ,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U, _E } }, ;
      { "Document"      , { _S, _T, _R+_U, _R, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Function"      , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "C Function"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Procedure"     , { _S, _T, _R+_U, _R, _R   , _O+_U, _O+_U, _O+_U,     0, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Command"       , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class"         , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E } }, ;
      { "Class method"  , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Class data"    , { _S, _T, _R+_U, _R, _R   , _O+_U, _R+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _E } }, ;
      { "Run time error", { _S, _T, _R+_U, _R,  0   , _O+_U,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _E } } }

   METHOD New( cType ) CONSTRUCTOR
   METHOD IsField( c, nType )
   METHOD IsTemplate( cType )
   METHOD SetTemplate( cTemplate )
   METHOD IsConstraint( cSectionName, cSection )
   METHOD IsComplete( cIncompleteFielsList )
   METHOD IsPreformatted( cField )
   METHOD IsRequired( cField )
   METHOD IsOptional( cField )
   METHOD IsOutput( cField )
   METHOD FieldName( cField )
   METHOD CategoryIndex( cCategory )
   METHOD SubcategoryIndex( cCategory, cSubcategory )

   VAR Group AS ARRAY
   VAR filename AS STRING
   VAR type_ AS STRING
   VAR sourcefile_ AS STRING
   VAR module_ AS STRING
   VAR sourcefileversion_ AS STRING
   VAR uid_ AS STRING

   CLASS VAR uid__ AS INTEGER INIT 0

ENDCLASS

METHOD New( cType ) CLASS Entry

   ::uid_ := hb_ntos( ++::uid__ )
   IF ! __objHasData( self, ::Fields[ 1 ][ 1 ] )
      AEval( ::Fields, {| a | __objAddData( self, a[ 1 ] ) } )
   ENDIF
   IF HB_ISSTRING( cType )
      ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) ][ 2 ]
   ENDIF

   RETURN self

METHOD IsField( c, nType ) CLASS Entry

   LOCAL idx
   LOCAL lResult

   IF ( lResult := ( idx := AScan( ::Fields, {| a | Upper( a[ 1 ] ) == Upper( c ) } ) ) > 0 )
      IF ::Group[ idx ] == 0
         lResult := .F.
      ELSEIF HB_ISNUMERIC( nType ) .AND. hb_bitAnd( ::Group[ idx ], nType ) != nType
         lResult := .F.
      ENDIF
   ENDIF

   RETURN lResult

METHOD IsTemplate( cType ) CLASS Entry
   RETURN AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cType ) } ) > 0

METHOD SetTemplate( cTemplate ) CLASS Entry

   LOCAL aData := Array( Len( ::Fields ) )
   LOCAL idx

   ::Group := ::Templates[ AScan( ::Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } ) ][ 2 ]
   FOR idx := 1 TO Len( aData )
      IF ::Fields[ idx ][ 1 ] == "TEMPLATE"
         aData[ idx ] := { ::Fields[ idx ][ 1 ], cTemplate }
      ELSE
         aData[ idx ] := { ::Fields[ idx ][ 1 ], iif( ::Group[ idx ] == TPL_REQUIRED, NIL, "" ) }
      ENDIF
   NEXT
   __objSetValueList( self, aData )

   RETURN self

METHOD IsConstraint( cSectionName, cSection ) CLASS Entry

   LOCAL lResult
   LOCAL idx := AScan( ::Fields, {| a | a[ 1 ] == cSectionName } )

   IF hb_bitAnd( ::Group[ idx ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      lResult := .T.
   ELSEIF cSectionName $ sc_hConstraint
      lResult := ;
         hb_AScan( sc_hConstraint[ cSectionName ], cSection, , , .T. ) .OR. ;
         hb_AScan( sc_hConstraint[ cSectionName ], Parse( cSection, "," ), , , .T. )
   ELSE
      lResult := .T.
   ENDIF

   RETURN lResult

METHOD IsComplete( cIncompleteFielsList ) CLASS Entry

   LOCAL lResult := .T.
   LOCAL idx

   cIncompleteFielsList := ""

   FOR idx := 1 TO Len( ::Fields )
      IF hb_bitAnd( ::Group[ idx ], TPL_REQUIRED ) != 0 .AND. Empty( ::&( ::Fields[ idx ][ 1 ] ) )
         cIncompleteFielsList += "," + ::Fields[ idx ][ 1 ]
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFielsList := SubStr( cIncompleteFielsList, 2 )

   RETURN lResult

METHOD IsPreformatted( cField ) CLASS Entry
   LOCAL nGroup := AScan( ::Fields, {| a | a[ 1 ] == cField } )
   RETURN nGroup > 0 .AND. hb_bitAnd( ::Group[ nGroup ], TPL_PREFORMATTED ) != 0

METHOD IsRequired( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_REQUIRED ) != 0

METHOD IsOptional( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OPTIONAL ) != 0

METHOD IsOutput( cField ) CLASS Entry
   RETURN hb_bitAnd( ::Group[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ], TPL_OUTPUT ) != 0

METHOD FieldName( cField ) CLASS Entry
   RETURN ::Fields[ AScan( ::Fields, {| a | a[ 1 ] == cField } ) ][ 2 ]

METHOD CategoryIndex( cCategory ) CLASS Entry
   RETURN AScan( sc_hConstraint[ "categories" ], {| a | HB_ISARRAY( a ) .AND. Len( a ) >= 1 .AND. a[ 1 ] == cCategory } )

METHOD SubcategoryIndex( cCategory, cSubcategory ) CLASS Entry
   RETURN ::CategoryIndex( cCategory ) >= 1 .AND. ;
      hb_AScan( sc_hConstraint[ "categories" ][ ::CategoryIndex( cCategory ) ][ 2 ], cSubcategory, , , .T. )

STATIC PROCEDURE init_Templates()

   LOCAL item
   LOCAL aSubCategories := { ;
      "Application", ;
      "Array", ;
      "Classes", ;
      "Conversion", ;
      "Database", ;
      "Date/Time", ;
      "Environment", ;
      "Error", ;
      "Events", ;
      "Execute and execution", ;  /* replace w/ "Environment"? */
      "Extend", ;
      "FileSys", ;
      "Fixed memory", ;
      "Garbage collector", ;
      "Hash table", ;
      "Idle states", ;
      "INET", ;
      "Internal", ;
      "Item", ;
      "Language and Nation", ;
      "Legacy", ;
      "Macro", ;
      "Math", ;
      "Objects", ;
      "Printer", ;
      "RDD", ;
      "Strings", ;
      "Terminal", ;
      "Undocumented", ;
      "User interface", ;
      "Variable management", ;
      "Virtual machine" }

   LOCAL aCategories := { ;
      { "Document", { "License", "Compiler", "" } }, ;
      { "API", AClone( aSubCategories ) }, ;
      { "C level API", AClone( aSubCategories ) }, ;
      { "C level API compatability", AClone( aSubCategories ) }, ;
      { "Class", { ;
            "", ;
            "Access", ;
            "Assign", ;
            "Constructor", ;
            "Data", ;
            "Definition", ;
            "Destructor", ;
            "Method", ;
            "Var" } }, ;
      { "Command", AClone( aSubCategories ) }, ;
      /* { "Compile time errors", { {} } }, */ ;
      { "Run time errors", { "" } } }

   LOCAL aCompliance := { ;
      { "",         "" }, ;
      { "C",        "This is CA-Cl*pper v5.2 compliant" }, ;
      { "C(array)", "This is CA-Cl*pper v5.2 compliant except that arrays in Harbour can have an unlimited number of elements" }, ;
      { "C(menu)",  "This is CA-Cl*pper v5.2 compliant except that menus (internally arrays) in Harbour can have an unlimited number of elements" }, ;
      { "C(arrayblock)",  "Codeblock calling frequency and order differs from  CA-Cl*pper, since Harbour uses a different (faster) sorting algorithm (quicksort)" }, ;
      { "C52S",     "? verbage: This is an CA-Cl*pper v5.2 compliant and is only visible if source was compiled with the HB_C52_STRICT flag" }, ;
      { "C52U",     "This is an undocumented CA-Cl*pper v5.2 function and is only visible if source was compiled with the HB_C52_UNDOC flag" }, ;
      { "C53",      "This is CA-Cl*pper v5.3 compliant and is only visible if source was compiled with the HB_COMPAT_C53 flag" }, ;
      { "H",        "This is Harbour specific" }, ;
      { "NA",       "Not applicable" } }

   LOCAL aPlatforms := { ;
      { "",          "" }, ;
      { "All",       "This is available on all platforms" }, ;
      { "All(GT)",   "This part of the GT API and supported only by some platforms." }, ;
      { "All(LFN)",  "This is available on all platforms." + hb_eol() + ;
                     "If long file names are available Harbour will use/display the first 15 characters " +;
                     "else Harbour will use/display a 8.3 file name consistent with CA-Cl*pper" }, ;
      { "Linux(GT)", "Under Linux the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "OS2(GT)",   "Under OS/2 the number of columns avaliable depends of the current Terminal screen size." }, ;
      { "Win(GT)",   "Under Windows, the return value of MaxRow() function is only affected if called after an SetMode() function" }, ;
      { "BSD",       "This is available on the BSD platform" }, ;
      { "DARWIN",    "This is available on the Darwin platform" }, ;
      { "DOS",       "This is available on the MS-DOS platform" }, ;
      { "HPUX",      "This is available on the HPUX platform" }, ;
      { "LINUX",     "This is available on the Linux platform" }, ;
      { "OS2",       "This is available on the OS/2 platform" }, ;
      { "SUNOS",     "This is available on the SunOS platform" }, ;
      { "Unix",      "This is available on the Unix platform(s)" }, ;
      { "Win",       "This is available on the Windows platform(s)" }, ;
      { "WinCE",     "This is available on the Windows CE platform" } }

   LOCAL aStatus := { ;
      { "",  "" }, ;
      { "R", "Ready" }, ;
      { "S", "Started" }, ;
      { "N", "Not started" } }

   FOR EACH item IN aCategories
      IF ! Empty( item )
         AAdd( item, Array( Len( item[ 2 ] ) ) )  /* holder array of sub-category entries */
         AAdd( item, "" )  /* holder for sub-category file name */
      ENDIF
   NEXT

   sc_hConstraint := { ;
      "categories" => aCategories, ;
      "compliance" => aCompliance, ;
      "platforms" => aPlatforms, ;
      "status" => aStatus }

   hb_HCaseMatch( sc_hConstraint, .F. )

   RETURN

STATIC PROCEDURE ShowTemplatesHelp( cTemplate, cDelimiter )

   LOCAL o := Entry():New( , sc_hConstraint )
   LOCAL idxTemplates, nFrom := 1, nTo := Len( o:Templates )
   LOCAL idx

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      IF o:IsTemplate( cTemplate )
         nFrom := nTo := AScan( o:Templates, {| a | Upper( a[ 1 ] ) == Upper( cTemplate ) } )
      ELSE
         ShowHelp( "Unrecognized template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   FOR idxTemplates := nFrom TO nTo
      IF ! Empty( o:Templates[ idxTemplates ] ) .AND. ;
         ! Empty( o:Templates[ idxTemplates ][ 1 ] ) .AND. ;
         !( o:Templates[ idxTemplates ][ 1 ] == "Template" )

#if 0
         IF nFrom != nTo
            ShowSubHelp( o:Templates[ idxTemplates ][ 1 ], 1, 0 )
         ENDIF
#endif

         o:SetTemplate( o:Templates[ idxTemplates ][ 1 ] )

         FOR idx := 1 TO Len( o:Fields )
            IF o:Group[ idx ] != 0
               ShowSubHelp( iif( idx == 1, "/", " " ) + "*  " + cDelimiter + o:Fields[ idx ][ 1 ] + cDelimiter, 1, 0 )
               IF o:Fields[ idx ][ 1 ] == "TEMPLATE"
                  ShowSubHelp( " *      " + o:Template, 1, 0 )
               ELSEIF o:Group[ idx ] != TPL_START .AND. o:Group[ idx ] != TPL_END .AND. .T.
                  ShowSubHelp( " *      " + iif( o:IsRequired( o:Fields[ idx ][ 1 ] ), "<required>", "<optional>" ), 1, 0 )
               ENDIF
            ENDIF
         NEXT
         ShowSubHelp( " */", 1, 0 )
         ShowSubHelp( "", 1, 0 )
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE ShowComplianceHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "compliance" ]
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "COMPLIANCE", , item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE ShowPlatformsHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "platforms" ]
      ShowSubHelp( item[ 1 ], 1, 0, item:__enumIndex() )
      ShowSubHelp( Decode( "PLATFORMS", , item[ 1 ] ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE DirLoadHBX( cDir, hAll )

   LOCAL aFile
   LOCAL cFileName

   cDir := hb_DirSepAdd( cDir )

   FOR EACH aFile IN hb_vfDirectory( cDir + "*.hbx" )
      IF hb_vfExists( cFileName := cDir + aFile[ F_NAME ] )
         LoadHBX( cFileName, hAll )
      ENDIF
   NEXT

   RETURN

STATIC FUNCTION LoadHBX( cFileName, hAll )

   LOCAL cName := StrTran( cFileName, "\", "/" )

   LOCAL cFile
   LOCAL pRegex
   LOCAL tmp
   LOCAL aDynamic := {}
   LOCAL cFilter

   IF ! HB_ISNULL( cFile := hb_MemoRead( cFileName ) )

      FOR EACH cFilter IN { ;
         "^DYNAMIC ([a-zA-Z0-9_]*)$", ;
         "ANNOUNCE ([a-zA-Z0-9_]*)$" }

         IF ! Empty( pRegex := hb_regexComp( cFilter, .T., .T. ) )
            FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
               IF tmp[ 2 ] $ hAll
                  hAll[ tmp[ 2 ] ] += "," + cName
               ELSE
                  hAll[ tmp[ 2 ] ] := cName
               ENDIF
            NEXT
         ENDIF
      NEXT
   ENDIF

   RETURN aDynamic

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_genbase.prg"
SET PROCEDURE TO "_gentxt.prg"
SET PROCEDURE TO "_genhtml.prg"
SET PROCEDURE TO "_genxml.prg"
#endif
