#!/usr/bin/env hbrun
/*
 * Document generator
 *
 * Copyright 2016 Viktor Szakats (vszakats.net/harbour)
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

/* TODO: hEntry in Entry objects directly, or eliminate the object
         altogether. */

/* Optimizations */
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "hbver.ch"

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST HB_CODEPAGE_UTF8EX

#define TPL_START            1
#define TPL_END              2
#define TPL_REQUIRED         4  // intentionally has a 'required' and 'optional' flag
#define TPL_OPTIONAL         8
#define TPL_PREFORMATTED     16
#define TPL_CONSTRAINTLIST   32
#define TPL_TEMPLATE         64
#define TPL_OUTPUT           128

STATIC sc_hFields
STATIC sc_hTemplates
STATIC sc_hConstraint
STATIC s_hSwitches
STATIC s_hHBX
STATIC s_hTree := { => }  /* component / category / subcategory */

STATIC s_generators := { ;
   "all"   =>, ;
   "html"  => @GenerateHTML(), ;
   "ascii" => @GenerateAscii(), ;
   "text"  => @GenerateText(), ;
   "xml"   => @GenerateXML() }

PROCEDURE Main()

   LOCAL aArgs := hb_AParams()
   LOCAL idx, item
   LOCAL arg, tmp, nLen, nCount
   LOCAL cArgName
   LOCAL cFormat
   LOCAL oDocument, oIndex
   LOCAL aContent
   LOCAL cCat1, cCat1Prev
   LOCAL cCat2, cCat2Prev
   LOCAL aComponent
   LOCAL cID, cName
   LOCAL nStart
   LOCAL cDir

   LOCAL generatorClass

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   init_Templates()

   /* Configuration settings */
   s_hSwitches := { ;
      "lang"      => "en", ;
      "contribs"  => .T., ;
      "format"    => { "html" }, ;
      "output"    => "component", ;
      "dir_out"    => hb_DirSepToOS( "./" ), ;
      "verbosity" => 1, ;
      "hHBX"      => {} }

   /* Find project root */
   nCount := 4
   DO WHILE nCount-- > 0 .AND. ! hb_DirExists( ( tmp := hb_DirBase() + hb_DirSepToOS( Replicate( "../", nCount ) ) ) + "doc" )
   ENDDO
   s_hSwitches[ "dir_in" ] := iif( nCount == 0, "./", hb_PathNormalize( tmp ) )

   IF Len( aArgs ) >= 1 .AND. ;
      ( aArgs[ 1 ] == "-h" .OR. ;
        aArgs[ 1 ] == "--help" )
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
         CASE hb_LeftEq( cArgName, "-v" ) ; s_hSwitches[ "verbosity" ] := Val( SubStr( cArgName, Len( "-v" ) + 1 ) )
         CASE cArgName == "-input" ; s_hSwitches[ "dir_in" ] := hb_DirSepAdd( arg )
         CASE cArgName == "-output" ; s_hSwitches[ "dir_out" ] := hb_DirSepAdd( arg )
         CASE cArgName == "-lang" ; s_hSwitches[ "lang" ] := Lower( arg )
         CASE cArgName == "-format"
            IF arg == "" .OR. ! arg $ s_generators
               ShowHelp( "Unrecognized format option '" + arg + "'" )
               RETURN
            ELSEIF arg == "all"
               s_hSwitches[ "format" ] := hb_HKeys( s_generators )
            ELSE
               AAdd( s_hSwitches[ "format" ], arg )
            ENDIF
         CASE hb_LeftEq( cArgName, "-output-" )
            s_hSwitches[ "output" ] := SubStr( cArgName, Len( "-output-" ) + 1 )
         OTHERWISE
            IF SubStr( cArgName, 2 ) $ s_generators
               IF SubStr( cArgName, 2 ) == "all"
                  s_hSwitches[ "format" ] := hb_HKeys( s_generators )
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

   OutStd( "! Input directory:", s_hSwitches[ "dir_in" ] + hb_eol() )

   s_hHBX := { => }
   hb_HCaseMatch( s_hHBX, .F. )
   aContent := ProcessDirs( s_hHBX )

#if 0
   hb_MemoWrit( "hbx.json", hb_jsonEncode( s_hHBX, .T. ) )
   hb_MemoWrit( "tree.json", hb_jsonEncode( s_hTree, .T. ) )
#endif

   OutStd( "!", hb_ntos( Len( aContent ) ), "entries found" + hb_eol() )

   ASort( aContent,,, {| oL, oR | ;
         PadR( SortWeight( oL[ "CATEGORY" ] ), 20 ) + ;
         PadR( SortWeight( oL[ "SUBCATEGORY" ] ), 20 ) + ;
         PadR( oL[ "NAME" ], 50 ) ;
      <= ;
         PadR( SortWeight( oR[ "CATEGORY" ] ), 20 ) + ;
         PadR( SortWeight( oR[ "SUBCATEGORY" ] ), 20 ) + ;
         PadR( oR[ "NAME" ], 50 ) ;
      } )

   nStart := hb_MilliSeconds()

   FOR EACH cFormat IN s_hSwitches[ "format" ]

      IF HB_ISEVALITEM( generatorClass := hb_HGetDef( s_generators, Lower( cFormat ) ) )

         cDir := s_hSwitches[ "dir_out" ] + cFormat

         OutStd( "! Output directory:", hb_PathNormalize( hb_PathJoin( hb_DirBase(), cDir ) ) + hb_eol() )

         SWITCH s_hSwitches[ "output" ]
         CASE "component"

            aComponent := ASort( hb_HKeys( s_hTree ),,, {| x, y | SortWeightPkg( x ) < SortWeightPkg( y ) } )

            oIndex := Eval( generatorClass ):NewIndex( cDir, "index", "Index", s_hSwitches[ "lang" ] )

            oIndex:BeginTOC()
            FOR EACH tmp IN aComponent

               Get_ID_Name( tmp, @cID, @cName )

               cCat1Prev := NIL

               oIndex:BeginTOCItem( cName, cID )
#if 0
               FOR EACH item IN ASort( hb_HKeys( s_hTree[ tmp ] ),,, {| x, y | SortWeight( x ) < SortWeight( y ) } )
                  oIndex:AddReference( cCat1, "", cID + "-" + Lower( cCat1 ) )
               NEXT
#endif
               oIndex:EndTOCItem()
            NEXT
            oIndex:EndTOC()

            OutStd( Chr( 13 ) )

            nLen := Len( aContent )
            nCount := 0
            FOR EACH tmp IN aComponent

               cCat1Prev := cCat2Prev := NIL

               Get_ID_Name( tmp, @cID, @cName )

               oDocument := Eval( generatorClass ):NewDocument( cDir, tmp, cName, s_hSwitches[ "lang" ] )

               oIndex:BeginSection( cName, oDocument:cFilename, cID )

               FOR EACH item IN aContent

                  IF item[ "_type" ] == tmp

                     IF ++nCount / 10 == Int( nCount / 10 ) .AND. s_hSwitches[ "verbosity" ] >= 1
                        OutStd( Chr( 13 ) + Str( Int( nCount / nLen * 100 ), 3 ) + "%" )
                     ENDIF

                     oDocument:AddEntry( item )

                     cCat1 := item[ "CATEGORY" ]
                     IF cCat1Prev == NIL .OR. !( cCat1 == cCat1Prev )
                        IF cCat1Prev != NIL
                           oIndex:EndSection()
                        ENDIF
                        oIndex:BeginSection( cCat1,, cID + "-" + Lower( cCat1 ) )
                        cCat1Prev := cCat1
                     ENDIF

                     cCat2 := hb_defaultValue( item[ "SUBCATEGORY" ], "" )
                     IF cCat2Prev == NIL .OR. !( cCat2 == cCat2Prev )
// #define SUBCAT_INDENT
#ifdef SUBCAT_INDENT
                        IF cCat2Prev != NIL
                           oIndex:EndSection()
                        ENDIF
                        oIndex:BeginSection( cCat2,, iif( Empty( cCat2 ), NIL, cID + "-" + Lower( cCat1 ) + "-" + Lower( cCat2 ) ) )
#else
                        IF cCat2Prev != NIL
                           oIndex:SubCategory( cCat2, iif( Empty( cCat2 ), NIL, cID + "-" + Lower( cCat1 ) + "-" + Lower( cCat2 ) ) )
                        ENDIF
#endif
                        cCat2Prev := cCat2
                     ENDIF

                     oIndex:AddReference( item )
                  ENDIF
               NEXT

#ifdef SUBCAT_INDENT
               IF cCat2Prev != NIL
                  oIndex:EndSection()
               ENDIF
#endif
               IF cCat1Prev != NIL
                  oIndex:EndSection()
               ENDIF

               oIndex:EndSection()

               oDocument:Generate()
            NEXT

            oIndex:Generate()

            OutStd( Chr( 13 ) + Str( 100, 3 ) + "%" + hb_eol() )

            EXIT

         CASE "entry"

            FOR EACH item IN aContent
               oDocument := Eval( generatorClass ):NewDocument( cDir, item[ "_filename" ], item[ "NAME" ], s_hSwitches[ "lang" ] )
               oDocument:AddEntry( item )
               oDocument:Generate()
            NEXT

            EXIT

         ENDSWITCH
      ENDIF
   NEXT

   OutStd( hb_StrFormat( "! Done in %1$s seconds", hb_ntos( Round( ( hb_MilliSeconds() - nStart ) / 1000, 2 ) ) ) + hb_eol() )

   RETURN

FUNCTION hbdoc_dir_in()
   RETURN s_hSwitches[ "dir_in" ]

STATIC PROCEDURE Get_ID_Name( cComponent, /* @ */ cID, /* @ */ cName )

   IF cComponent == "harbour"
      cID := "core"
      cName := "Harbour core"
   ELSE
      cID := cComponent
      cName := hb_StrFormat( "%1$s contrib", cComponent )
   ENDIF

   RETURN

/* Begin with Harbour core section */
STATIC FUNCTION SortWeightPkg( cString )
   RETURN iif( cString == "harbour", "A", "B" ) + cString

STATIC FUNCTION SortWeight( cString )

   SWITCH hb_defaultValue( cString, "" )
   CASE "Document" ; RETURN Chr( 31 ) + "001" + cString  /* category */
   CASE "Intro"    ; RETURN Chr( 31 ) + "001" + cString  /* subcategory */
   CASE "License"  ; RETURN Chr( 31 ) + "002" + cString  /* subcategory */
   CASE "Compiler" ; RETURN Chr( 31 ) + "003" + cString  /* subcategory */
   ENDSWITCH

   RETURN cString

STATIC FUNCTION ProcessDirs( hAll )

   LOCAL aContent := {}
   LOCAL cDir
   LOCAL file

   DirLoadHBX( s_hSwitches[ "dir_in" ] + "include", hAll )

   ProcessDocDir( s_hSwitches[ "dir_in" ], "harbour", @aContent )

   IF s_hSwitches[ "contribs" ]

      cDir := s_hSwitches[ "dir_in" ] + "contrib"

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
         OutStd( "!", cDir, "(" + hb_ntos( Len( aContent ) - nOldContentLen ), "entries)" + hb_eol() )
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION NewLineVoodoo( cSectionIn )

   LOCAL cSection := ""

   LOCAL lPreformatted := .F.
   LOCAL lTable := .F.
   LOCAL lLastPreformatted := .F.
   LOCAL nLastIndent := -1
   LOCAL lLastTable := .F.

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( cSectionIn, .T. )

      IF Empty( cLine )
         IF lPreformatted .AND. ! lTable
            cSection += hb_eol() + hb_eol()
         ELSE
            IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
               cSection += hb_eol()
            ENDIF
            nLastIndent := -1
         ENDIF
      ELSEIF hb_LeftEq( AllTrim( cLine ), "<table" ) .OR. AllTrim( cLine ) == "<fixed>" .OR. ( hb_LeftEq( AllTrim( cLine ), '```' ) .AND. ! lPreformatted )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += AllTrim( cLine )  // + hb_eol()
         lLastPreformatted := lPreformatted
         lLastTable := lTable
         lPreformatted := .T.
         lTable := hb_LeftEq( AllTrim( cLine ), "<table" )
      ELSEIF AllTrim( cLine ) == "</table>" .OR. AllTrim( cLine ) == "</fixed>" .OR. ( hb_LeftEq( AllTrim( cLine ), '```' ) .AND. lPreformatted )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() ) .OR. lPreformatted
            cSection += hb_eol()
         ENDIF
         cSection += AllTrim( cLine ) + hb_eol()
         lPreformatted := lLastPreformatted
         lTable := lLastTable
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
   LOCAL cComponent := hEntry[ "_COMPONENT" ]

   LOCAL cSectionName
   LOCAL cSection
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := NIL
   LOCAL item, cCat

   LOCAL hE

   /* Set template */
   IF ! "TEMPLATE" $ hEntry
      hEntry[ "TEMPLATE" ] := "Function"
   ENDIF
   IF ! hEntry[ "TEMPLATE" ] $ sc_hTemplates
      hEntry[ "TEMPLATE" ] := "Function"
      AddErrorCondition( cFile, "Unrecognized TEMPLATE '" + hEntry[ "TEMPLATE" ] + "'", .T. )
      lAccepted := .F.
   ENDIF

   hE := EntryNew( hEntry[ "TEMPLATE" ] )
   hE[ "_type" ] := cComponent
   hE[ "_sourcefile" ] := StrTran( cFile, "\", hb_ps() )

   /* Merge category/subcategory into tag list */
   hE[ "_tags" ] := { => }
   FOR EACH item IN hb_ATokens( ;
      hb_HGetDef( hEntry, "TAGS", "" ) + ;
      ", " + hb_HGetDef( hEntry, "CATEGORY", "" ) + ;
      ", " + hb_HGetDef( hEntry, "SUBCATEGORY", "" ), "," )

      IF ! HB_ISNULL( item := AllTrim( item ) )
         hE[ "_tags" ][ item ] := NIL
      ENDIF
   NEXT
   hEntry[ "TAGS" ] := ""
   FOR EACH item IN hb_HKeys( hE[ "_tags" ] )
      hEntry[ "TAGS" ] += item
      IF ! item:__enumIsLast()
         hEntry[ "TAGS" ] += ", "
      ENDIF
   NEXT

   IF "CATEGORY" $ hEntry
      IF hEntry[ "CATEGORY" ] $ sc_hConstraint[ "categories" ]
         idxCategory := hEntry[ "CATEGORY" ]
      ELSE
         AddErrorCondition( cFile, "Unrecognized CATEGORY '" + hEntry[ "CATEGORY" ] + "' for template '" + hE[ "TEMPLATE" ] )
      ENDIF
   ENDIF

   FOR EACH item IN hEntry

      cSectionName := item:__enumKey()
      cSection := StrTran( item, Chr( 13 ) + Chr( 10 ), hb_eol() )

      IF !( cSectionName == "EXAMPLES" ) .AND. ;
         !( cSectionName == "TESTS" )
         cSection := NewLineVoodoo( cSection )  /* Decides which EOLs to keep and which to drop */
      ENDIF

      cSection := StrTran( cSection, hb_eol(), Chr( 10 ) )

      IF hb_LeftEq( cSectionName, "_" ) .OR. ;
         cSectionName == "TEMPLATE"

         /* do nothing */

      ELSEIF IsField( hE, cSectionName )

         DO CASE
         CASE cSectionName == "SUBCATEGORY" .AND. IsField( hE, "SUBCATEGORY" )

            IF idxCategory != NIL .AND. ! cSection $ sc_hConstraint[ "categories" ][ idxCategory ]
               AddErrorCondition( cFile, "Unrecognized SUBCATEGORY '" + idxCategory + "'-" + cSection )
            ENDIF

         CASE IsField( hE, "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
               Empty( cSection ) .OR. ;
               Lower( cSection ) == "nil" .OR. ;
               Lower( cSection ) == "none" .OR. ;
               Lower( cSection ) == "none." )

            AddErrorCondition( cFile, "'" + hE[ "NAME" ] + "' is identified as template " + hEntry[ "TEMPLATE" ] + " but has no RETURNS value (" + cSection + ")" )

         CASE ! IsConstraint( hE, cSectionName, cSection )

            cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: "
#if 0
            cSource := hb_HKeyAt( hsTemplate, idx ) + " should be one of: "
#endif
            AEval( sc_hConstraint[ cSectionName ], {| c, n | cSource += iif( n == 1, "", "," ) + c } )
            AddErrorCondition( cFile, cSource )

         ENDCASE

         IF lAccepted
            hE[ cSectionName ] := ExpandAbbrevs( cSectionName, cSection )
         ENDIF
      ELSE
         AddErrorCondition( cFile, "Using template '" + hEntry[ "TEMPLATE" ] + "' encountered an unexpected section '" + cSectionName + "'", .T. )
         lAccepted := .F.
      ENDIF
   NEXT

   /* Verify entry-wide constraints */
   IF lAccepted

      DO CASE
      CASE ! IsComplete( hE, @cSource )
         AddErrorCondition( cFile, "Missing sections: '" + cSource + "'" )
#if 0
         lAccepted := .F.
#endif
      CASE hEntry[ "TEMPLATE" ] == "Function" .AND. ( ;
         Empty( hE[ "RETURNS" ] ) .OR. ;
         Lower( hE[ "RETURNS" ] ) == "nil" .OR. ;
         Lower( hE[ "RETURNS" ] ) == "none" .OR. ;
         Lower( hE[ "RETURNS" ] ) == "none." )

         AddErrorCondition( cFile, "'" + hE[ "NAME" ] + "' is identified as template " + hEntry[ "TEMPLATE" ] + " but has no RETURNS value (" + hE[ "RETURNS" ] + ")" )
#if 0
         lAccepted := .F.
#endif
      ENDCASE
   ENDIF

   IF lAccepted

      IF "(" $ hE[ "NAME" ]
         cSectionName := Parse( hE[ "NAME" ], "(" )
         IF ! cSectionName $ s_hHBX
            AddErrorCondition( cFile, "Not found in HBX: " + cSectionName + " " + cComponent )
         ENDIF
      ENDIF

      hE[ "_filename" ] := Filename( hE[ "NAME" ] )

      AAdd( aContent, hE )

      IF ! cComponent $ s_hTree
         s_hTree[ cComponent ] := { => }
         hb_HCaseMatch( s_hTree[ cComponent ], .F. )
      ENDIF
      IF "CATEGORY" $ hEntry
         cCat := hEntry[ "CATEGORY" ]
         IF ! cCat $ s_hTree[ cComponent ]
            s_hTree[ cComponent ][ cCat ] := { => }
            hb_HCaseMatch( s_hTree[ cComponent ][ cCat ], .F. )
         ENDIF
         IF "SUBCATEGORY" $ hEntry
            IF ! hEntry[ "SUBCATEGORY" ] $ s_hTree[ cComponent ]
               s_hTree[ cComponent ][ cCat ][ hEntry[ "SUBCATEGORY" ] ] := NIL
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ExpandAbbrevs( cSectionName, cCode )

   LOCAL cResult

   SWITCH cSectionName
   CASE "STATUS"
      IF "," $ cCode .AND. Parse( cCode, "," ) $ sc_hConstraint[ "status" ]
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + ExpandAbbrevs( cSectionName, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      IF cCode $ sc_hConstraint[ "status" ]
         RETURN sc_hConstraint[ "status" ][ cCode ]
      ELSEIF Len( cCode ) > 1
         RETURN cCode
      ELSEIF ! HB_ISNULL( cCode )
         RETURN "Unrecognized 'STATUS' code: '" + cCode + "'"
      ELSE
         RETURN sc_hConstraint[ "status" ][ "N" ]
      ENDIF

   CASE "PLATFORMS"
      cResult := ""
      FOR EACH cCode IN hb_ATokens( cCode, "," )
         IF ! HB_ISNULL( cCode := AllTrim( cCode ) )
            cResult += hb_eol() + hb_HGetDef( sc_hConstraint[ "platforms" ], cCode, cCode )
         ENDIF
      NEXT
      RETURN SubStr( cResult, Len( hb_eol() ) + 1 )

   CASE "COMPLIANCE"
      IF "," $ cCode .AND. Parse( cCode, "," ) $ sc_hConstraint[ "compliance" ]
         cResult := ""
         DO WHILE ! HB_ISNULL( cCode )
            cResult += hb_eol() + ExpandAbbrevs( cSectionName, Parse( @cCode, "," ) )
         ENDDO
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      RETURN hb_HGetDef( sc_hConstraint[ "compliance" ], cCode, cCode )

   ENDSWITCH

   RETURN cCode

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
            "-h                   this screen", ;
            "-h <option>          help on <option>, <option> is one of:", ;
            2, ;
            { "categories", "templates", "compliance", "platforms" }, ;
            1, ;
            "-[format=]<type>     output type, default is html, or one of:", ;
            2, ;
            hb_HKeys( s_generators ), ;
            1, ;
            "-v<n>                verbosity level, default: 1", ;
            "-lang=<lang>         language to process, default: " + s_hSwitches[ "lang" ], ;
            "-input=<directory>   input directory, default: " + s_hSwitches[ "dir_in" ], ;
            "-output=<directory>  output directory, default: " + s_hSwitches[ "dir_out" ], ;
            "-output-component    output is one file for each component and a central index (default)", ;
            "-output-entry        output is one file per entry", ;
         } }

   CASE aArgs[ 2 ] == "categories"
      aHelp := { ;
         "Defined categories and sub-categories are:", ;
         sc_hConstraint[ "categories" ] }

   CASE aArgs[ 2 ] == "templates"
      aHelp := { ;
         iif( Len( aArgs ) >= 3, aArgs[ 3 ] + " template is:", "Defined templates are:" ), ;
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], NIL ) ) } }

   CASE aArgs[ 2 ] == "compliance"
      aHelp := { ;
         "Defined 'COMPLIANCE' are:", ;
         "", ;
         {|| ShowComplianceHelp() } }

   CASE aArgs[ 2 ] == "platforms"
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

FUNCTION Parse( /* @ */ cVar, cDelimiter )

   LOCAL cResult
   LOCAL idx

   IF ( idx := At( cDelimiter, cVar ) ) > 0
      cResult := Left( cVar, idx - 1 )
      cVar := SubStr( cVar, idx + Len( cDelimiter ) )
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

   hb_default( @lFatal, .F. )

   IF s_hSwitches[ "verbosity" ] >= 2 .OR. lFatal
      OutStd( "! " + iif( lFatal, "Error:", "Warning" ), cFile + ":", cMessage + hb_eol() )
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
         IF hb_LeftEq( cLine, "<table" ) .OR. cLine == "<fixed>"
            lRaw := .T.
         ELSEIF cLine == "</table>" .OR. cLine == "</fixed>"
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

STATIC FUNCTION Filename( cFile )

   STATIC s_hFiles := { => }

   LOCAL cResult := ""
   LOCAL idx, tmp

   cFile := Lower( cFile )

   FOR idx := 1 TO Len( cFile )
      tmp := SubStr( cFile, idx, 1 )
      IF hb_asciiIsDigit( tmp ) .OR. hb_asciiIsAlpha( tmp ) .OR. tmp == "_"
         cResult += tmp
      ENDIF
   NEXT

   IF cResult $ s_hFiles
      idx := 0
      DO WHILE ( tmp := cResult + "_" + StrZero( ++idx, 3 ) ) $ s_hFiles
      ENDDO
      cResult := tmp
   ENDIF

   s_hFiles[ cResult ] := NIL

   RETURN cResult

STATIC FUNCTION EntryNew( cTemplate )

   LOCAL hE := { => }
   LOCAL item, key, idx

   hb_HCaseMatch( hE, .F. )
   hb_HEval( sc_hFields, {| k | hE[ k ] := "" } )

   hE[ "_group" ] := sc_hTemplates[ cTemplate ]

   FOR EACH item IN sc_hFields
      key := item:__enumKey()
      idx := item:__enumIndex()
      hE[ key ] := iif( key == "TEMPLATE", cTemplate, iif( hE[ "_group" ][ idx ] == TPL_REQUIRED,, "" ) )
   NEXT

   RETURN hE

FUNCTION IsField( hE, cField, nType )

   LOCAL idx

   IF ( idx := hb_HPos( sc_hFields, cField ) ) > 0
      IF hE[ "_group" ][ idx ] == 0
      ELSEIF HB_ISNUMERIC( nType ) .AND. hb_bitAnd( hE[ "_group" ][ idx ], nType ) != nType
      ELSE
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

STATIC FUNCTION IsConstraint( hE, cField, cSection )

   IF hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], hb_bitAnd( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
      RETURN .T.
   ELSEIF cField $ sc_hConstraint
      RETURN ;
         cSection $ sc_hConstraint[ cField ] .OR. ;
         Parse( cSection, "," ) $ sc_hConstraint[ cField ]
   ENDIF

   RETURN .T.

STATIC FUNCTION IsComplete( hE, cIncompleteFieldsList )

   LOCAL lResult := .T.
   LOCAL idx, key

   cIncompleteFieldsList := ""

   FOR idx := 1 TO Len( sc_hFields )
      key := hb_HKeyAt( sc_hFields, idx )
      IF hb_bitAnd( hE[ "_group" ][ idx ], TPL_REQUIRED ) != 0 .AND. Empty( hE[ key ] )
         cIncompleteFieldsList += "," + key
         lResult := .F.
      ENDIF
   NEXT

   cIncompleteFieldsList := SubStr( cIncompleteFieldsList, 2 )

   RETURN lResult

FUNCTION IsPreformatted( hE, cField )

   LOCAL nGroup := hb_HPos( sc_hFields, cField )

   RETURN nGroup > 0 .AND. hb_bitAnd( hE[ "_group" ][ nGroup ], TPL_PREFORMATTED ) != 0

STATIC FUNCTION IsRequired( hE, cField )
   RETURN hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], TPL_REQUIRED ) != 0

FUNCTION IsOutput( hE, cField )
   RETURN hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], TPL_OUTPUT ) != 0

FUNCTION FieldIDList()
   RETURN hb_HKeys( sc_hFields )

FUNCTION FieldCaption( cName )
   RETURN sc_hFields[ cName ]

STATIC PROCEDURE init_Templates()

   LOCAL hSubCategories := { ;
      "" =>, ;
      "Application" =>, ;
      "Array" =>, ;
      "Classes" =>, ;
      "Conversion" =>, ;
      "Database" =>, ;
      "Date/Time" =>, ;
      "Environment" =>, ;
      "Error" =>, ;
      "Events" =>, ;
      "Execute and execution" =>, ;  /* replace w/ "Environment"? */
      "Extend" =>, ;
      "FileSys" =>, ;
      "Fixed memory" =>, ;
      "Garbage collector" =>, ;
      "Hash table" =>, ;
      "Idle states" =>, ;
      "INET" =>, ;
      "Internal" =>, ;
      "Item" =>, ;
      "Language and Nation" =>, ;
      "Legacy" =>, ;
      "Macro" =>, ;
      "Math" =>, ;
      "Objects" =>, ;
      "Printer" =>, ;
      "RDD" =>, ;
      "Strings" =>, ;
      "Terminal" =>, ;
      "Undocumented" =>, ;
      "User interface" =>, ;
      "Variable management" =>, ;
      "Virtual machine" => }

   hb_HCaseMatch( hSubCategories, .F. )

   sc_hConstraint := { => }
   hb_HCaseMatch( sc_hConstraint, .F. )

   sc_hConstraint[ "categories" ] := { ;
      "Document"                  => { "License" =>, "Compiler" =>, "" => }, ;
      "API"                       => hSubCategories, ;
      "C level API"               => hSubCategories, ;
      "C level API compatibility" => hSubCategories, ;
      "Class"                     => { ;
          "" =>, ;
          "Access" =>, ;
          "Assign" =>, ;
          "Constructor" =>, ;
          "Data" =>, ;
          "Definition" =>, ;
          "Destructor" =>, ;
          "Method" =>, ;
          "Var" => }, ;
      "Command"                   => hSubCategories, ;
      /* "Compile time errors"    => { "" => }, */ ;
      "Run time errors"           => { "" => } }

   hb_HCaseMatch( sc_hConstraint[ "categories" ], .F. )

   sc_hConstraint[ "compliance" ] := { ;
      "C"       => "CA-Cl*pper v5.x compatible", ;
      "C52S"    => "CA-Cl*pper v5.x compatible in builds with HB_CLP_STRICT option enabled", ;
      "C52U"    => "Undocumented CA-Cl*pper v5.x function only available in builds with HB_CLP_UNDOC option enabled (default)", ;
      "C53"     => "CA-Cl*pper v5.3 function only available in builds with HB_COMPAT_C53 option enabled (default)", ;
      "H"       => "Harbour specific", ;
      "NA"      => "Not applicable" }

   sc_hConstraint[ "platforms" ] := { ;
      "All"     => "Available on all platforms", ;
      "Unix"    => "Available on Unix platform(s)", ;
      "Linux"   => "Available on Linux", ;
      "Windows" => "Available on Windows", ;
      "OS2"     => "Available on OS/2", ;
      "DOS"     => "Available on MS-DOS" }

   sc_hConstraint[ "status" ] := { ;
      "R" => "Ready", ;
      "S" => "Started", ;
      "N" => "Not started" }

   sc_hFields := { ;
      "DOC"           => "Doc", ;
      "TEMPLATE"      => "Template", ;
      "NAME"          => "", ;
      "ONELINER"      => "", ;
      "CATEGORY"      => "Category", ;
      "SUBCATEGORY"   => "Sub category", ;
      "SYNTAX"        => "Syntax", ;
      "ARGUMENTS"     => "Argument(s)", ;
      "RETURNS"       => "Returns", ;
      "DESCRIPTION"   => "Description", ;
      "DATALINK"      => "Data link", ;
      "DATANOLINK"    => "Data no link", ;
      "METHODSLINK"   => "Methods link", ;
      "METHODSNOLINK" => "Methods no link", ;
      "EXAMPLES"      => "Example(s)", ;
      "TESTS"         => "Test(s)", ;
      "STATUS"        => "Status", ;       /* sc_hConstraint[ "status" ] is the constraint list */
      "COMPLIANCE"    => "Compliance", ;   /* sc_hConstraint[ "compliance" ] is the constraint list */
      "PLATFORMS"     => "Platform(s)", ;  /* sc_hConstraint[ "platforms" ] is the constraint list */
      "FILES"         => "File(s)", ;
      "TAGS"          => "Tag(s)", ;
      "SEEALSO"       => "See also", ;
      "END"           => "End" }

   hb_HCaseMatch( sc_hFields, .F. )

   #define _S TPL_START
   #define _E TPL_END
   #define _T TPL_TEMPLATE
   #define _R TPL_REQUIRED
   #define _O TPL_OPTIONAL
   #define _P TPL_PREFORMATTED
   #define _U TPL_OUTPUT

   /* The columns of this array correspond to the elements of sc_hFields */
   sc_hTemplates := { ;
      "Template"       => { _S, _T,  0+_U,  0+_U,  0, _O,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U, _E }, ;
      "Document"       => { _S, _T, _R+_U, _O+_U, _R, _O,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Function"       => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "C Function"     => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Procedure"      => { _S, _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U,     0, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Command"        => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Class"          => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _E }, ;
      "Class method"   => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E }, ;
      "Class data"     => { _S, _T, _R+_U, _O+_U, _R, _R, _R+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E }, ;
      "Run time error" => { _S, _T, _R+_U, _O+_U, _R,  0,  0+_U,  0+_U,  0+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _O+_U, _E } }

   RETURN

STATIC PROCEDURE ShowTemplatesHelp( cTemplate )

   LOCAL idxTemplates, nFrom := 1, nTo := Len( sc_hTemplates )
   LOCAL idx, key, fldkey, hE
   LOCAL aEntry, hEntry

   IF ! Empty( cTemplate ) .AND. !( cTemplate == "Template" )
      nFrom := nTo := hb_HPos( sc_hTemplates, cTemplate )
      IF nFrom == 0
         ShowHelp( "Unrecognized template '" + cTemplate + "'" )
         RETURN
      ENDIF
   ENDIF

   aEntry := {}

   FOR idxTemplates := nFrom TO nTo

      key := hb_HKeyAt( sc_hTemplates, idxTemplates )
      IF !( key == "Template" )
         hE := EntryNew( key )

         hEntry := { => }
         FOR idx := 1 TO Len( sc_hFields )
            fldkey := hb_HKeyAt( sc_hFields, idx )
            IF hE[ "_group" ][ idx ] != TPL_START .AND. ;
               hE[ "_group" ][ idx ] != TPL_END .AND. ;
               hE[ "_group" ][ idx ] != 0
               hEntry[ fldkey ] := iif( fldkey == "TEMPLATE", key, iif( IsRequired( hE, fldkey ), "<required>", "<optional>" ) )
            ENDIF
         NEXT

         AAdd( aEntry, hEntry )
      ENDIF
   NEXT

   OutStd( __hbdoc_ToSource( aEntry ) )

   RETURN

STATIC PROCEDURE ShowComplianceHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "compliance" ]
      ShowSubHelp( item:__enumKey(), 1, 0, item:__enumIndex() )
      ShowSubHelp( ExpandAbbrevs( "COMPLIANCE", item:__enumKey() ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE ShowPlatformsHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "platforms" ]
      ShowSubHelp( item:__enumKey(), 1, 0, item:__enumIndex() )
      ShowSubHelp( ExpandAbbrevs( "PLATFORMS", item:__enumKey() ), 1, 6, item:__enumIndex() )
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
