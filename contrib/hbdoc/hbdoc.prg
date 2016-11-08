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

/* Optimizations */
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "hbhash.ch"
#include "hbver.ch"

#define I_( x )   hb_UTF8ToStr( hb_i18n_gettext( x /*, _SELF_NAME_ */ ) )
#define BI_( x )  {|| I_( x ) }

ANNOUNCE HB_GTSYS
REQUEST HB_GT_CGI_DEFAULT

REQUEST HB_CODEPAGE_UTF8EX

#define TPL_REQUIRED         1  // intentionally has a 'required' and 'optional' flag
#define TPL_OPTIONAL         2
#define TPL_PREFORMATTED     4
#define TPL_CONSTRAINTLIST   8
#define TPL_TEMPLATE         16
#define TPL_OUTPUT           32

STATIC s_hPO := { => }
STATIC sc_hFields
STATIC sc_hTemplates
STATIC sc_hConstraint := { => }
STATIC sc_generators := { ;
   "all"   =>, ;
   "html"  => @GenerateHTML(), ;
   "ascii" => @GenerateAscii(), ;
   "text"  => @GenerateText(), ;
   "xml"   => @GenerateXML() }

STATIC s_hSwitches

STATIC s_hHBX := { => }
STATIC s_hHBXStat := { => }
STATIC s_hDoc := { => }  /* lang => { entries => {}, nameid => { => }, tree => { component => category => subcategory } */

STATIC s_hNameID
STATIC s_cLang := "en"

PROCEDURE Main()

   LOCAL aArgs := hb_AParams()
   LOCAL idx, item
   LOCAL arg, tmp, nLen, nCount, aList
   LOCAL cArgName
   LOCAL cFormat
   LOCAL oDocument, oIndex
   LOCAL docs, cLang, aEntries, hTree
   LOCAL cCat1, cCat1Prev
   LOCAL cCat2, cCat2Prev
   LOCAL aComponent, hComponents
   LOCAL cID, cName, cNameShort
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
      "lang"      => {}, ;
      "contribs"  => .T., ;
      "format"    => { "html" }, ;
      "output"    => "component", ;
      "dir_out"   => hb_DirSepToOS( "./" ), ;
      "repr"      => .F., ;
      "verbosity" => 1, ;
      "dump"      => .F., ;
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
      IF ! HB_ISNULL( arg )
         IF ( idx := At( "=", arg ) ) == 0
            cArgName := arg
            arg := ""
         ELSE
            cArgName := Left( arg, idx - 1 )
            arg := SubStr( arg, idx + 1 )
         ENDIF

         DO CASE
         CASE hb_LeftEq( cArgName, "-v" )
            s_hSwitches[ "verbosity" ] := Val( SubStr( cArgName, Len( "-v" ) + 1 ) )
         CASE cArgName == "-input"
            s_hSwitches[ "dir_in" ] := hb_DirSepAdd( hb_DirSepToOS( arg ) )
         CASE cArgName == "-output"
            s_hSwitches[ "dir_out" ] := hb_DirSepAdd( hb_DirSepToOS( arg ) )
         CASE cArgName == "-repr"
            s_hSwitches[ "repr" ] := .T.
         CASE cArgName == "-dump"
            s_hSwitches[ "dump" ] := .T.
         CASE cArgName == "-lang"
            AAdd( s_hSwitches[ "lang" ], LangToInternal( arg ) )
         CASE cArgName == "-format"
            DO CASE
            CASE arg == "" .OR. ! arg $ sc_generators
               ShowHelp( "Unrecognized format option '" + arg + "'" )
               RETURN
            CASE arg == "all"
               s_hSwitches[ "format" ] := hb_HKeys( sc_generators )
            OTHERWISE
               AAdd( s_hSwitches[ "format" ], arg )
            ENDCASE
         CASE hb_LeftEq( cArgName, "-output-" )
            s_hSwitches[ "output" ] := SubStr( cArgName, Len( "-output-" ) + 1 )
         OTHERWISE
            DO CASE
            CASE ! SubStr( cArgName, 2 ) $ sc_generators
               ShowHelp( "Unrecognized option: " + cArgName + iif( Len( arg ) > 0, "=" + arg, "" ) )
               RETURN
            CASE SubStr( cArgName, 2 ) == "all"
               s_hSwitches[ "format" ] := hb_HKeys( sc_generators )
            OTHERWISE
               AAdd( s_hSwitches[ "format" ], SubStr( cArgName, 2 ) )
            ENDCASE
         ENDCASE
      ENDIF
   NEXT

   OutStd( hb_StrFormat( "! Input directory: %1$s", s_hSwitches[ "dir_in" ] ) + hb_eol() )

   ProcessDirs( s_hDoc, s_hHBX )

   IF s_hSwitches[ "dump" ]
      hb_MemoWrit( "__hbx.json", hb_jsonEncode( s_hHBX, .T. ) )
      hb_MemoWrit( "__doc.json", hb_jsonEncode( s_hDoc, .T. ) )
   ENDIF

   nStart := hb_MilliSeconds()

   FOR EACH docs IN s_hDoc

      cLang     := docs:__enumKey()
      aEntries  := docs[ "entries" ]
      hTree     := docs[ "tree" ]
      s_hNameID := docs[ "nameid" ]  /* hack */

      IF ! Empty( s_hSwitches[ "lang" ] ) .AND. ;
         hb_AScan( s_hSwitches[ "lang" ], Lower( cLang ),,, .T. ) == 0
         LOOP
      ENDIF

      UseLang( cLang )

      OutStd( hb_StrFormat( "! %1$d '%2$s' entries found", Len( aEntries ), cLang ) + hb_eol() )

      ASort( aEntries,,, {| oL, oR | ;
            PadR( SortWeight( oL[ "CATEGORY" ] ), 30 ) + ;
            PadR( SortWeight( oL[ "SUBCATEGORY" ] ), 60 ) + ;
            PadR( oL[ "NAME" ], 50 ) ;
         < ;
            PadR( SortWeight( oR[ "CATEGORY" ] ), 30 ) + ;
            PadR( SortWeight( oR[ "SUBCATEGORY" ] ), 60 ) + ;
            PadR( oR[ "NAME" ], 50 ) ;
         } )

      FOR EACH cFormat IN s_hSwitches[ "format" ]

         IF HB_ISEVALITEM( generatorClass := hb_HGetDef( sc_generators, Lower( cFormat ) ) )

            cDir := s_hSwitches[ "dir_out" ] + cFormat

            OutStd( hb_StrFormat( "! Output directory: %1$s", hb_PathNormalize( hb_PathJoin( hb_DirBase(), cDir ) ) ) + hb_eol() )

            aComponent := ASort( hb_HKeys( hTree ),,, {| x, y | SortWeightPkg( x ) < SortWeightPkg( y ) } )

            hComponents := { => }
            hb_HKeepOrder( hComponents, .T. )
            FOR EACH tmp IN aComponent
               Get_ID_Name( tmp, @cID, @cName, @cNameShort )
               hComponents[ tmp ] := { ;
                  "id" => cID, ;
                  "name" => cName, ;
                  "nameshort" => cNameShort }
            NEXT

            SWITCH s_hSwitches[ "output" ]
            CASE "component"

               oIndex := Eval( generatorClass ):NewIndex( cDir, "index", I_( "Index" ), Lower( cLang ), hComponents )

               /* index TOC */

               IF oIndex != NIL
#if 1
                  oIndex:BeginIndex()
                  FOR EACH tmp IN aComponent
                     Get_ID_Name( tmp, @cID,, @cName )
                     oIndex:AddIndexItem( hb_StrFormat( I_( "%1$s index" ), cName ), cID )
                  NEXT
                  oIndex:EndIndex()
#else
                  oIndex:BeginTOC()
                  FOR EACH tmp IN aComponent

                     Get_ID_Name( tmp, @cID, @cName )

                     cCat1Prev := NIL

                     oIndex:BeginTOCItem( cName, cID )
#if 0
                     FOR EACH item IN ASort( hb_HKeys( hTree[ tmp ] ),,, {| x, y | SortWeight( x ) < SortWeight( y ) } )
                        oIndex:AddReference( cCat1, "", cID + "-" + Lower( cCat1 ) )
                     NEXT
#endif
                     oIndex:EndTOCItem()
                  NEXT
                  oIndex:EndTOC()
#endif
                  oIndex:BeginContent()
               ENDIF

               IF s_hSwitches[ "verbosity" ] >= 1
                  OutStd( Chr( 13 ) + "!" + " " )
               ENDIF

               nLen := Len( aEntries )
               nCount := 0
               FOR EACH tmp IN aComponent

                  Get_ID_Name( tmp, @cID, @cName )

                  oDocument := Eval( generatorClass ):NewDocument( cDir, tmp, cName, Lower( cLang ), hComponents )

                  /* content TOC */

                  aList := {}
                  FOR EACH item IN aEntries
                     IF item[ "_type" ] == tmp
                        AAdd( aList, item )
                     ENDIF
                  NEXT

                  oDocument:BeginIndex()
                  FOR EACH item IN ASort( aList,,, {| oL, oR | ;
                        SortWeightTOC( oL[ "CATEGORY" ] ) + SortWeightTOC( oL[ "SUBCATEGORY" ] ) + PadR( oL[ "NAME" ], 50 ) < ;
                        SortWeightTOC( oR[ "CATEGORY" ] ) + SortWeightTOC( oR[ "SUBCATEGORY" ] ) + PadR( oR[ "NAME" ], 50 ) } )
                     oDocument:AddIndexItem( item[ "NAME" ], item[ "_filename" ] )
                  NEXT
                  oDocument:EndIndex()

                  IF oIndex != NIL
                     oIndex:BeginSection( cName, oDocument:cFilename, cID )
                  ENDIF

                  /* content */

                  cCat1Prev := cCat2Prev := NIL

                  oDocument:BeginContent()
                  FOR EACH item IN aEntries

                     IF item[ "_type" ] == tmp

                        IF ++nCount / 10 == Int( nCount / 10 ) .AND. s_hSwitches[ "verbosity" ] >= 1
                           OutStd( Chr( 13 ) + "!" + " " + Str( Int( nCount / nLen * 100 ), 3 ) + "%" )
                        ENDIF

                        oDocument:AddEntry( item )

                        IF oIndex != NIL
                           cCat1 := item[ "CATEGORY" ]
                           IF cCat1Prev == NIL .OR. !( cCat1 == cCat1Prev )
                              IF cCat1Prev != NIL
                                 oIndex:EndSection()
                              ENDIF
                              oIndex:BeginSection( cCat1,, cID + "-" + Lower( cCat1 ) )
                              cCat1Prev := cCat1
                           ENDIF

                           cCat2 := item[ "SUBCATEGORY" ]
                           IF cCat2Prev == NIL .OR. !( cCat2 == cCat2Prev )
// #define SUBCAT_INDENT
#ifdef SUBCAT_INDENT
                              IF cCat2Prev != NIL
                                 oIndex:EndSection()
                              ENDIF
                              oIndex:BeginSection( cCat2,, iif( Empty( cCat2 ),, cID + "-" + Lower( cCat1 ) + "-" + Lower( cCat2 ) ) )
#else
                              IF cCat2Prev != NIL
                                 oIndex:SubCategory( cCat2, iif( Empty( cCat2 ),, cID + "-" + Lower( cCat1 ) + "-" + Lower( cCat2 ) ) )
                              ENDIF
#endif
                              cCat2Prev := cCat2
                           ENDIF

                           oIndex:AddReference( item )
                        ENDIF
                     ENDIF
                  NEXT
                  oDocument:EndContent()

#ifdef SUBCAT_INDENT
                  IF cCat2Prev != NIL
                     oIndex:EndSection()
                  ENDIF
#endif
                  IF cCat1Prev != NIL
                     oIndex:EndSection()
                  ENDIF

                  IF oIndex != NIL
                     oIndex:EndSection()
                  ENDIF

                  oDocument:Generate()
               NEXT

               IF oIndex != NIL
                  oIndex:EndContent()
                  oIndex:Generate()
               ENDIF

               IF s_hSwitches[ "verbosity" ] >= 1
                  OutStd( Chr( 13 ) + "!" + " " + Str( 100, 3 ) + "%" + hb_eol() )
               ENDIF

               EXIT

            CASE "entry"

               FOR EACH item IN aEntries
                  oDocument := Eval( generatorClass ):NewDocument( cDir, item[ "_filename" ], item[ "NAME" ], Lower( cLang ) )
                  oDocument:AddEntry( item )
                  oDocument:Generate()
               NEXT

               EXIT

            ENDSWITCH
         ENDIF
      NEXT
   NEXT

   OutStd( hb_StrFormat( "! Done in %1$.2f seconds", Round( ( hb_MilliSeconds() - nStart ) / 1000, 2 ) ) + hb_eol() )

   RETURN

STATIC PROCEDURE UseLang( cLang )

   STATIC s_hLang := { => }
   STATIC s_cLangLast := ""

   LOCAL tmp, tmp1, tmp2

   IF !( s_cLangLast == cLang )
      s_cLangLast := cLang
      IF Lower( cLang ) == "en"
         hb_i18n_Set( NIL )
      ELSE
         IF ! cLang $ s_hLang
            /* Save in-memory file to disk, because __i18n_*() can only load from there */
            IF ( tmp2 := hb_vfTempFile( @tmp1,,, ".txt" ) ) != NIL
               hb_vfWrite( tmp2, hb_HGetDef( s_hPO, "hbdoc." + cLang + ".po", "" ) )
               hb_vfClose( tmp2 )
            ENDIF
            IF ( tmp := __i18n_potArrayLoad( tmp1, @tmp2 ) ) != NIL
               s_hLang[ cLang ] := __i18n_hashTable( __i18n_potArrayToHash( tmp, .F. ) )
               OutStd( hb_StrFormat( "! .po loaded: %1$s", cLang ) + hb_eol() )
            ELSE
               OutErr( hb_StrFormat( "! Error: Cannot load .po: %1$s", tmp2 ) + hb_eol() )
            ENDIF
            hb_vfErase( tmp1 )
         ENDIF
         IF cLang $ s_hLang
            hb_i18n_Set( s_hLang[ cLang ] )
         ELSE
            hb_i18n_Set( NIL )
         ENDIF
      ENDIF
   ENDIF

   RETURN

FUNCTION hbdoc_LangList()
   RETURN s_hDoc

FUNCTION hbdoc_NameID()
   RETURN s_hNameID

FUNCTION hbdoc_RootDir()
   RETURN s_hSwitches[ "dir_in" ]

FUNCTION hbdoc_HBX()
   RETURN s_hHBX

FUNCTION hbdoc_reproducible()
   RETURN s_hSwitches[ "repr" ] .AND. ! Empty( hb_Version( HB_VERSION_BUILD_TIMESTAMP_UTC ) )

FUNCTION hbdoc_dir_in()
   RETURN s_hSwitches[ "dir_in" ]

STATIC PROCEDURE Get_ID_Name( cComponent, /* @ */ cID, /* @ */ cName, /* @ */ cNameShort )

   DO CASE
   CASE cComponent == "harbour"
      cID := "core"
      cName := "Harbour core"
      cNameShort := cName
   CASE hb_LeftEq( cComponent, "cl" )
      cID := cComponent
      cName := hb_HGetDef( { ;
        "cl53"  => "Clipper 5.3", ;
        "clct3" => "Clipper Tools 3" }, cComponent, cComponent )
      cNameShort := cName
   OTHERWISE
      cID := cComponent
      cName := hb_StrFormat( I_( "%1$s contrib" ), cComponent )
      cNameShort := cComponent
   ENDCASE

   RETURN

STATIC FUNCTION LangToInternal( cLang )
   RETURN StrTran( cLang, "-", "_" )

/* Begin with Harbour core section */
STATIC FUNCTION SortWeightPkg( cString )
   RETURN iif( cString == "harbour", "A", iif( hb_LeftEq( cString, "cl" ), "B", "C" ) ) + cString

STATIC FUNCTION SortWeightTOC( cString )

   SWITCH cString
   CASE "Table"
   CASE "Appendix"  ; RETURN "Z"
   CASE "Document"
   CASE "Intro"     ; RETURN "A"
   CASE "Copyright" ; RETURN "0"
   ENDSWITCH

   RETURN "B"

STATIC FUNCTION SortWeight( cString )

   SWITCH cString
   CASE "Document" ; RETURN Chr( 31 ) + "001" + cString  /* category */
   CASE "Intro"    ; RETURN Chr( 31 ) + "001" + cString  /* subcategory */
   CASE "License"  ; RETURN Chr( 31 ) + "002" + cString  /* subcategory */
   CASE "Compiler" ; RETURN Chr( 31 ) + "003" + cString  /* subcategory */
   ENDSWITCH

   RETURN cString

STATIC PROCEDURE ProcessDirs( hDoc, hAll )

   LOCAL cDir
   LOCAL file

   DirLoadHBX( s_hSwitches[ "dir_in" ] + "include", hAll )

   ProcessDocDir( s_hSwitches[ "dir_in" ], "harbour", hDoc )

   IF s_hSwitches[ "contribs" ]

      cDir := s_hSwitches[ "dir_in" ] + "contrib"

      FOR EACH file IN hb_DirScan( cDir,, "D" )
         IF file[ F_ATTR ] == "D" .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == "." ) .AND. ;
            !( hb_FNameName( hb_DirSepDel( file[ F_NAME ] ) ) == ".." )

            DirLoadHBX( cDir + hb_ps() + file[ F_NAME ], hAll )

            IF ! ProcessDocDir( cDir + hb_ps() + file[ F_NAME ], hb_FNameName( file[ F_NAME ] ), hDoc )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION ProcessDocDir( cDir, cComponent, hDoc )

   LOCAL aErrMsg := {}
   LOCAL aEntry := __hbdoc_LoadDir( cDir, cComponent, aErrMsg )

   LOCAL hEntry
   LOCAL hCountA := { => }
   LOCAL hCountF := { => }
   LOCAL tmp

   hb_HAutoAdd( hCountA, HB_HAUTOADD_ALWAYS )
   hb_HDefault( hCountA, 0 )
   hb_HAutoAdd( hCountF, HB_HAUTOADD_ALWAYS )
   hb_HDefault( hCountF, 0 )

   FOR EACH tmp IN aErrMsg
      AddErrorCondition( cDir, tmp )
   NEXT

   IF ! Empty( aEntry )

      IF s_hSwitches[ "dump" ]
         hb_MemoWrit( "_" + aEntry[ 1 ][ "_COMPONENT" ] + ".json", hb_jsonEncode( aEntry, .T. ) )
      ENDIF

      FOR EACH hEntry IN aEntry

         tmp := LangToInternal( hEntry[ "_LANG" ] )

         IF ! tmp $ hDoc
            hDoc[ tmp ] := { ;
               "entries" => {}, ;
               "tree"    => { => }, ;
               "nameid"  => { => }, ;
               "uid"     => { => } }  /* separate for each language. TODO: make it global by matching component+name accross languages */
            hb_HCaseMatch( hDoc[ tmp ][ "nameid" ], .F. )
         ENDIF

         UseLang( tmp )

         ProcessBlock( hEntry, hDoc[ tmp ], @hCountA[ tmp ], @hCountF[ tmp ] )
      NEXT

      IF ! Empty( hCountA )
         FOR EACH tmp IN hCountA
            IF hb_LeftEq( cComponent, "cl" )
               s_hHBXStat[ cComponent ] := hCountF[ tmp:__enumKey() ]
            ENDIF
            OutStd( hb_StrFormat( "! %1$s %2$s (%3$d entries, %4$.2f%%)", ;
               cDir, tmp:__enumKey(), tmp, Round( hCountF[ tmp:__enumKey() ] * 100 / s_hHBXStat[ cComponent ], 2 ) ) + hb_eol() )
         NEXT
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
   LOCAL nFixedIndent := 1

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
         IF AllTrim( cLine ) == "<fixed>" .OR. hb_LeftEq( AllTrim( cLine ), '```' )
            nFixedIndent := Len( cLine ) - Len( LTrim( cLine ) ) + 1
         ELSE
            nFixedIndent := 1
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
      ELSEIF nLastIndent != ( Len( cLine ) - Len( LTrim( cLine ) ) ) .OR. lPreformatted
         nLastIndent := Len( cLine ) - Len( LTrim( cLine ) )
         IF !( Right( cSection, Len( hb_eol() ) ) == hb_eol() )
            cSection += hb_eol()
         ENDIF
         cSection += iif( lPreformatted, SubStr( cLine, nFixedIndent ), AllTrim( cLine ) )
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

STATIC PROCEDURE ProcessBlock( hEntry, docs, /* @ */ nCount, /* @ */ nCountFunc )

   LOCAL cFile := hEntry[ "_DOCSOURCE" ]
   LOCAL cComponent := hEntry[ "_COMPONENT" ]

   LOCAL cSectionName
   LOCAL cSection
   LOCAL lAccepted := .T.
   LOCAL cSource
   LOCAL idxCategory := NIL
   LOCAL item, cCat
   LOCAL hTree

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

   IF ! "PLATFORMS" $ hEntry .OR. HB_ISNULL( hEntry[ "PLATFORMS" ] )
      hEntry[ "PLATFORMS" ] := "All"
   ENDIF

   FOR EACH item IN hEntry

      cSectionName := item:__enumKey()
      cSection := StrTran( item, Chr( 13 ) + Chr( 10 ), hb_eol() )

      IF !( "|" + cSectionName + "|" $ "|SYNTAX|EXAMPLES|TESTS|FILES|" )
         cSection := NewLineVoodoo( cSection )  /* Decides which EOLs to keep and which to drop */
      ENDIF

      cSection := StrTran( cSection, hb_eol(), Chr( 10 ) )

      IF hb_LeftEq( cSectionName, "_" ) .OR. cSectionName == "TEMPLATE"

         /* do nothing */

      ELSEIF IsField( hE, cSectionName )

         DO CASE
         CASE cSectionName == "SUBCATEGORY" .AND. IsField( hE, "SUBCATEGORY" )

            IF idxCategory != NIL .AND. ! cSection $ sc_hConstraint[ "categories" ][ idxCategory ]
               AddErrorCondition( cFile, "Unrecognized SUBCATEGORY '" + idxCategory + "'-" + cSection )
            ENDIF

         CASE IsField( hE, "RETURNS" ) .AND. cSectionName == "RETURNS" .AND. ( ;
               Empty( cSection ) .OR. ;
               Upper( cSection ) == "NIL" .OR. ;
               Lower( cSection ) == "none" .OR. ;
               Lower( cSection ) == "none." )

            AddErrorCondition( cFile, "'" + hE[ "NAME" ] + "' is identified as template " + hEntry[ "TEMPLATE" ] + " but has no RETURNS value (" + cSection + ")" )

         CASE ! IsConstraint( hE, cSectionName, cSection )

            cSource := cSectionName + " is '" + iif( Len( cSection ) <= 20, cSection, Left( StrTran( cSection, hb_eol() ), 20 ) + "..." ) + "', should be one of: ..."
            AddErrorCondition( cFile, cSource )

         ENDCASE

         IF lAccepted
            hE[ cSectionName ] := ExpandAbbrevs( cFile, cSectionName, cSection )
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
         Upper( hE[ "RETURNS" ] ) == "NIL" .OR. ;
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

      hE[ "_filename" ] := GenUniqueID( docs[ "uid" ], cComponent, hE[ "NAME" ] )

      docs[ "nameid" ][ hE[ "NAME" ] ] := { "id" => hE[ "_filename" ], "component" => cComponent }

      AAdd( docs[ "entries" ], hE )

      ++nCount
      IF hEntry[ "TEMPLATE" ] == "Function"
         ++nCountFunc
      ENDIF

      hTree := docs[ "tree" ]
      IF ! cComponent $ hTree
         hTree[ cComponent ] := { => }
         hb_HCaseMatch( hTree[ cComponent ], .F. )
      ENDIF
      IF "CATEGORY" $ hEntry
         cCat := hEntry[ "CATEGORY" ]
         IF ! cCat $ hTree[ cComponent ]
            hTree[ cComponent ][ cCat ] := { => }
            hb_HCaseMatch( hTree[ cComponent ][ cCat ], .F. )
         ENDIF
         IF "SUBCATEGORY" $ hEntry
            IF ! hEntry[ "SUBCATEGORY" ] $ hTree[ cComponent ]
               hTree[ cComponent ][ cCat ][ hEntry[ "SUBCATEGORY" ] ] := {}
            ENDIF
//          AAdd( hTree[ cComponent ][ cCat ][ hEntry[ "SUBCATEGORY" ] ], hE )
         ENDIF
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION ExpandAbbrevs( cFile, cSectionName, cCode )

   LOCAL cResult
   LOCAL tmp

   SWITCH cSectionName
   CASE "STATUS"
      cResult := ""
      FOR EACH tmp IN ASort( hb_ATokens( cCode, "," ) )
         IF ! HB_ISNULL( tmp := AllTrim( tmp ) )
            IF ! HB_ISNULL( cResult )
               cResult += hb_eol()
            ENDIF
            IF tmp $ sc_hConstraint[ "status" ]
               tmp := Eval( sc_hConstraint[ "status" ][ tmp ] )
            ELSEIF Len( tmp ) == 1
               AddErrorCondition( cFile, "Unrecognized 'STATUS' code: '" + tmp + "'" )
            ENDIF
            cResult += tmp
         ENDIF
      NEXT
      IF HB_ISNULL( cResult )
         cResult := Eval( sc_hConstraint[ "status" ][ "N" ] )
      ENDIF
      RETURN cResult

   CASE "PLATFORMS"
      cResult := ""
      FOR EACH cCode IN ASort( hb_ATokens( cCode, "," ) )
         IF ! HB_ISNULL( cCode := AllTrim( cCode ) )
            cResult += hb_eol() + Eval( hb_HGetDef( sc_hConstraint[ "platforms" ], cCode, {|| cCode } ) )
         ENDIF
      NEXT
      RETURN SubStr( cResult, Len( hb_eol() ) + 1 )

   CASE "COMPLIANCE"
      IF "," $ cCode .AND. Parse( cCode, "," ) $ sc_hConstraint[ "compliance" ]  /* Detect if not free text */
         cResult := ""
         FOR EACH tmp IN ASort( hb_ATokens( cCode, "," ) )
            IF ! HB_ISNULL( tmp := AllTrim( tmp ) )
               cResult += hb_eol() + Eval( hb_HGetDef( sc_hConstraint[ "compliance" ], tmp, {|| tmp } ) )
            ENDIF
         NEXT
         RETURN SubStr( cResult, Len( hb_eol() ) + 1 )
      ENDIF

      RETURN Eval( hb_HGetDef( sc_hConstraint[ "compliance" ], cCode, {|| cCode } ) )

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
   CASE Empty( aArgs ) .OR. Len( aArgs ) <= 1 .OR. HB_ISNULL( aArgs[ 1 ] )
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
            "-[format=]<type>     output type, default: html. <type> is one of:", ;
            2, ;
            hb_HKeys( sc_generators ), ;
            1, ;
            "-v<n>                verbosity level, default: 1", ;
            "-repr                create reproducible output, default: false", ;
            "-lang=<lang>         language to process, default: all", ;
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
         {|| ShowTemplatesHelp( iif( Len( aArgs ) >= 3, aArgs[ 3 ], ) ) } }

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
      OutStd( hb_StrFormat( "! %1$s: %2$s: %3$s", iif( lFatal, "Error", "Warning" ), cFile, cMessage ) + hb_eol() )
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
      AEval( aText, {| c, n | aText[ n ] := RTrim( Space( nLeftMargin ) + SubStr( c, idx + 1 ) ) } )
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

STATIC FUNCTION GenUniqueID( hNameID, cComponent, cFile )

   LOCAL cResult := ""
   LOCAL idx, tmp

   HB_SYMBOL_UNUSED( cComponent )

   IF HB_ISNULL( cFile )
      cFile := "null"
   ENDIF

   IF Right( cFile, 1 ) == "*" .AND. Len( cFile ) > 1
      cFile := hb_StrShrink( cFile )
   ENDIF

   cFile := hb_StrReplace( cFile, { ;
      "%" => "pct", ;
      "#" => "-", ;
      "<" => "lt", ;
      ">" => "gt", ;
      "=" => "eq", ;
      "*" => "as", ;
      "$" => "do", ;
      "?" => "qe", ;
      "|" => "vl", ;
      " " => "-" } )

   FOR idx := 1 TO Len( cFile )
      tmp := SubStr( cFile, idx, 1 )
      IF hb_asciiIsDigit( tmp ) .OR. hb_asciiIsAlpha( tmp ) .OR. tmp $ "_-"
         cResult += tmp
      ENDIF
   NEXT

   cResult := hb_asciiLower( cResult )

   IF cResult $ hNameID
      idx := 0
      DO WHILE ( tmp := cResult + "_" + hb_ntos( ++idx ) ) $ hNameID
      ENDDO
      cResult := tmp
   ENDIF

   hNameID[ cResult ] := NIL

   RETURN cResult

STATIC FUNCTION EntryNew( cTemplate )

   LOCAL hE := { => }

   hb_HCaseMatch( hE, .F. )
   hb_HEval( sc_hFields, {| k | hE[ k ] := "" } )

   hE[ "TEMPLATE" ] := cTemplate
   hE[ "_group" ] := sc_hTemplates[ cTemplate ]

   RETURN hE

FUNCTION IsField( hE, cField )

   LOCAL idx := hb_HPos( sc_hFields, cField )

   RETURN idx > 0 .AND. hE[ "_group" ][ idx ] != 0

STATIC FUNCTION IsConstraint( hE, cField, cSection )

   IF hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], hb_bitOr( TPL_REQUIRED, TPL_OPTIONAL ) ) == 0
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
   RETURN hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], TPL_PREFORMATTED ) != 0

STATIC FUNCTION IsRequired( hE, cField )
   RETURN hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], TPL_REQUIRED ) != 0

FUNCTION IsOutput( hE, cField )
   RETURN hb_bitAnd( hE[ "_group" ][ hb_HPos( sc_hFields, cField ) ], TPL_OUTPUT ) != 0

FUNCTION FieldIDList()
   RETURN hb_HKeys( sc_hFields )

FUNCTION FieldCaption( cName, lPlural )

   LOCAL xResult := sc_hFields[ cName ]

   RETURN Eval( iif( HB_ISARRAY( xResult ), iif( lPlural, xResult[ 2 ], xResult[ 1 ] ), xResult ) )

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

   LoadPO()

   hb_HAutoAdd( s_hHBXStat, HB_HAUTOADD_ALWAYS )
   hb_HDefault( s_hHBXStat, 0 )

   hb_HCaseMatch( hSubCategories, .F. )

   hb_HCaseMatch( s_hHBX, .F. )
   hb_HCaseMatch( s_hDoc, .F. )
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
      "Runtime errors"            => { "" => } }

   hb_HCaseMatch( sc_hConstraint[ "categories" ], .F. )

   sc_hConstraint[ "compliance" ] := { ;
      "C"       => BI_( "CA-Cl*pper v5.x compatible" ), ;
      "C52S"    => BI_( "CA-Cl*pper v5.x compatible in builds with HB_CLP_STRICT option enabled" ), ;
      "C52U"    => BI_( "Undocumented CA-Cl*pper v5.x function available in builds with HB_CLP_UNDOC option enabled (default)" ), ;
      "C53"     => BI_( "CA-Cl*pper v5.3 function available in builds with HB_COMPAT_C53 option enabled (default)" ), ;
      "H"       => BI_( "Harbour specific" ), ;
      "NA"      => BI_( "Not applicable" ) }

   sc_hConstraint[ "platforms" ] := { ;
      "All"     => BI_( "Available on all platforms" ), ;
      "Unix"    => BI_( "Available on Unix platform(s)" ), ;
      "Linux"   => BI_( "Available on Linux" ), ;
      "Windows" => BI_( "Available on Windows" ), ;
      "OS2"     => BI_( "Available on OS/2" ), ;
      "DOS"     => BI_( "Available on MS-DOS" ) }

   sc_hConstraint[ "status" ] := { ;
      "R" => BI_( "Ready" ), ;
      "S" => BI_( "Started" ), ;
      "N" => BI_( "Not started" ) }

   sc_hFields := { ;
      "TEMPLATE"      => BI_( "Template" ), ;
      "NAME"          => BI_( "" ), ;
      "ONELINER"      => BI_( "" ), ;
      "CATEGORY"      => BI_( "Category" ), ;
      "SUBCATEGORY"   => BI_( "Subcategory" ), ;
      "SYNTAX"        => BI_( "Syntax" ), ;
      "ARGUMENTS"     => BI_( "Arguments" ), ;
      "RETURNS"       => BI_( "Returns" ), ;
      "DESCRIPTION"   => BI_( "Description" ), ;
      "NOTES"         => BI_( "Notes" ), ;
      "DATALINK"      => BI_( "Data link" ), ;
      "DATANOLINK"    => BI_( "Data no link" ), ;
      "METHODSLINK"   => BI_( "Methods link" ), ;
      "METHODSNOLINK" => BI_( "Methods no link" ), ;
      "EXAMPLES"      => BI_( "Examples" ), ;
      "TESTS"         => BI_( "Tests" ), ;
      "STATUS"        => BI_( "Status" ), ;       /* sc_hConstraint[ "status" ] is the constraint list */
      "COMPLIANCE"    => BI_( "Compliance" ), ;   /* sc_hConstraint[ "compliance" ] is the constraint list */
      "PLATFORMS"     => BI_( "Platforms" ), ;    /* sc_hConstraint[ "platforms" ] is the constraint list */
      "FILES"         => { BI_( "File" ), BI_( "Files" ) }, ;
      "TAGS"          => { BI_( "Tag" ), BI_( "Tags" ) }, ;
      "SEEALSO"       => BI_( "See also" ) }

   hb_HCaseMatch( sc_hFields, .F. )

   #define _T TPL_TEMPLATE
   #define _R TPL_REQUIRED
   #define _O TPL_OPTIONAL
   #define _P TPL_PREFORMATTED
   #define _U TPL_OUTPUT

   /* The columns of this array correspond to the elements of sc_hFields */
   sc_hTemplates := { ;
      "Template"       => { _T,  0+_U,  0+_U,  0, _O,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0+_U }, ;
      "Document"       => { _T, _R+_U, _O+_U, _R, _O,  0+_U,  0+_U,  0+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U,  0   +_U,  0   +_U,  0+_U,  0+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "Function"       => { _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "C Function"     => { _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "Procedure"      => { _T, _R+_U, _O+_U, _R, _R, _O+_U, _O+_U,     0, _O+_U, _O+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "Command"        => { _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U,  0+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "Class"          => { _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U, _R+_U, _O+_U, _O+_U, _O+_U, _O+_U, _P+_O+_U, _P+_O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U, _O+_U }, ;
      "Class method"   => { _T, _R+_U, _O+_U, _R, _R, _R+_U, _R+_U, _R+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U }, ;
      "Class data"     => { _T, _R+_U, _O+_U, _R, _R, _R+_U,  0+_U,  0+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U,  0+_U,  0+_U,  0+_U, _O+_U, _O+_U }, ;
      "Runtime error"  => { _T, _R+_U, _O+_U, _R,  0,  0+_U,  0+_U,  0+_U, _R+_U, _R+_U,  0+_U,  0+_U,  0+_U,  0+_U, _P+_O+_U,  0   +_U,  0+_U, _O+_U,  0+_U,  0+_U, _O+_U, _O+_U } }

   RETURN

STATIC PROCEDURE LoadPO()

   /* Command to store files in hash array */
   #xcommand ADD PO TO <hash> FILE <(cFile)> => ;
             #pragma __streaminclude <(cFile)> | <hash>\[ hb_FNameNameExt( <(cFile)> ) \] := %s

   ADD PO TO s_hPO FILE "po/hbdoc.pt_BR.po"

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
            IF hE[ "_group" ][ idx ] != 0
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
      ShowSubHelp( ExpandAbbrevs( "", "COMPLIANCE", item:__enumKey() ), 1, 6, item:__enumIndex() )
      ShowSubHelp( "", 1, 0 )
   NEXT

   RETURN

STATIC PROCEDURE ShowPlatformsHelp()

   LOCAL item

   FOR EACH item IN sc_hConstraint[ "platforms" ]
      ShowSubHelp( item:__enumKey(), 1, 0, item:__enumIndex() )
      ShowSubHelp( ExpandAbbrevs( "", "PLATFORMS", item:__enumKey() ), 1, 6, item:__enumIndex() )
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

   LOCAL cID := hb_FNameName( cName )

   IF ! HB_ISNULL( cFile := hb_MemoRead( cFileName ) ) .AND. ;
      ! Empty( pRegex := hb_regexComp( "^DYNAMIC ([a-zA-Z0-9_]*)$", .T., .T. ) )

      FOR EACH tmp IN hb_regexAll( pRegex, StrTran( cFile, Chr( 13 ) ),,,,, .T. )
         hAll[ tmp[ 2 ] ] := cName
         ++s_hHBXStat[ cID ]
      NEXT
   ENDIF

   RETURN aDynamic

#if defined( __HBSCRIPT__HBSHELL )
SET PROCEDURE TO "_genbase.prg"
SET PROCEDURE TO "_gentxt.prg"
SET PROCEDURE TO "_genhtml.prg"
SET PROCEDURE TO "_genxml.prg"
#endif
