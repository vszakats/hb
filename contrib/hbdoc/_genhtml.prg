/*
 * Document generator - HTML output
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

#include "hbclass.ch"
#include "hbver.ch"

#define I_( x )  hb_UTF8ToStr( hb_i18n_gettext( x /*, _SELF_NAME_ */ ) )

#define EXTENSION   ".html"
#define STYLEFILE   "hbdoc.css"
#define CODECLASS   "language-c"
#define CODEINLINE  "<code>"

STATIC s_tDate
STATIC s_cRevision

CREATE CLASS GenerateHTML INHERIT TPLGenerate

   HIDDEN:

   METHOD RecreateStyleDocument( cStyleFile )
   METHOD OpenTagInline( cText, ... )
   METHOD OpenTag( cText, ... )
   METHOD TaggedInline( cText, cTag, ... )
   METHOD Tagged( cText, cTag, ... )
   METHOD CloseTagInline( cText )
   METHOD CloseTag( cText )
   METHOD AppendInline( cText, cFormat, lCode, cField, cID )
   METHOD Append( cText, cFormat, lCode, cField, cID )
   METHOD Space() INLINE ::cFile += ", ", Self
   METHOD Spacer() INLINE ::cFile += hb_eol(), Self
   METHOD NewLine() INLINE ::cFile += "<br>" + hb_eol(), Self
   METHOD NewFile()

   CLASS VAR lCreateStyleDocument AS LOGICAL INIT .T.
   VAR TargetFilename AS STRING INIT ""

   VAR hNameIDM
   VAR hHBX

   EXPORTED:

   METHOD NewIndex( cDir, cFilename, cTitle, cLang, hComponents )
   METHOD NewDocument( cDir, cFilename, cTitle, cLang, hComponents )
   METHOD AddEntry( hEntry )
   METHOD AddReference( hEntry, cReference, cSubReference )
   METHOD BeginSection( cSection, cFilename, cID )
   METHOD EndSection()
   METHOD Generate()
   METHOD SubCategory( cCategory, cID )
   METHOD BeginTOC()
   METHOD EndTOC()
   METHOD BeginTOCItem( cName, cID )
   METHOD EndTOCItem() INLINE ::cFile += "</ul>" + hb_eol(), Self
   METHOD BeginContent() INLINE ::OpenTag( "main" ), Self
   METHOD EndContent() INLINE ::Spacer():CloseTag( "main" ), Self
   METHOD BeginIndex() INLINE ::OpenTag( "aside" ), Self
   METHOD EndIndex() INLINE ::CloseTag( "aside" ):Spacer(), Self
   METHOD AddIndexItem( cName, cID, lRawID )

   METHOD WriteEntry( cField, cContent, lPreformatted, cID ) HIDDEN

   VAR nIndent INIT 0

ENDCLASS

METHOD NewFile() CLASS GenerateHTML

   LOCAL tmp, tmp1
   LOCAL hDoc
   LOCAL cBaseTitle

   IF s_tDate == NIL
      IF hbdoc_reproducible()
         s_tDate := hb_Version( HB_VERSION_BUILD_TIMESTAMP_UTC )
         s_cRevision := hb_Version( HB_VERSION_ID )
      ELSE
         s_tDate := hb_DateTime() - ( hb_UTCOffset() / 86400 )
         s_cRevision := GitRev()
      ENDIF
   ENDIF

   ::hNameIDM := hbdoc_NameIDM()
   ::hHBX := hbdoc_HBX()

   ::cFile += "<!DOCTYPE html>" + hb_eol()

   ::OpenTag( "html", "lang", StrTran( ::cLang, "_", "-" ) )
   ::Spacer()

   ::OpenTag( "meta", "charset", "utf-8" )
   ::OpenTag( "meta", "name", "referrer", "content", "origin" )
   ::OpenTag( "meta", "name", "viewport", "content", "initial-scale=1" )
   ::Spacer()

   ::OpenTag( "meta", "name", "generator", "content", "hbdoc" )
   ::OpenTag( "meta", "name", "keywords", "content", ;
      "Harbour, Clipper, xBase, database, Free Software, GPL, compiler, cross-platform, 32-bit, 64-bit" )
   ::Spacer()

   IF ::lCreateStyleDocument
      ::lCreateStyleDocument := .F.
      ::RecreateStyleDocument( STYLEFILE )
   ENDIF

   cBaseTitle := hb_StrFormat( Eval( ::bBaseTitle ), iif( hb_LeftEq( ::cFilename, "cl" ), "Clipper", "Harbour" ) )

   ::Append( hb_StrFormat( I_( "%1$s · %2$s" ), cBaseTitle, ::cTitle ), "title" )
   ::Spacer()

#if 0
   ::OpenTag( "link", ;
      "rel", "stylesheet", ;
      "crossorigin", "anonymous", ;
      "referrerpolicy", "no-referrer", ;
      "href", "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" )
#endif
#if 0
   /* https://sourcefoundry.org/hack/ */
   ::OpenTag( "link", ;
      "rel", "stylesheet", ;
      "crossorigin", "anonymous", ;
      "referrerpolicy", "no-referrer", ;
      "href", "https://cdn.jsdelivr.net/font-hack/2.020/css/hack-extended.min.css" )
#endif

   ::OpenTag( "link", ;
      "rel", "stylesheet", ;
      "crossorigin", "anonymous", ;
      "referrerpolicy", "no-referrer", ;
      "href", "https://cdnjs.cloudflare.com/ajax/libs/prism/1.5.1/themes/prism-okaidia.min.css" )

   ::OpenTag( "link", ;
      "rel", "stylesheet", ;
      "href", iif( ::cLang == "en", "", "../" ) + STYLEFILE )

   ::Spacer()

   ::cFile += hb_MemoRead( "hbdoc_head.html" )

   ::OpenTag( "body" )
   ::Spacer()

   ::OpenTag( "header" )
   ::OpenTag( "div" )

   ::OpenTagInline( "div" )
   ::OpenTagInline( "a", "href", "index.html" )
   ::cFile += hb_MemoRead( hbdoc_dir_in() + hb_DirSepToOS( "docs/images/" + "harbour-nofill.svg" ) )
   ::AppendInline( cBaseTitle )
   ::CloseTagInline( "a" )
   ::CloseTag( "div" )

   IF HB_ISHASH( ::hComponents ) .OR. ;
      Len( hbdoc_LangList() ) > 1

      ::OpenTag( "div" )
      ::OpenTag( "nav", "class", "menu" )

      IF HB_ISHASH( ::hComponents )
         ::OpenTag( "nav", "class", "dropdown" )

         ::OpenTagInline( "span", "class", "dropbtn", "onclick" /* hack to make menu work on Safari iOS */, "" )
         ::AppendInline( ::cTitle )
         ::CloseTag( "span" )

         ::OpenTag( "nav", "class", "dropdown-content" )
#if 0
         ::OpenTagInline( "a", "href", "index.html" )
         ::AppendInline( "Index" )
         ::CloseTag( "a" )
         ::OpenTag( "hr" )
#endif
         tmp1 := ""
         FOR EACH tmp IN ::hComponents
            IF hb_LeftEq( tmp1, "cl" ) .AND. ;
               ! hb_LeftEq( tmp:__enumKey(), "cl" )
               ::OpenTag( "hr" )
            ENDIF
            ::OpenTagInline( "a", "href", tmp:__enumKey() + ".html" )
            ::AppendInline( tmp[ "nameshort" ] )
            ::CloseTag( "a" )
            /* This assumes that this item is first on the list */
            IF ( tmp1 := tmp:__enumKey() ) == "harbour"
               ::OpenTag( "hr" )
            ENDIF
         NEXT
         ::CloseTag( "nav" )
         ::CloseTag( "nav" )
      ENDIF

      ::OpenTag( "nav", "class", "dropdown lang" )
      ::OpenTagInline( "span", "class", "dropbtn flag" )
      ::OpenTag( "img", "src", flag_for_lang( ::cLang ), "width", "18", "alt", hb_StrFormat( I_( "%1$s flag" ), ::cLang ) )
      ::CloseTagInline( "span" )

      IF Len( hbdoc_LangList() ) > 1
         ::OpenTag( "nav", "class", "dropdown-content lang" )
         FOR EACH tmp IN ASort( hb_HKeys( hDoc := hbdoc_LangList() ) )
            ::OpenTagInline( "a", "href", GetLangDir( ::cLang, tmp ) + ;
               iif( ::cLang == tmp .OR. tmp == "en" .OR. ( tmp $ hDoc .AND. ::cFileName $ hDoc[ tmp ][ "tree" ] ), ;
                  ::cFilename, ;
                  "index" ) + ".html" )
            ::OpenTagInline( "img", "src", flag_for_lang( tmp ), "width", "24", "alt", hb_StrFormat( "%1$s flag", tmp ) )
            ::CloseTag( "a" )
         NEXT
         ::CloseTag( "nav" )
      ENDIF
      ::CloseTag( "nav" )

      ::CloseTag( "nav" )
      ::CloseTag( "div" )

   ENDIF

   ::CloseTag( "div" )
   ::CloseTag( "header" )
   ::Spacer()

   RETURN Self

STATIC FUNCTION GetLangDir( cCurLang, cTargetLang )

   DO CASE
   CASE cCurLang == cTargetLang
      RETURN ""
   CASE cCurLang == "en"
      RETURN Lower( StrTran( cTargetLang, "_", "-" ) ) + "/"
   ENDCASE

   RETURN ".." + "/"

STATIC FUNCTION flag_for_lang( cLang )

   LOCAL cSrc := ""

   SWITCH Lower( cLang )
   CASE "en"    ; cSrc := "flag-gb.svg" ; EXIT
   CASE "pt_br" ; cSrc := "flag-br.svg" ; EXIT
   ENDSWITCH

   IF ! HB_ISNULL( cSrc )
      cSrc := "data:image/svg+xml;base64," + hb_base64Encode( hb_MemoRead( hbdoc_dir_in() + hb_DirSepToOS( "docs/images/" + cSrc ) ) )
   ENDIF

   RETURN cSrc

STATIC FUNCTION GitRev()

   LOCAL cStdOut := ""

   hb_processRun( "git rev-parse --short HEAD",, @cStdOut )

   RETURN hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) )

METHOD Generate() CLASS GenerateHTML

   ::Spacer()
   ::OpenTag( "footer" )

   ::Append( hb_StrFormat( I_( "Generated by hbdoc on %1$s" ), hb_TToC( s_tDate, "yyyy-mm-dd", "hh:mm" ) + " " + "UTC" ), "div" )

   ::OpenTagInline( "div" )
   ::AppendInline( I_( "Based on revision" ) + " " )
   ::OpenTagInline( "a", "href", hb_Version( HB_VERSION_URL_BASE ) + "tree/" + s_cRevision )
   ::AppendInline( s_cRevision )
   ::CloseTagInline( "a" )
   ::CloseTag( "div" )

   ::OpenTagInline( "div" )
   ::AppendInline( hb_StrFormat( I_( "Content processing, layout & design © %1$04d" ), Year( Date() ) ) + " " )
   ::OpenTagInline( "a", "href", "https://vszakats.net/harbour" )
   ::AppendInline( "vszakats" )
   ::CloseTagInline( "a" )
   ::CloseTag( "div" )

   ::CloseTag( "footer" )

   ::OpenTagInline( "script", ;
      "crossorigin", "anonymous", ;
      "src", "https://cdnjs.cloudflare.com/ajax/libs/prism/1.5.1/prism.min.js" ):CloseTag( "script" )

   ::OpenTagInline( "script", ;
      "crossorigin", "anonymous", ;
      "src", "https://cdnjs.cloudflare.com/ajax/libs/prism/1.5.1/components/prism-c.min.js" ):CloseTag( "script" )

   ::super:Generate()

   RETURN Self

METHOD NewDocument( cDir, cFilename, cTitle, cLang, hComponents ) CLASS GenerateHTML

   ::super:NewDocument( cDir, cFilename, cTitle, EXTENSION, cLang, hComponents )
   ::NewFile()

   RETURN Self

METHOD NewIndex( cDir, cFilename, cTitle, cLang, hComponents ) CLASS GenerateHTML

   ::super:NewIndex( cDir, cFilename, cTitle, EXTENSION, cLang, hComponents )
   ::NewFile()

   RETURN Self

METHOD BeginTOC() CLASS GenerateHTML

   ::Spacer()
   ::OpenTag( "section", "id", "toc" )
   ::OpenTag( "ul" )

   RETURN Self

METHOD EndTOC() CLASS GenerateHTML

   ::CloseTag( "ul" )
   ::CloseTag( "section" )

   RETURN Self

METHOD BeginTOCItem( cName, cID ) CLASS GenerateHTML

   ::OpenTagInline( "li" )
   ::OpenTagInline( "a", "href", "#" + SymbolToHTMLID( cID ) )  // OK
   ::AppendInline( cName )
   ::CloseTag( "a" )
   ::OpenTag( "ul" )

   RETURN Self

METHOD AddIndexItem( cName, cID, lRawID ) CLASS GenerateHTML

   IF lRawID
      cID := SymbolToHTMLID( cID )  // OK
   ENDIF

   ::OpenTagInline( "a", "href", "#" + cID, "title", cName )
   IF "(" $ cName .OR. "#" $ cName .OR. Upper( cName ) == cName  // guess if it's code
      ::OpenTagInline( "code" )
      ::AppendInline( cName,,, "NAME" )
      ::CloseTagInline( "code" )
   ELSE
      ::AppendInline( cName,,, "NAME" )
   ENDIF
   ::CloseTag( "a" )

   RETURN Self

METHOD BeginSection( cSection, cFilename, cID ) CLASS GenerateHTML

   LOCAL cH

   cID := SymbolToHTMLID( hb_defaultValue( cID, cSection ) )  // OK

   IF ::IsIndex()
      cH := "h" + hb_ntos( ::nDepth + 1 )
      ::Spacer()
      ::OpenTag( "section", "id", cID, "class", "d-x d-id" )
      IF ! HB_ISSTRING( cFileName ) .OR. cFilename == ::cFilename
         ::OpenTagInline( cH )
         ::AppendInline( cSection )
         ::CloseTag( cH )
      ELSE
         ::OpenTagInline( cH )
         ::OpenTagInline( "a", "href", cFilename + ::cExtension + "#" + cID )
         ::AppendInline( cSection )
         ::CloseTagInline( "a" ):CloseTag( cH )
      ENDIF
      ::OpenTag( "div", "class", "d-y" )
   ELSE
      ::OpenTagInline( "div", "id", cID, "class", "d-id" )
      ::AppendInline( cSection, "h" + hb_ntos( ::nDepth + 1 ) )
      ::CloseTag( "div" )
   ENDIF

   IF HB_ISSTRING( cFileName )
      ::TargetFilename := cFilename
   ENDIF

   ++::nDepth

   RETURN Self

METHOD EndSection() CLASS GenerateHTML

   --::nDepth

   ::CloseTag( "div" )
   ::CloseTag( "section" )

   RETURN Self

METHOD SubCategory( cCategory, cID )

   IF HB_ISSTRING( cCategory ) .AND. ! HB_ISNULL( cCategory )
      IF Empty( cID )
         ::TaggedInline( cCategory, "h3", "class", "d-sc" )
      ELSE
         ::TaggedInline( cCategory, "h3", "class", "d-sc d-id", "id", SymbolToHTMLID( cID ) )
      ENDIF
   ELSE
      ::OpenTagInline( "hr" )
   ENDIF

   RETURN Self

METHOD AddReference( hEntry, cReference, cSubReference ) CLASS GenerateHTML

   DO CASE
   CASE HB_ISHASH( hEntry )
      ::OpenTagInline( "div" )
      ::OpenTagInline( "a", "href", ::TargetFilename + ::cExtension + "#" + hEntry[ "_id" ] )
      ::AppendInline( hEntry[ "NAME" ],,, "NAME" )
      ::CloseTagInline( "a" )
      // ::OpenTagInline( "div", "class", "d-r" )
      IF ! Empty( hEntry[ "ONELINER" ] )
         ::AppendInline( hb_UChar( 160 ) + hb_UChar( 160 ) + hb_UChar( 160 ) + hEntry[ "ONELINER" ] )
      ENDIF
      // ::CloseTagInline( "div" )
      ::CloseTagInline( "div" )
   CASE HB_ISSTRING( cSubReference )
      ::OpenTagInline( "div" )
      ::OpenTagInline( "a", "href", cReference + "#" + SymbolToHTMLID( cSubReference ) )  // OK
      ::AppendInline( hEntry )
      ::CloseTagInline( "a" )
      ::CloseTagInline( "div" )
   OTHERWISE
      ::OpenTagInline( "a", "href", cReference )
      ::AppendInline( hEntry )
      ::CloseTagInline( "a" )
   ENDCASE

   ::cFile += hb_eol()

   RETURN Self

METHOD AddEntry( hEntry ) CLASS GenerateHTML

   LOCAL item
   LOCAL cEntry
   LOCAL tmp

   ::Spacer()
   ::OpenTag( "section" )

   FOR EACH item IN FieldIDList()
      IF item == "NAME"  // Mandatory section
         cEntry := hEntry[ "NAME" ]
         ::OpenTagInline( "h4" )
         ::OpenTagInline( "a", "href", "#" + hEntry[ "_id" ], "class", "d-id", "id", hEntry[ "_id" ], "title", "∞" )
         IF "(" $ cEntry .OR. "#" $ cEntry .OR. Upper( cEntry ) == cEntry  // guess if it's code
            ::OpenTagInline( "code" ):AppendInline( cEntry,,, item ):CloseTagInline( "code" )
            ::CloseTagInline( "a" )
            /* Link to original source code if it could be automatically found based
               on doc source filename */
            IF ! HB_ISNULL( tmp := SourceURL( ::cFilename, s_cRevision, hEntry[ "_sourcefile" ] ) )
               ::OpenTagInline( "a", "href", tmp, "class", "d-so" )
               ::AppendInline( I_( "Source code" ) )
               ::CloseTagInline( "a" )
            ENDIF
         ELSE
            ::AppendInline( cEntry,,, item )
            ::CloseTagInline( "a" )
         ENDIF

         ::OpenTagInline( "span", "class", "d-eb" )

         ::OpenTagInline( "a", "href", "#", "title", I_( "Top" ), "class", "d-ebi" )
         ::AppendInline( "⌃" )
         ::CloseTagInline( "a" )

         ::AppendInline( hb_UChar( 160 ) + "|" + hb_UChar( 160 ) )
         ::OpenTagInline( "a", "href", "index.html", "title", I_( "Index" ), "class", "d-ebi" )
         ::AppendInline( "☰" )
         ::CloseTagInline( "a" )

         IF ! hb_LeftEq( ::cFilename, "cl" )
            ::AppendInline( hb_UChar( 160 ) + "|" + hb_UChar( 160 ) )
            ::OpenTagInline( "a", "href", hb_Version( HB_VERSION_URL_BASE ) + "edit/master/" + hEntry[ "_sourcefile" ] )
            ::AppendInline( I_( "Improve this doc" ) )
            ::CloseTagInline( "a" )
         ENDIF

         ::CloseTagInline( "span" )

         ::CloseTag( "h4" )
      ELSEIF IsField( hEntry, item ) .AND. IsOutput( hEntry, item ) .AND. ! HB_ISNULL( hEntry[ item ] )
         ::WriteEntry( item, hEntry[ item ], IsPreformatted( hEntry, item ), hEntry[ "_id" ] )
      ENDIF
   NEXT

   ::CloseTag( "section" )

   RETURN Self

/* Try to locate original source code based on the source filename of the doc. */
STATIC FUNCTION SourceURL( cComponent, cRevision, cFileName )

   LOCAL cExt, cDir, tmp

   cFileName := hb_FNameName( cFileName )

   FOR EACH cExt IN { ".c", ".prg" }

      FOR EACH cDir IN iif( cComponent == "harbour", ;
         { "src/rtl", ;
           "src/vm", ;
           "src/rdd" }, ;
         { "contrib/" + cComponent } )

         IF hb_FileExists( hbdoc_dir_in() + ( tmp := hb_DirSepToOS( cDir + "/" ) + cFileName + cExt ) )
            RETURN hb_Version( HB_VERSION_URL_BASE ) + "blob/" + cRevision + "/" + tmp
         ENDIF
      NEXT
   NEXT

   RETURN ""

METHOD PROCEDURE WriteEntry( cField, cContent, lPreformatted, cID ) CLASS GenerateHTML

   STATIC s_class := { ;
      "NAME"     => "d-na", ;
      "ONELINER" => "d-ol", ;
      "SYNTAX"   => "d-sy", ;
      "EXAMPLES" => "d-ex", ;
      "TESTS"    => "d-te" }

   STATIC s_cAddP := "DESCRIPTION|"

   LOCAL cTagClass
   LOCAL cCaption
   LOCAL lFirst
   LOCAL tmp, tmp1
   LOCAL cLine
   LOCAL lCode, lTable, lTablePrev, cHeaderClass
   LOCAL cFile, cAnchor, cTitle, cLangOK
   LOCAL cNameCanon
   LOCAL aSEEALSO

   IF ! Empty( cContent )

      cTagClass := hb_HGetDef( s_class, cField, "d-it" )

      IF ! HB_ISNULL( cCaption := FieldCaption( cField, ;
         ( cField == "TAGS" .AND. "," $ cContent ) .OR. ;
         ( cField == "FILES" .AND. Chr( 10 ) $ cContent ) ) )
         ::Tagged( cCaption, "div", "class", "d-d" )
      ENDIF

      DO CASE
      CASE lPreformatted  /* EXAMPLES, TESTS */

         ::OpenTagInline( "pre", "class", cTagClass ):OpenTagInline( "code", "class", CODECLASS )
#if 1
         /* logic to remove PROCEDURE Main()/RETURN enclosure
            to fit more interesting information on the screen.
            TODO: better do this in the doc sources. */

         IF hb_LeftEqI( cContent, "PROCEDURE Main()" ) .OR. ;
            Lower( cContent ) == "procedure main" .OR. ;
            Lower( cContent ) == "proc main"

            tmp1 := ""
            FOR EACH tmp IN hb_ATokens( cContent, .T. )
               DO CASE
               CASE tmp:__enumIndex() == 1
                  /* do nothing */
               CASE tmp:__enumIndex() == 2
                  IF ! HB_ISNULL( tmp )
                     IF ! Empty( Left( tmp, 3 ) )
                        tmp1 := cContent
                        EXIT
                     ENDIF
                     tmp1 += SubStr( tmp, 4 ) + Chr( 10 )
                  ENDIF
               CASE tmp:__enumIsLast()
                  IF AllTrim( tmp ) == "RETURN"
                     IF Right( tmp, Len( hb_eol() ) ) == hb_eol()
                        tmp1 := hb_StrShrink( tmp1, Len( hb_eol() ) )
                     ENDIF
                  ELSE
                     IF ! Empty( Left( tmp, 3 ) )
                        tmp1 := cContent
                        EXIT
                     ENDIF
                     tmp1 += SubStr( tmp, 4 )
                  ENDIF
               OTHERWISE
                  IF ! Empty( Left( tmp, 3 ) )
                     tmp1 := cContent
                     EXIT
                  ENDIF
                  tmp1 += SubStr( tmp, 4 ) + Chr( 10 )
               ENDCASE
            NEXT
            cContent := tmp1
         ENDIF
#endif
         ::Append( cContent,, .T., cField )
         ::CloseTagInline( "code" ):CloseTag( "pre" )

      CASE cField == "SEEALSO"

         ::OpenTagInline( "div", "class", cTagClass )
         lFirst := .T.

         FOR EACH tmp IN aSEEALSO := hb_ATokens( cContent, "," )
            tmp := AllTrim( tmp )
         NEXT

         FOR EACH tmp IN ASort( aSEEALSO )
            IF ! HB_ISNULL( tmp )
               IF lFirst
                  lFirst := .F.
               ELSE
                  ::Space()
               ENDIF
               cNameCanon := NameCanon( tmp )
               IF cNameCanon $ ::hNameIDM[ cLangOK := ::cLang ] .OR. ;
                  iif( ::cLang == "en", .F., cNameCanon $ ::hNameIDM[ cLangOK := "en" ] )

                  cFile := ""
                  cAnchor := cTitle := NIL
                  /* search order to resolve 'see also' links: self, ... */
                  FOR EACH tmp1 IN { ::cFilename, "harbour", "clc53", "hbct", "clct3", hb_HKeyAt( ::hNameIDM[ cLangOK ][ cNameCanon ], 1 ) }
                     IF tmp1 $ ::hNameIDM[ cLangOK ][ cNameCanon ]
                        cAnchor := ::hNameIDM[ cLangOK ][ cNameCanon ][ tmp1 ][ "id" ]
                        IF ! tmp1:__enumIsFirst()
                           cFile := GetLangDir( ::cLang, cLangOK ) + tmp1 + ".html"
                           cTitle := iif( cLangOK == ::cLang, tmp1, hb_StrFormat( I_( "%1$s (%2$s)" ), tmp1, cLangOK ) )
                        ENDIF
                        EXIT
                     ENDIF
                  NEXT
                  IF Len( ::hNameIDM[ cLangOK ][ cNameCanon ] ) > 1
                     ::OpenTagInline( "nav", "class", "dropdown" )
                  ENDIF
                  ::OpenTagInline( "code" )
                  IF cTitle != NIL
                     ::OpenTagInline( "a", "href", cFile + "#" + cAnchor, "title", cTitle )
                  ELSE
                     ::OpenTagInline( "a", "href", cFile + "#" + cAnchor )
                  ENDIF
                  ::AppendInline( tmp,,, "NAME" ):CloseTagInline( "a" ):CloseTagInline( "code" )
                  IF Len( ::hNameIDM[ cLangOK ][ cNameCanon ] ) > 1
                     ::OpenTagInline( "nav", "class", "dropdown-content" + " " + "d-dd" )
                     FOR EACH tmp1 IN ASort( hb_HKeys( ::hNameIDM[ cLangOK ][ cNameCanon ] ) )
                        GetComponentInfo( tmp1,, @cCaption )
                        ::OpenTagInline( "a", "href", tmp1 + ".html" + "#" + ::hNameIDM[ cLangOK ][ cNameCanon ][ tmp1 ][ "id" ] )
                        ::AppendInline( cCaption ):CloseTagInline( "a" )
                     NEXT
                     ::CloseTagInline( "nav" )
                     ::CloseTagInline( "nav" )
                  ENDIF
               ELSE
//                ? "broken 'see also' link:", ::cFilename, "|" + cNameCanon + "|"
                  ::OpenTagInline( "code" ):AppendInline( tmp,,, "NAME" ):CloseTagInline( "code" )
               ENDIF
            ENDIF
         NEXT
         ::CloseTag( "div" )

      CASE cField == "SYNTAX"

         IF Chr( 10 ) $ cContent
            ::OpenTag( "div", "class", cTagClass + " " + "d-sym" )
            ::OpenTagInline( "pre" ):OpenTagInline( "code" )
            ::Append( StrSYNTAX( cContent ),, .T., cField )
            ::CloseTagInline( "code" ):CloseTag( "pre" )
         ELSE
            ::OpenTagInline( "div", "class", cTagClass )
            ::OpenTagInline( "pre" ):OpenTagInline( "code" )
            ::AppendInline( StrSYNTAX( cContent ),, .T., cField )
            ::CloseTagInline( "code" ):CloseTagInline( "pre" )
         ENDIF
         ::CloseTag( "div" )

      CASE ! Chr( 10 ) $ cContent

         ::OpenTagInline( "div", "class", cTagClass )
         ::AppendInline( cContent,, .F., cField, cID )
         ::CloseTag( "div" )

      OTHERWISE

         ::OpenTag( "div", "class", cTagClass )
         ::nIndent++

         lTable := .F.

         DO WHILE ! HB_ISNULL( cContent )

            lCode := .F.
            lTablePrev := lTable

            tmp1 := ""
            DO WHILE ! HB_ISNULL( cContent )

               cLine := Parse( @cContent, hb_eol() )

               DO CASE
               CASE hb_LeftEq( LTrim( cLine ), "```" )
                  IF lCode
                     EXIT
                  ELSE
                     lCode := .T.
                  ENDIF
               CASE cLine == "<fixed>"
                  lCode := .T.
               CASE cLine == "</fixed>"
                  IF lCode
                     EXIT
                  ENDIF
               CASE hb_LeftEq( cLine, "<table" )
                  lTable := .T.
                  SWITCH cLine
                  CASE "<table-noheader>"     ; cHeaderClass := "d-t0" ; EXIT
                  CASE "<table-doubleheader>" ; cHeaderClass := "d-t1 d-t2" ; EXIT
                  OTHERWISE                   ; cHeaderClass := "d-t1"
                  ENDSWITCH
               CASE cLine == "</table>"
                  lTable := .F.
               OTHERWISE
                  tmp1 += cLine + hb_eol()
                  IF ! lCode
                     EXIT
                  ENDIF
               ENDCASE
            ENDDO

            IF lTable != lTablePrev
               IF lTable
                  ::OpenTag( "div", "class", "d-t" + iif( HB_ISNULL( cHeaderClass ), "", " " + cHeaderClass ) )
               ELSE
                  ::CloseTag( "div" )
               ENDIF
            ENDIF

            DO CASE
            CASE lCode
               ::OpenTagInline( "pre" ):OpenTagInline( "code", "class", CODECLASS )
               ::Append( tmp1,, .T., cField )
            CASE lTable
               ::OpenTagInline( "div" )
               ::AppendInline( iif( lTable, StrTran( tmp1, " ", hb_UChar( 160 ) ), tmp1 ),, .T., cField )
            OTHERWISE
               ::OpenTagInline( "div" )
               IF cField $ s_cAddP
                  ::OpenTagInline( "p" )
               ENDIF
               ::AppendInline( iif( lTable, StrTran( tmp1, " ", hb_UChar( 160 ) ), tmp1 ),, .F., cField, cID )
            ENDCASE
            IF lCode
               ::CloseTagInline( "code" ):CloseTag( "pre" )
            ELSE
               ::CloseTag( "div" )
            ENDIF
         ENDDO

         IF lTable
            ::CloseTag( "div" )
         ENDIF

         ::nIndent--
         ::CloseTag( "div" )

      ENDCASE
   ENDIF

   RETURN

METHOD OpenTagInline( cText, ... ) CLASS GenerateHTML

   LOCAL aArgs := hb_AParams()
   LOCAL idx

   IF ! "|" + cText + "|" $ "|p|pre|code|"
      ::cFile += Replicate( "  ", ::nIndent )
   ENDIF

   FOR idx := 2 TO Len( aArgs ) STEP 2
      cText += " " + aArgs[ idx ] + "=" + '"' + aArgs[ idx + 1 ] + '"'
   NEXT

   ::cFile += "<" + cText + ">"

   RETURN Self

METHOD OpenTag( cText, ... ) CLASS GenerateHTML

   ::OpenTagInline( cText, ... )

   ::cFile += hb_eol()

   RETURN Self

METHOD TaggedInline( cText, cTag, ... ) CLASS GenerateHTML

   LOCAL aArgs := hb_AParams()
   LOCAL cResult := ""
   LOCAL idx

   FOR idx := 3 TO Len( aArgs ) STEP 2
      cResult += " " + aArgs[ idx ] + "=" + '"' + aArgs[ idx + 1 ] + '"'
   NEXT

   ::cFile += "<" + cTag + cResult + ">" + cText + "</" + cTag + ">"

   RETURN Self

METHOD Tagged( cText, cTag, ... ) CLASS GenerateHTML

   ::TaggedInline( cText, cTag, ... )

   ::cFile += hb_eol()

   RETURN Self

METHOD CloseTagInline( cText ) CLASS GenerateHTML

   ::cFile += "</" + cText + ">"

   RETURN Self

METHOD CloseTag( cText ) CLASS GenerateHTML

   ::cFile += "</" + cText + ">" + hb_eol()

   RETURN Self

#define _RESULT_ARROW  "→"

STATIC FUNCTION StrSYNTAX( cString )

   STATIC s_html := { ;
      "==>" => _RESULT_ARROW, ;
      "-->" => _RESULT_ARROW }

   RETURN hb_StrReplace( cString, s_html )

STATIC FUNCTION StrEsc( cString )

   STATIC s_html := { ;
      "&" => "&amp;", ;
      '"' => "&quot;", ;
      "<" => "&lt;", ;
      ">" => "&gt;" }

   RETURN hb_StrReplace( cString, s_html )

STATIC FUNCTION MDSpace( cChar )
   RETURN Empty( cChar ) .OR. cChar $ ".,"

METHOD AppendInline( cText, cFormat, lCode, cField, cID ) CLASS GenerateHTML

   LOCAL idx

   LOCAL cChar, cPrev, cNext, cOut, tmp, tmp1, nLen
   LOCAL lST, lEM, lPR, cPR
   LOCAL nST, nEM, nPR
   LOCAL cdp
   LOCAL lNAME

   IF ! HB_ISNULL( cText )

      hb_default( @lCode, .F. )

      lNAME := ( cField == "NAME" )

      IF lCode
         cText := StrEsc( cText )
      ELSE
         cdp := hb_cdpSelect( "EN" )  /* make processing loop much faster */

         lST := lEM := lPR := .F.
         cOut := ""
         nLen := Len( cText )
         FOR tmp := 1 TO nLen

            /* TOFIX: In real Markdown,
                      *text*   and _text_   both result in <em>text</em>,
                      **text** and __text__ both result in <strong>text</strong>. */

            cPrev := iif( tmp > 1, SubStr( cText, tmp - 1, 1 ), "" )
            cChar := SubStr( cText, tmp, 1 )
            cNext := SubStr( cText, tmp + 1, 1 )

            DO CASE
            CASE ! lPR .AND. cChar == "\" .AND. tmp < Len( cText ) .AND. ! hb_asciiIsAlpha( cNext )
               tmp++
               cChar := cNext
            CASE ! lPR .AND. cChar == "`" .AND. cNext == "`"  // `` -> `
               tmp++
            CASE ! lPR .AND. cChar == "_" .AND. cNext == "_"
               tmp++
               cChar := "__"
            CASE ! lPR .AND. SubStr( cText, tmp, 3 ) == "<b>"
               tmp += 2
               cChar := "<strong>"
            CASE ! lPR .AND. SubStr( cText, tmp, 4 ) == "</b>"
               tmp += 3
               cChar := "</strong>"
            CASE ! lPR .AND. ;
               ( SubStr( cText, tmp, 5 ) == "<http" .AND. ( tmp1 := hb_At( ">", cText, tmp + 1 ) ) > 0 )
               tmp1 := SubStr( cText, tmp + 1, tmp1 - tmp - 1 )
               tmp += Len( tmp1 ) + 1
               cChar := "<a href=" + '"' + tmp1 + '"' + ">" + tmp1 + "</a>"
            CASE ! lPR .AND. cChar == "*" .AND. !( cNext == "*" ) .AND. ! lEM .AND. ;
                 iif( lST, ! MDSpace( cPrev ) .AND. MDSpace( cNext ), MDSpace( cPrev ) .AND. ! MDSpace( cNext ) )
               lST := ! lST
               IF lST
                  nST := Len( cOut ) + 1
               ENDIF
               cChar := iif( lST, "<strong>", "</strong>" )
            CASE ! lPR .AND. cChar == "_" .AND. ! lST .AND. ;
                 ( ( ! lEM .AND. MDSpace( cPrev ) .AND. ! MDSpace( cNext ) ) .OR. ;
                   (   lEM .AND. ! MDSpace( cPrev ) .AND. MDSpace( cNext ) ) )
               lEM := ! lEM
               IF lEM
                  nEM := Len( cOut ) + 1
               ENDIF
               cChar := iif( lEM, "<em>", "</em>" )
            CASE ! lPR .AND. ;
                 ( SubStr( cText, tmp, 3 ) == ".T." .OR. ;
                   SubStr( cText, tmp, 3 ) == ".F." )
               cChar := CODEINLINE + SubStr( cText, tmp, 3 ) + "</code>"
               tmp += 2
            CASE cChar == "`" .OR. ;
                 ( cChar == "<" .AND. !( Empty( cNext ) .OR. cNext $ ">=" ) .AND. ! lPR ) .OR. ;
                 ( cChar == ">" .AND.                                               lPR .AND. cPR $ "<#" )
               lPR := ! lPR
               IF lPR
                  nPR := Len( cOut ) + 1
                  cPR := cChar
               ENDIF
               SWITCH cChar
               CASE "<"
               CASE ">"
                  IF lPR .AND. ;
                     ( "|" + hb_asciiUpper( SubStr( cText, tmp + 1, 2 ) ) + "|" $ "|F1|F2|F2|F3|F4|F5|F6|F7|F8|F9|UP|" .OR. ;
                       "|" +                SubStr( cText, tmp + 1, 2 )   + "|" $ "|BS|" .OR. ;
                       "|" + hb_asciiUpper( SubStr( cText, tmp + 1, 3 ) ) + "|" $ "|F10|F11|F12|ESC|INS|DEL|ALT|END|TAB|" .OR. ;
                       "|" + hb_asciiUpper( SubStr( cText, tmp + 1, 4 ) ) + "|" $ "|CTRL|META|DOWN|LEFT|HOME|PGDN|PGUP|" .OR. ;
                       "|" + hb_asciiUpper( SubStr( cText, tmp + 1, 5 ) ) + "|" $ "|SHIFT|RIGHT|ENTER|SPACE|" .OR. ;
                       "|" + hb_asciiUpper( SubStr( cText, tmp + 1, 6 ) ) + "|" $ "|RETURN|KEYPAD|PRTSCR|" .OR. ;
                       hb_LeftEqI( SubStr( cText, tmp + 1, 10 ), "CURSORPAD" ) .OR. ;
                       ( ( hb_asciiIsUpper( cNext ) .OR. hb_asciiIsDigit( cNext ) ) .AND. SubStr( cText, tmp + 2, 1 ) == ">" ) )
                     cPR := "#"
                  ENDIF
                  IF cPR == "#"
                     cChar := iif( lPR, "<span class=" + '"' + "d-key" + '"' + ">", "</span>" )
                  ELSE
                     cChar := iif( lPR, CODEINLINE, "</code>" )
                  ENDIF
                  EXIT
               OTHERWISE
                  cChar := iif( lPR, CODEINLINE, "</code>" )
               ENDSWITCH
               IF ! lPR
                  cPR := ""
               ENDIF
            CASE ! lPR .AND. ;
               ( SubStr( cText, tmp, 3 ) == "===" .OR. SubStr( cText, tmp, 3 ) == "---" )
               DO WHILE tmp < nLen .AND. SubStr( cText, tmp, 1 ) == cChar
                  tmp++
               ENDDO
               cChar := "<hr>"
            CASE ! lPR .AND. ;
               ( SubStr( cText, tmp, 3 ) == "==>" .OR. SubStr( cText, tmp, 3 ) == "-->" )
               tmp += 2
               cChar := _RESULT_ARROW
            CASE ! lPR .AND. SubStr( cText, tmp, 2 ) == "--" .AND. ! lNAME
               tmp += 1
               cChar := "—"  // &emdash;
            CASE cChar == "&"
               cChar := "&amp;"
            CASE cChar == '"'
               cChar := "&quot;"
            CASE cChar == "<"
               cChar := "&lt;"
            CASE cChar == ">"
               cChar := "&gt;"
            ENDCASE

            cOut += cChar
         NEXT

         /* Remove these tags if they weren't closed */
         IF lPR
            cOut := Stuff( cOut, nPR, Len( CODEINLINE ), "`" )
         ENDIF
         IF lST
            cOut := Stuff( cOut, nST, Len( "<strong>" ), "*" )
         ENDIF
         IF lEM
            cOut := Stuff( cOut, nEM, Len( "<i>" ), "_" )
         ENDIF

         cText := cOut

         hb_cdpSelect( cdp )
      ENDIF

      IF !( "|" + hb_defaultValue( cField, "" ) + "|" $ "||NAME|ONELINER|" )
         cText := AutoLink( ::hHBX, cText, ::cFilename, s_cRevision, ::hNameIDM, ::cLang, lCode, cID )
#if 0
         IF ! lCode .AND. "( " $ cText
            FOR EACH tmp1 IN en_hb_regexAll( "([a-zA-Z0-9]+)\( ", cText,,,,, .F. )
               ? ::cFileName, hb_ValToExp( tmp1[ 1 ] )
            NEXT
         ENDIF
#endif
      ENDIF

      FOR EACH idx IN hb_ATokens( hb_defaultValue( cFormat, "" ), "," ) DESCEND
         IF ! Empty( idx )
            cText := "<" + idx + ">" + cText + "</" + idx + ">"
         ENDIF
      NEXT

      DO WHILE Right( cText, Len( hb_eol() ) ) == hb_eol()
         cText := hb_StrShrink( cText, Len( hb_eol() ) )
      ENDDO

      ::cFile += cText
   ENDIF

   RETURN Self

METHOD Append( cText, cFormat, lCode, cField, cID ) CLASS GenerateHTML

   ::AppendInline( cText, cFormat, lCode, cField, cID )
   ::cFile += hb_eol()

   RETURN Self

METHOD RecreateStyleDocument( cStyleFile ) CLASS GenerateHTML

   #pragma __streaminclude "hbdoc.css" | LOCAL cString := %s

   IF ::cLang == "en"
      IF ! hb_vfDirExists( ::cDir )
         hb_DirBuild( ::cDir )
      ENDIF

      IF ! hb_MemoWrit( cStyleFile := hb_DirSepAdd( ::cDir ) + cStyleFile, cString )
         OutErr( hb_StrFormat( "! Error: Cannot create file '%1$s'", cStyleFile ) + hb_eol() )
      ELSEIF hbdoc_reproducible()
         hb_vfTimeSet( cStyleFile, hb_Version( HB_VERSION_BUILD_TIMESTAMP_UTC ) )
      ENDIF
   ENDIF

   RETURN Self

STATIC FUNCTION SymbolToHTMLID( cID )

   STATIC s_conv := { ;
      "%" => "pc", ;
      "#" => "ha", ;
      "<" => "lt", ;
      ">" => "gt", ;
      "=" => "eq", ;
      "*" => "ml", ;
      "-" => "mi", ;
      "+" => "pl", ;
      "/" => "sl", ;
      "$" => "dl", ;
      "&" => "et", ;
      "(" => "bo", ;
      ")" => "bc", ;
      "[" => "so", ;
      "]" => "sc", ;
      "{" => "co", ;
      "}" => "cc", ;
      ":" => "co", ;
      "!" => "no", ;
      "?" => "qu", ;
      "|" => "or", ;
      "@" => "at", ;
      " " => "-" }

   IF Right( cID, 1 ) == "*" .AND. Len( cID ) > 1
      cID := hb_StrShrink( cID )
   ENDIF

   RETURN hb_StrReplace( cID, s_conv )

#define R_( x )  ( x )

/* Based on FixFuncCase() in hbmk2 */
STATIC FUNCTION AutoLink( hHBX, cFile, cComponent, cRevision, hNameIDM, cLang, lCodeAlready, cID )

   LOCAL match
   LOCAL cProper
   LOCAL cName, lFound
   LOCAL cTag, cAnchor, cTitle, cLangOK
   LOCAL nShift
   LOCAL tmp1

   HB_SYMBOL_UNUSED( cLang )

   IF !( cComponent == "index" )

      #define _MATCH_cStr    1
      #define _MATCH_nStart  2
      #define _MATCH_nEnd    3

      IF ! lCodeAlready
         nShift := 0
         FOR EACH match IN en_hb_regexAll( R_( "([A-Za-z] |[^A-Za-z_:]|^)([A-Za-z_][A-Za-z0-9_]+\(\))" ), cFile,,,,, .F. )
            IF Len( match[ 2 ][ _MATCH_cStr ] ) != 2 .OR. !( Left( match[ 2 ][ _MATCH_cStr ], 1 ) $ "D" /* "METHOD" */ )
               cProper := ProperCase( hHBX, hb_StrShrink( match[ 3 ][ _MATCH_cStr ], 2 ), @lFound ) + "()"
               IF cProper $ hNameIDM[ cLangOK := cLang ] .OR. ;
                  iif( cLang == "en", .F., cProper $ hNameIDM[ cLangOK := "en" ] )

                  cTag := cTitle := ""
                  cAnchor := NIL
                  /* search order to resolve 'see also' links: self, ... */
                  FOR EACH tmp1 IN { cComponent, "harbour", "clc53", "hbct", "clct3", hb_HKeyAt( hNameIDM[ cLangOK ][ cProper ], 1 ) }
                     IF tmp1 $ hNameIDM[ cLangOK ][ cProper ]
                        cAnchor := hNameIDM[ cLangOK ][ cProper ][ tmp1 ][ "id" ]
                        IF ! tmp1:__enumIsFirst()
                           cTag := GetLangDir( cLang, cLangOK ) + tmp1 + ".html"
                           cTitle := " " + "title=" + '"' + iif( cLangOK == cLang, tmp1, hb_StrFormat( I_( "%1$s (%2$s)" ), tmp1, cLangOK ) ) + '"'
                        ENDIF
                        EXIT
                     ENDIF
                  NEXT
                  IF cID != NIL .AND. cAnchor == cID  /* do not link to self */
                     cTag := cProper
                  ELSE
                     cTag := "<a href=" + '"' + cTag + "#" + cAnchor + '"' + cTitle + ">" + cProper + "</a>"
                  ENDIF
               ELSE
//                ? "broken 'autodetect' link:", cLangOK, cComponent, "|" + cProper + "|"
                  cTag := cProper
               ENDIF
               cTag := CODEINLINE + cTag + "</code>"
               cFile := hb_BLeft( cFile, match[ 3 ][ _MATCH_nStart ] - 1 + nShift ) + cTag + hb_BSubStr( cFile, match[ 3 ][ _MATCH_nEnd ] + 1 + nShift )
               nShift += Len( cTag ) - Len( cProper )
            ENDIF
         NEXT
      ENDIF

      nShift := 0
      FOR EACH match IN en_hb_regexAll( R_( " ([A-Za-z0-9_/]+\.[a-z]{1,3})([^A-Za-z0-9]|$)" ), cFile,,,,, .F. )
         cName := match[ 2 ][ _MATCH_cStr ]
         cTag := "|" + hb_FNameExt( cName ) + "|"
         IF hb_BLen( cTag ) >= 2 + 3 .OR. cTag $ "|.c|.h|"
            IF cTag $ "|.ch|.h|.c|.txt|.prg|"
               IF cComponent == "harbour"
                  IF cTag $ "|.ch|.h|"
                     cTag := "include/"
                  ELSE
                     cTag := ""
                  ENDIF
                  cTag += cName
               ELSE
                  cTag := "contrib/" + cComponent + "/" + cName
               ENDIF
               IF hb_FileExists( hbdoc_dir_in() + cTag ) .OR. ;
                  hb_FileExists( hbdoc_dir_in() + ( cTag := "include/" + cName ) )
#if 0
                  /* link to the most-recent version */
                  cTag := "<a href=" + '"' + hb_Version( HB_VERSION_URL_BASE ) + "tree/master/" + Lower( cTag ) + '"' + ">" + cName + "</a>"
#endif
                  /* link to the matching source revision */
                  cTag := "<a href=" + '"' + hb_Version( HB_VERSION_URL_BASE ) + "blob/" + cRevision + "/" + Lower( cTag ) + '"' + ">" + cName + "</a>"
               ELSE
                  cTag := cName
               ENDIF
            ELSE
               cTag := cName
            ENDIF
            IF ! lCodeAlready
               cTag := CODEINLINE + cTag + "</code>"
            ENDIF
            cFile := hb_BLeft( cFile, match[ 2 ][ _MATCH_nStart ] - 1 + nShift ) + cTag + hb_BSubStr( cFile, match[ 2 ][ _MATCH_nEnd ] + 1 + nShift )
            nShift += Len( cTag ) - Len( cName )
         ENDIF
      NEXT

      IF ! lCodeAlready
         nShift := 0
         FOR EACH match IN en_hb_regexAll( R_( "( |^)([A-Z_][A-Z0-9_]+)([^A-Z0-9_]|$)" ), cFile,,,,, .F. )
            cName := match[ 3 ][ _MATCH_cStr ]
            IF ( hb_BLen( cName ) > 3 .OR. "|" + cName + "|" $ "|ON|OFF|SET|USE|ZAP|SAY|RUN|NUL|NIL|ALL|TO|GET|VAR|SUM|DIR|DO|FOR|NEW|" ) .AND. ;
               !( "|" + cName + "|" $ "|ANSI|ASCII|JPEG|WBMP|NOTE|INET|TODO|CMOS|ATTENTION|DOUBLE|NUMBER|DATE|CHARACTER|LOGICAL|WARNING|TRUE|FALSE|PLUS|" )
               cTag := CODEINLINE + cName + "</code>"
               cFile := hb_BLeft( cFile, match[ 3 ][ _MATCH_nStart ] - 1 + nShift ) + cTag + hb_BSubStr( cFile, match[ 3 ][ _MATCH_nEnd ] + 1 + nShift )
               nShift += Len( cTag ) - Len( cName )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN cFile

STATIC FUNCTION en_hb_regexAll( ... )

   LOCAL cOldCP := hb_cdpSelect( "cp437" )
   LOCAL aMatch := hb_regexAll( ... )

   hb_cdpSelect( cOldCP )

   RETURN aMatch

STATIC FUNCTION ProperCase( hHBX, cName, /* @ */ lFound )
   RETURN iif( lFound := ( cName $ hHBX ), hb_HKeyAt( hHBX, hb_HPos( hHBX, cName ) ), cName )
