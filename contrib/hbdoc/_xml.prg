/*
 * Document generator - XML output
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

/* Optimizations */
#pragma -km+
#pragma -ko+

#include "hbclass.ch"

#define _FIL_EOL  Chr( 10 )

CREATE CLASS GenerateXML INHERIT TPLGenerate

   EXPORTED:

   METHOD NewIndex( cDir, cFilename, cTitle, cLang )
   METHOD NewDocument( cDir, cFilename, cTitle, cLang )
   METHOD AddEntry( hEntry )
   METHOD AddIndex( hEntry ) HIDDEN
   METHOD BeginSection( cSection, cFilename )
   METHOD EndSection( cSection, cFilename )
   METHOD Generate()

   HIDDEN:

   METHOD WriteEntry( cCaption, cContent, lPreformatted )

ENDCLASS

METHOD NewDocument( cDir, cFilename, cTitle, cLang ) CLASS GenerateXML

   ::super:NewDocument( cDir, cFilename, cTitle, ".xml", cLang )
   ::cFile += ;
     '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + _FIL_EOL + ;
     '<HarbourReference>' + _FIL_EOL

   RETURN Self

METHOD NewIndex( cDir, cFilename, cTitle, cLang ) CLASS GenerateXML

   ::super:NewIndex( cDir, cFilename, cTitle, ".xml", cLang )
   ::cFile += ;
     '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' + _FIL_EOL + ;
     '<HarbourReference>' + _FIL_EOL

   RETURN Self

METHOD BeginSection( cSection, cFilename ) CLASS GenerateXML

   IF ::nDepth == 0
      ::cFile += Replicate( Chr( 9 ), ::nDepth ) + '<Section name="' + cSection + '" file="' + cFilename + ::cExtension + '">' + _FIL_EOL
   ELSE
      ::cFile += Replicate( Chr( 9 ), ::nDepth ) + '<Section name="' + cSection + '">' + _FIL_EOL
   ENDIF
   ::nDepth++

   RETURN Self

METHOD EndSection( cSection, cFilename ) CLASS GenerateXML

   HB_SYMBOL_UNUSED( cSection )
   HB_SYMBOL_UNUSED( cFilename )
   ::nDepth--
   ::cFile += Replicate( Chr( 9 ), ::nDepth ) + '</Section>' + _FIL_EOL

   RETURN Self

METHOD AddIndex( hEntry ) CLASS GenerateXML

   ::WriteEntry( "ENTRY", hEntry[ "NAME" ] + " - " + hEntry[ "ONELINER" ], .F. )

   RETURN Self

METHOD AddEntry( hEntry ) CLASS GenerateXML

   LOCAL item

   IF ::IsIndex()
      ::AddIndex( hEntry )
   ELSE
      ::cFile += '<Entry>' + _FIL_EOL
      ::nDepth++
      FOR EACH item IN FieldIDList()
         ::WriteEntry( item, hEntry[ item ], IsPreformatted( hEntry, item ) )
      NEXT
      ::nDepth--
      ::cFile += '</Entry>' + _FIL_EOL
   ENDIF

   RETURN Self

METHOD Generate() CLASS GenerateXML

   ::cFile += '</HarbourReference>' + _FIL_EOL

   ::super:Generate()

   RETURN Self

METHOD PROCEDURE WriteEntry( cCaption, cContent, lPreformatted ) CLASS GenerateXML

   IF ! Empty( cContent )

      IF _FIL_EOL $ cContent
         cContent := _FIL_EOL + cContent
      ENDIF

      ::cFile += ;
         Replicate( Chr( 9 ), ::nDepth ) + "<" + cCaption + iif( lPreformatted, ' preformatted="yes"', "" ) + ">" + ;
         hb_StrReplace( cContent, { ;
            "&" => "&amp;", ;
            '"' => "&quot;", ;
            "<" => "&lt;", ;
            ">" => "&gt;" } ) + ;
         "</" + cCaption + ">" + _FIL_EOL
   ENDIF

   RETURN
