#require "xhb"

#include "hbxml.ch"

PROCEDURE Main()

   LOCAL cNote, cDiscount
   LOCAL oDoc, oBook, oIterator, oCurrent

   LOCAL cString := MemoRead( hb_DirBase() + "test.xml" )

   IF HB_ISNULL( cString )
      ? "XML file unavailable"
      RETURN
   ENDIF

   oDoc := TXMLDocument():New( cString, HBXML_STYLE_NOESCAPE )
   IF oDoc:nError != HBXML_ERROR_NONE
      ? "XML file parsing error", hb_ntos( oDoc:nError )
      RETURN
   ENDIF

   IF ( oBook := oDoc:findfirst( "book" ) ) == NIL
      ? "no books found"
      RETURN
   ENDIF

   ? "cloned:", oBook:CloneTree():ToString( HBXML_STYLE_THREESPACES )

   DO WHILE .T.

      IF "id" $ oBook:aAttributes
         ? "book ID:", oBook:aAttributes[ "id" ]
      ELSE
         ? "no attribute book ID"
      ENDIF

      cNote := cDiscount := ""
      oIterator := TXMLIterator():New( oBook )

      DO WHILE .T.
         IF ( oCurrent := oIterator:Next() ) == NIL
            ? "end branch"
            ? "values:", cNote, cDiscount
            Inkey( 0 )
            EXIT
         ELSE
            ? "current tag:", oCurrent:cName
            DO CASE
            CASE oCurrent:cName == "note"
               cNote := oCurrent:cData
            CASE oCurrent:cName == "discount"
               cDiscount := oCurrent:cData
            ENDCASE
         ENDIF
      ENDDO

      IF ( oBook := oDoc:findnext() ) == NIL
         ? "no more books found"
         EXIT
      ENDIF
   ENDDO

   RETURN
