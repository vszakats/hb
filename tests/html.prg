/* Harbour Test of a HTML-Generator class.
 *
 * Tips: - Use ShowResults to make dynamic html (to test dynamic
 *         results, put the exe file on CGI-BIN dir or equivalent);
 *       - Use SaveToFile to make static html page
 */

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oHTML := THtml():New()

   oHTML:SetTitle( "Harbour Power Demonstration" )
   oHTML:AddHead( "Harbour" )
   oHTML:AddPara( "<strong>Harbour</strong> is xBase at its best. Have a taste today!" )
   oHTML:AddPara( "<strong>Links</strong>" )
   oHTML:AddLink( "https://example.org", "Meet the Harbour power!" )
   oHTML:Generate()

#if 0
   // Uncomment the following if you don't have a Web Server to test
   // this sample
   oHTML:SaveToFile( "test.html" )
#endif

   // If the above is uncommented, you may comment this line:
   oHTML:ShowResult()

   RETURN

CREATE CLASS THTML STATIC

   VAR cTitle      INIT "Untitled"        // Page Title
   VAR cBody       INIT ""                // HTML Body Handler
   VAR cBGColor    INIT "#FFFFFF"         // Background Color
   VAR cLinkColor  INIT "#0000FF"         // Link Color
   VAR cvLinkColor INIT "#FF0000"         // Visited Link Color
   VAR cContent    INIT ""                // Page Content Handler

   METHOD New()                           // New Method
   METHOD SetTitle( cTitle )              // Set Page Title
   METHOD AddLink( cLinkTo, cLinkName )   // Add <H1> Header
   METHOD AddHead( cDescr )               // Add Hyperlink
   METHOD AddPara( cPara, cAlign )        // Add Paragraph
   METHOD Generate()                      // Generate HTML
   METHOD ShowResult()                    // Saves Content to File
   METHOD SaveToFile( cFile )             // Show Result

ENDCLASS

METHOD New() CLASS THTML
   RETURN Self

METHOD SetTitle( cTitle ) CLASS THTML

   ::cTitle := cTitle

   RETURN Self

METHOD AddLink( cLinkTo, cLinkName ) CLASS THTML

   ::cBody += "<a href='" + cLinkTo + "'>" + cLinkName + "</a>"

   RETURN Self

METHOD AddHead( cDescr ) CLASS THTML

   ::cBody += "<h1>" + cDescr + "</h1>"

   RETURN Self

METHOD AddPara( cPara, cAlign ) CLASS THTML

   ::cBody += "<p>" + cPara + "</p>"

   RETURN Self

METHOD Generate() CLASS THTML

   ::cContent := ;
      "<!DOCTYPE html>" + hb_eol() + ;
      '<html lang="en">' + hb_eol() + ;
      '<meta charset="utf-8">' + hb_eol() + ;
      "<title>" + ::cTitle + "</title>" + hb_eol() + ;
      ::cBody + hb_eol()

   RETURN Self

METHOD ShowResult() CLASS THTML

   OutStd( ;
;//   "HTTP/1.1 200 OK" + hb_eol() + ;
      "Content-Type: text/html" + hb_eol() + hb_eol() + ;
      ::cContent )

   RETURN Self

METHOD SaveToFile( cFile ) CLASS THTML

   hb_MemoWrit( cFile, ::cContent )

   RETURN Self
