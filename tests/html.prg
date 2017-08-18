/* Example HTML generator class for Harbour
 *
 * - Use :ShowResults() to make dynamic HTML (to test dynamic results, copy
 *   the executable into cgi-bin directory or equivalent)
 * - Use :SaveToFile() to make static HTML page
 */

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL oHTML := THtml():New()

   oHTML:SetTitle( "Harbour Power Demonstration" )
   oHTML:AddHead( "Harbour" )
   oHTML:AddPara( "<strong>Harbour</strong> is xBase at its best. Have a taste today!" )
   oHTML:AddPara( "<strong>Links</strong>" )
   oHTML:AddLink( "https://example.org/", "Meet the Harbour power!" )
   oHTML:Generate()

#if 0
   // Uncomment the following if you don't have a Web Server to test
   // this sample
   oHTML:SaveToFile( "html_test.html" )
#endif

   // If the above is uncommented, you may comment this line:
   oHTML:ShowResult()

   RETURN

CREATE CLASS THTML STATIC

   VAR cTitle      INIT "Untitled"       // Page title
   VAR cBody       INIT ""               // HTML body handler
   VAR cBGColor    INIT "#fff"           // Background color
   VAR cLinkColor  INIT "#00f"           // Link color
   VAR cvLinkColor INIT "#f00"           // Visited link color
   VAR cContent    INIT ""               // Page content handler

   METHOD New()                          // New method
   METHOD SetTitle( cTitle )             // Set page title
   METHOD AddLink( cLinkTo, cLinkName )  // Add <h1> header
   METHOD AddHead( cDescr )              // Add hyperlink
   METHOD AddPara( cPara, cAlign )       // Add paragraph
   METHOD Generate()                     // Generate HTML
   METHOD ShowResult()                   // Saves content to file
   METHOD SaveToFile( cFile )            // Show result

ENDCLASS

METHOD New() CLASS THTML
   RETURN Self

METHOD SetTitle( cTitle ) CLASS THTML

   ::cTitle := cTitle

   RETURN Self

METHOD AddLink( cLinkTo, cLinkName ) CLASS THTML

   ::cBody += "<a href=" + '"' + cLinkTo + '"' + ">" + cLinkName + "</a>"

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
   RETURN hb_MemoWrit( cFile, ::cContent )
