var divpart;

/**
 * Requests table data for a specific page.
 *
 * @param pageNum the page number to request data for
 */
function getTableData(pageNum) {
  xslGet( "/xsl/based.xsl" );
  xmlGet( '/cgi-bin/tableservletdb.hrb?page=' + pageNum + "&sid=" + Math.random(), tableResponseHandler);
  divpart = 'table-section';
}

function getTablePages() {
  xslGet( "/xsl/basep.xsl" );
  xmlGet( '/cgi-bin/tableservletdb.hrb?count=true' + "&sid=" + Math.random(), tableResponseHandler);
  divpart = 'page-section';
}


/**
 * Handler for server's response to table requests.
 * Table content is pulled from response XML and a HTML
 * table is built.  The table is then inserted into the
 * 'table-section' DIV.
 */
function tableResponseHandler() {
  // Make sure the request is loaded (readyState = 4)
  if (req.readyState == 4) {
    // Make sure the status is "OK"
    if (req.status == 200) {
      // shutdown Ajax loading progress
      EndProgress();

      // Make sure the XSL document is loaded
      if (!xsldocloaded) {
        alert('Cannot transform data.  XSL is not yet loaded.');
        // break out of the function
        return;
      }

      // transform
      //document.write( xsldoc );
      combine_XLM_XSLT_HTML( req, xsldoc, document, divpart );

      xsldocloaded = null;
      xsldoc = null;
    } else {
      alert("There was a problem retrieving the XML data:\n" +
        req.statusText);
    }
  }
}
