var divpart;

/**
 * Requests table data for a specific page.
 *
 * @param pageNum the page number to request data for
 */
function sendData( force ) {
  var qstr = getquerystring();
  if (force || qstr.length > 4) {
//    qstr = 'w1=' + escape(qstr);  // NOTE: no '?' before querystring
//    xmlPost('/cgi-bin/showcounter.hrb', qstr + "&sid=" + Math.random(), tableResponseHandler);
    divpart = 'result';
    updatepage( escape( qstr ) );
  }
  return false;
}

function getquerystring() {
  var form = document.forms[ 'f1' ];
  var word = form.word.value;
//alert( 'qstr: ' + qstr );
  return word;
}

function updatepage( str ) {
//document.getElementById( divpart ).innerHTML = str; /* "<img src='/counter/" + str + "' alt='counter'>"; */
  document.getElementById( divpart ).innerHTML = "<img src='/cgi-bin/showcounter.hrb?w=" + str + "' alt='counter'>";
}

/**
 * Handler for server's response to table requests.
 * Table content is pulled from response XML and a HTML
 * table is built.  The table is then inserted into the
 * 'tableSection' DIV.
 */
function tableResponseHandler() {
  // Make sure the request is loaded (readyState = 4)
  if (req.readyState == 4) {
    // Make sure the status is "OK"
    if (req.status == 200) {
      // shutdown Ajax loading progress
      EndProgress();

      // transform
      //document.write( xsldoc );
      updatepage( req.responseText );
    } else {
      EndProgress();
      alert("There was a problem retrieving the XML data:\n" +
        req.statusText);
    }
  }
}
