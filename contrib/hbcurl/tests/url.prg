/* Copyright 2019-present Viktor Szakats */

#require "hbcurl"

#include "simpleio.ch"

PROCEDURE Main( cURL )

   LOCAL url
   LOCAL res
   LOCAL str

   ? url := curl_url()
   ? res := curl_url_set( url, HB_CURLUPART_URL, hb_defaultValue( cURL, "https://example.net/" ), 0 )
   IF res == HB_CURLUE_OK
      ? curl_url_get( url, HB_CURLUPART_SCHEME,, 0 )
      ? curl_url_get( url, HB_CURLUPART_SCHEME, @str, 0 )
      ? str
      ?
      ? curl_url_get( url, HB_CURLUPART_SCHEME,, 0 )
      ?
   ENDIF

   curl_url_set( url, HB_CURLUPART_URL, "https://user:passwd@example.net:443/mypages/mysite/page.html?avar=0&avar1=1" )
   dump( url )

   curl_url_set( url, HB_CURLUPART_URL, "http://127.0.0.1&@2.2.2.2# @3.3.3.3/" )
   dump( url )

   RETURN

STATIC PROCEDURE dump( url )

   ? "HB_CURLUPART_URL"      , curl_url_get( url, HB_CURLUPART_URL )
   ? "HB_CURLUPART_SCHEME"   , curl_url_get( url, HB_CURLUPART_SCHEME )
   ? "HB_CURLUPART_USER"     , curl_url_get( url, HB_CURLUPART_USER )
   ? "HB_CURLUPART_PASSWORD" , curl_url_get( url, HB_CURLUPART_PASSWORD )
   ? "HB_CURLUPART_OPTIONS"  , curl_url_get( url, HB_CURLUPART_OPTIONS )
   ? "HB_CURLUPART_HOST"     , curl_url_get( url, HB_CURLUPART_HOST )
   ? "HB_CURLUPART_PATH"     , curl_url_get( url, HB_CURLUPART_PATH )
   ? "HB_CURLUPART_QUERY"    , curl_url_get( url, HB_CURLUPART_QUERY )
   ? "HB_CURLUPART_FRAGMENT" , curl_url_get( url, HB_CURLUPART_FRAGMENT )
   ? "HB_CURLUPART_ZONEID"   , curl_url_get( url, HB_CURLUPART_ZONEID )
   ? "HB_CURLUPART_PORT"     , curl_url_get( url, HB_CURLUPART_PORT )
   ?

   RETURN
