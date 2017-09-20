/*
 * MIME functions
 *
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"

/* Detects the MIME type of a given file */

typedef struct tag_mime
{
   HB_SIZE            pos;       /* Position in stream from which the match begins */
   const char *       pattern;   /* String to match */
   const char *       mime_type; /* MIME type if complete */
   int                next;      /* following entry to determine a MIME type, relative to current position (or 0) */
   int                alternate; /* alternative entry to determine a MIME type, relative to current position (or 0) */
   short unsigned int flags;     /* flags for confrontation */
} MIME_ENTRY;

#define MIME_FLAG_CASESENS    0x0000
#define MIME_FLAG_TRIMSPACES  0x0001
#define MIME_FLAG_TRIMTABS    0x0002
#define MIME_FLAG_CASEINSENS  0x0004
#define MIME_FLAG_CONTINUE    0x0008

static const MIME_ENTRY s_mimeTable[] =
{
   /* MS-DOS/Windows executable */
   { 0,  "MZ",                                "application/x-dosexec",         0, 0, 0                                                                },
   /* ELF file */
   { 0,  "\177ELF",                           NULL,                            1, 0, 0                                                                },
   { 4,  "\x00",                              NULL,                            3, 1, MIME_FLAG_CONTINUE                                               },
   { 4,  "\x01",                              NULL,                            2, 1, MIME_FLAG_CONTINUE                                               },
   { 4,  "\x02",                              NULL,                            1, 0, MIME_FLAG_CONTINUE                                               },
   { 5,  "\x00",                              NULL,                            2, 1, MIME_FLAG_CONTINUE                                               },
   { 5,  "\x01",                              NULL,                            1, 0, MIME_FLAG_CONTINUE                                               },
   { 16, "\x00",                              "application/x-object",          0, 1, MIME_FLAG_CONTINUE                                               },
   { 16, "\x01",                              "application/x-object",          0, 1, MIME_FLAG_CONTINUE                                               },
   { 16, "\x02",                              "application/x-executable",      0, 1, MIME_FLAG_CONTINUE                                               },
   { 16, "\x03",                              "application/x-sharedlib",       0, 1, MIME_FLAG_CONTINUE                                               },
   { 16, "\x04",                              "application/x-coredump",        0, 0, MIME_FLAG_CONTINUE                                               },
   /* Scripts */
   { 0,  "#!/bin/sh",                         "application/x-shellscript",     0, 0, 0                                                                },
   { 0,  "#!/usr/local/env bash",             "application/x-shellscript",     0, 0, 0                                                                },
   { 0,  "#!/usr/local/env perl",             "application/x-perl",            0, 0, 0                                                                },
   { 0,  "#!/usr/local/env python",           "application/x-python",          0, 0, 0                                                                },
   /* Java object code*/
   { 0,  "\xCA\xFE\xBA\xBE",                  "application/java",              0, 0, 0                                                                },
   /* Unix compress (.z) */
   { 0,  "\x1F\x9D",                          "application/x-compress",        0, 0, 0                                                                },
   /* Unix gzip */
   { 0,  "\x1F\x8B",                          "application/x-gzip",            0, 0, 0                                                                },
   /* PKZIP */
   #if 0
   { 0,  "PK\x03\x04",                        "application/x-zip",             0, 0, 0 },  /* 2010-12-15 support of xlsx/ods */
   #endif
   /* xml */
   { 0,  "<?xml",                             "application/xml",               0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* html */
   { 0,  "<html",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   { 0,  "<title",                            "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   { 0,  "<head",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   { 0,  "<body",                             "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   { 0,  "<!--",                              "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS                        },
   { 0,  "<h",                                "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   { 0,  "<!",                                "text/html",                     0, 0, MIME_FLAG_TRIMSPACES | MIME_FLAG_TRIMTABS | MIME_FLAG_CASEINSENS },
   /* Postscript */
   { 0,  "%!",                                "application/postscript",        0, 0, 0                                                                },
   { 0,  "\x04%!",                            "application/postscript",        0, 0, 0                                                                },
   /* PDF */
   { 0,  "%PDF-",                             "application/pdf",               0, 0, 0                                                                },
   /* PNG image */
   { 0,  "\x89PNG",                           "image/png",                     0, 0, 0                                                                },
   /* XPM image */
   { 0,  "/* XPM",                            "image/x-xpm",                   0, 0, 0                                                                },
   /* TIFF image */
   { 0,  "II",                                "image/tiff",                    0, 0, 0                                                                },
   { 0,  "MM",                                "image/tiff",                    0, 0, 0                                                                },
   /* GIF image */
   { 0,  "GIF89z",                            "image/x-compressed-gif",        0, 0, 0                                                                },
   { 0,  "GIF",                               "image/gif",                     0, 0, 0                                                                },
   /* JPEG image */
   { 0,  "\xFF\xD8",                          "image/jpeg",                    0, 0, 0                                                                },
   /* ICO image */
   { 2,  "\x01\x00",                          "image/x-icon",                  0, 0, 0                                                                },
   /* OGG file */
   { 0,  "OggS",                              "application/ogg",               0, 0, 0                                                                }
};

/* Find MIME by extension */

typedef struct tag_mime_ext
{
   const char * pattern;    /* Extension to match */
   HB_USHORT    flags;      /* flags for confrontation */
   const char * mime_type;  /* MIME type if complete */
} EXT_MIME_ENTRY;

/* https://www.iana.org/assignments/media-types/media-types.xhtml */

/* Keep this table well sorted, it's necessary for binary search algorithm */

static const EXT_MIME_ENTRY s_extMimeTable[] =
{
   { "7z"      , MIME_FLAG_CASEINSENS, "application/x-7z-compressed" },
   { "asc"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "atom"    , MIME_FLAG_CASEINSENS, "application/atom+xml" },
   { "avi"     , MIME_FLAG_CASEINSENS, "video/x-msvideo" },
   { "bin"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "bmp"     , MIME_FLAG_CASEINSENS, "image/x-ms-bmp" },
   { "bz2"     , MIME_FLAG_CASEINSENS, "application/x-bzip2" },
   { "c"       , MIME_FLAG_CASEINSENS, "text/x-csrc" },
   { "class"   , MIME_FLAG_CASESENS  , "application/java-vm" },
   { "cpp"     , MIME_FLAG_CASEINSENS, "text/x-c++src" },
   { "crt"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "css"     , MIME_FLAG_CASEINSENS, "text/css" },
   { "csv"     , MIME_FLAG_CASEINSENS, "text/csv" },
   { "cxx"     , MIME_FLAG_CASEINSENS, "text/x-c++src" },
   { "dbf"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "deb"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "der"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "dll"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "dmg"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "doc"     , MIME_FLAG_CASEINSENS, "application/msword" },
   { "docx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.wordprocessingml.document" },
   { "eps"     , MIME_FLAG_CASEINSENS, "application/postscript" },
   { "exe"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "gif"     , MIME_FLAG_CASEINSENS, "image/gif" },
   { "gz"      , MIME_FLAG_CASEINSENS, "application/x-gzip" },
   { "h"       , MIME_FLAG_CASEINSENS, "text/x-chdr" },
   { "hpp"     , MIME_FLAG_CASEINSENS, "text/x-c++hdr" },
   { "htm"     , MIME_FLAG_CASEINSENS, "text/html" },
   { "html"    , MIME_FLAG_CASEINSENS, "text/html" },
   { "hxx"     , MIME_FLAG_CASEINSENS, "text/x-c++hdr" },
   { "ico"     , MIME_FLAG_CASEINSENS, "image/x-icon" },
   { "ics"     , MIME_FLAG_CASEINSENS, "text/calendar" },
   { "ini"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "iso"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "jar"     , MIME_FLAG_CASEINSENS, "application/java-archive" },
   { "java"    , MIME_FLAG_CASESENS  , "text/java" },
   { "jpeg"    , MIME_FLAG_CASEINSENS, "image/jpeg" },
   { "jpg"     , MIME_FLAG_CASEINSENS, "image/jpeg" },
   { "js"      , MIME_FLAG_CASEINSENS, "application/javascript" },
   { "json"    , MIME_FLAG_CASEINSENS, "application/json" },
   { "log"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "m3u"     , MIME_FLAG_CASEINSENS, "audio/x-mpegurl" },
   { "m4a"     , MIME_FLAG_CASEINSENS, "audio/x-m4a" },
   { "m4v"     , MIME_FLAG_CASEINSENS, "video/x-m4v" },
   { "map"     , MIME_FLAG_CASEINSENS, "application/x-httpd-imap" },
   { "markdown", MIME_FLAG_CASEINSENS, "text/x-markdown" },
   { "md"      , MIME_FLAG_CASEINSENS, "text/x-markdown" },
   { "mov"     , MIME_FLAG_CASEINSENS, "video/quicktime" },
   { "mp3"     , MIME_FLAG_CASEINSENS, "audio/mpeg" },
   { "mp4"     , MIME_FLAG_CASEINSENS, "video/mp4" },
   { "mpeg"    , MIME_FLAG_CASEINSENS, "video/mpeg" },
   { "mpg"     , MIME_FLAG_CASEINSENS, "video/mpeg" },
   { "msi"     , MIME_FLAG_CASEINSENS, "application/octet-stream" },
   { "odp"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.presentation" },
   { "ods"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.spreadsheet" },
   { "odt"     , MIME_FLAG_CASEINSENS, "application/vnd.oasis.opendocument.text" },
   { "ogg"     , MIME_FLAG_CASEINSENS, "audio/ogg" },
   { "p7s"     , MIME_FLAG_CASEINSENS, "application/pkcs7-signature" },
   { "pdf"     , MIME_FLAG_CASEINSENS, "application/pdf" },
   { "pem"     , MIME_FLAG_CASEINSENS, "application/x-x509-ca-cert" },
   { "pl"      , MIME_FLAG_CASEINSENS, "application/x-perl" },
   { "png"     , MIME_FLAG_CASEINSENS, "image/png" },
   { "ppt"     , MIME_FLAG_CASEINSENS, "application/powerpoint" },
   { "pptx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.presentationml.presentation" },
   { "ps"      , MIME_FLAG_CASEINSENS, "application/postscript" },
   { "rar"     , MIME_FLAG_CASEINSENS, "application/x-rar-compressed" },
   { "rpm"     , MIME_FLAG_CASEINSENS, "application/x-redhat-package-manager" },
   { "rss"     , MIME_FLAG_CASEINSENS, "application/rss+xml" },
   { "rst"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "rtf"     , MIME_FLAG_CASEINSENS, "text/rtf" },
   { "sh"      , MIME_FLAG_CASEINSENS, "application/x-sh" },
   { "svg"     , MIME_FLAG_CASEINSENS, "image/svg+xml" },
   { "tar"     , MIME_FLAG_CASEINSENS, "application/x-tar" },
   { "tcl"     , MIME_FLAG_CASEINSENS, "application/x-tcl" },
   { "tgz"     , MIME_FLAG_CASEINSENS, "application/x-gtar" },
   { "tif"     , MIME_FLAG_CASEINSENS, "image/tiff" },
   { "tiff"    , MIME_FLAG_CASEINSENS, "image/tiff" },
   { "toml"    , MIME_FLAG_CASEINSENS, "text/x-toml" },
   { "tsv"     , MIME_FLAG_CASEINSENS, "text/tab-separated-values" },
   { "txt"     , MIME_FLAG_CASEINSENS, "text/plain" },
   { "vcf"     , MIME_FLAG_CASEINSENS, "text/directory" },
   { "war"     , MIME_FLAG_CASEINSENS, "application/java-archive" },
   { "wav"     , MIME_FLAG_CASEINSENS, "audio/x-wav" },
   { "webm"    , MIME_FLAG_CASEINSENS, "video/webm" },
   { "webp"    , MIME_FLAG_CASEINSENS, "image/webp" },
   { "wmv"     , MIME_FLAG_CASEINSENS, "video/x-ms-wmv" },
   { "xbm"     , MIME_FLAG_CASEINSENS, "image/x-xbitmap" },
   { "xls"     , MIME_FLAG_CASEINSENS, "application/excel" },
   { "xlsx"    , MIME_FLAG_CASEINSENS, "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" },
   { "xml"     , MIME_FLAG_CASEINSENS, "application/xml" },
   { "xpm"     , MIME_FLAG_CASEINSENS, "image/x-xpixmap" },
   { "yaml"    , MIME_FLAG_CASEINSENS, "text/x-yaml" },
   { "z"       , MIME_FLAG_CASEINSENS, "application/x-compress" },
   { "zip"     , MIME_FLAG_CASEINSENS, "application/zip" }
};

static const char * s_findExtMimeType( const char * szFileExt )
{
   unsigned int uiFirst = 0, uiLast = HB_SIZEOFARRAY( s_extMimeTable );
   char szExt[ 16 ];

   if( *szFileExt == '.' )
      ++szFileExt;
   hb_strncpyLower( szExt, szFileExt, sizeof( szExt ) - 1 );

   do
   {
      unsigned int uiMiddle;
      int i;

      uiMiddle = ( uiFirst + uiLast ) >> 1;
      i = strcmp( szExt, s_extMimeTable[ uiMiddle ].pattern );
      if( i == 0 )
      {
         if( s_extMimeTable[ uiMiddle ].flags == MIME_FLAG_CASEINSENS ||
             strcmp( szExt, szFileExt ) == 0 )
            return s_extMimeTable[ uiMiddle ].mime_type;
         break;
      }
      else if( i < 0 )
         uiLast = uiMiddle;
      else /* if( i > 0 ) */
         uiFirst = uiMiddle + 1;
   }
   while( uiFirst < uiLast );

   return NULL;
}

static const char * s_findMimeStringInTree( const char * cData, HB_SIZE nLen, int iElem )
{
   const MIME_ENTRY * elem = s_mimeTable + iElem;
   HB_SIZE nPos     = elem->pos;
   HB_SIZE nDataLen = strlen( elem->pattern );

   /* allow \0 to be used for matches */
   if( nDataLen == 0 )
      nDataLen = 1;

   /* trim spaces if required */
   while( nPos < nLen &&
          ( ( ( elem->flags & MIME_FLAG_TRIMSPACES ) != 0 && (
                 cData[ nPos ] == ' ' || cData[ nPos ] == '\r' || cData[ nPos ] == '\n' ) ) ||
            ( ( elem->flags & MIME_FLAG_TRIMTABS ) != 0 && cData[ nPos ] == '\t' ) ) )
   {
      nPos++;
   }

   if( nPos < nLen && ( nLen - nPos ) >= nDataLen )
   {
      if( ( elem->flags & MIME_FLAG_CASEINSENS ) != 0 )
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || hb_strnicmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            /* is this the begin of a match tree? */
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iElem + elem->next );
            else
               return elem->mime_type;
         }
      }
      else
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || strncmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, iElem + elem->next );
            else
               return elem->mime_type;
         }
      }
   }

   /* match failed! */
   if( elem->alternate != 0 )
      return s_findMimeStringInTree( cData, nLen, iElem + elem->alternate );

   return NULL;  /* give up */
}

static const char * s_findStringMimeType( const char * cData, HB_SIZE nLen )
{
   unsigned int uiCount;

   for( uiCount = 0; uiCount < HB_SIZEOFARRAY( s_mimeTable ); uiCount++ )
   {
      const MIME_ENTRY * elem = s_mimeTable + uiCount;
      HB_SIZE nPos     = elem->pos;
      HB_SIZE nDataLen = ( HB_SIZE ) strlen( elem->pattern );

      if( ( elem->flags & MIME_FLAG_CONTINUE ) != 0 )
         continue;

      /* trim spaces if required */
      while( nPos < nLen &&
             ( ( ( elem->flags & MIME_FLAG_TRIMSPACES ) != 0 && (
                    cData[ nPos ] == ' ' || cData[ nPos ] == '\r' || cData[ nPos ] == '\n' ) ) ||
               ( ( elem->flags & MIME_FLAG_TRIMTABS ) != 0 && cData[ nPos ] == '\t' ) ) )
      {
         nPos++;
      }

      if( nPos >= nLen )
         continue;

      if( nLen - nPos < nDataLen )
         continue;

      if( ( elem->flags & MIME_FLAG_CASEINSENS ) != 0 )
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || hb_strnicmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            /* is this the begin of a match tree? */
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, uiCount + elem->next );
            else
               return elem->mime_type;
         }
      }
      else
      {
         if( ( *elem->pattern == 0 && cData[ nPos ] == 0 ) || strncmp( cData + nPos, elem->pattern, nDataLen ) == 0 )
         {
            if( elem->next != 0 )
               return s_findMimeStringInTree( cData, nLen, uiCount + elem->next );
            else
               return elem->mime_type;
         }
      }
   }
   return NULL;
}

static const char * s_findFileMimeType( PHB_FILE fileIn )
{
   char buf[ 512 ];

   HB_FOFFSET nPos = hb_fileSeek( fileIn, 0, FS_RELATIVE );
   HB_SIZE    nLen = hb_fileResult( hb_fileReadAt( fileIn, buf, sizeof( buf ), 0 ) );

   if( nLen > 0 )
   {
      hb_fileSeek( fileIn, nPos, FS_SET );
      return s_findStringMimeType( buf, nLen );
   }

   return NULL;
}

/* hb_mimeStr( <cContent>, [<cDefaultType>] ) --> cMimeType */
HB_FUNC( HB_MIMESTR )
{
   PHB_ITEM pData = hb_param( 1, HB_IT_STRING );

   if( pData )
   {
      const char * magic_type = s_findStringMimeType( hb_itemGetCPtr( pData ), hb_itemGetCLen( pData ) );

      if( magic_type )
         hb_retc_const( magic_type );
      else if( HB_ISCHAR( 2 ) )
         hb_retc( hb_parc( 2 ) );
      else
         hb_retc_const( "application/unknown" );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_mimeFName( <cFileName>, [<cDefaultType>] ) --> cMimeType */
HB_FUNC( HB_MIMEFNAME )
{
   const char * fname = hb_parc( 1 );

   if( fname )
   {
      PHB_FNAME pFileName = hb_fsFNameSplit( fname );
      const char * ext_type = pFileName->szExtension ? s_findExtMimeType( pFileName->szExtension ) : NULL;
      hb_xfree( pFileName );

       if( ext_type )
         hb_retc_const( ext_type );
      else if( HB_ISCHAR( 2 ) )
         hb_retc( hb_parc( 2 ) );
      else
         hb_retc_const( "application/unknown" );
  }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_mimeFile( <cFileName|hFile|nFile>, [<cDefaultType>] ) --> cMimeType */
HB_FUNC( HB_MIMEFILE )
{
   PHB_ITEM pFile = hb_param( 1, HB_IT_STRING | HB_IT_POINTER | HB_IT_NUMERIC );

   if( pFile )
   {
      const char * ext_type   = NULL;
      const char * magic_type = NULL;

      if( HB_IS_STRING( pFile ) )
      {
         const char * fname = hb_itemGetCPtr( pFile );

         PHB_FNAME pFileName = hb_fsFNameSplit( fname );
         PHB_FILE fileIn;

         ext_type = pFileName->szExtension ? s_findExtMimeType( pFileName->szExtension ) : NULL;
         hb_xfree( pFileName );

         if( ( fileIn = hb_fileExtOpen( fname, NULL,
                                        FO_READ | FO_SHARED | FO_PRIVATE |
                                        FXO_SHARELOCK,
                                        NULL, NULL ) ) != NULL )
         {
            magic_type = s_findFileMimeType( fileIn );
            hb_fileClose( fileIn );
         }
      }
      else if( hb_fileItemGet( pFile ) )
         magic_type = s_findFileMimeType( hb_fileItemGet( pFile ) );
      else
      {
         PHB_FILE fileIn = hb_fileFromHandle( hb_numToHandle( hb_itemGetNInt( pFile ) ) );
         magic_type = s_findFileMimeType( fileIn );
         hb_fileDetach( fileIn );
      }

      if( magic_type )
         hb_retc_const( magic_type );
      else if( ext_type )
         hb_retc_const( ext_type );
      else if( HB_ISCHAR( 2 ) )
         hb_retc( hb_parc( 2 ) );
      else
         hb_retc_const( "application/unknown" );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
