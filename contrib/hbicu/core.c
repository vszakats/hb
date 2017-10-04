/*
 * ICU wrappers
 *
 * Copyright 2015 Viktor Szakats (vszakats.net/harbour)
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

#include "unicode/utypes.h"
#include "unicode/uversion.h"

#include "hbapi.h"

HB_FUNC( U_ERRORNAME )
{
   hb_retc( u_errorName( ( UErrorCode ) hb_parnint( 1 ) ) );
}

HB_FUNC( U_SUCCESS )
{
   hb_retl( U_SUCCESS( ( UErrorCode ) hb_parnint( 1 ) ) );
}

HB_FUNC( U_FAILURE )
{
   hb_retl( U_FAILURE( ( UErrorCode ) hb_parnint( 1 ) ) );
}

HB_FUNC( HB_U_GETVERSION )
{
   UVersionInfo versionArray;
   char szVersion[ U_MAX_VERSION_STRING_LENGTH ];

   u_getVersion( versionArray );

   szVersion[ 0 ] = '\0';

   u_versionToString( versionArray, szVersion );

   hb_retc( szVersion );
}

HB_FUNC( HB_ICU_PARSE_DATETIME )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) && HB_ISCHAR( 3 ) && HB_ISCHAR( 4 ) )
   {

      UErrorCode status = U_ZERO_ERROR;

      UnicodeString formatted( hb_parc( 1 ) );
      UnicodeString pattern( hb_parc( 2 ) );
      Locale        locale( hb_parc( 3 ) );
      UnicodeString timeZone( hb_parc( 4 ) );

      SimpleDateFormat * formatter = new SimpleDateFormat( pattern, locale, status );

      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      UDate dateTime = formatter->parse( formatted, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      TimeZone * zone = TimeZone::createTimeZone( timeZone );

      Calendar * cal = Calendar::createInstance( zone, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      cal->setTime( dateTime, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      int    iYear = 0, iMonth = 0, iDay = 0, iHour = 0, iMin = 0;
      double dSec  = 0;

      for( int part = 1; part <= 6; part++ )
      {
         switch( part )
         {
            case 1:
               iYear = cal->get( UCAL_YEAR, status );
               break;
            case 2:
               iMonth = cal->get( UCAL_MONTH, status ) + 1;
               break;
            case 3:
               iDay = cal->get( UCAL_DATE, status );
               break;
            case 4:
               iHour = cal->get( UCAL_HOUR_OF_DAY, status );
               break;
            case 5:
               iMin = cal->get(  UCAL_MINUTE, status );
               break;
            case 6:
               dSec = cal->get(  UCAL_SECOND, status );
               break;
         }

         if( U_FAILURE( status ) )
         {
            if( formatter )
               delete formatter;
            if( zone )
               delete zone;
            if( cal )
               delete cal;

            hb_stornint( status, 6 );
            hb_retl( FALSE );
            return;
         }

      }

      double ts = hb_timeStampPackD(  iYear, iMonth, iDay,
                                      iHour, iMin, dSec );

      hb_stortd( ts, 5 );
      hb_stornint( U_ZERO_ERROR, 6 );

      hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_ICU_FORMAT_DATETIME )
{
   if( HB_ISDATETIME( 1 ) && HB_ISCHAR( 2 ) && HB_ISCHAR( 3 ) && HB_ISCHAR( 4 ) )
   {

      UErrorCode status = U_ZERO_ERROR;

      double        dTimeStamp = hb_partd( 1 );
      UnicodeString pattern( hb_parc( 2 ) );
      Locale        locale( hb_parc( 3 ) );
      UnicodeString timeZone( hb_parc( 4 ) );

      SimpleDateFormat * formatter = new SimpleDateFormat( pattern, locale, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      int    iYear = 0, iMonth = 0, iDay = 0, iHour = 0, iMin = 0;
      double dSec  = 0;

      hb_timeStampUnpackD( dTimeStamp,
                           &iYear, &iMonth, &iDay,
                           &iHour, &iMin, &dSec );

      TimeZone * zone = TimeZone::createTimeZone( timeZone );

      Calendar * cal = Calendar::createInstance( zone, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      cal->set( iYear, iMonth - 1, iDay, iHour, iMin, ( int ) dSec );

      UnicodeString formatted;
      UDate         time = cal->getTime( status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      formatted = formatter->format( time, formatted, status );
      if( U_FAILURE( status ) )
      {
         if( formatter )
            delete formatter;
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 6 );
         hb_retl( FALSE );
         return;
      }

      char * pFormatted = ( char * ) hb_xgrab( FORMAT_BUFFER_SIZE );
      memset( pFormatted, 0, FORMAT_BUFFER_SIZE );

      int extracted = formatted.extract( 0, formatted.length(), pFormatted, FORMAT_BUFFER_SIZE );
      pFormatted = ( char * ) hb_xrealloc( pFormatted, extracted + 1 );

      hb_storc( pFormatted, 5 );
      hb_stornint( U_ZERO_ERROR, 6 );

      hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

}

HB_FUNC( HB_ICU_UNIXEPOCH_TO_DATETIME )
{
   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) )
   {

      UErrorCode status = U_ZERO_ERROR;

      UDate dateTime = hb_parnd( 1 );
      UnicodeString timeZone( hb_parc( 2 ) );

      TimeZone * zone = TimeZone::createTimeZone( timeZone );

      Calendar * cal = Calendar::createInstance( zone, status );
      if( U_FAILURE( status ) )
      {
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 4 );
         hb_retl( FALSE );
         return;
      }

      cal->setTime( dateTime, status );
      if( U_FAILURE( status ) )
      {
         if( zone )
            delete zone;
         if( cal )
            delete cal;

         hb_stornint( status, 4 );
         hb_retl( FALSE );
         return;
      }

      int    iYear = 0, iMonth = 0, iDay = 0, iHour = 0, iMin = 0;
      double dSec  = 0;

      for( int part = 1; part <= 6; part++ )
      {
         switch( part )
         {
            case 1:
               iYear = cal->get( UCAL_YEAR, status );
               break;
            case 2:
               iMonth = cal->get( UCAL_MONTH, status ) + 1;
               break;
            case 3:
               iDay = cal->get( UCAL_DATE, status );
               break;
            case 4:
               iHour = cal->get( UCAL_HOUR_OF_DAY, status );
               break;
            case 5:
               iMin = cal->get(  UCAL_MINUTE, status );
               break;
            case 6:
               dSec = cal->get(  UCAL_SECOND, status );
               break;
         }

         if( U_FAILURE( status ) )
         {
            if( zone )
               delete zone;
            if( cal )
               delete cal;

            hb_stornint( status, 4 );
            hb_retl( FALSE );
            return;
         }

      }

      double ts = hb_timeStampPackD(  iYear, iMonth, iDay,
                                      iHour, iMin, dSec );

      hb_stortd( ts, 3 );
      hb_stornint( U_ZERO_ERROR, 4 );

      hb_retl( TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
