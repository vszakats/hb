/*
 * curl helper functions
 *
 * Copyright 1999-present Viktor Szakats
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
#include "hbdate.h"

/* Internet timestamp based on:
   https://datatracker.ietf.org/doc/html/rfc822
   https://datatracker.ietf.org/doc/html/rfc2822 */
HB_FUNC( HB_CURL_DATE )
{
   static const char * s_days[]   = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
   static const char * s_months[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

   char szRet[ 32 ];
   int  iYear, iMonth, iDay, iHour, iMinute, iSecond, iMSec;
   long lOffset;

   if( HB_ISDATETIME( 1 ) )
      hb_timeStampUnpack( hb_partd( 1 ), &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );
   else
      hb_timeStampGetLocal( &iYear, &iMonth, &iDay, &iHour, &iMinute, &iSecond, &iMSec );

   if( HB_ISNUM( 2 ) )
      lOffset = hb_parnl( 2 );
   else
      lOffset = hb_timeStampUTCOffset( iYear, iMonth, iDay, iHour, iMinute, iSecond );

   hb_snprintf( szRet, sizeof( szRet ), "%s, %02d %s %04d %02d:%02d:%02d %+03d%02d",
                s_days[ hb_dateDOW( iYear, iMonth, iDay ) - 1 ],
                iDay, s_months[ iMonth == 0 ? 0 : iMonth - 1 ], iYear,
                iHour, iMinute, iSecond,
                ( int ) ( lOffset / 3600 ),
                ( int ) ( ( lOffset % 3600 ) / 60 ) );

   hb_retc( szRet );
}
