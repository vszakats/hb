/*
 * sql_sprintf() function
 *
 * Copyright 2008 Xavi <jarabal/at/gmail.com>
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
#include "hbdate.h"
#include "hbset.h"

#if defined( HB_LEGACY_LEVEL4 )

static void s_itemUnescape( PHB_ITEM pPar )
{
   HB_SIZE      nParLen = hb_itemGetCLen( pPar ), i;
   const char * cParStr = hb_itemGetCPtr( pPar ), * c;
   char *       cRes;

   for( i = 3, c = cParStr; *c; c++ )
   {
      if( *c == '\'' )
         i++;  /* Count tokens */
   }
   cRes = ( char * ) hb_xgrab( nParLen + i * sizeof( char ) );
   i    = 0; c = cParStr; cRes[ i++ ] = '\'';
   while( *c )
   {
      if( *c == '\'' )
         cRes[ i++ ] = '\'';
      cRes[ i++ ] = *c++;
   }
   cRes[ i++ ] = '\'';
   hb_itemPutCLPtr( pPar, cRes, i );
}

static HB_SIZE s_ReplPar( char * cBuffer, HB_SIZE nBufLen,
                          const char * cFrmStr,
                          int iCOut,
                          HB_BOOL fIsIndW, int iIndWidth, HB_BOOL fIsIndP, int iIndPrec,
                          PHB_ITEM pPar )
{
   HB_SIZE s;

   if( fIsIndW && fIsIndP )
   {
      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iIndWidth, iIndPrec, hb_itemGetPtr( pPar ) );
            break;
         case 's': case 'S':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iIndWidth, iIndPrec, hb_itemGetCPtr( pPar ) );
            break;
         case 'e': case 'E':
         case 'f':
         case 'g': case 'G':
         case 'a': case 'A':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iIndWidth, iIndPrec, hb_itemGetND( pPar ) );
            break;
         default:  /* 'c', 'C', 'd', 'i', 'o', 'u', 'x', 'X' */
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iIndWidth, iIndPrec,
                             HB_IS_LONG( pPar ) ? hb_itemGetNL( pPar ) : hb_itemGetNI( pPar ) );
      }
   }
   else if( fIsIndW || fIsIndP )
   {
      int iInd = ( fIsIndW ? iIndWidth : iIndPrec );

      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iInd, hb_itemGetPtr( pPar ) );
            break;
         case 's':
         case 'S':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iInd, hb_itemGetCPtr( pPar ) );
            break;
         case 'e': case 'E':
         case 'f':
         case 'g': case 'G':
         case 'a': case 'A':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iInd, hb_itemGetND( pPar ) );
            break;
         default:  /* 'c', 'C', 'd', 'i', 'o', 'u', 'x', 'X' */
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, iInd,
                             HB_IS_LONG( pPar ) ? hb_itemGetNL( pPar ) : hb_itemGetNI( pPar ) );
      }
   }
   else
   {
      switch( iCOut )
      {
         case 'p':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, hb_itemGetPtr( pPar ) );
            break;
         case 's': case 'S':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, hb_itemGetCPtr( pPar ) );
            break;
         case 'e': case 'E':
         case 'f':
         case 'g': case 'G':
         case 'a': case 'A':
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr, hb_itemGetND( pPar ) );
            break;
         default:  /* 'c', 'C', 'd', 'i', 'o', 'u', 'x', 'X' */
            s = hb_snprintf( cBuffer, nBufLen, cFrmStr,
                             HB_IS_LONG( pPar ) ? hb_itemGetNL( pPar ) : hb_itemGetNI( pPar ) );
      }
   }
   return s;
}

/**
 * ANSI C sprintf() for ANSI SQL with DATE, DATETIME, LOGICAL, NIL, NUMERIC
 * ------------------------------------------------------------------------
 * sql_sprintf( cFrm, ... ) --> cResult
 *
 * Full compatible ANSI C99 formats with C,S converters wchar_t (UNICODE)
 * Integer & Floating point converters with Width and Precision for NUMERIC & STRING
 * a,A converters Hexadecimal floating point format. Thanks Rafa.
 * %m$,*m$ Index & Indirect arguments C99. Thanks Viktor.
 *
 * s converter for format Harbour data types.
 *    NUMERIC with FIXED DECIMALS = n | n.d   STRING = String's ANSI\C
 *    DATE = HB_SET_DATEFORMAT      DATETIME = HB_SET_DATEFORMAT hh:mm:ss
 *     New Internal Modifier {}. Thanks Mindaugas.
 *     Date and Time Format separate by first space {YYYY-MM-DD hh:mm:ss.fff pp}
 *     {YYYY-MM-DD} = Only Date | { hh:mm:ss.fff pp} = Only Time
 *    LOGICAL = TRUE | FALSE        %d converter for LOGICAL = 1 | 0
 *     Accepts Internal Modifier TRUE and FALSE Format separate by first comma
 *     {T .T.,F .F.} = TRUE & FALSE | {ON} = Only TRUE | {,OFF} = Only FALSE
 *
 * New t,T converter for format ANSI SQL types.
 *    NUMERIC with FIXED DECIMALS = n | n.d   STRING = 'String''s ANSI\\SQL'
 *     Print DEFAULT if 0 length STRING for T converter
 *    DATE = 'YYYY-MM-DD' DATETIME = 'YYYY-MM-DD HH:MM:SS'
 *     Accepts Internal Modifier like s converter {YYYY-MM-DD hh:mm:ss.fff pp}
 *     Print DEFAULT if the DATE, DATETIME is EMPTY for T converter or print
 *           DK_EMPTYDATE, DK_EMPTYDATETIME for t converter
 *    LOGICAL = TRUE | FALSE        Accepts Internal Modifier like s {ON,OFF}
 *
 * Print DEFAULT if the parameter is NIL for T converter.
 * Print NULL if the parameter is HB_IT_NULL or NIL for the rest of converters.
 * Processing %% and n converter Position.
 */

#define DK_INCRES         1024
#define DK_INCBUF         512
#define DK_BLKBUF         HB_MAX_DOUBLE_LENGTH  /* Expense of DK_INCBUF */
#define DK_EMPTYDATE      "'0001-01-01 BC'"
#define DK_EMPTYDATETIME  "'0001-01-01 00:00:00 BC'"

HB_FUNC( SQL_SPRINTF )
{
   PHB_ITEM     pItmFrm = hb_param( 1, HB_IT_STRING );
   int          argc    = hb_pcount() - 1;
   HB_SIZE      nFrmLen;
   const char * cFrmStr;

   if( ! pItmFrm || ( cFrmStr = hb_itemGetCPtr( pItmFrm ) ) == NULL )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   else if( ( nFrmLen = hb_itemGetCLen( pItmFrm ) ) == 0 )
      hb_retc_null();
   else if( argc == 0 )
      hb_retclen( cFrmStr, nFrmLen );
   else
   {
      static const char s_szToken[] = "stTcdiouxXaAeEfgGpnSC";

      HB_SIZE nResPos = 0, nBufLen = DK_INCBUF, nMaxRes = DK_INCRES;
      int p, iErrorPar = 0;

      char * cIntMod = NULL;
      char * cRes    = ( char * ) hb_xgrab( nMaxRes );
      char * cBuffer = ( char * ) hb_xgrab( nBufLen );
      char * cParStr = ( char * ) hb_xgrab( nFrmLen + sizeof( char ) );

      for( p = 0; p < argc;  /* Not p++ to support index & indirect arguments */ )
      {
         PHB_ITEM pPar, pParCpy;
         int      arg, iCOut, IsType, iIndWidth, iIndPrec;
         HB_BOOL  fIsIndW, fIsIndP;
         HB_SIZE  s, f, i, nWidth, nParPos;

         const char * c = cFrmStr;

         s = f = i = nWidth = nParPos = 0;
         arg = iCOut = IsType = iIndWidth = iIndPrec = 0;
         fIsIndW = fIsIndP = HB_FALSE;

         do  /* Get Par Format */
         {
            cParStr[ i++ ] = *c;
            if( f && *c == '%' )
            {
               f = nWidth = 0;
               fIsIndW = fIsIndP = 0;
            }
            else if( f && nWidth == 0 && *c >= '0' && *c <= '9' )
            {
               nWidth = atol( c );
            }
            else if( f && *c == '.' )
            {
               if( f++ == 2 )
                  iErrorPar = 1;
            }
            else if( f && *c == '*' )
            {
               if( f == 2 )
               {
                  if( fIsIndP )
                  {
                     f         = 3;
                     iErrorPar = 1;
                  }
                  else
                     fIsIndP = 1;
               }
               else if( ! fIsIndW )
                  fIsIndW = 1;
               else
               {
                  f         = 3;
                  iErrorPar = 1;
               }
            }
            else if( f && *c == '$' )
            {
               if( nWidth && fIsIndP && iIndPrec == 0 )
               {
                  iIndPrec = ( int ) nWidth;
                  iCOut    = '*';
               }
               else if( nWidth && fIsIndW && iIndWidth == 0 )
               {
                  iIndWidth = ( int ) nWidth;
                  nWidth    = 0;
                  iCOut     = '*';
               }
               else if( nWidth && arg == 0 )
               {
                  arg    = nWidth;
                  nWidth = 0;
                  iCOut  = '%';
               }
               else
               {
                  f         = 3;
                  iErrorPar = 1;
               }
               while( i && cParStr[ --i ] != iCOut )
               {
               }
               ++i;
               iCOut = 0;
            }
            else if( f && *c == '{' )
            {
               if( s )
               {
                  f         = 3;
                  iErrorPar = 1;
               }
               else  /* Remove Internal Modifier */
               {
                  if( cIntMod == NULL )
                     cIntMod = ( char * ) hb_xgrab( nFrmLen + sizeof( char ) );

                  while( *c++ && *c != '}' )
                     cIntMod[ s++ ] = *c;
                  --i;
                  cIntMod[ s ] = '\0';
                  if( *( c - 1 ) == '\0' )
                  {
                     f         = 3;
                     iErrorPar = 1;
                  }
               }
            }
            else if( f && strchr( s_szToken, *c ) )
            {
               f     = 3;
               iCOut = *c;
            }
            else if( *c == '%' )
               f = 1;
            c++;
         }
         while( f < 3 && *c );
         cParStr[ f = i ] = '\0';
         if( iErrorPar )
            break;

         if( iCOut == 't' || iCOut == 'T' )
         {
            if( cParStr[ f - 2 ] == '%' )
            {
               IsType = ( iCOut == 'T' ? 2 : 1 );
               iCOut  = cParStr[ f - 1 ] = 's';
            }
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( fIsIndW )  /* Get Par Indirectly Width Item */
         {
            pPar = hb_param( iIndWidth ? iIndWidth + 1 : p++ + 2, HB_IT_INTEGER );
            if( pPar )
            {
               if( ( iIndWidth = hb_itemGetNI( pPar ) ) < 0 )
                  nWidth = -iIndWidth;
               else
                  nWidth = iIndWidth;
            }
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( fIsIndP )  /* Get Par Indirectly Precision Item */
         {
            pPar = hb_param( iIndPrec ? iIndPrec + 1 : p++ + 2, HB_IT_INTEGER );
            if( pPar )
               iIndPrec = hb_itemGetNI( pPar );
            else
            {
               iErrorPar = 1;
               break;
            }
         }

         if( arg == 0 && *c && p == argc - 1 )  /* No more Par Items */
         {
            do
               cParStr[ i++ ] = *c;
            while( *c++ );
            i--;
         }  /* i == strlen( cParStr ) */

         pPar = hb_param( arg ? arg + 1 : p++ + 2, HB_IT_ANY );  /* Get Par Item */
         if( ! pPar )
         {
            iErrorPar = 1;
            break;
         }

         if( iCOut == 0 || iCOut == 'n' )  /* Par Text Out */
         {
            for( f = i, i = 0; i < f; i++ )  /* Change %% with % */
            {
               if( cParStr[ i ] == '%' && cParStr[ i + 1 ] == '%' )
               {
                  memcpy( cParStr + i, cParStr + i + 1, f - i );
                  f--;
               }
            }  /* i == strlen( cParStr ) */
            if( iCOut )
            {
               for( f = 0; f < i; f++ )  /* Erase %n */
               {
                  if( cParStr[ f ] == '%' && cParStr[ f + 1 ] == 'n' )
                  {
                     memcpy( cParStr + f, cParStr + f + 2, i - f - 1 );
                     break;
                  }
               }  /* f == Index % of n */
               if( f < i )
               {
                  i -= 2;  /* i == strlen( cParStr ) */
                  hb_itemPutNS( pPar, nResPos + f );
               }
               else
               {
                  iErrorPar = 1;
                  break;
               }
            }
            if( ( f = i + sizeof( char ) ) > nBufLen )
            {
               nBufLen += f + DK_INCBUF;
               cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
            }
            hb_strncpy( cBuffer, cParStr, i );
            s = i;
         }
         else  /* Par Item sprintf() Out */
         {
#ifdef HB_IT_NULL
            if( ( HB_IS_NIL( pPar ) || HB_IS_NULL( pPar ) ) )
            {
#else
            if( HB_IS_NIL( pPar ) )
            {
#endif
               nWidth = f;
               fIsIndW = fIsIndP = 0;
               while( cParStr[ --f ] != '%' )
               {
               }
               iCOut = cParStr[ f + 1 ] = 's';  /* Change format with %s */
               memcpy( cParStr + f + 2, cParStr + nWidth, i - nWidth + 1 );
               i -= nWidth - f - 2;             /* i == strlen(cParStr) */
               if( ( f = i + 8 ) > nBufLen )    /* size of "DEFAULT" == 8 */
               {
                  nBufLen += f + DK_INCBUF;
                  cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
               }
               pParCpy = hb_itemNew( NULL );
#ifdef HB_IT_NULL
               if( IsType == 2 && ! HB_IS_NULL( pPar ) )
                  hb_itemPutCL( pParCpy, "DEFAULT", 7 );
#else
               /* Print DEFAULT if NIL for T converter if not NULL, print NULL for the rest of converters */
               if( IsType == 2 )
                  hb_itemPutCL( pParCpy, "DEFAULT", 7 );
#endif
               else
                  hb_itemPutCL( pParCpy, "NULL", 4 );
               s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pParCpy );
               hb_itemRelease( pParCpy );

            }
            else if( HB_IS_STRING( pPar ) && ( iCOut == 's' || iCOut == 'S' ) )
            {
               if( IsType )
               {
                  hb_itemCopy( pParCpy = hb_itemNew( NULL ), pPar );
                  pPar = pParCpy;
                  if( IsType == 2 && hb_itemGetCLen( pPar ) == 0 )  /* 0 length string print DEFAULT for T converter */
                     hb_itemPutCL( pPar, "DEFAULT", 7 );
                  else
                     s_itemUnescape( pPar );
               }
               f = hb_itemGetCLen( pPar );
               if( ( f = i + HB_MAX( nWidth, f ) ) > nBufLen )
               {
                  nBufLen += f + DK_INCBUF;
                  cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
               }
               s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pPar );
               if( IsType )
                  hb_itemRelease( pPar );

            }
            else if( HB_IS_DATETIME( pPar ) && iCOut == 's' )
            {
               long lDate, lTime;
               char cDTFrm[ 27 ];

               if( s )  /* Internal modifier */
               {
                  for( f = 0; cIntMod[ f ] && cIntMod[ f ] != ' '; f++ )
                  {
                  }
                  if( f != s )
                     cIntMod[ f++ ] = '\0';  /* Date & Time */
               }

               if( HB_IS_TIMESTAMP( pPar ) )
               {
                  hb_itemGetTDT( pPar, &lDate, &lTime );
                  hb_timeStampFormat( cDTFrm,
                                      s ? cIntMod : ( IsType ? "YYYY-MM-DD" :
                                                      hb_setGetDateFormat() ),
                                      s ? cIntMod + f : ( IsType ? "HH:MM:SS" :
                                                      hb_setGetTimeFormat() ),
                                      lDate, lTime );
                  if( s )
                  {
                     if( cIntMod[ 0 ] == 0 )
                        memmove( cDTFrm, cDTFrm + 1, 26 );  /* LTrim 1 space if only Time */
                     else if( cDTFrm[ s ] == ' ' )
                        cDTFrm[ s ] = '\0';                 /* RTrim 1 space if only Date */
                  }
               }
               else
               {
                  char cDTBuf[ 9 ];
                  hb_dateFormat( hb_itemGetDS( pPar, cDTBuf ), cDTFrm,
                                 s ? cIntMod : ( IsType ? "YYYY-MM-DD" : hb_setGetDateFormat() ) );
               }

               /* 27 + 2 if %t and change format time */
               if( ( f = i + HB_MAX( nWidth, 29 ) ) > nBufLen )
               {
                  nBufLen += f + DK_INCBUF;
                  cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
               }
               pParCpy = hb_itemNew( NULL );
               hb_itemPutC( pParCpy, cDTFrm );
               if( IsType )
               {
                  /* Empty DATE, DATETIME print DEFAULT for T converter or DK_EMPTYDATE, DK_EMPTYDATETIME for t converter */
                  if( *cDTFrm == ' ' )
                     hb_itemPutC( pParCpy, HB_IS_TIMESTAMP( pPar ) ?
                                           ( IsType == 2 ? "DEFAULT" : DK_EMPTYDATETIME ) :
                                           ( IsType == 2 ? "DEFAULT" : DK_EMPTYDATE ) );
                  else
                     s_itemUnescape( pParCpy );
               }
               s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pParCpy );
               hb_itemRelease( pParCpy );
            }
            else if( HB_IS_LOGICAL( pPar ) )
            {
               if( s )  /* Internal modifier */
               {
                  for( f = 0; cIntMod[ f ] && cIntMod[ f ] != ','; f++ )
                  {
                  }
                  if( f != s )
                     cIntMod[ f++ ] = '\0';  /* TRUE & FALSE */
               }
               if( iCOut == 's' )
               {
                  hb_itemCopy( pParCpy = hb_itemNew( NULL ), pPar );
                  pPar = pParCpy;
                  hb_itemPutC( pPar,
                               hb_itemGetL( pPar ) ? ( s ? cIntMod : "TRUE" ) :
                                                     ( s ? cIntMod + f : "FALSE" ) );
               }
               if( ( f = i +
                         ( iCOut ==
                           's' ? HB_MAX( nWidth, s ? s : 6 ) :
                                 HB_MAX( nWidth, DK_BLKBUF ) ) ) > nBufLen )
               {
                  nBufLen += f + DK_INCBUF;  /* size of "FALSE" == 6 */
                  cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
               }
               s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pPar );
               if( iCOut == 's' )
                  hb_itemRelease( pPar );

            }
            else if( iCOut == 's' )
            {
               char * cStr = hb_itemStr( pPar, NULL, NULL );

               if( cStr )
               {
                  HB_SIZE nLen = strlen( cStr );
                  const char * cTrimStr = hb_strLTrim( cStr, &nLen );

                  f = nLen;
                  if( ( f = i + HB_MAX( nWidth, f ) ) > nBufLen )
                  {
                     nBufLen += f + DK_INCBUF;
                     cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
                  }
                  pParCpy = hb_itemNew( NULL );
                  hb_itemPutCL( pParCpy, cTrimStr, f );
                  s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pParCpy );
                  hb_itemRelease( pParCpy );
                  hb_xfree( cStr );
               }
               else
               {
                  iErrorPar = p + 2;
                  break;
               }
            }
            else if( HB_IS_NUMERIC( pPar ) || HB_IS_POINTER( pPar ) )
            {
               if( ( f = i + HB_MAX( nWidth, DK_BLKBUF ) ) > nBufLen )
               {
                  nBufLen += f + DK_INCBUF;
                  cBuffer  = ( char * ) hb_xrealloc( cBuffer, nBufLen );
               }
               s = s_ReplPar( cBuffer, nBufLen, cParStr, iCOut, fIsIndW, iIndWidth, fIsIndP, iIndPrec, pPar );
            }
            else
            {
               iErrorPar = p + 2;
               break;
            }
         }

         if( ( f = s + nResPos + sizeof( char ) ) > nMaxRes )
         {
            nMaxRes += f + DK_INCRES;
            cRes     = ( char * ) hb_xrealloc( cRes, nMaxRes );
         }

         hb_strncpy( cRes + nResPos, cBuffer, s );
         nResPos += s;

         if( ( nParPos = ( c - cFrmStr ) ) >= nFrmLen )
            break;  /* No more Par Format */
      }

      if( cIntMod )
         hb_xfree( cIntMod );

      hb_xfree( cParStr );
      hb_xfree( cBuffer );

      if( iErrorPar )
      {
         hb_xfree( cRes );

         if( iErrorPar > 1 )
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 2, hb_paramError( 1 ), hb_paramError( iErrorPar ) );
         else
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
      }
      else
         hb_retclen_buffer( cRes, nResPos );
   }
}

#endif
