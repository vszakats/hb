/* This is an original work by Ralph Oliver (TRANSCOM SYSTEMS)
   and is placed in the public domain.

   This program uses the preprocessor #defines and #command
   by David Husnian.

      Rev 1.1   15 Aug 1991 23:05:22   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   07 Jun 1991 23:03:20   GLENN
   Initial revision.
 */

#define FORCE_BETWEEN( x, y, z )  ( y := Max( Min( y, z ), x ) )

FUNCTION ft_AMedian( aArray, nStart, nEnd )

   LOCAL nTempLen, aTempArray

   __defaultNIL( @nStart, 1 )
   __defaultNIL( @nEnd, Len( aArray ) )

   // Make sure bounds are in range
   FORCE_BETWEEN( 1, nEnd, Len( aArray ) )
   FORCE_BETWEEN( 1, nStart, nEnd )

   // Length of aTempArray
   nTempLen := ( nEnd - nStart ) + 1

   // Initialize and sort aTempArray
   aTempArray := ASort( ACopy( aArray, Array( nTempLen ), nStart, nTempLen ) )

   // Determine middle value(s)
   IF nTempLen % 2 == 0
      RETURN Int( ( ;
         aTempArray[ nTempLen / 2 ] + ;
         aTempArray[ Int( nTempLen / 2 ) + 1 ] ) / 2 )
   ENDIF

   RETURN aTempArray[ Int( nTempLen / 2 ) + 1 ]
