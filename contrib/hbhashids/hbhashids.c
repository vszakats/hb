/*

   libhashids wrapper

 */

#include "hbhashids.h"

#define ULONGLONG    unsigned long long
#define MAX_NUMBERS  256

// HASHIDS_ENCODE( aNumbers | nNumber, [cSalt], [nMinHashLength], [cAlphabet] )
HB_FUNC( HASHIDS_ENCODE )
{
   if( HB_ISARRAY( 1 ) || HB_ISNUM( 1 ) )
   {
      const char * salt = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : HASHIDS_DEFAULT_SALT;
      size_t       minHashLength = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 0;
      const char * alphabet      = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : HASHIDS_DEFAULT_ALPHABET;

      hashids_t * pHash = hashids_init3( salt, minHashLength, alphabet );

      if( pHash )
      {
         size_t numbers_count;

         if( HB_ISNUM( 1 ) )
            numbers_count = 1;
         else       // array
            numbers_count = hb_arrayLen( hb_param( 1, HB_IT_ARRAY ) );

         ULONGLONG numbers[ numbers_count ];

         if( HB_ISNUM( 1 ) )
         {
            numbers[ 0 ] = ( ULONGLONG ) hb_parnll( 1 );
         }
         else       // array
         {
            PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

            for( size_t i = 0; i < numbers_count; ++i )
               numbers[ i ] = ( ULONGLONG ) hb_arrayGetNLL( pArray, i + 1 );
         }

         char * pBuffer = ( char * ) hb_xgrabz( hashids_estimate_encoded_size( pHash, numbers_count, numbers ) );

         size_t bytes_encoded = hashids_encode( pHash, pBuffer, numbers_count, numbers );

         hb_retclen( pBuffer, bytes_encoded );

         hb_xfree( pBuffer );
         hashids_free( pHash );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

// HASHIDS_DECODE( cEncoded, [cSalt], [nMinHashLength], [cAlphabet] ) -> aNumbers
HB_FUNC( HASHIDS_DECODE )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * salt = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : HASHIDS_DEFAULT_SALT;
      size_t       minHashLength = HB_ISNUM( 3 ) ? hb_parni( 3 ) : 0;
      const char * alphabet      = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : HASHIDS_DEFAULT_ALPHABET;

      hashids_t * pHash = hashids_init3( salt, minHashLength, alphabet );

      if( pHash )
      {

         ULONGLONG numbers[ MAX_NUMBERS ];

         size_t numbers_count = hashids_decode( pHash, ( char * ) hb_parc( 1 ), numbers );

         PHB_ITEM pArray = hb_itemArrayNew( numbers_count );

         for( size_t i = 0; i < numbers_count; i++ )
            hb_arraySetNLL( pArray, i + 1, numbers[ i ] );

         hb_itemReturnRelease( pArray );

         hashids_free( pHash );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

}
