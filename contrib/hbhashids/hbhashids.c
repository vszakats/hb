/* libhashids wrapper */

#include "hashids.h"

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#define MAX_NUMBERS  256

/* hashids_encode( aNumbers | nNumber, [cSalt], [nMinHashLength], [cAlphabet] ) */
HB_FUNC( HASHIDS_ENCODE )
{
   if( HB_ISARRAY( 1 ) || HB_ISNUM( 1 ) )
   {
      const char * salt = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : HASHIDS_DEFAULT_SALT;
      HB_SIZE      minHashLength = hb_parns( 3 );
      const char * alphabet      = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : HASHIDS_DEFAULT_ALPHABET;

      hashids_t * pHash = hashids_init3( salt, ( size_t ) minHashLength, alphabet );

      if( pHash )
      {
         HB_SIZE numbers_count;
         HB_ULONGLONG numbers[ MAX_NUMBERS ];
         char * pBuffer;
         HB_SIZE bytes_encoded;

         if( HB_ISNUM( 1 ) )
         {
            numbers_count = 1;
            numbers[ 0 ] = ( HB_ULONGLONG ) hb_parnll( 1 );
         }
         else  /* array */
         {
            PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

            numbers_count = hb_arrayLen( hb_param( 1, HB_IT_ARRAY ) );
            if( numbers_count > HB_SIZEOFARRAY( numbers ) )
               numbers_count = HB_SIZEOFARRAY( numbers );

            for( size_t i = 0; i < numbers_count; ++i )
               numbers[ i ] = ( HB_ULONGLONG ) hb_arrayGetNLL( pArray, i + 1 );
         }

         pBuffer = ( char * ) hb_xgrabz( hashids_estimate_encoded_size( pHash, ( size_t ) numbers_count, numbers ) + 1 );
         bytes_encoded = hashids_encode( pHash, pBuffer, ( size_t ) numbers_count, numbers );

         hb_retclen_buffer( pBuffer, bytes_encoded );

         hashids_free( pHash );
      }
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hashids_decode( cEncoded, [cSalt], [nMinHashLength], [cAlphabet] ) -> aNumbers */
HB_FUNC( HASHIDS_DECODE )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * salt = HB_ISCHAR( 2 ) ? hb_parc( 2 ) : HASHIDS_DEFAULT_SALT;
      HB_SIZE      minHashLength = hb_parns( 3 );
      const char * alphabet      = HB_ISCHAR( 4 ) ? hb_parc( 4 ) : HASHIDS_DEFAULT_ALPHABET;

      hashids_t * pHash = hashids_init3( salt, ( size_t ) minHashLength, alphabet );

      if( pHash )
      {
         HB_ULONGLONG numbers[ MAX_NUMBERS ];
         HB_SIZE numbers_count = ( HB_SIZE ) hashids_decode( pHash, ( char * ) hb_parc( 1 ), numbers );
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
