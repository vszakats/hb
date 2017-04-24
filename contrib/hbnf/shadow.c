#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_SHADOW )
{
   hb_gtDrawShadow( hb_parni( 1 ),
                    hb_parni( 2 ),
                    hb_parni( 3 ),
                    hb_parni( 4 ),
                    hb_parnidef( 5, 8 ) );
}
