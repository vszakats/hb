/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __I_SWAP__
#define __I_SWAP__

#define SYS_LITTLE_ENDIAN

#xtranslate SHORT( <x> ) => ISwapShort( <x> )
#xtranslate LONG( <x> )  => ISwapLong( <x> )

#xtranslate doom_swap_s( <x> ) => ISwapBytes16( <x> )
#xtranslate doom_wtohs( <x> )  => ISwapShort( <x> )

#endif
