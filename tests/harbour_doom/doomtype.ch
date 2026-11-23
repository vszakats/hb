/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __DOOMTYPE__
#define __DOOMTYPE__

#ifndef CMAP256
#define CMAP256 1
#endif

#define strcasecmp  hb_stricmp
#define strncasecmp hb_strnicmp

#define PACKEDATTR

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR   hb_ps()
#define DIR_SEPARATOR_S hb_ps()
#ifdef __PLATFORM__WINDOWS
#define PATH_SEPARATOR  ";"
#else
#define PATH_SEPARATOR  ":"
#endif
#endif

#ifndef arrlen
#define arrlen( array ) Len( array )
#endif

#endif
