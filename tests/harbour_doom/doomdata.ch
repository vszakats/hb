/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __DOOMDATA__
#define __DOOMDATA__

/* Original: doomtype.h, doomdef.h */
#include "doomdef.ch"

#ifndef ML_LABEL
#define ML_LABEL     0
#define ML_THINGS    1
#define ML_LINEDEFS  2
#define ML_SIDEDEFS  3
#define ML_VERTEXES  4
#define ML_SEGS      5
#define ML_SSECTORS  6
#define ML_NODES     7
#define ML_SECTORS   8
#define ML_REJECT    9
#define ML_BLOCKMAP  10
#endif

#define ML_BLOCKING      1
#define ML_BLOCKMONSTERS 2
#define ML_TWOSIDED      4
#define ML_DONTPEGTOP    8
#define ML_DONTPEGBOTTOM 16
#define ML_SECRET        32
#define ML_SOUNDBLOCK    64
#define ML_DONTDRAW      128
#define ML_MAPPED        256

#define NF_SUBSECTOR     32768









#endif
