/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

STATIC s_Fb
STATIC rgb565_palette := {}
STATIC fb_scaling
STATIC palette_changed
STATIC s_video_hb := .F.
STATIC s_HbScreen := ""
STATIC s_hb_frames := 0
STATIC s_showfps := .F.
STATIC s_fps_frames := 0
STATIC s_fps_last_ms := 0
STATIC s_fps_value := 0
STATIC s_fps_total := 0
STATIC s_fps_start_ms := 0
STATIC s_fps_avg := 0

#include "fileio.ch"
#include "doomtype.ch"
#include "doomgeneric.ch"
#include "i_video.ch"

CLASS fb_bitfield_t
    DATA offset
    DATA length
    METHOD New()
ENDCLASS
CLASS fb_screeninfo_t
    DATA xres
    DATA yres
    DATA xres_virtual
    DATA yres_virtual
    DATA bits_per_pixel
    DATA red
    DATA green
    DATA blue
    DATA transp
    METHOD New()
ENDCLASS
CLASS color_t
    DATA b
    DATA g
    DATA r
    DATA a
    METHOD New()
ENDCLASS

#ifndef PU_STATIC
#define PU_STATIC 1
#endif
#ifndef PU_CACHE
#define PU_CACHE 8
#endif

#ifndef INT_MAX
#define INT_MAX 2147483647
#endif

#define GFX_RGB565( r, g, b ) ( ( ( ( (r) & 0xF8 ) / 8 ) * 2048 ) | ( ( ( (g) & 0xFC ) / 4 ) * 32 ) | ( ( (b) & 0xF8 ) / 8 ) )
#define GFX_RGB565_R( color ) Int( ( ( 0xF800 & (color) ) ) / 2048 )
#define GFX_RGB565_G( color ) Int( ( ( 0x07E0 & (color) ) ) / 32 )
#define GFX_RGB565_B( color ) ( ( 0x001F & (color) ) )



METHOD New() CLASS fb_bitfield_t
    ::offset := 0
    ::length := 0
RETURN Self

METHOD New() CLASS fb_screeninfo_t
    ::xres := 0
    ::yres := 0
    ::xres_virtual := 0
    ::yres_virtual := 0
    ::bits_per_pixel := 0
    ::red := fb_bitfield_t():New()
    ::green := fb_bitfield_t():New()
    ::blue := fb_bitfield_t():New()
    ::transp := fb_bitfield_t():New()
RETURN Self

METHOD New() CLASS color_t
    ::b := 0
    ::g := 0
    ::r := 0
    ::a := 0
RETURN Self

INIT PROCEDURE init_i_video
    LOCAL i

    PUBLIC usemouse
    PUBLIC colors
    PUBLIC I_VideoBuffer
    PUBLIC screensaver_mode
    PUBLIC screenvisible
    PUBLIC mouse_acceleration
    PUBLIC mouse_threshold
    PUBLIC usegamma

    fb_scaling := 1
    s_video_hb := I_VIDEO_HARBOUR
    s_HbScreen := ""
    s_hb_frames := 0
    usemouse := 0
    palette_changed := .F.
    I_VideoBuffer := NIL
    screensaver_mode := .F.
    screenvisible := .F.
    mouse_acceleration := 2.0
    mouse_threshold := 10
    usegamma := 0

    s_Fb := fb_screeninfo_t():New()
    colors := {}
    rgb565_palette := {}
    FOR i := 1 TO 256
        AAdd( colors, color_t():New() )
        AAdd( rgb565_palette, 0 )
    NEXT
RETURN

FUNCTION I_InitGraphics()
    LOCAL i
    LOCAL gfxmodeparm
    LOCAL cMode
    MEMVAR I_VideoBuffer
    MEMVAR myargv
    MEMVAR screenvisible

    s_Fb := fb_screeninfo_t():New()
    s_Fb:xres := DOOMGENERIC_RESX
    s_Fb:yres := DOOMGENERIC_RESY
    s_Fb:xres_virtual := s_Fb:xres
    s_Fb:yres_virtual := s_Fb:yres

#ifdef CMAP256
    s_Fb:bits_per_pixel := 8
#else
    gfxmodeparm := M_CheckParmWithArgs( "-gfxmode", 1 )
    IF gfxmodeparm != 0
        cMode := myargv[ gfxmodeparm + 1 + 1 ]
    ELSE
        cMode := "rgba8888"
    ENDIF

    IF cMode == "rgba8888"
        s_Fb:bits_per_pixel := 32
        s_Fb:blue:length := 8
        s_Fb:green:length := 8
        s_Fb:red:length := 8
        s_Fb:transp:length := 8
        s_Fb:blue:offset := 0
        s_Fb:green:offset := 8
        s_Fb:red:offset := 16
        s_Fb:transp:offset := 24
    ELSEIF cMode == "rgb565"
        s_Fb:bits_per_pixel := 16
        s_Fb:blue:length := 5
        s_Fb:green:length := 6
        s_Fb:red:length := 5
        s_Fb:transp:length := 0
        s_Fb:blue:offset := 11
        s_Fb:green:offset := 5
        s_Fb:red:offset := 0
        s_Fb:transp:offset := 16
    ELSE
        I_Error( "Unknown gfxmode value: " + cMode + hb_eol() )
    ENDIF
#endif

    OutStd( "I_InitGraphics: framebuffer: x_res: " + hb_ntos( s_Fb:xres ) + ;
        ", y_res: " + hb_ntos( s_Fb:yres ) + ;
        ", x_virtual: " + hb_ntos( s_Fb:xres_virtual ) + ;
        ", y_virtual: " + hb_ntos( s_Fb:yres_virtual ) + ;
        ", bpp: " + hb_ntos( s_Fb:bits_per_pixel ) + hb_eol() )

    OutStd( "I_InitGraphics: framebuffer: RGBA: " + ;
        hb_ntos( s_Fb:red:length ) + hb_ntos( s_Fb:green:length ) + ;
        hb_ntos( s_Fb:blue:length ) + hb_ntos( s_Fb:transp:length ) + ;
        ", red_off: " + hb_ntos( s_Fb:red:offset ) + ;
        ", green_off: " + hb_ntos( s_Fb:green:offset ) + ;
        ", blue_off: " + hb_ntos( s_Fb:blue:offset ) + ;
        ", transp_off: " + hb_ntos( s_Fb:transp:offset ) + hb_eol() )

    OutStd( "I_InitGraphics: DOOM screen size: w x h: " + ;
        hb_ntos( SCREENWIDTH ) + " x " + hb_ntos( SCREENHEIGHT ) + hb_eol() )

    IF M_ParmExists( "-videoc" )
        s_video_hb := .F.
    ENDIF
    s_showfps := M_ParmExists( "-fps" )
    IF M_ParmExists( "-neogeo" )
        IVideoSetMaxColors( 75 )
        IVideoSetShadePct( 75 )
        IVideoSetGrayPct( 20 )
        OutStd( "I_InitGraphics: neo geo preset" + hb_eol() )
    ENDIF
    i := M_CheckParmWithArgs( "-colors", 1 )
    IF i > 0
        IVideoSetMaxColors( Int( Val( myargv[ i + 1 + 1 ] ) ) )
    ENDIF
    i := M_CheckParmWithArgs( "-shades", 1 )
    IF i > 0
        IVideoSetShadePct( Int( Val( myargv[ i + 1 + 1 ] ) ) )
    ENDIF
    i := M_CheckParmWithArgs( "-gray", 1 )
    IF i > 0
        IVideoSetGrayPct( Int( Val( myargv[ i + 1 + 1 ] ) ) )
    ENDIF
    OutStd( "I_InitGraphics: video path: " + iif( s_video_hb, "Harbour", "C" ) + hb_eol() )
    IF s_showfps
        OutStd( "I_InitGraphics: FPS overlay: on" + hb_eol() )
    ENDIF
    IF IVideoGetMaxColors() < 256
        OutStd( "I_InitGraphics: palette quantized to " + ;
            hb_ntos( IVideoGetMaxColors() ) + " colors, shades " + ;
            hb_ntos( IVideoGetShadePct() ) + "%, gray " + ;
            hb_ntos( IVideoGetGrayPct() ) + "%" + hb_eol() )
    ENDIF

    i := M_CheckParmWithArgs( "-scaling", 1 )
    IF i > 0
        fb_scaling := Int( Val( myargv[ i + 1 + 1 ] ) )
        OutStd( "I_InitGraphics: Scaling factor: " + hb_ntos( fb_scaling ) + hb_eol() )
    ELSE
        fb_scaling := Int( s_Fb:xres / SCREENWIDTH )
        IF Int( s_Fb:yres / SCREENHEIGHT ) < fb_scaling
            fb_scaling := Int( s_Fb:yres / SCREENHEIGHT )
        ENDIF
        OutStd( "I_InitGraphics: Auto-scaling factor: " + hb_ntos( fb_scaling ) + hb_eol() )
    ENDIF

    I_VideoBuffer := Replicate( Chr( 0 ), SCREENWIDTH * SCREENHEIGHT )
    screenvisible := .T.
    I_SetPalette( W_CacheLumpName( "PLAYPAL", PU_CACHE ) )
    I_InitInput()
RETURN NIL

FUNCTION I_ShutdownGraphics()
    MEMVAR I_VideoBuffer
    I_VideoBuffer := NIL
RETURN NIL

FUNCTION I_StartFrame()
RETURN NIL

FUNCTION I_StartTic()
    I_GetEvent()
RETURN NIL

FUNCTION I_UpdateNoBlit()
RETURN NIL

STATIC PROCEDURE IVideoSetPaletteHb( palette, aGamma )
    LOCAL i
    LOCAL n
    LOCAL nR
    LOCAL nG
    LOCAL nB
    LOCAL cRaw
    MEMVAR colors

    IF ValType( palette ) != "C" .OR. ValType( aGamma ) != "A"
        RETURN
    ENDIF

    cRaw := ""
    n := 1
    FOR i := 1 TO 256
        nR := aGamma[ Asc( SubStr( palette, n, 1 ) ) + 1 ]
        nG := aGamma[ Asc( SubStr( palette, n + 1, 1 ) ) + 1 ]
        nB := aGamma[ Asc( SubStr( palette, n + 2, 1 ) ) + 1 ]
        n += 3
        colors[ i ]:a := 0
        colors[ i ]:r := nR
        colors[ i ]:g := nG
        colors[ i ]:b := nB
        cRaw += Chr( nR ) + Chr( nG ) + Chr( nB )
    NEXT
    IVideoSetPaletteRaw( cRaw )
RETURN

STATIC PROCEDURE IVideoFinishUpdateHb( nScale, nXres, nYres, nBpp, cSrc )
    LOCAL nXOff
    LOCAL nXOffEnd
    LOCAL nY
    LOCAL nI
    LOCAL nJ
    LOCAL cLine
    LOCAL cScaled
    LOCAL cPadL
    LOCAL cPadR
    LOCAL nH

    HB_SYMBOL_UNUSED( nYres )

    IF ValType( cSrc ) != "C" .OR. nScale < 1
        RETURN
    ENDIF

    nXOff := Int( ( ( nXres - ( SCREENWIDTH * nScale ) ) * nBpp / 8 ) / 2 )
    nXOffEnd := ( ( nXres - ( SCREENWIDTH * nScale ) ) * nBpp / 8 ) - nXOff
    IF nXOff < 0
        nXOff := 0
    ENDIF
    IF nXOffEnd < 0
        nXOffEnd := 0
    ENDIF
    cPadL := Replicate( Chr( 0 ), nXOff )
    cPadR := Replicate( Chr( 0 ), nXOffEnd )
    s_HbScreen := ""

    FOR nY := 0 TO SCREENHEIGHT - 1
        cLine := SubStr( cSrc, nY * SCREENWIDTH + 1, SCREENWIDTH )
        IF nScale == 1
            cScaled := cLine
        ELSE
            cScaled := ""
            FOR nJ := 1 TO SCREENWIDTH
                cScaled += Replicate( SubStr( cLine, nJ, 1 ), nScale )
            NEXT
        ENDIF
        FOR nI := 1 TO nScale
            s_HbScreen += cPadL + cScaled + cPadR
        NEXT
    NEXT
    s_hb_frames++
    IF s_hb_frames == 1 .OR. ( s_hb_frames % 70 ) == 0
        OutStd( "I_FinishUpdate Harbour: frames=" + hb_ntos( s_hb_frames ) + ;
            " dest=" + hb_ntos( Len( s_HbScreen ) ) + " bytes" + hb_eol() )
        IF s_hb_frames == 1
            nH := FCreate( "videohb.log" )
            IF nH != F_ERROR
                FWrite( nH, "Harbour I_FinishUpdate ativo" + hb_eol() )
                FClose( nH )
            ENDIF
        ENDIF
    ENDIF
RETURN

STATIC PROCEDURE I_DrawFps()
    LOCAL nNow
    LOCAL nSpan
    LOCAL cText
    LOCAL nX
    MEMVAR hu_font

    IF ! s_showfps
        RETURN
    ENDIF

    nNow := I_GetTimeMS()
    IF s_fps_last_ms == 0
        s_fps_last_ms := nNow
        s_fps_start_ms := nNow
    ENDIF
    s_fps_frames++
    IF ( ( nNow - s_fps_last_ms ) & 0xFFFFFFFF ) >= 1000
        s_fps_value := s_fps_frames
        s_fps_total += s_fps_frames
        nSpan := ( ( nNow - s_fps_start_ms ) & 0xFFFFFFFF )
        IF nSpan > 0
            s_fps_avg := Int( s_fps_total * 1000 / nSpan )
        ENDIF
        s_fps_frames := 0
        s_fps_last_ms := nNow
    ENDIF

    IF ValType( hu_font ) != "A" .OR. Len( hu_font ) < 1
        RETURN
    ENDIF

    cText := hb_ntos( s_fps_value ) + " FPS  " + hb_ntos( s_fps_avg ) + " AVG"
    nX := SCREENWIDTH - M_StringWidth( cText ) - 2
    IF nX < 0
        nX := 0
    ENDIF
    M_WriteText( nX, 1, cText )
RETURN

FUNCTION I_FinishUpdate()
    I_DrawFps()
    DG_DrawFrame()
RETURN NIL

FUNCTION I_ReadScreen( scr )
    LOCAL cBuf
    MEMVAR I_VideoBuffer

    cBuf := iif( ValType( I_VideoBuffer ) == "C", I_VideoBuffer, Replicate( Chr( 0 ), SCREENWIDTH * SCREENHEIGHT ) )
    IF ValType( scr ) == "C"
        IF Len( scr ) > Len( cBuf )
            scr := cBuf + SubStr( scr, Len( cBuf ) + 1 )
        ELSE
            scr := cBuf
        ENDIF
    ELSE
        scr := cBuf
    ENDIF
RETURN scr

FUNCTION I_SetPalette( palette )
    MEMVAR usegamma
    LOCAL nG := usegamma
    IF nG == NIL .OR. nG < 0
        nG := 0
    ENDIF
    IF nG > 4
        nG := 4
    ENDIF
    IF s_video_hb
        IVideoSetPaletteHb( palette, GammaTableRow( nG ) )
    ELSE
        IVideoSetPalette( palette, GammaTableRow( nG ) )
    ENDIF
    palette_changed := .T.
RETURN NIL

FUNCTION I_GetPaletteIndex( r, g, b )
    LOCAL nBest
    LOCAL nBestDiff
    LOCAL nDiff
    LOCAL i
    LOCAL nR
    LOCAL nG
    LOCAL nB

    OutStd( "I_GetPaletteIndex" + hb_eol() )

    nBest := 0
    nBestDiff := INT_MAX
    FOR i := 0 TO 255
        nR := GFX_RGB565_R( rgb565_palette[ i + 1 ] )
        nG := GFX_RGB565_G( rgb565_palette[ i + 1 ] )
        nB := GFX_RGB565_B( rgb565_palette[ i + 1 ] )
        nDiff := ( r - nR ) * ( r - nR ) + ( g - nG ) * ( g - nG ) + ( b - nB ) * ( b - nB )
        IF nDiff < nBestDiff
            nBest := i
            nBestDiff := nDiff
        ENDIF
        IF nDiff == 0
            EXIT
        ENDIF
    NEXT
RETURN nBest

FUNCTION I_BeginRead()
RETURN NIL

FUNCTION I_EndRead()
RETURN NIL

FUNCTION I_SetWindowTitle( title )
    DG_SetWindowTitle( title )
RETURN NIL

FUNCTION I_GraphicsCheckCommandLine()
RETURN NIL

FUNCTION I_SetGrabMouseCallback( func )
RETURN NIL

FUNCTION I_EnableLoadingDisk()
RETURN NIL

FUNCTION I_BindVideoVariables()
RETURN NIL

FUNCTION I_DisplayFPSDots( dots_on )
RETURN NIL

FUNCTION I_CheckIsScreensaver()
RETURN NIL

FUNCTION I_InitWindowTitle()
RETURN NIL

FUNCTION I_InitWindowIcon()
RETURN NIL

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#include <string.h>

#ifndef CMAP256
#define CMAP256 1
#endif
#ifndef SCREENWIDTH
#define SCREENWIDTH  320
#endif
#ifndef SCREENHEIGHT
#define SCREENHEIGHT 200
#endif

typedef unsigned char byte;
typedef int boolean;

struct color
{
   unsigned char b;
   unsigned char g;
   unsigned char r;
   unsigned char a;
};

extern unsigned char *DG_ScreenBuffer;

boolean palette_changed;
struct color colors[ 256 ];

static int s_max_colors = 256;
/* Percentual das cores livres gasto em rampas de luz (chao/teto).
   0 = so farthest-point. */
static int s_shade_pct = 75;
/* Quanto as rampas (cinza/marrom/oliva/azul) caminham para o cinza.
   0 deixa o tom original. O verde vivo (HUD, sangue, lodo) nao entra. */
static int s_gray_pct = 20;

static int ColorDist2( int r1, int g1, int b1, int r2, int g2, int b2 )
{
   int dr = r1 - r2;
   int dg = g1 - g2;
   int db = b1 - b2;

   return dr * dr + dg * dg + db * db;
}

/* 0..2550, peso maior no verde (aproxima luminance) */
static int ColorLum( int r, int g, int b )
{
   return r * 3 + g * 6 + b;
}

static int ColorSat( int r, int g, int b );
static int ToneBin( int r, int g, int b );

static int AddFarthest( const unsigned char *src_r, const unsigned char *src_g,
                        const unsigned char *src_b, int *pal_r, int *pal_g,
                        int *pal_b, int nChosen, int nWant, int nMaxLum,
                        int chromaOnly )
{
   int i;
   int k;
   int d;
   int min_d;
   int best_i;
   int best_d;

   while( nChosen < nWant )
   {
      best_i = -1;
      best_d = -1;
      for( i = 1; i < 256; i++ )
      {
         if( nMaxLum >= 0 && ColorLum( src_r[ i ], src_g[ i ], src_b[ i ] ) > nMaxLum )
            continue;
         if( chromaOnly && ToneBin( src_r[ i ], src_g[ i ], src_b[ i ] ) >= 0 )
            continue;
         min_d = 0x7fffffff;
         for( k = 0; k < nChosen; k++ )
         {
            d = ColorDist2( src_r[ i ], src_g[ i ], src_b[ i ],
                            pal_r[ k ], pal_g[ k ], pal_b[ k ] );
            if( d < min_d )
               min_d = d;
         }
         if( min_d > best_d )
         {
            best_d = min_d;
            best_i = i;
         }
      }
      if( best_i < 0 || best_d <= 0 )
         break;
      pal_r[ nChosen ] = src_r[ best_i ];
      pal_g[ nChosen ] = src_g[ best_i ];
      pal_b[ nChosen ] = src_b[ best_i ];
      nChosen++;
   }
   return nChosen;
}

static int ColorSat( int r, int g, int b )
{
   int mx = r;
   int mn = r;

   if( g > mx )
      mx = g;
   if( b > mx )
      mx = b;
   if( g < mn )
      mn = g;
   if( b < mn )
      mn = b;
   return mx - mn;
}

/* 0 cinza, 1 marrom/bege, 2 oliva, 3 azul acinzentado.
   -1 fica para o bloco cromatico (sangue, HUD, ceu vivo). */
static int ToneBin( int r, int g, int b )
{
   int sat = ColorSat( r, g, b );

   if( sat <= 28 )
      return 0;
   if( r > g + 28 && r > b + 28 && ( g * 2 ) < r )
      return -1;
   if( g > r + 28 && g > b + 28 && sat > 64 )
      return -1;
   if( b > r + 24 && b > g + 16 && sat > 56 )
      return -1;
   if( b >= r && b >= g )
      return 3;
   if( g + 6 >= r && g >= b )
      return 2;
   if( r >= b && g + 8 >= b )
      return 1;
   return -1;
}

/* Escolhe degraus de luz dentro de um tom. A distancia e luminancia,
   para o colormap do chao/teto nao cair no mesmo RGB. */
static int AddToneRamp( const unsigned char *src_r, const unsigned char *src_g,
                        const unsigned char *src_b, int *pal_r, int *pal_g,
                        int *pal_b, int nChosen, int nStop, int bin )
{
   int i;
   int k;
   int d;
   int dl;
   int min_d;
   int best_i;
   int best_d;
   int lum_i;

   while( nChosen < nStop )
   {
      best_i = -1;
      best_d = -1;
      for( i = 1; i < 256; i++ )
      {
         if( ToneBin( src_r[ i ], src_g[ i ], src_b[ i ] ) != bin )
            continue;
         lum_i = ColorLum( src_r[ i ], src_g[ i ], src_b[ i ] );
         min_d = 0x7fffffff;
         for( k = 0; k < nChosen; k++ )
         {
            dl = lum_i - ColorLum( pal_r[ k ], pal_g[ k ], pal_b[ k ] );
            if( dl < 0 )
               dl = -dl;
            d = dl * dl;
            d += ColorDist2( src_r[ i ], src_g[ i ], src_b[ i ],
                             pal_r[ k ], pal_g[ k ], pal_b[ k ] ) / 64;
            if( d < min_d )
               min_d = d;
         }
         if( min_d > best_d )
         {
            best_d = min_d;
            best_i = i;
         }
      }
      /* degrau curto demais nao separa o chao; a vaga volta ao croma */
      if( best_i < 0 || best_d <= 400 )
         break;
      pal_r[ nChosen ] = src_r[ best_i ];
      pal_g[ nChosen ] = src_g[ best_i ];
      pal_b[ nChosen ] = src_b[ best_i ];
      nChosen++;
   }
   return nChosen;
}

/* Reduz colors[] a no maximo nWant RGBs distintos.
   Indice 0 fica reservado. s_shade_pct % das demais vagas viram
   rampas de luz (cinza, marrom, oliva, azul). O resto e
   farthest-point nas cores vivas. Cinza quase neutro prefere
   alvo pouco saturado. O resultado e escurecido em 5%. */
static void QuantizeColors( int nWant )
{
   unsigned char src_r[ 256 ];
   unsigned char src_g[ 256 ];
   unsigned char src_b[ 256 ];
   int pal_r[ 256 ];
   int pal_g[ 256 ];
   int pal_b[ 256 ];
   int quota[ 4 ];
   int nChosen;
   int nFree;
   int nShade;
   int left;
   int used;
   int bin;
   int i;
   int k;
   int best_i;
   int best_d;
   int d;
   int src_lum;
   int src_sat;
   int lift;
   int sat_d;

   if( nWant < 2 )
      nWant = 2;
   if( nWant >= 256 )
      return;

   for( i = 0; i < 256; i++ )
   {
      src_r[ i ] = colors[ i ].r;
      src_g[ i ] = colors[ i ].g;
      src_b[ i ] = colors[ i ].b;
   }

   pal_r[ 0 ] = src_r[ 0 ];
   pal_g[ 0 ] = src_g[ 0 ];
   pal_b[ 0 ] = src_b[ 0 ];
   nChosen = 1;
   nFree = nWant - 1;
   nShade = ( nFree * s_shade_pct ) / 100;
   if( nShade > nFree )
      nShade = nFree;
   if( nShade < 0 )
      nShade = 0;

   if( nShade > 0 )
   {
      /* 45% cinza, 40% marrom, 5% oliva, 10% azul.
         Oliva baixo: com shades alto ele puxava a cena para o verde. */
      for( bin = 0; bin < 4; bin++ )
         quota[ bin ] = 0;
      left = nShade;
      if( nShade >= 4 )
      {
         quota[ 0 ] = quota[ 1 ] = quota[ 2 ] = quota[ 3 ] = 1;
         left = nShade - 4;
      }
      quota[ 0 ] += ( left * 45 ) / 100;
      quota[ 1 ] += ( left * 40 ) / 100;
      quota[ 2 ] += ( left * 5 ) / 100;
      quota[ 3 ] += ( left * 10 ) / 100;
      used = quota[ 0 ] + quota[ 1 ] + quota[ 2 ] + quota[ 3 ];
      quota[ 0 ] += nShade - used;
      for( bin = 0; bin < 4; bin++ )
         nChosen = AddToneRamp( src_r, src_g, src_b, pal_r, pal_g, pal_b,
                                nChosen, nChosen + quota[ bin ], bin );
   }

   nChosen = AddFarthest( src_r, src_g, src_b, pal_r, pal_g, pal_b,
                          nChosen, nWant, -1, nShade > 0 );
   if( nChosen < nWant )
      nChosen = AddFarthest( src_r, src_g, src_b, pal_r, pal_g, pal_b,
                             nChosen, nWant, -1, 0 );

   for( i = 0; i < 256; i++ )
   {
      src_lum = ColorLum( src_r[ i ], src_g[ i ], src_b[ i ] );
      src_sat = ColorSat( src_r[ i ], src_g[ i ], src_b[ i ] );
      best_i = 0;
      best_d = 0x7fffffff;
      for( k = 0; k < nChosen; k++ )
      {
         d = ColorDist2( src_r[ i ], src_g[ i ], src_b[ i ],
                         pal_r[ k ], pal_g[ k ], pal_b[ k ] );
         lift = ColorLum( pal_r[ k ], pal_g[ k ], pal_b[ k ] ) - src_lum;
         /* so quase-preto: cinza de chao usa vizinho RGB */
         if( src_lum <= 250 && lift > 240 )
            d += ( lift * lift ) / 16;
         /* com rampas, o pixel fica no degrau de luz mais proximo */
         if( nShade > 0 )
            d += ( lift * lift ) / 48;
         sat_d = ColorSat( pal_r[ k ], pal_g[ k ], pal_b[ k ] ) - src_sat;
         if( src_sat <= 24 && sat_d > 16 )
            d += sat_d * sat_d;
         if( d < best_d )
         {
            best_d = d;
            best_i = k;
            if( d == 0 )
               break;
         }
      }
      /* 5% mais escuro: o farthest-point tende a privilegiar midtones claros.
         Rampas perdem o excesso de verde e caminham para o cinza.
         Sangue, HUD e lodo ficam de fora. */
      {
         int nR = ( pal_r[ best_i ] * 95 ) / 100;
         int nG = ( pal_g[ best_i ] * 95 ) / 100;
         int nB = ( pal_b[ best_i ] * 95 ) / 100;
         int nGray;
         int nMid;
         int nKeep;

         if( nShade > 0 && s_gray_pct > 0 &&
             ToneBin( pal_r[ best_i ], pal_g[ best_i ], pal_b[ best_i ] ) >= 0 )
         {
            nMid = ( nR + nB ) / 2;
            if( nG > nMid )
            {
               nKeep = 100 - s_gray_pct;
               nG = nMid + ( ( nG - nMid ) * nKeep ) / 100;
            }
            nGray = ( nR + nG + nB ) / 3;
            nKeep = 100 - s_gray_pct;
            nR = ( nR * nKeep + nGray * s_gray_pct ) / 100;
            nG = ( nG * nKeep + nGray * s_gray_pct ) / 100;
            nB = ( nB * nKeep + nGray * s_gray_pct ) / 100;
         }
         else if( nShade == 0 &&
                  ColorDist2( pal_r[ best_i ], pal_g[ best_i ], pal_b[ best_i ], 19, 34, 11 ) <= 625 )
         {
            nGray = ( ( nR + nG + nB ) / 3 * 85 ) / 100;
            nR = ( nR + nGray * 2 ) / 3;
            nG = ( nG + nGray * 2 ) / 3;
            nB = ( nB + nGray * 2 ) / 3;
         }
         colors[ i ].r = ( unsigned char ) nR;
         colors[ i ].g = ( unsigned char ) nG;
         colors[ i ].b = ( unsigned char ) nB;
      }
   }
}

HB_FUNC( IVIDEOSETMAXCOLORS )
{
   int n = hb_parni( 1 );

   if( n < 2 )
      n = 2;
   if( n > 256 )
      n = 256;
   s_max_colors = n;
}

HB_FUNC( IVIDEOGETMAXCOLORS )
{
   hb_retni( s_max_colors );
}

HB_FUNC( IVIDEOSETSHADEPCT )
{
   int n = hb_parni( 1 );

   if( n < 0 )
      n = 0;
   if( n > 100 )
      n = 100;
   s_shade_pct = n;
}

HB_FUNC( IVIDEOGETSHADEPCT )
{
   hb_retni( s_shade_pct );
}

HB_FUNC( IVIDEOSETGRAYPCT )
{
   int n = hb_parni( 1 );

   if( n < 0 )
      n = 0;
   if( n > 100 )
      n = 100;
   s_gray_pct = n;
}

HB_FUNC( IVIDEOGETGRAYPCT )
{
   hb_retni( s_gray_pct );
}

HB_FUNC( IVIDEOSETPALETTE )
{
   const byte *palette;
   PHB_ITEM pGamma;
   int i;

   palette = ( const byte * ) hb_parc( 1 );
   pGamma = hb_param( 2, HB_IT_ARRAY );
   if( palette == NULL || pGamma == NULL )
      return;

   for( i = 0; i < 256; ++i )
   {
      colors[ i ].a = 0;
      colors[ i ].r = (byte) hb_arrayGetNI( pGamma, ( HB_USHORT ) *palette++ + 1 );
      colors[ i ].g = (byte) hb_arrayGetNI( pGamma, ( HB_USHORT ) *palette++ + 1 );
      colors[ i ].b = (byte) hb_arrayGetNI( pGamma, ( HB_USHORT ) *palette++ + 1 );
   }
   QuantizeColors( s_max_colors );
#ifdef CMAP256
   palette_changed = 1;
#endif
}

HB_FUNC( IVIDEOSETPALETTERAW )
{
   const byte *p;
   int i;

   p = ( const byte * ) hb_parc( 1 );
   if( p == NULL || hb_parclen( 1 ) < 768 )
      return;

   for( i = 0; i < 256; ++i )
   {
      colors[ i ].r = *p++;
      colors[ i ].g = *p++;
      colors[ i ].b = *p++;
      colors[ i ].a = 0;
   }
   QuantizeColors( s_max_colors );
#ifdef CMAP256
   palette_changed = 1;
#endif
}

HB_FUNC( IVIDEOFINISHUPDATE )
{
   int fb_scaling;
   int xres;
   int yres;
   int bpp;
   const unsigned char *src;
   unsigned char *line_in;
   unsigned char *line_out;
   int x_offset;
   int x_offset_end;
   int y;

   fb_scaling = hb_parni( 1 );
   xres = hb_parni( 2 );
   yres = hb_parni( 3 );
   bpp = hb_parni( 4 );
   src = ( const unsigned char * ) hb_parc( 5 );

   if( src == NULL || DG_ScreenBuffer == NULL || fb_scaling < 1 )
      return;

   x_offset = ( ( ( xres - ( SCREENWIDTH * fb_scaling ) ) * bpp / 8 ) ) / 2;
   x_offset_end = ( ( xres - ( SCREENWIDTH * fb_scaling ) ) * bpp / 8 ) - x_offset;

   line_in = ( unsigned char * ) src;
   line_out = ( unsigned char * ) DG_ScreenBuffer;
   y = SCREENHEIGHT;

   while( y-- )
   {
      int i;
      for( i = 0; i < fb_scaling; i++ )
      {
         line_out += x_offset;
#ifdef CMAP256
         if( fb_scaling == 1 )
         {
            memcpy( line_out, line_in, SCREENWIDTH );
         }
         else
         {
            int j;
            for( j = 0; j < SCREENWIDTH; j++ )
            {
               int k;
               for( k = 0; k < fb_scaling; k++ )
                  line_out[ j * fb_scaling + k ] = line_in[ j ];
            }
         }
#else
         ( void ) bpp;
         ( void ) yres;
#endif
         line_out += ( SCREENWIDTH * fb_scaling * ( bpp / 8 ) ) + x_offset_end;
      }
      line_in += SCREENWIDTH;
   }
}

#pragma ENDDUMP
