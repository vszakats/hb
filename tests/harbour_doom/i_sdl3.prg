/*
DOOM generic portado para Harbour com janela em SDL3.
Por Wagner Nunes da Silva
vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com
www.vagucs.com.br
*/

#pragma BEGINDUMP
#include "hbapi.h"
#include "hbapiitm.h"
#include <stdlib.h>
#include <string.h>

#ifndef __has_include
#define __has_include(x) 0
#endif
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_extension
#define __has_extension(x) 0
#endif

#define SDL_MAIN_HANDLED
#include <SDL3/SDL.h>

#define SDL_DOOM_W 320
#define SDL_DOOM_H 200

#define DK_RIGHTARROW  0xae
#define DK_LEFTARROW   0xac
#define DK_UPARROW     0xad
#define DK_DOWNARROW   0xaf
#define DK_STRAFE_L    0xa0
#define DK_STRAFE_R    0xa1
#define DK_USE         0xa2
#define DK_FIRE        0xa3
#define DK_ESCAPE      27
#define DK_ENTER       13
#define DK_TAB         9
#define DK_F1          ( 0x80 + 0x3b )
#define DK_F2          ( 0x80 + 0x3c )
#define DK_F3          ( 0x80 + 0x3d )
#define DK_F4          ( 0x80 + 0x3e )
#define DK_F5          ( 0x80 + 0x3f )
#define DK_F6          ( 0x80 + 0x40 )
#define DK_F7          ( 0x80 + 0x41 )
#define DK_F8          ( 0x80 + 0x42 )
#define DK_F9          ( 0x80 + 0x43 )
#define DK_F10         ( 0x80 + 0x44 )
#define DK_F11         ( 0x80 + 0x57 )
#define DK_F12         ( 0x80 + 0x58 )
#define DK_BACKSPACE   0x7f
#define DK_PAUSE       0xff
#define DK_EQUALS      0x3d
#define DK_MINUS       0x2d
#define DK_RSHIFT      ( 0x80 + 0x36 )
#define DK_RCTRL       ( 0x80 + 0x1d )
#define DK_RALT        ( 0x80 + 0x38 )
#define DK_CAPSLOCK    ( 0x80 + 0x3a )
#define DK_NUMLOCK     ( 0x80 + 0x45 )
#define DK_SCRLCK      ( 0x80 + 0x46 )
#define DK_PRTSCR      ( 0x80 + 0x59 )
#define DK_HOME        ( 0x80 + 0x47 )
#define DK_END         ( 0x80 + 0x4f )
#define DK_PGUP        ( 0x80 + 0x49 )
#define DK_PGDN        ( 0x80 + 0x51 )
#define DK_INS         ( 0x80 + 0x52 )
#define DK_DEL         ( 0x80 + 0x53 )
#define DK_KEYP_0      0
#define DK_KEYP_1      DK_END
#define DK_KEYP_2      DK_DOWNARROW
#define DK_KEYP_3      DK_PGDN
#define DK_KEYP_4      DK_LEFTARROW
#define DK_KEYP_5      53
#define DK_KEYP_6      DK_RIGHTARROW
#define DK_KEYP_7      DK_HOME
#define DK_KEYP_8      DK_UPARROW
#define DK_KEYP_9      DK_PGUP
#define DK_KEYP_DIVIDE     47
#define DK_KEYP_PLUS       43
#define DK_KEYP_MINUS      45
#define DK_KEYP_MULTIPLY   42
#define DK_KEYP_PERIOD     0
#define DK_KEYP_EQUALS     DK_EQUALS
#define DK_KEYP_ENTER      DK_ENTER

struct color
{
   unsigned char b;
   unsigned char g;
   unsigned char r;
   unsigned char a;
};

extern struct color colors[ 256 ];

static SDL_Window *s_window = NULL;
static SDL_Renderer *s_renderer = NULL;
static SDL_Texture *s_texture = NULL;
static int s_quit = 0;
static int s_fullscreen = 0;
static char s_err[ 256 ];

#define SDL_KEYQ 64
static int s_key_code[ SDL_KEYQ ];
static int s_key_down[ SDL_KEYQ ];
static int s_key_r = 0;
static int s_key_w = 0;

static void SdlSetErr( const char *msg )
{
   if( msg == NULL )
      msg = "";
   strncpy( s_err, msg, sizeof( s_err ) - 1 );
   s_err[ sizeof( s_err ) - 1 ] = 0;
}

static void SdlPushKey( int code, int down )
{
   int next;

   if( code == 0 )
      return;
   next = ( s_key_w + 1 ) % SDL_KEYQ;
   if( next == s_key_r )
      return;
   s_key_code[ s_key_w ] = code;
   s_key_down[ s_key_w ] = down ? 1 : 0;
   s_key_w = next;
}

static int SdlDoomKey( SDL_Scancode code )
{
   if( code >= SDL_SCANCODE_A && code <= SDL_SCANCODE_Z )
      return 'a' + ( int ) ( code - SDL_SCANCODE_A );
   if( code >= SDL_SCANCODE_1 && code <= SDL_SCANCODE_9 )
      return '1' + ( int ) ( code - SDL_SCANCODE_1 );
   if( code == SDL_SCANCODE_0 )
      return '0';

   switch( code )
   {
   case SDL_SCANCODE_RIGHT: return DK_RIGHTARROW;
   case SDL_SCANCODE_LEFT: return DK_LEFTARROW;
   case SDL_SCANCODE_UP: return DK_UPARROW;
   case SDL_SCANCODE_DOWN: return DK_DOWNARROW;
   case SDL_SCANCODE_COMMA: return DK_STRAFE_L;
   case SDL_SCANCODE_PERIOD: return DK_STRAFE_R;
   case SDL_SCANCODE_SPACE: return DK_USE;
   case SDL_SCANCODE_LCTRL: return DK_FIRE;
   case SDL_SCANCODE_ESCAPE: return DK_ESCAPE;
   case SDL_SCANCODE_RETURN: return DK_ENTER;
   case SDL_SCANCODE_TAB: return DK_TAB;
   case SDL_SCANCODE_F1: return DK_F1;
   case SDL_SCANCODE_F2: return DK_F2;
   case SDL_SCANCODE_F3: return DK_F3;
   case SDL_SCANCODE_F4: return DK_F4;
   case SDL_SCANCODE_F5: return DK_F5;
   case SDL_SCANCODE_F6: return DK_F6;
   case SDL_SCANCODE_F7: return DK_F7;
   case SDL_SCANCODE_F8: return DK_F8;
   case SDL_SCANCODE_F9: return DK_F9;
   case SDL_SCANCODE_F10: return DK_F10;
   case SDL_SCANCODE_F11: return DK_F11;
   case SDL_SCANCODE_F12: return DK_F12;
   case SDL_SCANCODE_BACKSPACE: return DK_BACKSPACE;
   case SDL_SCANCODE_PAUSE: return DK_PAUSE;
   case SDL_SCANCODE_EQUALS: return DK_EQUALS;
   case SDL_SCANCODE_MINUS: return DK_MINUS;
   case SDL_SCANCODE_LSHIFT:
   case SDL_SCANCODE_RSHIFT: return DK_RSHIFT;
   case SDL_SCANCODE_RCTRL: return DK_RCTRL;
   case SDL_SCANCODE_LALT:
   case SDL_SCANCODE_RALT: return DK_RALT;
   case SDL_SCANCODE_CAPSLOCK: return DK_CAPSLOCK;
   case SDL_SCANCODE_NUMLOCKCLEAR: return DK_NUMLOCK;
   case SDL_SCANCODE_SCROLLLOCK: return DK_SCRLCK;
   case SDL_SCANCODE_PRINTSCREEN: return DK_PRTSCR;
   case SDL_SCANCODE_HOME: return DK_HOME;
   case SDL_SCANCODE_END: return DK_END;
   case SDL_SCANCODE_PAGEUP: return DK_PGUP;
   case SDL_SCANCODE_PAGEDOWN: return DK_PGDN;
   case SDL_SCANCODE_INSERT: return DK_INS;
   case SDL_SCANCODE_DELETE: return DK_DEL;
   case SDL_SCANCODE_KP_0: return DK_KEYP_0;
   case SDL_SCANCODE_KP_1: return DK_KEYP_1;
   case SDL_SCANCODE_KP_2: return DK_KEYP_2;
   case SDL_SCANCODE_KP_3: return DK_KEYP_3;
   case SDL_SCANCODE_KP_4: return DK_KEYP_4;
   case SDL_SCANCODE_KP_5: return DK_KEYP_5;
   case SDL_SCANCODE_KP_6: return DK_KEYP_6;
   case SDL_SCANCODE_KP_7: return DK_KEYP_7;
   case SDL_SCANCODE_KP_8: return DK_KEYP_8;
   case SDL_SCANCODE_KP_9: return DK_KEYP_9;
   case SDL_SCANCODE_KP_DIVIDE: return DK_KEYP_DIVIDE;
   case SDL_SCANCODE_KP_PLUS: return DK_KEYP_PLUS;
   case SDL_SCANCODE_KP_MINUS: return DK_KEYP_MINUS;
   case SDL_SCANCODE_KP_MULTIPLY: return DK_KEYP_MULTIPLY;
   case SDL_SCANCODE_KP_PERIOD: return DK_KEYP_PERIOD;
   case SDL_SCANCODE_KP_EQUALS: return DK_KEYP_EQUALS;
   case SDL_SCANCODE_KP_ENTER: return DK_KEYP_ENTER;
   default: break;
   }
   return 0;
}

static void SdlToggleFullscreen( void )
{
   if( s_window == NULL )
      return;
   s_fullscreen = s_fullscreen ? 0 : 1;
   SDL_SetWindowFullscreen( s_window, s_fullscreen ? true : false );
}

static void SdlPump( void )
{
   SDL_Event ev;

   if( s_window == NULL )
      return;

   while( SDL_PollEvent( &ev ) )
   {
      if( ev.type == SDL_EVENT_QUIT || ev.type == SDL_EVENT_WINDOW_CLOSE_REQUESTED )
      {
         s_quit = 1;
         continue;
      }
      if( ev.type != SDL_EVENT_KEY_DOWN && ev.type != SDL_EVENT_KEY_UP )
         continue;
      if( ( ev.key.scancode == SDL_SCANCODE_RETURN || ev.key.scancode == SDL_SCANCODE_KP_ENTER ) &&
          ( ev.key.mod & SDL_KMOD_ALT ) && ev.type == SDL_EVENT_KEY_DOWN && ! ev.key.repeat )
      {
         SdlToggleFullscreen();
         continue;
      }
      SdlPushKey( SdlDoomKey( ev.key.scancode ), ev.type == SDL_EVENT_KEY_DOWN );
   }
}

HB_FUNC( SDLOPEN )
{
   int w = hb_parni( 1 );
   int h = hb_parni( 2 );

   s_err[ 0 ] = 0;
   if( w < SDL_DOOM_W )
      w = SDL_DOOM_W * 2;
   if( h < SDL_DOOM_H )
      h = SDL_DOOM_H * 2;

   if( ! SDL_Init( SDL_INIT_VIDEO ) )
   {
      SdlSetErr( SDL_GetError() );
      hb_retni( 0 );
      return;
   }

   s_window = SDL_CreateWindow( "DOOM", w, h, SDL_WINDOW_RESIZABLE );
   if( s_window == NULL )
   {
      SdlSetErr( SDL_GetError() );
      hb_retni( 0 );
      return;
   }

   s_renderer = SDL_CreateRenderer( s_window, NULL );
   if( s_renderer == NULL )
   {
      SdlSetErr( SDL_GetError() );
      hb_retni( 0 );
      return;
   }

   s_texture = SDL_CreateTexture( s_renderer, SDL_PIXELFORMAT_ARGB8888,
                                  SDL_TEXTUREACCESS_STREAMING, SDL_DOOM_W, SDL_DOOM_H );
   if( s_texture == NULL )
   {
      SdlSetErr( SDL_GetError() );
      hb_retni( 0 );
      return;
   }
   SDL_SetTextureScaleMode( s_texture, SDL_SCALEMODE_NEAREST );

   s_fullscreen = hb_parl( 3 ) ? 1 : 0;
   if( s_fullscreen )
      SDL_SetWindowFullscreen( s_window, true );

   hb_retni( 1 );
}

HB_FUNC( SDLSHUTDOWN )
{
   if( s_texture != NULL )
   {
      SDL_DestroyTexture( s_texture );
      s_texture = NULL;
   }
   if( s_renderer != NULL )
   {
      SDL_DestroyRenderer( s_renderer );
      s_renderer = NULL;
   }
   if( s_window != NULL )
   {
      SDL_DestroyWindow( s_window );
      s_window = NULL;
   }
   SDL_Quit();
}

HB_FUNC( SDLLASTERROR )
{
   hb_retc( s_err );
}

HB_FUNC( SDLISFULLSCREEN )
{
   hb_retl( s_fullscreen != 0 );
}

HB_FUNC( SDLQUITREQUESTED )
{
   SdlPump();
   hb_retl( s_quit != 0 );
}

HB_FUNC( SDLSETTITLE )
{
   if( s_window != NULL && HB_ISCHAR( 1 ) )
      SDL_SetWindowTitle( s_window, hb_parc( 1 ) );
}

HB_FUNC( SDLTICKS )
{
   hb_retnint( ( HB_MAXINT ) ( SDL_GetTicks() & 0xFFFFFFFF ) );
}

HB_FUNC( SDLDELAY )
{
   int ms = hb_parni( 1 );
   if( ms > 0 )
      SDL_Delay( ( Uint32 ) ms );
}

HB_FUNC( SDLKEYPOP )
{
   int code;
   int down;

   SdlPump();
   if( s_key_r == s_key_w )
   {
      hb_ret();
      return;
   }
   code = s_key_code[ s_key_r ];
   down = s_key_down[ s_key_r ];
   s_key_r = ( s_key_r + 1 ) % SDL_KEYQ;
   hb_reta( 2 );
   hb_storvni( down, -1, 1 );
   hb_storvni( code, -1, 2 );
}

HB_FUNC( SDLPRESENT )
{
   const unsigned char *src = ( const unsigned char * ) hb_parc( 1 );
   Uint32 pixels[ SDL_DOOM_W * SDL_DOOM_H ];
   int i;
   int n;
   int ww = 0;
   int hh = 0;
   SDL_FRect dst;

   SdlPump();
   if( s_renderer == NULL || s_texture == NULL || src == NULL )
      return;
   if( hb_parclen( 1 ) < ( HB_SIZE ) ( SDL_DOOM_W * SDL_DOOM_H ) )
      return;

   n = SDL_DOOM_W * SDL_DOOM_H;
   for( i = 0; i < n; i++ )
   {
      struct color c = colors[ src[ i ] ];
      pixels[ i ] = ( 0xFFu << 24 ) | ( ( Uint32 ) c.r << 16 ) | ( ( Uint32 ) c.g << 8 ) | ( Uint32 ) c.b;
   }

   SDL_UpdateTexture( s_texture, NULL, pixels, SDL_DOOM_W * ( int ) sizeof( Uint32 ) );
   SDL_GetWindowSize( s_window, &ww, &hh );
   if( ww < 1 )
      ww = SDL_DOOM_W;
   if( hh < 1 )
      hh = SDL_DOOM_H;
   dst.x = 0.0f;
   dst.y = 0.0f;
   dst.w = ( float ) ww;
   dst.h = ( float ) hh;
   SDL_RenderClear( s_renderer );
   SDL_RenderTexture( s_renderer, s_texture, NULL, &dst );
   SDL_RenderPresent( s_renderer );
}
#pragma ENDDUMP
