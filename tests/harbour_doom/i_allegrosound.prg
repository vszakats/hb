/*
DOOM generic portado para Harbour. Efeitos sonoros misturados na SDL3.

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

STATIC sound_initialized := .F.
STATIC channels_playing := {}
STATIC use_sfx_prefix := .F.

#include "deh_str.ch"
#include "doomtype.ch"
#include "i_allegrosound.ch"

#ifndef PU_STATIC
#define PU_STATIC 1
#endif


INIT PROCEDURE init_i_allegrosound
    LOCAL i

    PUBLIC use_libsamplerate
    PUBLIC libsamplerate_scale
    PUBLIC DG_sound_module

    use_libsamplerate := 0
    libsamplerate_scale := 0.65

    sound_initialized := .F.
    use_sfx_prefix := .F.

    channels_playing := {}
    FOR i := 1 TO NUM_CHANNELS
        AAdd( channels_playing, NIL )
    NEXT

    DG_sound_module := sound_module_t():New()
    DG_sound_module:sound_devices := { ;
        SNDDEVICE_SB, ;
        SNDDEVICE_PAS, ;
        SNDDEVICE_GUS, ;
        SNDDEVICE_WAVEBLASTER, ;
        SNDDEVICE_SOUNDCANVAS, ;
        SNDDEVICE_AWE32 }
    DG_sound_module:num_sound_devices := Len( DG_sound_module:sound_devices )
    DG_sound_module:Init := {| lPrefix | I_Allegro_InitSound( lPrefix ) }
    DG_sound_module:Shutdown := {|| I_Allegro_ShutdownSound() }
    DG_sound_module:GetSfxLumpNum := {| sfx | I_Allegro_GetSfxLumpNum( sfx ) }
    DG_sound_module:Update := {|| I_Allegro_UpdateSound() }
    DG_sound_module:UpdateSoundParams := {| handle, vol, sep | I_Allegro_UpdateSoundParams( handle, vol, sep ) }
    DG_sound_module:StartSound := {| sfx, channel, vol, sep | I_Allegro_StartSound( sfx, channel, vol, sep ) }
    DG_sound_module:StopSound := {| handle | I_Allegro_StopSound( handle ) }
    DG_sound_module:SoundIsPlaying := {| handle | I_Allegro_SoundIsPlaying( handle ) }
    DG_sound_module:CacheSounds := {| sounds, num | I_Allegro_PrecacheSounds( sounds, num ) }
RETURN

STATIC FUNCTION LumpByte( cData, nIdx0 )
    LOCAL nPos

    nPos := nIdx0 + 1
    IF ValType( cData ) != "C" .OR. nPos < 1 .OR. nPos > Len( cData )
        RETURN 0
    ENDIF
RETURN ( Asc( SubStr( cData, nPos, 1 ) ) & 0xFF )

STATIC FUNCTION CacheSFX( sfxinfo )
    LOCAL nLump
    LOCAL nLumpLen
    LOCAL nRate
    LOCAL nLength
    LOCAL cData
    LOCAL sample

    nLump := sfxinfo:lumpnum
    cData := W_CacheLumpNum( nLump, PU_STATIC )
    nLumpLen := W_LumpLength( nLump )

    IF ValType( cData ) != "C" .OR. nLumpLen < 8 ;
         .OR. LumpByte( cData, 0 ) != 3 .OR. LumpByte( cData, 1 ) != 0
        RETURN .F.
    ENDIF

    nRate := LumpByte( cData, 2 ) + LumpByte( cData, 3 ) * 256
    nLength := LumpByte( cData, 4 ) + LumpByte( cData, 5 ) * 256 ;
         + LumpByte( cData, 6 ) * 65536 + LumpByte( cData, 7 ) * 16777216

    IF nLength > nLumpLen - 8 .OR. nLength <= 48
        RETURN .F.
    ENDIF

    nLength := nLength - 32
    sample := SdlSndCreate( SubStr( cData, 17, nLength ), nRate )
    IF sample == NIL
        RETURN .F.
    ENDIF

    sfxinfo:driver_data := sample
    W_ReleaseLumpNum( nLump )
RETURN .T.

STATIC FUNCTION GetSfxLumpName( sfx )
    LOCAL cName

    IF sfx:link != NIL
        sfx := sfx:link
    ENDIF

    IF use_sfx_prefix
        cName := "ds" + DEH_String( sfx:name )
    ELSE
        cName := DEH_String( sfx:name )
    ENDIF
RETURN cName

STATIC FUNCTION I_Allegro_PrecacheSounds( sounds, num_sounds )
    LOCAL i
    LOCAL cName

    OutStd( "I_Allegro_PrecacheSounds: Precaching all sound effects.." )

    FOR i := 0 TO num_sounds - 1
        IF ( i % 6 ) == 0
            OutStd( "." )
        ENDIF
        cName := GetSfxLumpName( sounds[ i + 1 ] )
        sounds[ i + 1 ]:lumpnum := W_CheckNumForName( cName )
        IF sounds[ i + 1 ]:lumpnum != -1
            CacheSFX( sounds[ i + 1 ] )
        ENDIF
    NEXT

    OutStd( hb_eol() )
RETURN NIL

STATIC FUNCTION I_Allegro_GetSfxLumpNum( sfx )
RETURN W_GetNumForName( GetSfxLumpName( sfx ) )

STATIC FUNCTION I_Allegro_UpdateSoundParams( handle, vol, sep )
    IF ! sound_initialized .OR. handle < 0 .OR. handle >= NUM_CHANNELS
        RETURN NIL
    ENDIF
    IF channels_playing[ handle + 1 ] == NIL
        RETURN NIL
    ENDIF
    SdlSndParams( handle, vol, sep )
RETURN NIL

STATIC FUNCTION I_Allegro_StartSound( sfxinfo, channel, vol, sep )
    IF ! sound_initialized .OR. channel < 0 .OR. channel >= NUM_CHANNELS
        RETURN -1
    ENDIF

    IF channels_playing[ channel + 1 ] != NIL
        SdlSndStop( channel )
        channels_playing[ channel + 1 ] := NIL
    ENDIF

    IF sfxinfo:driver_data == NIL
        IF ! CacheSFX( sfxinfo )
            RETURN -1
        ENDIF
    ENDIF

    SdlSndStart( channel, sfxinfo:driver_data, vol, sep )
    SdlSndPump()

    channels_playing[ channel + 1 ] := sfxinfo
RETURN channel

STATIC FUNCTION I_Allegro_StopSound( handle )
    IF ! sound_initialized .OR. handle < 0 .OR. handle >= NUM_CHANNELS
        RETURN NIL
    ENDIF
    IF channels_playing[ handle + 1 ] == NIL
        RETURN NIL
    ENDIF
    SdlSndStop( handle )
    channels_playing[ handle + 1 ] := NIL
RETURN NIL

STATIC FUNCTION I_Allegro_SoundIsPlaying( handle )
    IF ! sound_initialized .OR. handle < 0 .OR. handle >= NUM_CHANNELS
        RETURN .F.
    ENDIF
    IF channels_playing[ handle + 1 ] == NIL
        RETURN .F.
    ENDIF
RETURN SdlSndPos( handle ) >= 0

STATIC FUNCTION I_Allegro_UpdateSound()
    LOCAL i

    IF ! sound_initialized
        RETURN NIL
    ENDIF

    SdlSndPump()
    FOR i := 0 TO NUM_CHANNELS - 1
        IF channels_playing[ i + 1 ] != NIL .AND. SdlSndPos( i ) < 0
            channels_playing[ i + 1 ] := NIL
        ENDIF
    NEXT
RETURN NIL

STATIC FUNCTION I_Allegro_ShutdownSound()
    LOCAL i

    IF ! sound_initialized
        RETURN NIL
    ENDIF

    FOR i := 0 TO NUM_CHANNELS - 1
        channels_playing[ i + 1 ] := NIL
    NEXT

    SdlSndShutdown()
    sound_initialized := .F.
RETURN NIL

STATIC FUNCTION I_Allegro_InitSound( lUsePrefix )
    LOCAL i

    use_sfx_prefix := lUsePrefix

    FOR i := 0 TO NUM_CHANNELS - 1
        channels_playing[ i + 1 ] := NIL
    NEXT

    IF SdlSndInit() != 0
        OutStd( "SDL3 audio failed: " + SdlSndError() + hb_eol() )
        RETURN .F.
    ENDIF

    OutStd( "SDL3 audio: 11025 Hz, 16 voices" + hb_eol() )
    sound_initialized := .T.
RETURN .T.

#pragma BEGINDUMP
#include "hbapi.h"

#include <SDL3/SDL.h>

#include <stdlib.h>
#include <string.h>

#define VOICES 16
#define MAX_CLIPS 512
#define MIX_RATE 11025

typedef struct clip_s {
    int16_t *pcm;
    int n;
} clip_t;

typedef struct voice_s {
    clip_t *clip;
    int pos;
    int on;
    int vol;
    int sep;
} voice_t;

static SDL_AudioStream *s_stream = NULL;
static voice_t s_voice[ VOICES ];
static clip_t *s_clips[ MAX_CLIPS ];
static int s_nclips = 0;
static char s_err[ 160 ];

static void SndSetErr( const char *msg )
{
   if( msg == NULL || msg[ 0 ] == 0 )
      msg = "audio indisponivel";
   strncpy( s_err, msg, sizeof( s_err ) - 1 );
   s_err[ sizeof( s_err ) - 1 ] = 0;
}

static void SndGain( int vol, int sep, int *left, int *right )
{
   if( vol < 0 )
      vol = 0;
   if( vol > 127 )
      vol = 127;
   if( sep < 0 )
      sep = 0;
   if( sep > 254 )
      sep = 254;
   if( sep <= 128 )
   {
      *left = vol;
      *right = vol * sep / 128;
   }
   else
   {
      *left = vol * ( 254 - sep ) / 126;
      *right = vol;
   }
}

static void SndMix( int16_t *out, int frames )
{
   int i;
   int v;
   int acc_l;
   int acc_r;
   int left;
   int right;
   int16_t sample;

   for( i = 0; i < frames; i++ )
   {
      acc_l = 0;
      acc_r = 0;
      for( v = 0; v < VOICES; v++ )
      {
         if( ! s_voice[ v ].on || s_voice[ v ].clip == NULL )
            continue;
         if( s_voice[ v ].pos >= s_voice[ v ].clip->n )
         {
            s_voice[ v ].on = 0;
            continue;
         }
         sample = s_voice[ v ].clip->pcm[ s_voice[ v ].pos++ ];
         SndGain( s_voice[ v ].vol, s_voice[ v ].sep, &left, &right );
         acc_l += ( int ) sample * left / 127;
         acc_r += ( int ) sample * right / 127;
      }
      if( acc_l > 32767 )
         acc_l = 32767;
      if( acc_l < -32768 )
         acc_l = -32768;
      if( acc_r > 32767 )
         acc_r = 32767;
      if( acc_r < -32768 )
         acc_r = -32768;
      out[ i * 2 ] = ( int16_t ) acc_l;
      out[ i * 2 + 1 ] = ( int16_t ) acc_r;
   }
}

static void SndPumpBytes( int cap_bytes )
{
   static int16_t buf[ 8192 ];
   int queued;
   int bytes;
   int frames;
   int v;
   int any;
   int guard;

   if( s_stream == NULL || cap_bytes < 4 )
      return;
   if( SDL_AudioStreamDevicePaused( s_stream ) )
      SDL_ResumeAudioStreamDevice( s_stream );
   for( guard = 0; guard < 8; guard++ )
   {
      any = 0;
      for( v = 0; v < VOICES; v++ )
      {
         if( s_voice[ v ].on )
         {
            any = 1;
            break;
         }
      }
      if( ! any )
         return;
      queued = SDL_GetAudioStreamQueued( s_stream );
      if( queued < 0 || queued >= cap_bytes )
         return;
      bytes = cap_bytes - queued;
      if( bytes > ( int ) sizeof( buf ) )
         bytes = ( int ) sizeof( buf );
      bytes &= ~3;
      frames = bytes / 4;
      if( frames < 1 )
         return;
      SDL_LockAudioStream( s_stream );
      SndMix( buf, frames );
      SDL_UnlockAudioStream( s_stream );
      if( ! SDL_PutAudioStreamData( s_stream, buf, bytes ) )
         return;
   }
}

HB_FUNC( SDLSNDINIT )
{
   SDL_AudioSpec want;

   s_err[ 0 ] = 0;
   if( s_stream != NULL )
   {
      hb_retni( 0 );
      return;
   }
   if( ! SDL_InitSubSystem( SDL_INIT_AUDIO ) )
   {
      SndSetErr( SDL_GetError() );
      hb_retni( -1 );
      return;
   }
   memset( &want, 0, sizeof( want ) );
   want.freq = MIX_RATE;
   want.format = SDL_AUDIO_S16;
   want.channels = 2;
   s_stream = SDL_OpenAudioDeviceStream( SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK,
                                         &want, NULL, NULL );
   if( s_stream == NULL )
   {
      SndSetErr( SDL_GetError() );
      hb_retni( -1 );
      return;
   }
   if( ! SDL_ResumeAudioStreamDevice( s_stream ) )
   {
      SndSetErr( SDL_GetError() );
      SDL_DestroyAudioStream( s_stream );
      s_stream = NULL;
      hb_retni( -1 );
      return;
   }
   memset( s_voice, 0, sizeof( s_voice ) );
   hb_retni( 0 );
}

HB_FUNC( SDLSNDSHUTDOWN )
{
   int i;

   if( s_stream != NULL )
   {
      SDL_DestroyAudioStream( s_stream );
      s_stream = NULL;
   }
   for( i = 0; i < VOICES; i++ )
      s_voice[ i ].on = 0;
   for( i = 0; i < s_nclips; i++ )
   {
      if( s_clips[ i ] != NULL )
      {
         free( s_clips[ i ]->pcm );
         free( s_clips[ i ] );
         s_clips[ i ] = NULL;
      }
   }
   s_nclips = 0;
   SDL_QuitSubSystem( SDL_INIT_AUDIO );
}

HB_FUNC( SDLSNDERROR )
{
   hb_retc( s_err );
}

HB_FUNC( SDLSNDCREATE )
{
   const unsigned char *src;
   int len;
   int rate;
   int out_n;
   int i;
   int o;
   clip_t *clip;

   src = ( const unsigned char * ) hb_parc( 1 );
   len = ( int ) hb_parclen( 1 );
   rate = hb_parni( 2 );
   if( src == NULL || len < 1 || rate < 1000 || rate > 48000 || s_nclips >= MAX_CLIPS )
   {
      hb_ret();
      return;
   }
   if( rate == MIX_RATE )
      out_n = len;
   else
   {
      out_n = ( int ) ( ( ( long long ) len * MIX_RATE ) / rate );
      if( out_n < 1 )
         out_n = 1;
   }
   if( out_n > 200000 )
      out_n = 200000;
   clip = ( clip_t * ) malloc( sizeof( clip_t ) );
   if( clip == NULL )
   {
      hb_ret();
      return;
   }
   clip->pcm = ( int16_t * ) malloc( ( size_t ) out_n * sizeof( int16_t ) );
   if( clip->pcm == NULL )
   {
      free( clip );
      hb_ret();
      return;
   }
   clip->n = out_n;
   for( i = 0; i < out_n; i++ )
   {
      if( rate == MIX_RATE )
         o = i;
      else
      {
         o = ( int ) ( ( ( long long ) i * rate ) / MIX_RATE );
         if( o >= len )
            o = len - 1;
      }
      clip->pcm[ i ] = ( int16_t ) ( ( ( ( int ) src[ o ] ) - 128 ) << 8 );
   }
   s_clips[ s_nclips++ ] = clip;
   hb_retptr( clip );
}

HB_FUNC( SDLSNDSTART )
{
   int ch = hb_parni( 1 );
   clip_t *clip = ( clip_t * ) hb_parptr( 2 );

   if( ch < 0 || ch >= VOICES || clip == NULL )
      return;
   s_voice[ ch ].clip = clip;
   s_voice[ ch ].pos = 0;
   s_voice[ ch ].vol = hb_parni( 3 );
   s_voice[ ch ].sep = hb_parni( 4 );
   s_voice[ ch ].on = 1;
}

HB_FUNC( SDLSNDSTOP )
{
   int ch = hb_parni( 1 );

   if( ch < 0 || ch >= VOICES )
      return;
   s_voice[ ch ].on = 0;
   s_voice[ ch ].clip = NULL;
}

HB_FUNC( SDLSNDPARAMS )
{
   int ch = hb_parni( 1 );

   if( ch < 0 || ch >= VOICES || ! s_voice[ ch ].on )
      return;
   s_voice[ ch ].vol = hb_parni( 2 );
   s_voice[ ch ].sep = hb_parni( 3 );
}

HB_FUNC( SDLSNDPOS )
{
   int ch = hb_parni( 1 );

   if( ch < 0 || ch >= VOICES || ! s_voice[ ch ].on || s_voice[ ch ].clip == NULL )
   {
      hb_retni( -1 );
      return;
   }
   if( s_voice[ ch ].pos >= s_voice[ ch ].clip->n )
   {
      s_voice[ ch ].on = 0;
      hb_retni( -1 );
      return;
   }
   hb_retni( s_voice[ ch ].pos );
}

HB_FUNC( SDLSNDPUMP )
{
   /* Estereo 16-bit, cerca de meio segundo a 11025 Hz. */
   SndPumpBytes( MIX_RATE * 4 / 2 );
}

#pragma ENDDUMP
