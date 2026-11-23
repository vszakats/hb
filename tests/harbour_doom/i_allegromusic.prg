/*
DOOM generic portado para Harbour. O MIDI sai pelo sequenciador do Windows.
A SDL3 nao toca MIDI; o mesmo caminho e o do cobol_doom.

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

STATIC music_initialized := .F.
STATIC musicpaused := .F.
STATIC current_music_volume := 0
STATIC current_track_music := NIL
STATIC current_track_loop := .F.
STATIC s_mid_seq := 0

#include "deh_str.ch"
#include "doomtype.ch"
#include "gusconf.ch"
#include "i_allegromusic.ch"

#define MID_HEADER_MAGIC "MThd"
#define MUS_HEADER_MAGIC ( "MUS" + Chr( 26 ) )


INIT PROCEDURE init_i_allegromusic

    PUBLIC timidity_cfg_path
    PUBLIC gus_patch_path
    PUBLIC gus_ram_kb
    PUBLIC DG_music_module

    timidity_cfg_path := ""
    gus_patch_path := ""
    gus_ram_kb := 1024

    music_initialized := .F.
    musicpaused := .F.
    current_music_volume := 0
    current_track_music := NIL
    current_track_loop := .F.

    DG_music_module := music_module_t():New()
    DG_music_module:sound_devices := { ;
        SNDDEVICE_PAS, ;
        SNDDEVICE_GUS, ;
        SNDDEVICE_WAVEBLASTER, ;
        SNDDEVICE_SOUNDCANVAS, ;
        SNDDEVICE_GENMIDI, ;
        SNDDEVICE_AWE32 }
    DG_music_module:num_sound_devices := Len( DG_music_module:sound_devices )
    DG_music_module:Init := {|| I_Allegro_InitMusic() }
    DG_music_module:Shutdown := {|| I_Allegro_ShutdownMusic() }
    DG_music_module:SetMusicVolume := {| volume | I_Allegro_SetMusicVolume( volume ) }
    DG_music_module:PauseMusic := {|| I_Allegro_PauseSong() }
    DG_music_module:ResumeMusic := {|| I_Allegro_ResumeSong() }
    DG_music_module:RegisterSong := {| data, len | I_Allegro_RegisterSong( data, len ) }
    DG_music_module:UnRegisterSong := {| handle | I_Allegro_UnRegisterSong( handle ) }
    DG_music_module:PlaySong := {| handle, looping | I_Allegro_PlaySong( handle, looping ) }
    DG_music_module:StopSong := {|| I_Allegro_StopSong() }
    DG_music_module:MusicIsPlaying := {|| I_Allegro_MusicIsPlaying() }
    DG_music_module:Poll := {|| I_Allegro_PollMusic() }
RETURN

STATIC FUNCTION I_Allegro_ShutdownMusic()
    IF current_track_music != NIL
        SdlMusStop()
        FErase( current_track_music )
        current_track_music := NIL
    ENDIF
    music_initialized := .F.
RETURN NIL

STATIC FUNCTION I_Allegro_InitMusic()
    music_initialized := .T.
    OutStd( "Music: Windows sequencer (MCI)" + hb_eol() )
RETURN .T.

STATIC FUNCTION I_Allegro_SetMusicVolume( volume )
    current_music_volume := volume
    SdlMusVolume( volume )
RETURN NIL

STATIC FUNCTION I_Allegro_PlaySong( handle, looping )
    IF ! music_initialized
        RETURN NIL
    ENDIF
    IF handle == NIL
        RETURN NIL
    ENDIF

    current_track_music := handle
    current_track_loop := looping
    musicpaused := .F.

    IF SdlMusPlay( handle, looping, current_music_volume ) != 0
        OutErr( "Error playing midi: " + SdlMusError() + hb_eol() )
    ENDIF
RETURN NIL

STATIC FUNCTION I_Allegro_PauseSong()
    IF ! music_initialized
        RETURN NIL
    ENDIF
    musicpaused := .T.
    SdlMusPause()
RETURN NIL

STATIC FUNCTION I_Allegro_ResumeSong()
    IF ! music_initialized
        RETURN NIL
    ENDIF
    musicpaused := .F.
    SdlMusResume()
RETURN NIL

STATIC FUNCTION I_Allegro_StopSong()
    IF ! music_initialized
        RETURN NIL
    ENDIF
    SdlMusStop()
    current_track_music := NIL
    musicpaused := .F.
RETURN NIL

STATIC FUNCTION I_Allegro_UnRegisterSong( handle )
    IF ! music_initialized
        RETURN NIL
    ENDIF
    IF handle == NIL
        RETURN NIL
    ENDIF
    IF handle == current_track_music
        SdlMusStop()
        current_track_music := NIL
    ENDIF
    FErase( handle )
RETURN NIL

STATIC FUNCTION IsMid( mem, nLen )
    IF nLen <= 4
        RETURN .F.
    ENDIF
    IF ValType( mem ) == "C"
        RETURN Left( mem, 4 ) == MID_HEADER_MAGIC
    ENDIF
RETURN .F.

STATIC FUNCTION ConvertMus( musdata, nLen, filename )
    LOCAL instream
    LOCAL outstream
    LOCAL outbuf := NIL
    LOCAL outbuf_len := 0
    LOCAL nResult

    instream := mem_fopen_read( musdata, nLen )
    outstream := mem_fopen_write()

    nResult := mus2mid( instream, outstream )

    IF nResult == 0
        mem_get_buf( outstream, @outbuf, @outbuf_len )
        M_WriteFile( filename, outbuf, outbuf_len )
    ENDIF

    mem_fclose( instream )
    mem_fclose( outstream )
RETURN nResult

STATIC FUNCTION I_Allegro_RegisterSong( data, nLen )
    LOCAL filename
    LOCAL nWrote

    IF ! music_initialized
        RETURN NIL
    ENDIF

    s_mid_seq++
    filename := M_TempFile( "doom" + hb_ntos( s_mid_seq ) + ".mid" )

    IF IsMid( data, nLen ) .AND. nLen < MAXMIDLENGTH
        nWrote := M_WriteFile( filename, data, nLen )
    ELSE
        nWrote := iif( ConvertMus( data, nLen, filename ) == 0, 1, 0 )
    ENDIF

    IF nWrote == 0
        FErase( filename )
        OutErr( "Error loading midi: conversao MUS vazia" + hb_eol() )
        RETURN NIL
    ENDIF
RETURN filename

STATIC FUNCTION I_Allegro_MusicIsPlaying()
    IF ! music_initialized .OR. current_track_music == NIL .OR. musicpaused
        RETURN .F.
    ENDIF
RETURN SdlMusPlaying()

STATIC FUNCTION GetMusicPosition()
RETURN SdlMusTime()

STATIC FUNCTION I_Allegro_PollMusic()
    IF music_initialized .AND. current_track_loop .AND. ! musicpaused
        SdlMusPump()
    ENDIF
RETURN NIL

#pragma BEGINDUMP
#include "hbapi.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <mmsystem.h>

#include <stdio.h>
#include <string.h>

static char s_err[ 160 ];
static int s_on = 0;
static int s_loop = 0;
static int s_paused = 0;
static int s_volume = 0;

static void MusSetErr( const char *msg )
{
   if( msg == NULL || msg[ 0 ] == 0 )
      msg = "midi indisponivel";
   strncpy( s_err, msg, sizeof( s_err ) - 1 );
   s_err[ sizeof( s_err ) - 1 ] = 0;
}

static int MusSend( const char *cmd )
{
   MCIERROR err;

   err = mciSendStringA( cmd, NULL, 0, NULL );
   if( err != 0 )
   {
      if( ! mciGetErrorStringA( err, s_err, sizeof( s_err ) ) )
         MusSetErr( "mciSendString falhou" );
      return 0;
   }
   s_err[ 0 ] = 0;
   return 1;
}

static void MusVolume( int volume )
{
   char cmd[ 64 ];
   int v;

   if( volume < 0 )
      volume = 0;
   if( volume > 127 )
      volume = 127;
   s_volume = volume;
   v = volume * 1000 / 127;
   snprintf( cmd, sizeof( cmd ), "setaudio doommus volume to %d", v );
   mciSendStringA( cmd, NULL, 0, NULL );
}

static int MusMode( char *buf, int n )
{
   buf[ 0 ] = 0;
   if( mciSendStringA( "status doommus mode", buf, ( UINT ) n, NULL ) != 0 )
      return 0;
   return 1;
}

HB_FUNC( SDLMUSPLAY )
{
   const char *path = hb_parc( 1 );
   char cmd[ 520 ];

   s_err[ 0 ] = 0;
   if( path == NULL || path[ 0 ] == 0 )
   {
      MusSetErr( "arquivo midi vazio" );
      hb_retni( -1 );
      return;
   }
   mciSendStringA( "close doommus", NULL, 0, NULL );
   snprintf( cmd, sizeof( cmd ), "open \"%s\" type sequencer alias doommus", path );
   if( ! MusSend( cmd ) )
   {
      snprintf( cmd, sizeof( cmd ), "open \"%s\" alias doommus", path );
      if( ! MusSend( cmd ) )
      {
         hb_retni( -1 );
         return;
      }
   }
   if( ! MusSend( "play doommus from 0" ) )
   {
      mciSendStringA( "close doommus", NULL, 0, NULL );
      hb_retni( -1 );
      return;
   }
   s_loop = hb_parl( 2 ) ? 1 : 0;
   s_paused = 0;
   s_on = 1;
   MusVolume( hb_parni( 3 ) );
   hb_retni( 0 );
}

HB_FUNC( SDLMUSSTOP )
{
   mciSendStringA( "close doommus", NULL, 0, NULL );
   s_on = 0;
   s_loop = 0;
   s_paused = 0;
}

HB_FUNC( SDLMUSPAUSE )
{
   if( ! s_on )
      return;
   if( MusSend( "pause doommus" ) )
      s_paused = 1;
}

HB_FUNC( SDLMUSRESUME )
{
   if( ! s_on )
      return;
   if( MusSend( "resume doommus" ) )
      s_paused = 0;
}

HB_FUNC( SDLMUSVOLUME )
{
   if( s_on )
      MusVolume( hb_parni( 1 ) );
   else if( hb_parni( 1 ) >= 0 )
      s_volume = hb_parni( 1 );
}

HB_FUNC( SDLMUSPLAYING )
{
   char buf[ 64 ];

   if( ! s_on )
   {
      hb_retl( 0 );
      return;
   }
   if( ! MusMode( buf, ( int ) sizeof( buf ) ) )
   {
      hb_retl( s_on );
      return;
   }
   hb_retl( strncmp( buf, "playing", 7 ) == 0 || strncmp( buf, "paused", 6 ) == 0 );
}

HB_FUNC( SDLMUSPUMP )
{
   char buf[ 64 ];

   if( ! s_on || ! s_loop || s_paused )
      return;
   if( ! MusMode( buf, ( int ) sizeof( buf ) ) )
      return;
   if( strncmp( buf, "playing", 7 ) != 0 && strncmp( buf, "paused", 6 ) != 0 )
      mciSendStringA( "play doommus from 0", NULL, 0, NULL );
}

HB_FUNC( SDLMUSTIME )
{
   char buf[ 64 ];
   long ms;

   buf[ 0 ] = 0;
   if( ! s_on || mciSendStringA( "status doommus position", buf, sizeof( buf ), NULL ) != 0 )
   {
      hb_retnd( 0 );
      return;
   }
   ms = atol( buf );
   hb_retnd( ( double ) ms / 1000.0 );
}

HB_FUNC( SDLMUSERROR )
{
   hb_retc( s_err );
}

#pragma ENDDUMP
