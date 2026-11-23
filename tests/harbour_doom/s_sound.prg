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

STATIC channels
STATIC snd_SfxVolume
STATIC mus_paused
STATIC mus_playing

#include "s_sound.ch"

CLASS channel_t
    DATA sfxinfo
    DATA origin
    DATA handle
    METHOD New()
ENDCLASS
#include "sounds.ch"
#include "p_local.ch"
#include "tables.ch"
#include "z_zone.ch"
#include "d_mode.ch"
#include "i_sound.ch"
#include "doomdef.ch"


#define S_ATTENUATOR 1000


METHOD New() CLASS channel_t
    ::sfxinfo := NIL
    ::origin := NIL
    ::handle := 0
RETURN Self

STATIC FUNCTION AsU32( n )
RETURN ( n & 0xFFFFFFFF )

PROCEDURE S_Init( nSfxVolume, nMusicVolume )
    LOCAL i
    MEMVAR S_sfx
    MEMVAR snd_channels

    I_PrecacheSounds( S_sfx, NUMSFX )
    S_SetSfxVolume( nSfxVolume )
    S_SetMusicVolume( nMusicVolume )

    channels := {}
    FOR i := 1 TO snd_channels
        AAdd( channels, channel_t():New() )
    NEXT

    mus_paused := .F.

    FOR i := 1 TO NUMSFX - 1
        S_sfx[ i + 1 ]:lumpnum := -1
        S_sfx[ i + 1 ]:usefulness := -1
    NEXT

    I_AtExit( {|| S_Shutdown() }, .T. )
RETURN

PROCEDURE S_Shutdown()
    I_ShutdownSound()
    I_ShutdownMusic()
RETURN

STATIC PROCEDURE S_StopChannel( cnum )
    LOCAL i
    LOCAL c := channels[ cnum + 1 ]
    MEMVAR snd_channels

    IF !( c:sfxinfo == NIL )
        IF I_SoundIsPlaying( c:handle )
            I_StopSound( c:handle )
        ENDIF
        FOR i := 0 TO snd_channels - 1
            IF cnum != i .AND. c:sfxinfo == channels[ i + 1 ]:sfxinfo
                EXIT
            ENDIF
        NEXT
        c:sfxinfo:usefulness--
        c:sfxinfo := NIL
    ENDIF
RETURN

PROCEDURE S_Start()
    LOCAL cnum
    LOCAL mnum
    LOCAL spmus
    MEMVAR gameepisode
    MEMVAR gamemap
    MEMVAR gamemode
    MEMVAR snd_channels

    FOR cnum := 0 TO snd_channels - 1
        IF !( channels[ cnum + 1 ]:sfxinfo == NIL )
            S_StopChannel( cnum )
        ENDIF
    NEXT

    mus_paused := .F.

    IF gamemode == commercial
        mnum := mus_runnin + gamemap - 1
    ELSE
        spmus := { mus_e3m4, mus_e3m2, mus_e3m3, mus_e1m5, mus_e2m7, mus_e2m4, mus_e2m6, mus_e2m5, mus_e1m9 }
        IF gameepisode < 4
            mnum := mus_e1m1 + ( gameepisode - 1 ) * 9 + gamemap - 1
        ELSE
            mnum := spmus[ gamemap ]
        ENDIF
    ENDIF

    S_ChangeMusic( mnum, .T. )
RETURN

PROCEDURE S_StopSound( origin )
    LOCAL cnum
    MEMVAR snd_channels
    IF ValType( origin ) != "O"
        origin := NIL
    ENDIF
    FOR cnum := 0 TO snd_channels - 1
        IF !( channels[ cnum + 1 ]:sfxinfo == NIL ) .AND. channels[ cnum + 1 ]:origin == origin
            S_StopChannel( cnum )
            EXIT
        ENDIF
    NEXT
RETURN

STATIC FUNCTION S_GetChannel( origin, sfxinfo )
    LOCAL cnum
    LOCAL c
    MEMVAR snd_channels
    IF ValType( origin ) != "O"
        origin := NIL
    ENDIF

    FOR cnum := 0 TO snd_channels - 1
        IF channels[ cnum + 1 ]:sfxinfo == NIL
            EXIT
        ELSEIF !( origin == NIL ) .AND. channels[ cnum + 1 ]:origin == origin
            S_StopChannel( cnum )
            EXIT
        ENDIF
    NEXT

    IF cnum == snd_channels
        FOR cnum := 0 TO snd_channels - 1
            IF channels[ cnum + 1 ]:sfxinfo:priority >= sfxinfo:priority
                EXIT
            ENDIF
        NEXT
        IF cnum == snd_channels
            RETURN -1
        ELSE
            S_StopChannel( cnum )
        ENDIF
    ENDIF

    c := channels[ cnum + 1 ]
    c:sfxinfo := sfxinfo
    c:origin := origin
RETURN cnum

STATIC FUNCTION S_AdjustSoundParams( listener, source, vol, sep )
    LOCAL approx_dist
    LOCAL adx
    LOCAL ady
    LOCAL angle
    MEMVAR finesine
    MEMVAR gamemap

    IF listener == NIL .OR. source == NIL
        RETURN 0
    ENDIF

    adx := Abs( listener:x - source:x )
    ady := Abs( listener:y - source:y )
    approx_dist := adx + ady - Shar( iif( adx < ady, adx, ady ), 1 )

    IF gamemap != 8 .AND. approx_dist > S_CLIPPING_DIST
        RETURN 0
    ENDIF

    angle := R_PointToAngle2( listener:x, listener:y, source:x, source:y )
    IF angle > listener:angle
        angle := AsU32( angle - listener:angle )
    ELSE
        angle := AsU32( angle + ( 0xFFFFFFFF - listener:angle ) )
    ENDIF
    angle := UShr( angle, ANGLETOFINESHIFT )
    sep := 128 - Shar( FixedMul( S_STEREO_SWING, finesine[ angle + 1 ] ), FRACBITS )

    IF approx_dist < S_CLOSE_DIST
        vol := snd_SfxVolume
    ELSEIF gamemap == 8
        IF approx_dist > S_CLIPPING_DIST
            approx_dist := S_CLIPPING_DIST
        ENDIF
        vol := 15 + Int( ( ( snd_SfxVolume - 15 ) * Shar( S_CLIPPING_DIST - approx_dist, FRACBITS ) ) / S_ATTENUATOR )
    ELSE
        vol := Int( ( snd_SfxVolume * Shar( S_CLIPPING_DIST - approx_dist, FRACBITS ) ) / S_ATTENUATOR )
    ENDIF
RETURN iif( vol > 0, 1, 0 )

PROCEDURE S_StartSound( origin_p, sfx_id )
    LOCAL sfx
    LOCAL origin
    LOCAL rc
    LOCAL sep
    LOCAL cnum
    LOCAL volume
    MEMVAR consoleplayer
    MEMVAR players
    MEMVAR S_sfx

    origin := origin_p
    IF ValType( origin ) != "O"
        origin := NIL
    ENDIF
    volume := snd_SfxVolume

    IF sfx_id < 1 .OR. sfx_id > NUMSFX
        I_Error( "Bad sfx #: " + LTrim( Str( sfx_id ) ) )
    ENDIF

    sfx := S_sfx[ sfx_id + 1 ]

    IF !( sfx:link == NIL )
        volume += sfx:volume
        IF volume < 1
            RETURN
        ENDIF
        IF volume > snd_SfxVolume
            volume := snd_SfxVolume
        ENDIF
    ENDIF

    IF !( origin == NIL ) .AND. !( origin == players[ consoleplayer + 1 ]:mo )
        rc := S_AdjustSoundParams( players[ consoleplayer + 1 ]:mo, origin, @volume, @sep )
        IF origin:x == players[ consoleplayer + 1 ]:mo:x .AND. origin:y == players[ consoleplayer + 1 ]:mo:y
            sep := NORM_SEP
        ENDIF
        IF rc == 0
            RETURN
        ENDIF
    ELSE
        sep := NORM_SEP
    ENDIF

    S_StopSound( origin )
    cnum := S_GetChannel( origin, sfx )
    IF cnum < 0
        RETURN
    ENDIF

    sfx:usefulness++
    IF sfx:usefulness <= 0
        sfx:usefulness := 1
    ENDIF

    IF sfx:lumpnum < 0
        sfx:lumpnum := I_GetSfxLumpNum( sfx )
    ENDIF

    channels[ cnum + 1 ]:handle := I_StartSound( sfx, cnum, volume, sep )
RETURN

PROCEDURE S_PauseSound()
    IF !( mus_playing == NIL ) .AND. ! mus_paused
        I_PauseSong()
        mus_paused := .T.
    ENDIF
RETURN

PROCEDURE S_ResumeSound()
    IF !( mus_playing == NIL ) .AND. mus_paused
        I_ResumeSong()
        mus_paused := .F.
    ENDIF
RETURN

PROCEDURE S_UpdateSounds( listener )
    LOCAL cnum
    LOCAL volume
    LOCAL sep
    LOCAL sfx
    LOCAL c
    LOCAL audible
    MEMVAR snd_channels

    I_UpdateSound()

    FOR cnum := 0 TO snd_channels - 1
        c := channels[ cnum + 1 ]
        sfx := c:sfxinfo
        IF !( c:sfxinfo == NIL )
            IF I_SoundIsPlaying( c:handle )
                volume := snd_SfxVolume
                sep := NORM_SEP
                IF !( sfx:link == NIL )
                    volume += sfx:volume
                    IF volume < 1
                        S_StopChannel( cnum )
                        LOOP
                    ELSEIF volume > snd_SfxVolume
                        volume := snd_SfxVolume
                    ENDIF
                ENDIF
                IF !( c:origin == NIL ) .AND. !( listener == c:origin )
                    audible := S_AdjustSoundParams( listener, c:origin, @volume, @sep )
                    IF audible == 0
                        S_StopChannel( cnum )
                    ELSE
                        I_UpdateSoundParams( c:handle, volume, sep )
                    ENDIF
                ENDIF
            ELSE
                S_StopChannel( cnum )
            ENDIF
        ENDIF
    NEXT
RETURN

PROCEDURE S_SetMusicVolume( volume )
    IF volume < 0 .OR. volume > 127
        I_Error( "Attempt to set music volume at " + LTrim( Str( volume ) ) )
    ENDIF
    I_SetMusicVolume( volume )
RETURN

PROCEDURE S_SetSfxVolume( volume )
    IF volume < 0 .OR. volume > 127
        I_Error( "Attempt to set sfx volume at " + LTrim( Str( volume ) ) )
    ENDIF
    snd_SfxVolume := volume
RETURN

PROCEDURE S_StartMusic( m_id )
    S_ChangeMusic( m_id, .F. )
RETURN

PROCEDURE S_ChangeMusic( musicnum, looping )
    LOCAL music
    LOCAL namebuf
    LOCAL handle
    MEMVAR S_music
    MEMVAR snd_musicdevice

    IF musicnum == mus_intro .AND. ( snd_musicdevice == SNDDEVICE_ADLIB .OR. snd_musicdevice == SNDDEVICE_SB )
        musicnum := mus_introa
    ENDIF

    IF musicnum <= mus_None .OR. musicnum >= NUMMUSIC
        I_Error( "Bad music number " + LTrim( Str( musicnum ) ) )
    ELSE
        music := S_music[ musicnum + 1 ]
    ENDIF

    IF mus_playing == music
        RETURN
    ENDIF

    S_StopMusic()

    IF music:lumpnum == 0
        namebuf := "d_" + DEH_String( music:name )
        music:lumpnum := W_GetNumForName( namebuf )
    ENDIF

    music:data := W_CacheLumpNum( music:lumpnum, PU_STATIC )
    handle := I_RegisterSong( music:data, W_LumpLength( music:lumpnum ) )
    music:handle := handle
    I_PlaySong( handle, looping )
    mus_playing := music
RETURN

FUNCTION S_MusicPlaying()
RETURN I_MusicIsPlaying()

PROCEDURE S_StopMusic()
    IF !( mus_playing == NIL )
        IF mus_paused
            I_ResumeSong()
        ENDIF
        I_StopSong()
        I_UnRegisterSong( mus_playing:handle )
        W_ReleaseLumpNum( mus_playing:lumpnum )
        mus_playing:data := NIL
        mus_playing := NIL
    ENDIF
RETURN

INIT PROCEDURE init_s_sound
    PUBLIC sfxVolume
    PUBLIC musicVolume
    PUBLIC snd_channels

    channels := {}
    sfxVolume := 8
    musicVolume := 8
    snd_channels := 8
    snd_SfxVolume := 0
    mus_paused := .F.
    mus_playing := NIL
RETURN
