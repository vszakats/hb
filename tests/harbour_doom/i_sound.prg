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

STATIC sound_module := NIL
STATIC music_module := NIL
STATIC snd_sbport := 0
STATIC snd_sbirq := 0
STATIC snd_sbdma := 0
STATIC snd_mport := 0
STATIC snd_samplerate
STATIC snd_cachesize
STATIC snd_maxslicetime_ms
STATIC snd_musiccmd
STATIC snd_sfxdevice

#include "i_sound.ch"

CLASS sfxinfo_t
    DATA tagname
    DATA name
    DATA priority
    DATA link
    DATA pitch
    DATA volume
    DATA usefulness
    DATA lumpnum
    DATA numchannels
    DATA driver_data
    METHOD New()
ENDCLASS
CLASS musicinfo_t
    DATA name
    DATA lumpnum
    DATA data
    DATA handle
    METHOD New()
ENDCLASS
CLASS sound_module_t
    DATA sound_devices
    DATA num_sound_devices
    DATA Init
    DATA Shutdown
    DATA GetSfxLumpNum
    DATA Update
    DATA UpdateSoundParams
    DATA StartSound
    DATA StopSound
    DATA SoundIsPlaying
    DATA CacheSounds
    METHOD New()
ENDCLASS
CLASS music_module_t
    DATA sound_devices
    DATA num_sound_devices
    DATA Init
    DATA Shutdown
    DATA SetMusicVolume
    DATA PauseMusic
    DATA ResumeMusic
    DATA RegisterSong
    DATA UnRegisterSong
    DATA PlaySong
    DATA StopSong
    DATA MusicIsPlaying
    DATA Poll
    METHOD New()
ENDCLASS



METHOD New() CLASS sfxinfo_t
    ::tagname := ""
    ::name := ""
    ::priority := 0
    ::link := NIL
    ::pitch := -1
    ::volume := -1
    ::usefulness := 0
    ::lumpnum := -1
    ::numchannels := 0
    ::driver_data := NIL
RETURN Self

METHOD New() CLASS musicinfo_t
    ::name := ""
    ::lumpnum := 0
    ::data := NIL
    ::handle := NIL
RETURN Self

METHOD New() CLASS sound_module_t
    ::sound_devices := {}
    ::num_sound_devices := 0
    ::Init := NIL
    ::Shutdown := NIL
    ::GetSfxLumpNum := NIL
    ::Update := NIL
    ::UpdateSoundParams := NIL
    ::StartSound := NIL
    ::StopSound := NIL
    ::SoundIsPlaying := NIL
    ::CacheSounds := NIL
RETURN Self

METHOD New() CLASS music_module_t
    ::sound_devices := {}
    ::num_sound_devices := 0
    ::Init := NIL
    ::Shutdown := NIL
    ::SetMusicVolume := NIL
    ::PauseMusic := NIL
    ::ResumeMusic := NIL
    ::RegisterSong := NIL
    ::UnRegisterSong := NIL
    ::PlaySong := NIL
    ::StopSong := NIL
    ::MusicIsPlaying := NIL
    ::Poll := NIL
RETURN Self

INIT PROCEDURE init_i_sound

    PUBLIC snd_musicdevice

    snd_samplerate := 44100
    snd_cachesize := 64 * 1024 * 1024
    snd_maxslicetime_ms := 28
    snd_musiccmd := ""
    snd_musicdevice := SNDDEVICE_SB
    snd_sfxdevice := SNDDEVICE_SB

    sound_module := NIL
    music_module := NIL
    snd_sbport := 0
    snd_sbirq := 0
    snd_sbdma := 0
    snd_mport := 0
RETURN

STATIC FUNCTION IfaceCall( xFun, x1, x2, x3, x4 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF

    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ELSEIF nArgs == 2
            RETURN Eval( xFun, x1, x2 )
        ELSEIF nArgs == 3
            RETURN Eval( xFun, x1, x2, x3 )
        ENDIF
        RETURN Eval( xFun, x1, x2, x3, x4 )
    ENDIF
RETURN NIL

STATIC FUNCTION SndDeviceInList( nDevice, aList, nLen )
    LOCAL i

    IF ValType( aList ) != "A"
        RETURN .F.
    ENDIF
    IF nLen == NIL
        nLen := Len( aList )
    ENDIF
    FOR i := 0 TO nLen - 1
        IF aList[ i + 1 ] == nDevice
            RETURN .T.
        ENDIF
    NEXT
RETURN .F.

STATIC PROCEDURE InitSfxModule( lPrefix )
    LOCAL aMods
    LOCAL i
    LOCAL oMod
    MEMVAR DG_sound_module

    sound_module := NIL
    aMods := { DG_sound_module }
    FOR i := 1 TO Len( aMods )
        oMod := aMods[ i ]
        IF oMod == NIL
            LOOP
        ENDIF
        IF SndDeviceInList( snd_sfxdevice, oMod:sound_devices, oMod:num_sound_devices )
            IF IfaceCall( oMod:Init, lPrefix )
                sound_module := oMod
                RETURN
            ENDIF
        ENDIF
    NEXT
RETURN

STATIC PROCEDURE InitMusicModule()
    MEMVAR DG_music_module
#ifdef FEATURE_SOUND
    music_module := DG_music_module
#endif
RETURN

FUNCTION I_InitSound( lPrefix )
    LOCAL lNoSound
    LOCAL lNoSfx
    LOCAL lNoMusic
    MEMVAR screensaver_mode
    MEMVAR snd_musicdevice

    lNoSound := M_CheckParm( "-nosound" ) > 0
    lNoSfx := M_CheckParm( "-nosfx" ) > 0
    lNoMusic := M_CheckParm( "-nomusic" ) > 0

    IF ! lNoSound .AND. Empty( screensaver_mode )
        IF ! lNoMusic ;
             .AND. ( snd_musicdevice == SNDDEVICE_GENMIDI ;
             .OR. snd_musicdevice == SNDDEVICE_GUS )
        ENDIF

        IF ! lNoSfx
            InitSfxModule( lPrefix )
        ENDIF

        IF ! lNoMusic
            InitMusicModule()
        ENDIF
    ENDIF
RETURN NIL

FUNCTION I_ShutdownSound()
    IF sound_module != NIL
        IfaceCall( sound_module:Shutdown )
    ENDIF
    IF music_module != NIL
        IfaceCall( music_module:Shutdown )
    ENDIF
RETURN NIL

FUNCTION I_GetSfxLumpNum( sfxinfo )
    IF sound_module != NIL
        RETURN IfaceCall( sound_module:GetSfxLumpNum, sfxinfo )
    ENDIF
RETURN 0

FUNCTION I_UpdateSound()
    IF sound_module != NIL
        IfaceCall( sound_module:Update )
    ENDIF
    IF music_module != NIL .AND. music_module:Poll != NIL
        IfaceCall( music_module:Poll )
    ENDIF
RETURN NIL

STATIC PROCEDURE CheckVolumeSeparation( nVol, nSep )
    IF nSep < 0
        nSep := 0
    ELSEIF nSep > 254
        nSep := 254
    ENDIF
    IF nVol < 0
        nVol := 0
    ELSEIF nVol > 127
        nVol := 127
    ENDIF
RETURN

FUNCTION I_UpdateSoundParams( channel, vol, sep )
    IF sound_module != NIL
        CheckVolumeSeparation( @vol, @sep )
        IfaceCall( sound_module:UpdateSoundParams, channel, vol, sep )
    ENDIF
RETURN NIL

FUNCTION I_StartSound( sfxinfo, channel, vol, sep )
    IF sound_module != NIL
        CheckVolumeSeparation( @vol, @sep )
        RETURN IfaceCall( sound_module:StartSound, sfxinfo, channel, vol, sep )
    ENDIF
RETURN 0

FUNCTION I_StopSound( channel )
    IF sound_module != NIL
        IfaceCall( sound_module:StopSound, channel )
    ENDIF
RETURN NIL

FUNCTION I_SoundIsPlaying( channel )
    IF sound_module != NIL
        RETURN IfaceCall( sound_module:SoundIsPlaying, channel )
    ENDIF
RETURN .F.

FUNCTION I_PrecacheSounds( sounds, num_sounds )
    IF sound_module != NIL .AND. sound_module:CacheSounds != NIL
        IfaceCall( sound_module:CacheSounds, sounds, num_sounds )
    ENDIF
RETURN NIL

FUNCTION I_InitMusic()
    IF music_module != NIL
        IfaceCall( music_module:Init )
    ENDIF
RETURN NIL

FUNCTION I_ShutdownMusic()
RETURN NIL

FUNCTION I_SetMusicVolume( volume )
    IF music_module != NIL
        IfaceCall( music_module:SetMusicVolume, volume )
    ENDIF
RETURN NIL

FUNCTION I_PauseSong()
    IF music_module != NIL
        IfaceCall( music_module:PauseMusic )
    ENDIF
RETURN NIL

FUNCTION I_ResumeSong()
    IF music_module != NIL
        IfaceCall( music_module:ResumeMusic )
    ENDIF
RETURN NIL

FUNCTION I_RegisterSong( data, nLen )
    IF music_module != NIL
        RETURN IfaceCall( music_module:RegisterSong, data, nLen )
    ENDIF
RETURN NIL

FUNCTION I_UnRegisterSong( handle )
    IF music_module != NIL
        IfaceCall( music_module:UnRegisterSong, handle )
    ENDIF
RETURN NIL

FUNCTION I_PlaySong( handle, looping )
    IF music_module != NIL
        IfaceCall( music_module:PlaySong, handle, looping )
    ENDIF
RETURN NIL

FUNCTION I_StopSong()
    IF music_module != NIL
        IfaceCall( music_module:StopSong )
    ENDIF
RETURN NIL

FUNCTION I_MusicIsPlaying()
    IF music_module != NIL
        RETURN IfaceCall( music_module:MusicIsPlaying )
    ENDIF
RETURN .F.

FUNCTION I_BindSoundVariables()
    MEMVAR gus_patch_path
    MEMVAR gus_ram_kb
    MEMVAR libsamplerate_scale
    MEMVAR snd_musicdevice
    MEMVAR timidity_cfg_path
    MEMVAR use_libsamplerate
    M_BindVariable( "snd_musicdevice",    @snd_musicdevice )
    M_BindVariable( "snd_sfxdevice",      @snd_sfxdevice )
    M_BindVariable( "snd_sbport",         @snd_sbport )
    M_BindVariable( "snd_sbirq",          @snd_sbirq )
    M_BindVariable( "snd_sbdma",          @snd_sbdma )
    M_BindVariable( "snd_mport",          @snd_mport )
    M_BindVariable( "snd_maxslicetime_ms", @snd_maxslicetime_ms )
    M_BindVariable( "snd_musiccmd",       @snd_musiccmd )
    M_BindVariable( "snd_samplerate",     @snd_samplerate )
    M_BindVariable( "snd_cachesize",      @snd_cachesize )
#ifdef FEATURE_SOUND
    M_BindVariable( "use_libsamplerate",   @use_libsamplerate )
    M_BindVariable( "libsamplerate_scale", @libsamplerate_scale )
    M_BindVariable( "timidity_cfg_path",   @timidity_cfg_path )
    M_BindVariable( "gus_patch_path",      @gus_patch_path )
    M_BindVariable( "gus_ram_kb",          @gus_ram_kb )
#endif
RETURN NIL
