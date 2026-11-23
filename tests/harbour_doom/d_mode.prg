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

STATIC valid_modes := {}
STATIC valid_versions := {}

#include "d_mode.ch"

CLASS valid_mode_t
    DATA mission
    DATA mode
    DATA episode
    DATA map
    METHOD New()
ENDCLASS
CLASS valid_version_t
    DATA mission
    DATA version
    METHOD New()
ENDCLASS


METHOD New( mission, mode, episode, map ) CLASS valid_mode_t
    ::mission := iif( mission == NIL, 0, mission )
    ::mode    := iif( mode == NIL, 0, mode )
    ::episode := iif( episode == NIL, 0, episode )
    ::map     := iif( map == NIL, 0, map )
RETURN Self

METHOD New( mission, version ) CLASS valid_version_t
    ::mission := iif( mission == NIL, 0, mission )
    ::version := iif( version == NIL, 0, version )
RETURN Self

INIT PROCEDURE init_d_mode

    valid_modes := {}
    AAdd( valid_modes, valid_mode_t():New( pack_chex, shareware,  1, 5 ) )
    AAdd( valid_modes, valid_mode_t():New( doom,      shareware,  1, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( doom,      registered, 3, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( doom,      retail,     4, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( doom2,     commercial, 1, 32 ) )
    AAdd( valid_modes, valid_mode_t():New( pack_tnt,  commercial, 1, 32 ) )
    AAdd( valid_modes, valid_mode_t():New( pack_plut, commercial, 1, 32 ) )
    AAdd( valid_modes, valid_mode_t():New( pack_hacx, commercial, 1, 32 ) )
    AAdd( valid_modes, valid_mode_t():New( heretic,   shareware,  1, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( heretic,   registered, 3, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( heretic,   retail,     5, 9 ) )
    AAdd( valid_modes, valid_mode_t():New( hexen,     commercial, 1, 60 ) )
    AAdd( valid_modes, valid_mode_t():New( strife,    commercial, 1, 34 ) )

    valid_versions := {}
    AAdd( valid_versions, valid_version_t():New( doom,    exe_doom_1_9 ) )
    AAdd( valid_versions, valid_version_t():New( doom,    exe_hacx ) )
    AAdd( valid_versions, valid_version_t():New( doom,    exe_ultimate ) )
    AAdd( valid_versions, valid_version_t():New( doom,    exe_final ) )
    AAdd( valid_versions, valid_version_t():New( doom,    exe_final2 ) )
    AAdd( valid_versions, valid_version_t():New( doom,    exe_chex ) )
    AAdd( valid_versions, valid_version_t():New( heretic, exe_heretic_1_3 ) )
    AAdd( valid_versions, valid_version_t():New( hexen,   exe_hexen_1_1 ) )
    AAdd( valid_versions, valid_version_t():New( strife,  exe_strife_1_2 ) )
    AAdd( valid_versions, valid_version_t():New( strife,  exe_strife_1_31 ) )
RETURN

FUNCTION D_ValidGameMode( mission, mode )
    LOCAL i

    FOR i := 0 TO Len( valid_modes ) - 1
        IF valid_modes[ i + 1 ]:mode == mode .AND. valid_modes[ i + 1 ]:mission == mission
            RETURN .T.
        ENDIF
    NEXT
RETURN .F.

FUNCTION D_ValidEpisodeMap( mission, mode, episode, map )
    LOCAL i

    IF mission == heretic
        IF mode == retail .AND. episode == 6
            RETURN map >= 1 .AND. map <= 3
        ELSEIF mode == registered .AND. episode == 4
            RETURN map == 1
        ENDIF
    ENDIF

    FOR i := 0 TO Len( valid_modes ) - 1
        IF mission == valid_modes[ i + 1 ]:mission .AND. mode == valid_modes[ i + 1 ]:mode
            RETURN episode >= 1 .AND. episode <= valid_modes[ i + 1 ]:episode ;
                .AND. map >= 1 .AND. map <= valid_modes[ i + 1 ]:map
        ENDIF
    NEXT
RETURN .F.

FUNCTION D_GetNumEpisodes( mission, mode )
    LOCAL episode

    episode := 1
    DO WHILE D_ValidEpisodeMap( mission, mode, episode, 1 )
        episode := episode + 1
    ENDDO
RETURN episode - 1

FUNCTION D_ValidGameVersion( mission, version )
    LOCAL i

    IF mission == doom2 .OR. mission == pack_plut .OR. mission == pack_tnt ;
         .OR. mission == pack_hacx .OR. mission == pack_chex
        mission := doom
    ENDIF

    FOR i := 0 TO Len( valid_versions ) - 1
        IF valid_versions[ i + 1 ]:mission == mission ;
             .AND. valid_versions[ i + 1 ]:version == version
            RETURN .T.
        ENDIF
    NEXT
RETURN .F.

FUNCTION D_IsEpisodeMap( mission )
    SWITCH mission
    CASE doom
    CASE heretic
    CASE pack_chex
        RETURN .T.
    CASE none
    CASE hexen
    CASE doom2
    CASE pack_hacx
    CASE pack_tnt
    CASE pack_plut
    CASE strife
    OTHERWISE
        RETURN .F.
    ENDSWITCH
RETURN .F.

FUNCTION D_GameMissionString( mission )
    SWITCH mission
    CASE doom
        RETURN "doom"
    CASE doom2
        RETURN "doom2"
    CASE pack_tnt
        RETURN "tnt"
    CASE pack_plut
        RETURN "plutonia"
    CASE pack_hacx
        RETURN "hacx"
    CASE pack_chex
        RETURN "chex"
    CASE heretic
        RETURN "heretic"
    CASE hexen
        RETURN "hexen"
    CASE strife
        RETURN "strife"
    CASE none
    OTHERWISE
        RETURN "none"
    ENDSWITCH
RETURN "none"
