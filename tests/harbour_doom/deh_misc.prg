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

#include "deh_misc.ch"
#include "deh_main.ch"

INIT PROCEDURE init_deh_misc
    PUBLIC deh_initial_health
    PUBLIC deh_initial_bullets
    PUBLIC deh_max_health
    PUBLIC deh_max_armor
    PUBLIC deh_green_armor_class
    PUBLIC deh_blue_armor_class
    PUBLIC deh_max_soulsphere
    PUBLIC deh_soulsphere_health
    PUBLIC deh_megasphere_health
    PUBLIC deh_god_mode_health
    PUBLIC deh_idfa_armor
    PUBLIC deh_idfa_armor_class
    PUBLIC deh_idkfa_armor
    PUBLIC deh_idkfa_armor_class
    PUBLIC deh_bfg_cells_per_shot
    PUBLIC deh_species_infighting

    deh_initial_health      := DEH_DEFAULT_INITIAL_HEALTH
    deh_initial_bullets     := DEH_DEFAULT_INITIAL_BULLETS
    deh_max_health          := DEH_DEFAULT_MAX_HEALTH
    deh_max_armor           := DEH_DEFAULT_MAX_ARMOR
    deh_green_armor_class   := DEH_DEFAULT_GREEN_ARMOR_CLASS
    deh_blue_armor_class    := DEH_DEFAULT_BLUE_ARMOR_CLASS
    deh_max_soulsphere      := DEH_DEFAULT_MAX_SOULSPHERE
    deh_soulsphere_health   := DEH_DEFAULT_SOULSPHERE_HEALTH
    deh_megasphere_health   := DEH_DEFAULT_MEGASPHERE_HEALTH
    deh_god_mode_health     := DEH_DEFAULT_GOD_MODE_HEALTH
    deh_idfa_armor          := DEH_DEFAULT_IDFA_ARMOR
    deh_idfa_armor_class    := DEH_DEFAULT_IDFA_ARMOR_CLASS
    deh_idkfa_armor         := DEH_DEFAULT_IDKFA_ARMOR
    deh_idkfa_armor_class   := DEH_DEFAULT_IDKFA_ARMOR_CLASS
    deh_bfg_cells_per_shot  := DEH_DEFAULT_BFG_CELLS_PER_SHOT
    deh_species_infighting  := DEH_DEFAULT_SPECIES_INFIGHTING
RETURN

FUNCTION DEH_MiscStart( oCtx, cLine )
    HB_SYMBOL_UNUSED( oCtx )
    HB_SYMBOL_UNUSED( cLine )
RETURN NIL

FUNCTION DEH_MiscParseLine( oCtx, cLine, xTag )
    LOCAL aAsg
    LOCAL nVal
    MEMVAR deh_initial_health
    MEMVAR deh_initial_bullets
    MEMVAR deh_max_health
    MEMVAR deh_max_armor
    MEMVAR deh_green_armor_class
    MEMVAR deh_blue_armor_class
    MEMVAR deh_max_soulsphere
    MEMVAR deh_soulsphere_health
    MEMVAR deh_megasphere_health
    MEMVAR deh_god_mode_health
    MEMVAR deh_idfa_armor
    MEMVAR deh_idfa_armor_class
    MEMVAR deh_idkfa_armor
    MEMVAR deh_idkfa_armor_class
    MEMVAR deh_bfg_cells_per_shot
    MEMVAR deh_species_infighting

    HB_SYMBOL_UNUSED( xTag )
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    nVal := Int( Val( aAsg[ 2 ] ) )
    IF hb_stricmp( aAsg[ 1 ], "Monsters Infight" ) == 0
        IF nVal == 202
            deh_species_infighting := 0
        ELSEIF nVal == 221
            deh_species_infighting := 1
        ELSE
            DEH_Warning( oCtx, "Invalid value for 'Monsters Infight': " + hb_ntos( nVal ) )
        ENDIF
        RETURN NIL
    ENDIF
    IF hb_stricmp( aAsg[ 1 ], "Initial Health" ) == 0
        deh_initial_health := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Initial Bullets" ) == 0
        deh_initial_bullets := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Max Health" ) == 0
        deh_max_health := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Max Armor" ) == 0
        deh_max_armor := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Green Armor Class" ) == 0
        deh_green_armor_class := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Blue Armor Class" ) == 0
        deh_blue_armor_class := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Max Soulsphere" ) == 0
        deh_max_soulsphere := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Soulsphere Health" ) == 0
        deh_soulsphere_health := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "Megasphere Health" ) == 0
        deh_megasphere_health := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "God Mode Health" ) == 0
        deh_god_mode_health := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "IDFA Armor" ) == 0
        deh_idfa_armor := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "IDFA Armor Class" ) == 0
        deh_idfa_armor_class := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "IDKFA Armor" ) == 0
        deh_idkfa_armor := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "IDKFA Armor Class" ) == 0
        deh_idkfa_armor_class := nVal
    ELSEIF hb_stricmp( aAsg[ 1 ], "BFG Cells/Shot" ) == 0
        deh_bfg_cells_per_shot := nVal
    ELSE
        DEH_Warning( oCtx, "Unknown Misc variable '" + aAsg[ 1 ] + "'" )
    ENDIF
RETURN NIL
