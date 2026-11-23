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

#include "deh_main.ch"
#include "d_englsh.ch"

STATIC FUNCTION BexTable()
RETURN { ;
    { "D_DEVSTR", D_DEVSTR }, { "D_CDROM", D_CDROM }, { "QUITMSG", QUITMSG }, ;
    { "LOADNET", LOADNET }, { "QLOADNET", QLOADNET }, { "QSAVESPOT", QSAVESPOT }, ;
    { "SAVEDEAD", SAVEDEAD }, { "QSPROMPT", QSPROMPT }, { "QLPROMPT", QLPROMPT }, ;
    { "NEWGAME", NEWGAME }, { "NIGHTMARE", NIGHTMARE }, { "SWSTRING", SWSTRING }, ;
    { "MSGOFF", MSGOFF }, { "MSGON", MSGON }, { "NETEND", NETEND }, { "ENDGAME", ENDGAME }, ;
    { "DETAILHI", DETAILHI }, { "DETAILLO", DETAILLO }, ;
    { "GAMMALVL0", GAMMALVL0 }, { "GAMMALVL1", GAMMALVL1 }, { "GAMMALVL2", GAMMALVL2 }, ;
    { "GAMMALVL3", GAMMALVL3 }, { "GAMMALVL4", GAMMALVL4 }, ;
    { "EMPTYSTRING", EMPTYSTRING }, { "GGSAVED", GGSAVED }, ;
    { "GOTARMOR", GOTARMOR }, { "GOTMEGA", GOTMEGA }, { "GOTHTHBONUS", GOTHTHBONUS }, ;
    { "GOTARMBONUS", GOTARMBONUS }, { "GOTSTIM", GOTSTIM }, { "GOTMEDINEED", GOTMEDINEED }, ;
    { "GOTMEDIKIT", GOTMEDIKIT }, { "GOTSUPER", GOTSUPER }, ;
    { "GOTBLUECARD", GOTBLUECARD }, { "GOTYELWCARD", GOTYELWCARD }, { "GOTREDCARD", GOTREDCARD }, ;
    { "GOTBLUESKUL", GOTBLUESKUL }, { "GOTYELWSKUL", GOTYELWSKUL }, { "GOTREDSKULL", GOTREDSKULL }, ;
    { "GOTINVUL", GOTINVUL }, { "GOTBERSERK", GOTBERSERK }, { "GOTINVIS", GOTINVIS }, ;
    { "GOTSUIT", GOTSUIT }, { "GOTMAP", GOTMAP }, { "GOTVISOR", GOTVISOR }, { "GOTMSPHERE", GOTMSPHERE }, ;
    { "GOTCLIP", GOTCLIP }, { "GOTCLIPBOX", GOTCLIPBOX }, { "GOTROCKET", GOTROCKET }, ;
    { "GOTROCKBOX", GOTROCKBOX }, { "GOTCELL", GOTCELL }, { "GOTCELLBOX", GOTCELLBOX }, ;
    { "GOTSHELLS", GOTSHELLS }, { "GOTSHELLBOX", GOTSHELLBOX }, { "GOTBACKPACK", GOTBACKPACK }, ;
    { "GOTBFG9000", GOTBFG9000 }, { "GOTCHAINGUN", GOTCHAINGUN }, { "GOTCHAINSAW", GOTCHAINSAW }, ;
    { "GOTLAUNCHER", GOTLAUNCHER }, { "GOTPLASMA", GOTPLASMA }, { "GOTSHOTGUN", GOTSHOTGUN }, ;
    { "GOTSHOTGUN2", GOTSHOTGUN2 }, ;
    { "PD_BLUEO", PD_BLUEO }, { "PD_REDO", PD_REDO }, { "PD_YELLOWO", PD_YELLOWO }, ;
    { "PD_BLUEK", PD_BLUEK }, { "PD_REDK", PD_REDK }, { "PD_YELLOWK", PD_YELLOWK }, ;
    { "HUSTR_E1M1", HUSTR_E1M1 }, { "HUSTR_E1M2", HUSTR_E1M2 }, { "HUSTR_E1M3", HUSTR_E1M3 }, ;
    { "HUSTR_E1M4", HUSTR_E1M4 }, { "HUSTR_E1M5", HUSTR_E1M5 }, { "HUSTR_E1M6", HUSTR_E1M6 }, ;
    { "HUSTR_E1M7", HUSTR_E1M7 }, { "HUSTR_E1M8", HUSTR_E1M8 }, { "HUSTR_E1M9", HUSTR_E1M9 }, ;
    { "HUSTR_E2M1", HUSTR_E2M1 }, { "HUSTR_E2M2", HUSTR_E2M2 }, { "HUSTR_E2M3", HUSTR_E2M3 }, ;
    { "HUSTR_E2M4", HUSTR_E2M4 }, { "HUSTR_E2M5", HUSTR_E2M5 }, { "HUSTR_E2M6", HUSTR_E2M6 }, ;
    { "HUSTR_E2M7", HUSTR_E2M7 }, { "HUSTR_E2M8", HUSTR_E2M8 }, { "HUSTR_E2M9", HUSTR_E2M9 }, ;
    { "HUSTR_E3M1", HUSTR_E3M1 }, { "HUSTR_E3M2", HUSTR_E3M2 }, { "HUSTR_E3M3", HUSTR_E3M3 }, ;
    { "HUSTR_E3M4", HUSTR_E3M4 }, { "HUSTR_E3M5", HUSTR_E3M5 }, { "HUSTR_E3M6", HUSTR_E3M6 }, ;
    { "HUSTR_E3M7", HUSTR_E3M7 }, { "HUSTR_E3M8", HUSTR_E3M8 }, { "HUSTR_E3M9", HUSTR_E3M9 }, ;
    { "HUSTR_E4M1", HUSTR_E4M1 }, { "HUSTR_E4M2", HUSTR_E4M2 }, { "HUSTR_E4M3", HUSTR_E4M3 }, ;
    { "HUSTR_E4M4", HUSTR_E4M4 }, { "HUSTR_E4M5", HUSTR_E4M5 }, { "HUSTR_E4M6", HUSTR_E4M6 }, ;
    { "HUSTR_E4M7", HUSTR_E4M7 }, { "HUSTR_E4M8", HUSTR_E4M8 }, { "HUSTR_E4M9", HUSTR_E4M9 }, ;
    { "HUSTR_1", HUSTR_1 }, { "HUSTR_2", HUSTR_2 }, { "HUSTR_3", HUSTR_3 }, { "HUSTR_4", HUSTR_4 }, ;
    { "HUSTR_5", HUSTR_5 }, { "HUSTR_6", HUSTR_6 }, { "HUSTR_7", HUSTR_7 }, { "HUSTR_8", HUSTR_8 }, ;
    { "HUSTR_9", HUSTR_9 }, { "HUSTR_10", HUSTR_10 }, { "HUSTR_11", HUSTR_11 }, { "HUSTR_12", HUSTR_12 }, ;
    { "HUSTR_13", HUSTR_13 }, { "HUSTR_14", HUSTR_14 }, { "HUSTR_15", HUSTR_15 }, { "HUSTR_16", HUSTR_16 }, ;
    { "HUSTR_17", HUSTR_17 }, { "HUSTR_18", HUSTR_18 }, { "HUSTR_19", HUSTR_19 }, { "HUSTR_20", HUSTR_20 }, ;
    { "HUSTR_21", HUSTR_21 }, { "HUSTR_22", HUSTR_22 }, { "HUSTR_23", HUSTR_23 }, { "HUSTR_24", HUSTR_24 }, ;
    { "HUSTR_25", HUSTR_25 }, { "HUSTR_26", HUSTR_26 }, { "HUSTR_27", HUSTR_27 }, { "HUSTR_28", HUSTR_28 }, ;
    { "HUSTR_29", HUSTR_29 }, { "HUSTR_30", HUSTR_30 }, { "HUSTR_31", HUSTR_31 }, { "HUSTR_32", HUSTR_32 }, ;
    { "AMSTR_FOLLOWON", AMSTR_FOLLOWON }, { "AMSTR_FOLLOWOFF", AMSTR_FOLLOWOFF }, ;
    { "AMSTR_GRIDON", AMSTR_GRIDON }, { "AMSTR_GRIDOFF", AMSTR_GRIDOFF }, ;
    { "AMSTR_MARKEDSPOT", AMSTR_MARKEDSPOT }, { "AMSTR_MARKSCLEARED", AMSTR_MARKSCLEARED }, ;
    { "STSTR_MUS", STSTR_MUS }, { "STSTR_NOMUS", STSTR_NOMUS }, ;
    { "STSTR_DQDON", STSTR_DQDON }, { "STSTR_DQDOFF", STSTR_DQDOFF }, ;
    { "STSTR_KFAADDED", STSTR_KFAADDED }, { "STSTR_FAADDED", STSTR_FAADDED }, ;
    { "STSTR_NCON", STSTR_NCON }, { "STSTR_NCOFF", STSTR_NCOFF }, ;
    { "STSTR_BEHOLD", STSTR_BEHOLD }, { "STSTR_BEHOLDX", STSTR_BEHOLDX }, ;
    { "STSTR_CHOPPERS", STSTR_CHOPPERS }, { "STSTR_CLEV", STSTR_CLEV }, ;
    { "E1TEXT", E1TEXT }, { "E2TEXT", E2TEXT }, { "E3TEXT", E3TEXT }, { "E4TEXT", E4TEXT }, ;
    { "C1TEXT", C1TEXT }, { "C2TEXT", C2TEXT }, { "C3TEXT", C3TEXT }, ;
    { "C4TEXT", C4TEXT }, { "C5TEXT", C5TEXT }, { "C6TEXT", C6TEXT }, ;
    { "CC_ZOMBIE", CC_ZOMBIE }, { "CC_SHOTGUN", CC_SHOTGUN }, { "CC_HEAVY", CC_HEAVY }, ;
    { "CC_IMP", CC_IMP }, { "CC_DEMON", CC_DEMON }, { "CC_LOST", CC_LOST }, ;
    { "CC_CACO", CC_CACO }, { "CC_HELL", CC_HELL }, { "CC_BARON", CC_BARON }, ;
    { "CC_ARACH", CC_ARACH }, { "CC_PAIN", CC_PAIN }, { "CC_REVEN", CC_REVEN }, ;
    { "CC_MANCU", CC_MANCU }, { "CC_ARCH", CC_ARCH }, { "CC_SPIDER", CC_SPIDER }, ;
    { "CC_CYBER", CC_CYBER }, { "CC_HERO", CC_HERO }, ;
    { "BGFLATE1", "FLOOR4_8" }, { "BGFLATE2", "SFLR6_1" }, { "BGFLATE3", "MFLR8_4" }, ;
    { "BGFLATE4", "MFLR8_3" }, { "BGFLAT06", "SLIME16" }, { "BGFLAT11", "RROCK14" }, ;
    { "BGFLAT20", "RROCK07" }, { "BGFLAT30", "RROCK17" }, { "BGFLAT15", "RROCK13" }, ;
    { "BGFLAT31", "RROCK19" }, { "BGCASTCALL", "BOSSBACK" } }

FUNCTION DEH_BEXStrStart( oCtx, cLine )
    IF hb_stricmp( Left( AllTrim( cLine ), 9 ), "[STRINGS]" ) != 0
        DEH_Warning( oCtx, "Parse error on section start" )
    ENDIF
RETURN NIL

FUNCTION DEH_BEXStrParseLine( oCtx, cLine, xTag )
    LOCAL aAsg
    LOCAL aTab
    LOCAL i

    HB_SYMBOL_UNUSED( xTag )
    aAsg := DEH_ParseAssignment( cLine )
    IF aAsg == NIL
        DEH_Warning( oCtx, "Failed to parse assignment" )
        RETURN NIL
    ENDIF
    aTab := BexTable()
    FOR i := 1 TO Len( aTab )
        IF aTab[ i, 1 ] == aAsg[ 1 ]
            DEH_AddStringReplacement( aTab[ i, 2 ], aAsg[ 2 ] )
            RETURN NIL
        ENDIF
    NEXT
RETURN NIL
