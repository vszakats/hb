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

#include "net_defs.ch"

CLASS net_packet_t
    DATA data
    DATA len
    DATA alloced
    DATA pos
    METHOD New()
ENDCLASS
CLASS net_module_t
    DATA InitClient
    DATA InitServer
    DATA SendPacket
    DATA RecvPacket
    DATA AddrToString
    DATA FreeAddress
    DATA ResolveAddress
    METHOD New()
ENDCLASS
CLASS net_addr_t
    DATA module
    DATA handle
    METHOD New()
ENDCLASS
CLASS net_context_t
    DATA mods
    DATA num_mods
    METHOD New()
ENDCLASS
CLASS net_connect_data_t
    DATA gamemode
    DATA gamemission
    DATA lowres_turn
    DATA drone
    DATA max_players
    DATA is_freedoom
    DATA wad_sha1sum
    DATA deh_sha1sum
    DATA player_class
    METHOD New()
ENDCLASS
CLASS net_gamesettings_t
    DATA ticdup
    DATA extratics
    DATA deathmatch
    DATA episode
    DATA nomonsters
    DATA fast_monsters
    DATA respawn_monsters
    DATA map
    DATA skill
    DATA gameversion
    DATA lowres_turn
    DATA new_sync
    DATA timelimit
    DATA loadgame
    DATA random
    DATA num_players
    DATA consoleplayer
    DATA player_classes
    METHOD New()
ENDCLASS
CLASS net_ticdiff_t
    DATA diff
    DATA cmd
    METHOD New()
ENDCLASS
CLASS net_full_ticcmd_t
    DATA latency
    DATA seq
    DATA playeringame
    DATA cmds
    METHOD New()
ENDCLASS
CLASS net_querydata_t
    DATA version
    DATA server_state
    DATA num_players
    DATA max_players
    DATA gamemode
    DATA gamemission
    DATA description
    METHOD New()
ENDCLASS
CLASS net_waitdata_t
    DATA num_players
    DATA num_drones
    DATA ready_players
    DATA max_players
    DATA is_controller
    DATA consoleplayer
    DATA player_names
    DATA player_addrs
    DATA wad_sha1sum
    DATA deh_sha1sum
    DATA is_freedoom
    METHOD New()
ENDCLASS

STATIC FUNCTION Sha1Zeros()
    LOCAL a
    LOCAL i

    a := {}
    FOR i := 1 TO 20
        AAdd( a, 0 )
    NEXT
RETURN a

METHOD New() CLASS net_packet_t
    ::data    := ""
    ::len     := 0
    ::alloced := 0
    ::pos     := 0
RETURN Self

METHOD New() CLASS net_module_t
    ::InitClient     := NIL
    ::InitServer     := NIL
    ::SendPacket     := NIL
    ::RecvPacket     := NIL
    ::AddrToString   := NIL
    ::FreeAddress    := NIL
    ::ResolveAddress := NIL
RETURN Self

METHOD New() CLASS net_addr_t
    ::module := NIL
    ::handle := NIL
RETURN Self

METHOD New() CLASS net_context_t
    ::mods     := {}
    ::num_mods := 0
RETURN Self

METHOD New() CLASS net_connect_data_t
    ::gamemode     := 0
    ::gamemission  := 0
    ::lowres_turn  := 0
    ::drone        := 0
    ::max_players  := 0
    ::is_freedoom  := 0
    ::wad_sha1sum  := Sha1Zeros()
    ::deh_sha1sum  := Sha1Zeros()
    ::player_class := 0
RETURN Self

METHOD New() CLASS net_gamesettings_t
    LOCAL i

    ::ticdup            := 0
    ::extratics         := 0
    ::deathmatch        := 0
    ::episode           := 0
    ::nomonsters        := 0
    ::fast_monsters     := 0
    ::respawn_monsters  := 0
    ::map               := 0
    ::skill             := 0
    ::gameversion       := 0
    ::lowres_turn       := 0
    ::new_sync          := 0
    ::timelimit         := 0
    ::loadgame          := 0
    ::random            := 0
    ::num_players       := 0
    ::consoleplayer     := 0
    ::player_classes    := {}
    FOR i := 1 TO NET_MAXPLAYERS
        AAdd( ::player_classes, 0 )
    NEXT
RETURN Self

METHOD New() CLASS net_ticdiff_t
    ::diff := 0
    ::cmd  := ticcmd_t():New()
RETURN Self

METHOD New() CLASS net_full_ticcmd_t
    LOCAL i

    ::latency      := 0
    ::seq          := 0
    ::playeringame := {}
    ::cmds         := {}
    FOR i := 1 TO NET_MAXPLAYERS
        AAdd( ::playeringame, .F. )
        AAdd( ::cmds, net_ticdiff_t():New() )
    NEXT
RETURN Self

METHOD New() CLASS net_querydata_t
    ::version      := ""
    ::server_state := 0
    ::num_players  := 0
    ::max_players  := 0
    ::gamemode     := 0
    ::gamemission  := 0
    ::description  := ""
RETURN Self

METHOD New() CLASS net_waitdata_t
    LOCAL i

    ::num_players    := 0
    ::num_drones     := 0
    ::ready_players  := 0
    ::max_players    := 0
    ::is_controller  := 0
    ::consoleplayer  := 0
    ::player_names   := {}
    ::player_addrs   := {}
    ::wad_sha1sum    := Sha1Zeros()
    ::deh_sha1sum    := Sha1Zeros()
    ::is_freedoom    := 0
    FOR i := 1 TO NET_MAXPLAYERS
        AAdd( ::player_names, "" )
        AAdd( ::player_addrs, "" )
    NEXT
RETURN Self
