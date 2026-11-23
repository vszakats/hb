/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef NET_DEFS_H
#define NET_DEFS_H

#include "doomtype.ch"
#include "d_ticcmd.ch"

#ifndef MAXNETNODES
#define MAXNETNODES 16
#endif

#ifndef NET_MAXPLAYERS
#define NET_MAXPLAYERS 8
#endif

#ifndef MAXPLAYERNAME
#define MAXPLAYERNAME 30
#endif

#ifndef BACKUPTICS
#define BACKUPTICS 128
#endif

#define NET_MAGIC_NUMBER 3436803284

#ifndef NET_RELIABLE_PACKET
#define NET_RELIABLE_PACKET 32768
#endif

#define NET_PACKET_TYPE_SYN              0
#define NET_PACKET_TYPE_ACK              1
#define NET_PACKET_TYPE_REJECTED         2
#define NET_PACKET_TYPE_KEEPALIVE        3
#define NET_PACKET_TYPE_WAITING_DATA     4
#define NET_PACKET_TYPE_GAMESTART        5
#define NET_PACKET_TYPE_GAMEDATA         6
#define NET_PACKET_TYPE_GAMEDATA_ACK     7
#define NET_PACKET_TYPE_DISCONNECT       8
#define NET_PACKET_TYPE_DISCONNECT_ACK   9
#define NET_PACKET_TYPE_RELIABLE_ACK     10
#define NET_PACKET_TYPE_GAMEDATA_RESEND  11
#define NET_PACKET_TYPE_CONSOLE_MESSAGE  12
#define NET_PACKET_TYPE_QUERY            13
#define NET_PACKET_TYPE_QUERY_RESPONSE   14
#define NET_PACKET_TYPE_LAUNCH           15

#define NET_MASTER_PACKET_TYPE_ADD                    0
#define NET_MASTER_PACKET_TYPE_ADD_RESPONSE           1
#define NET_MASTER_PACKET_TYPE_QUERY                  2
#define NET_MASTER_PACKET_TYPE_QUERY_RESPONSE         3
#define NET_MASTER_PACKET_TYPE_GET_METADATA           4
#define NET_MASTER_PACKET_TYPE_GET_METADATA_RESPONSE  5
#define NET_MASTER_PACKET_TYPE_SIGN_START             6
#define NET_MASTER_PACKET_TYPE_SIGN_START_RESPONSE    7
#define NET_MASTER_PACKET_TYPE_SIGN_END               8
#define NET_MASTER_PACKET_TYPE_SIGN_END_RESPONSE      9

#define NET_TICDIFF_FORWARD      1
#define NET_TICDIFF_SIDE         2
#define NET_TICDIFF_TURN         4
#define NET_TICDIFF_BUTTONS      8
#define NET_TICDIFF_CONSISTANCY  16
#define NET_TICDIFF_CHATCHAR     32
#define NET_TICDIFF_RAVEN        64
#define NET_TICDIFF_STRIFE       128

#endif
