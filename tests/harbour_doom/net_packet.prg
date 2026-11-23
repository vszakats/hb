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

#include "net_packet.ch"

STATIC PROCEDURE PacketIncrease( packet )
    LOCAL nNew
    LOCAL cExtra

    nNew := packet:alloced * 2
    IF nNew < 256
        nNew := 256
    ENDIF
    cExtra := Replicate( Chr( 0 ), nNew - Len( packet:data ) )
    packet:data := packet:data + cExtra
    packet:alloced := nNew
RETURN

FUNCTION NET_NewPacket( initial_size )
    LOCAL packet

    IF initial_size == NIL .OR. initial_size <= 0
        initial_size := 256
    ENDIF

    packet := net_packet_t():New()
    packet:data    := Replicate( Chr( 0 ), initial_size )
    packet:alloced := initial_size
    packet:len     := 0
    packet:pos     := 0
RETURN packet

FUNCTION NET_PacketDup( packet )
    LOCAL newpacket
    LOCAL nLen

    IF packet == NIL
        RETURN NIL
    ENDIF

    nLen := packet:len
    newpacket := NET_NewPacket( iif( nLen > 0, nLen, 256 ) )
    IF nLen > 0
        newpacket:data := Left( packet:data, nLen ) + Replicate( Chr( 0 ), Max( 0, newpacket:alloced - nLen ) )
        IF Len( newpacket:data ) < newpacket:alloced
            newpacket:data += Replicate( Chr( 0 ), newpacket:alloced - Len( newpacket:data ) )
        ENDIF
    ENDIF
    newpacket:len := nLen
    newpacket:pos := 0
RETURN newpacket

FUNCTION NET_FreePacket( packet )
    IF packet == NIL
        RETURN NIL
    ENDIF
    packet:data    := ""
    packet:len     := 0
    packet:alloced := 0
    packet:pos     := 0
RETURN NIL

FUNCTION NET_ReadInt8( packet, data )
    IF packet == NIL .OR. packet:pos + 1 > packet:len
        RETURN .F.
    ENDIF
    data := ( Asc( SubStr( packet:data, packet:pos + 1, 1 ) ) & 0xFF )
    packet:pos += 1
RETURN .T.

FUNCTION NET_ReadInt16( packet, data )
    LOCAL nLo
    LOCAL nHi

    IF packet == NIL .OR. packet:pos + 2 > packet:len
        RETURN .F.
    ENDIF
    nLo := ( Asc( SubStr( packet:data, packet:pos + 1, 1 ) ) & 0xFF )
    nHi := ( Asc( SubStr( packet:data, packet:pos + 2, 1 ) ) & 0xFF )
    data := ( ( nLo + ( nHi * 256 ) ) & 0xFFFF )
    packet:pos += 2
RETURN .T.

FUNCTION NET_ReadInt32( packet, data )
    LOCAL n0
    LOCAL n1
    LOCAL n2
    LOCAL n3

    IF packet == NIL .OR. packet:pos + 4 > packet:len
        RETURN .F.
    ENDIF
    n0 := ( Asc( SubStr( packet:data, packet:pos + 1, 1 ) ) & 0xFF )
    n1 := ( Asc( SubStr( packet:data, packet:pos + 2, 1 ) ) & 0xFF )
    n2 := ( Asc( SubStr( packet:data, packet:pos + 3, 1 ) ) & 0xFF )
    n3 := ( Asc( SubStr( packet:data, packet:pos + 4, 1 ) ) & 0xFF )
    data := ( n0 + ( n1 * 256 ) + ( n2 * 65536 ) + ( n3 * 16777216 ) )
    data := ( data & 0xFFFFFFFF )
    packet:pos += 4
RETURN .T.

FUNCTION NET_ReadSInt8( packet, data )
    LOCAL n

    IF ! NET_ReadInt8( packet, @n )
        RETURN .F.
    ENDIF
    IF n >= 128
        n := n - 256
    ENDIF
    data := n
RETURN .T.

FUNCTION NET_ReadSInt16( packet, data )
    LOCAL n

    IF ! NET_ReadInt16( packet, @n )
        RETURN .F.
    ENDIF
    IF n >= 32768
        n := n - 65536
    ENDIF
    data := n
RETURN .T.

FUNCTION NET_ReadSInt32( packet, data )
    LOCAL n

    IF ! NET_ReadInt32( packet, @n )
        RETURN .F.
    ENDIF
    IF n >= 2147483648
        n := n - 4294967296
    ENDIF
    data := n
RETURN .T.

FUNCTION NET_ReadString( packet )
    LOCAL nStart
    LOCAL nPos
    LOCAL cCh
    LOCAL cOut

    IF packet == NIL
        RETURN NIL
    ENDIF

    nStart := packet:pos
    nPos := packet:pos
    DO WHILE nPos < packet:len
        cCh := SubStr( packet:data, nPos + 1, 1 )
        IF cCh == Chr( 0 )
            cOut := SubStr( packet:data, nStart + 1, nPos - nStart )
            packet:pos := nPos + 1
            RETURN cOut
        ENDIF
        nPos += 1
    ENDDO
RETURN NIL

STATIC PROCEDURE PacketWriteByte( packet, nByte )
    DO WHILE packet:len + 1 > packet:alloced
        PacketIncrease( packet )
    ENDDO
    packet:data := Stuff( packet:data, packet:len + 1, 1, Chr( ( nByte & 0xFF ) ) )
    packet:len += 1
RETURN

FUNCTION NET_WriteInt8( packet, i )
    IF packet == NIL
        RETURN NIL
    ENDIF
    PacketWriteByte( packet, i )
RETURN NIL

FUNCTION NET_WriteInt16( packet, i )
    LOCAL n

    IF packet == NIL
        RETURN NIL
    ENDIF
    n := ( i & 0xFFFF )
    PacketWriteByte( packet, n )
    PacketWriteByte( packet, Int( n / 256 ) )
RETURN NIL

FUNCTION NET_WriteInt32( packet, i )
    LOCAL n

    IF packet == NIL
        RETURN NIL
    ENDIF
    n := ( i & 0xFFFFFFFF )
    PacketWriteByte( packet, n )
    PacketWriteByte( packet, Int( n / 256 ) )
    PacketWriteByte( packet, Int( n / 65536 ) )
    PacketWriteByte( packet, Int( n / 16777216 ) )
RETURN NIL

FUNCTION NET_WriteString( packet, cString )
    LOCAL i
    LOCAL nLen

    IF packet == NIL
        RETURN NIL
    ENDIF
    IF cString == NIL
        cString := ""
    ENDIF
    nLen := Len( cString )
    FOR i := 1 TO nLen
        PacketWriteByte( packet, Asc( SubStr( cString, i, 1 ) ) )
    NEXT
    PacketWriteByte( packet, 0 )
RETURN NIL
