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

#include "memio.ch"

CLASS MEMFILE
    DATA buf
    DATA buflen
    DATA alloced
    DATA position
    DATA mode
    METHOD New()
ENDCLASS

METHOD New() CLASS MEMFILE
    ::buf      := ""
    ::buflen   := 0
    ::alloced  := 0
    ::position := 0
    ::mode     := MODE_READ
RETURN Self

STATIC FUNCTION MemToBin( ptr, nBytes )
    LOCAL c
    LOCAL i
    LOCAL n

    IF ValType( ptr ) == "N"
        n := ( ptr & 0xFFFFFFFF )
        c := ""
        FOR i := 1 TO nBytes
            c += Chr( ( n & 0xFF ) )
            n := Int( n / 256 )
        NEXT
        RETURN c
    ENDIF
    IF ValType( ptr ) == "A"
        c := ""
        FOR i := 1 TO nBytes
            c += Chr( ( ptr[ i ] & 0xFF ) )
        NEXT
        RETURN c
    ENDIF
    IF ptr == NIL
        RETURN Replicate( Chr( 0 ), nBytes )
    ENDIF
RETURN Left( ptr, nBytes )

STATIC FUNCTION MemFromBin( cChunk, size, nmemb )
    LOCAL nBytes
    LOCAL n

    nBytes := size * nmemb
    IF nBytes <= 0
        RETURN NIL
    ENDIF
    IF nBytes == 1
        RETURN Asc( Left( cChunk, 1 ) )
    ENDIF
    IF size == 2 .AND. nmemb == 1
        n := Asc( SubStr( cChunk, 1, 1 ) ) + ( Asc( SubStr( cChunk, 2, 1 ) ) * 256 )
        RETURN ( n & 0xFFFF )
    ENDIF
RETURN Left( cChunk, nBytes )

FUNCTION mem_fopen_read( buf, buflen )
    LOCAL file

    file := MEMFILE():New()
    IF ValType( buf ) == "C"
        file:buf := buf
        IF buflen == NIL
            buflen := Len( buf )
        ENDIF
    ELSE
        file:buf := ""
        IF buflen == NIL
            buflen := 0
        ENDIF
    ENDIF
    file:buflen   := buflen
    file:position := 0
    file:mode     := MODE_READ
RETURN file

FUNCTION mem_fread( buf, size, nmemb, stream )
    LOCAL items
    LOCAL nBytes
    LOCAL cChunk

    IF stream:mode != MODE_READ
        OutStd( "not a read stream" + hb_eol() )
        RETURN -1
    ENDIF

    items := nmemb
    IF items * size > stream:buflen - stream:position
        IF size == 0
            items := 0
        ELSE
            items := Int( ( stream:buflen - stream:position ) / size )
        ENDIF
    ENDIF

    nBytes := items * size
    cChunk := SubStr( stream:buf, stream:position + 1, nBytes )
    IF Len( cChunk ) < nBytes
        cChunk += Replicate( Chr( 0 ), nBytes - Len( cChunk ) )
    ENDIF
    stream:position := stream:position + nBytes
    buf := MemFromBin( cChunk, size, items )
RETURN items

FUNCTION mem_fopen_write()
    LOCAL file

    file := MEMFILE():New()
    file:alloced  := 1024
    file:buf      := Replicate( Chr( 0 ), 1024 )
    file:buflen   := 0
    file:position := 0
    file:mode     := MODE_WRITE
RETURN file

FUNCTION mem_fwrite( ptr, size, nmemb, stream )
    LOCAL nBytes
    LOCAL cData

    IF stream:mode != MODE_WRITE
        RETURN -1
    ENDIF

    nBytes := size * nmemb
    cData := MemToBin( ptr, nBytes )

    DO WHILE nBytes > stream:alloced - stream:position
        stream:alloced := stream:alloced * 2
        stream:buf += Replicate( Chr( 0 ), stream:alloced - Len( stream:buf ) )
    ENDDO

    stream:buf := Left( stream:buf, stream:position ) + cData + SubStr( stream:buf, stream:position + nBytes + 1 )
    stream:position := stream:position + nBytes
    IF stream:position > stream:buflen
        stream:buflen := stream:position
    ENDIF
RETURN nmemb

FUNCTION mem_get_buf( stream, buf, buflen )
    buf := Left( stream:buf, stream:buflen )
    buflen := stream:buflen
RETURN NIL

FUNCTION mem_fclose( stream )
    HB_SYMBOL_UNUSED( stream )
RETURN NIL

FUNCTION mem_ftell( stream )
RETURN stream:position

FUNCTION mem_fseek( stream, position, whence )
    LOCAL newpos

    SWITCH whence
    CASE MEM_SEEK_SET
        newpos := position
        EXIT
    CASE MEM_SEEK_CUR
        newpos := stream:position + position
        EXIT
    CASE MEM_SEEK_END
        newpos := stream:buflen + position
        EXIT
    OTHERWISE
        RETURN -1
    ENDSWITCH

    IF newpos < stream:buflen
        stream:position := newpos
        RETURN 0
    ENDIF
    OutStd( "Error seeking to " + hb_ntos( newpos ) + hb_eol() )
RETURN -1
