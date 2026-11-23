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

STATIC lumphash := NIL

#include "w_wad.ch"

CLASS lumpinfo_t
    DATA name
    DATA wad_file
    DATA position
    DATA size
    DATA cache
    DATA next
    DATA iLump
    METHOD New()
ENDCLASS
#include "i_swap.ch"
#include "z_zone.ch"
#include "d_mode.ch"



METHOD New() CLASS lumpinfo_t
    ::name := ""
    ::wad_file := NIL
    ::position := 0
    ::size := 0
    ::cache := NIL
    ::next := NIL
    ::iLump := 0
RETURN Self

STATIC FUNCTION LumpULong( cData, nOff0 )
    LOCAL n
    n := Asc( SubStr( cData, nOff0 + 1, 1 ) ) ;
       + Asc( SubStr( cData, nOff0 + 2, 1 ) ) * 256 ;
       + Asc( SubStr( cData, nOff0 + 3, 1 ) ) * 65536 ;
       + Asc( SubStr( cData, nOff0 + 4, 1 ) ) * 16777216
RETURN ( n & 0xFFFFFFFF )

STATIC FUNCTION LumpLong( cData, nOff0 )
RETURN LONG( LumpULong( cData, nOff0 ) )

STATIC FUNCTION Name8( cData, nOff0 )
    LOCAL i
    LOCAL ch
    LOCAL c := ""
    FOR i := 0 TO 7
        ch := SubStr( cData, nOff0 + i + 1, 1 )
        IF ch == Chr( 0 ) .OR. ch == ""
            EXIT
        ENDIF
        c += ch
    NEXT
RETURN c

STATIC FUNCTION LumpNameNorm( cName )
    LOCAL c
    LOCAL nZero

    IF ValType( cName ) != "C"
        RETURN ""
    ENDIF
    c := Upper( Left( cName, 8 ) )
    nZero := At( Chr( 0 ), c )
    IF nZero > 0
        c := Left( c, nZero - 1 )
    ENDIF
    DO WHILE ! Empty( c ) .AND. Right( c, 1 ) == " "
        c := Left( c, Len( c ) - 1 )
    ENDDO
RETURN c

FUNCTION W_LumpNameHash( s )
    LOCAL result := 5381
    LOCAL i
    LOCAL ch
    LOCAL cName

    cName := LumpNameNorm( s )
    FOR i := 0 TO 7
        IF i >= Len( cName )
            EXIT
        ENDIF
        ch := SubStr( cName, i + 1, 1 )
        result := ( ( ( ( result * 32 ) & 0xFFFFFFFF ) ^^ result ) ^^ Asc( ch ) )
        result := ( result & 0xFFFFFFFF )
    NEXT
RETURN result

STATIC PROCEDURE ExtendLumpInfo( newnumlumps )
    LOCAL i
    LOCAL o
    MEMVAR lumpinfo
    MEMVAR numlumps

    DO WHILE Len( lumpinfo ) < newnumlumps
        o := lumpinfo_t():New()
        AAdd( lumpinfo, o )
        o:iLump := Len( lumpinfo ) - 1
    ENDDO
    numlumps := newnumlumps
    FOR i := 1 TO numlumps
        lumpinfo[ i ]:iLump := i - 1
    NEXT
    HB_SYMBOL_UNUSED( i )
RETURN

FUNCTION W_AddFile( filename )
    LOCAL header
    LOCAL i
    LOCAL wad_file
    LOCAL length
    LOCAL startlump
    LOCAL fileinfo
    LOCAL newnumlumps
    LOCAL nLumps
    LOCAL nInfoOfs
    LOCAL cId
    LOCAL nOff
    LOCAL lump_p
    MEMVAR lumpinfo
    MEMVAR numlumps

    wad_file := W_OpenFile( filename )
    IF wad_file == NIL
        OutStd( " couldn't open " + filename + hb_eol() )
        RETURN NIL
    ENDIF

    newnumlumps := numlumps

    IF Lower( Right( filename, 3 ) ) != "wad"
        fileinfo := Replicate( Chr( 0 ), 16 )
        fileinfo := Stuff( fileinfo, 1, 4, Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 ) )
        fileinfo := Stuff( fileinfo, 5, 4, ;
            Chr( wad_file:length & 0xFF ) + ;
            Chr( Int( wad_file:length / 256 ) & 0xFF ) + ;
            Chr( Int( wad_file:length / 65536 ) & 0xFF ) + ;
            Chr( Int( wad_file:length / 16777216 ) & 0xFF ) )
        fileinfo := Stuff( fileinfo, 9, 8, PadR( M_ExtractFileBase( filename ), 8, Chr( 0 ) ) )
        nLumps := 1
        newnumlumps++
    ELSE
        header := W_Read( wad_file, 0, 12 )
        cId := Left( header, 4 )
        IF cId != "IWAD" .AND. cId != "PWAD"
            I_Error( "Wad file " + filename + " doesn't have IWAD or PWAD id" )
        ENDIF
        nLumps := LumpLong( header, 4 )
        nInfoOfs := LumpLong( header, 8 )
        length := nLumps * 16
        fileinfo := W_Read( wad_file, nInfoOfs, length )
        newnumlumps += nLumps
    ENDIF

    startlump := numlumps
    ExtendLumpInfo( newnumlumps )

    nOff := 0
    FOR i := startlump TO numlumps - 1
        lump_p := lumpinfo[ i + 1 ]
        lump_p:wad_file := wad_file
        lump_p:position := LumpLong( fileinfo, nOff )
        lump_p:size := LumpLong( fileinfo, nOff + 4 )
        lump_p:cache := NIL
        lump_p:name := Name8( fileinfo, nOff + 8 )
        lump_p:next := NIL
        lump_p:iLump := i
        nOff += 16
    NEXT

    lumphash := NIL
RETURN wad_file

FUNCTION W_NumLumps()
    MEMVAR numlumps
RETURN numlumps

FUNCTION W_CheckNumForName( name )
    LOCAL lump_p
    LOCAL i
    LOCAL hash
    MEMVAR lumpinfo
    MEMVAR numlumps

    IF lumphash != NIL
        hash := W_LumpNameHash( name ) % numlumps
        lump_p := lumphash[ hash + 1 ]
        DO WHILE lump_p != NIL
            IF LumpNameNorm( lump_p:name ) == LumpNameNorm( name )
                RETURN lump_p:iLump
            ENDIF
            lump_p := lump_p:next
        ENDDO
    ELSE
        FOR i := numlumps - 1 TO 0 STEP -1
            IF LumpNameNorm( lumpinfo[ i + 1 ]:name ) == LumpNameNorm( name )
                RETURN i
            ENDIF
        NEXT
    ENDIF
RETURN -1

FUNCTION W_GetNumForName( name )
    LOCAL i := W_CheckNumForName( name )
    IF i < 0
        I_Error( "W_GetNumForName: " + name + " not found!" )
    ENDIF
RETURN i

FUNCTION W_LumpLength( lump )
    MEMVAR lumpinfo
    MEMVAR numlumps
    IF lump >= numlumps
        I_Error( "W_LumpLength: " + LTrim( Str( lump ) ) + " >= numlumps" )
    ENDIF
RETURN lumpinfo[ lump + 1 ]:size

FUNCTION W_ReadLump( lump )
    LOCAL l
    LOCAL c
    MEMVAR lumpinfo
    MEMVAR numlumps

    IF lump >= numlumps
        I_Error( "W_ReadLump: " + LTrim( Str( lump ) ) + " >= numlumps" )
    ENDIF
    l := lumpinfo[ lump + 1 ]
    I_BeginRead()
    c := W_Read( l:wad_file, l:position, l:size )
    IF Len( c ) < l:size
        I_Error( "W_ReadLump: only read " + LTrim( Str( Len( c ) ) ) + " of " + LTrim( Str( l:size ) ) + " on lump " + LTrim( Str( lump ) ) )
    ENDIF
    I_EndRead()
RETURN c

FUNCTION W_CacheLumpNum( lumpnum, tag )
    LOCAL lump
    MEMVAR lumpinfo
    MEMVAR numlumps

    HB_SYMBOL_UNUSED( tag )
    IF lumpnum < 0 .OR. lumpnum >= numlumps
        I_Error( "W_CacheLumpNum: " + LTrim( Str( lumpnum ) ) + " >= numlumps" )
    ENDIF
    lump := lumpinfo[ lumpnum + 1 ]
    IF lump:cache == NIL
        lump:cache := W_ReadLump( lumpnum )
    ENDIF
RETURN lump:cache

FUNCTION W_CacheLumpName( name, tag )
RETURN W_CacheLumpNum( W_GetNumForName( name ), tag )

PROCEDURE W_ReleaseLumpNum( lumpnum )
    MEMVAR numlumps
    IF lumpnum < 0 .OR. lumpnum >= numlumps
        I_Error( "W_ReleaseLumpNum: " + LTrim( Str( lumpnum ) ) + " >= numlumps" )
    ENDIF
RETURN

PROCEDURE W_ReleaseLumpName( name )
    W_ReleaseLumpNum( W_GetNumForName( name ) )
RETURN

PROCEDURE W_GenerateHashTable()
    LOCAL i
    LOCAL hash
    MEMVAR lumpinfo
    MEMVAR numlumps

    IF numlumps > 0
        lumphash := Array( numlumps )
        FOR i := 1 TO numlumps
            lumphash[ i ] := NIL
        NEXT
        FOR i := 0 TO numlumps - 1
            hash := W_LumpNameHash( lumpinfo[ i + 1 ]:name ) % numlumps
            lumpinfo[ i + 1 ]:next := lumphash[ hash + 1 ]
            lumphash[ hash + 1 ] := lumpinfo[ i + 1 ]
        NEXT
    ENDIF
RETURN

PROCEDURE W_CheckCorrectIWAD( mission )
    LOCAL unique_lumps
    LOCAL i
    LOCAL lumpnum

    unique_lumps := { ;
        { doom, "POSSA1" }, ;
        { heretic, "IMPXA1" }, ;
        { hexen, "ETTNA1" }, ;
        { strife, "AGRDA1" } }

    FOR i := 1 TO Len( unique_lumps )
        IF mission != unique_lumps[ i, 1 ]
            lumpnum := W_CheckNumForName( unique_lumps[ i, 2 ] )
            IF lumpnum >= 0
                I_Error( "You are trying to use a mismatched IWAD file with this binary." )
            ENDIF
        ENDIF
    NEXT
RETURN

INIT PROCEDURE init_w_wad
    PUBLIC lumpinfo
    PUBLIC numlumps
    lumpinfo := {}
    numlumps := 0
    lumphash := NIL
RETURN
