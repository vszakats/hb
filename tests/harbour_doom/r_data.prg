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

STATIC lastflat
STATIC firstpatch
STATIC lastpatch
STATIC numpatches
STATIC numtextures
STATIC textures
STATIC textures_hashtable
STATIC texturewidthmask
STATIC texturecompositesize
STATIC texturecolumnlump
STATIC texturecolumnofs
STATIC texturecomposite
STATIC flatmemory
STATIC texturememory
STATIC spritememory

#include "r_data.ch"

CLASS maptexturedef_t
    DATA name
    DATA masked
    DATA width
    DATA height
    DATA obsolete
    DATA patchcount
    DATA patches
    METHOD New()
ENDCLASS
CLASS mappatch_t
    DATA originx
    DATA originy
    DATA patch
    DATA stepdir
    DATA colormap
    METHOD New()
ENDCLASS
CLASS texpatch_t
    DATA originx
    DATA originy
    DATA patch
    METHOD New()
ENDCLASS
CLASS texture_t
    DATA name
    DATA width
    DATA height
    DATA index
    DATA next
    DATA patchcount
    DATA patches
    METHOD New()
ENDCLASS
#include "r_state.ch"
#include "r_sky.ch"
#include "i_swap.ch"
#include "m_fixed.ch"
#include "doomstat.ch"


#ifndef PU_STATIC
#define PU_STATIC 1
#endif
#ifndef PU_CACHE
#define PU_CACHE 101
#endif

#define MAPTEXTURE_HEADER_SIZE 22
#define MAPPATCH_SIZE          10


INIT PROCEDURE init_r_data
    PUBLIC firstflat
    PUBLIC numflats
    PUBLIC firstspritelump
    PUBLIC lastspritelump
    PUBLIC numspritelumps
    PUBLIC textureheight
    PUBLIC flattranslation
    PUBLIC texturetranslation
    PUBLIC spritewidth
    PUBLIC spriteoffset
    PUBLIC spritetopoffset
    PUBLIC colormaps

    firstflat := 0
    lastflat := 0
    numflats := 0
    firstpatch := 0
    lastpatch := 0
    numpatches := 0
    firstspritelump := 0
    lastspritelump := 0
    numspritelumps := 0
    numtextures := 0
    textures := {}
    textures_hashtable := {}
    texturewidthmask := {}
    textureheight := {}
    texturecompositesize := {}
    texturecolumnlump := {}
    texturecolumnofs := {}
    texturecomposite := {}
    flattranslation := {}
    texturetranslation := {}
    spritewidth := {}
    spriteoffset := {}
    spritetopoffset := {}
    colormaps := ""
    flatmemory := 0
    texturememory := 0
    spritememory := 0
RETURN

METHOD New() CLASS maptexturedef_t
    ::name := ""
    ::masked := 0
    ::width := 0
    ::height := 0
    ::obsolete := 0
    ::patchcount := 0
    ::patches := {}
RETURN Self

METHOD New() CLASS mappatch_t
    ::originx := 0
    ::originy := 0
    ::patch := 0
    ::stepdir := 0
    ::colormap := 0
RETURN Self

METHOD New() CLASS texpatch_t
    ::originx := 0
    ::originy := 0
    ::patch := 0
RETURN Self

METHOD New() CLASS texture_t
    ::name := ""
    ::width := 0
    ::height := 0
    ::index := 0
    ::next := NIL
    ::patchcount := 0
    ::patches := {}
RETURN Self

STATIC FUNCTION LumpByte( cData, nOff0 )
RETURN Asc( SubStr( cData, nOff0 + 1, 1 ) )

STATIC FUNCTION LumpUShort( cData, nOff0 )
RETURN ( ( LumpByte( cData, nOff0 ) + LumpByte( cData, nOff0 + 1 ) * 256 ) & 0xFFFF )

STATIC FUNCTION LumpShort( cData, nOff0 )
RETURN SHORT( LumpUShort( cData, nOff0 ) )

STATIC FUNCTION LumpULong( cData, nOff0 )
    LOCAL n

    n := LumpByte( cData, nOff0 ) + ;
         LumpByte( cData, nOff0 + 1 ) * 256 + ;
         LumpByte( cData, nOff0 + 2 ) * 65536 + ;
         LumpByte( cData, nOff0 + 3 ) * 16777216
RETURN ( n & 0xFFFFFFFF )

STATIC FUNCTION LumpLong( cData, nOff0 )
RETURN LONG( LumpULong( cData, nOff0 ) )

STATIC FUNCTION Name8( cData, nOff0 )
    LOCAL cName
    LOCAL nZero

    cName := SubStr( cData, nOff0 + 1, 8 )
    nZero := At( Chr( 0 ), cName )
    IF nZero > 0
        cName := Left( cName, nZero - 1 )
    ENDIF
RETURN Upper( cName )

STATIC FUNCTION NameKey( cName )
    LOCAL cKey
    LOCAL nZero

    cKey := Left( Upper( cName ), 8 )
    nZero := At( Chr( 0 ), cKey )
    IF nZero > 0
        cKey := Left( cKey, nZero - 1 )
    ENDIF
RETURN cKey

STATIC FUNCTION BlankArray( nLen, xValue )
    LOCAL aResult
    LOCAL i

    aResult := {}
    FOR i := 1 TO nLen
        AAdd( aResult, xValue )
    NEXT
RETURN aResult

STATIC FUNCTION PatchWidth( cPatch )
RETURN LumpShort( cPatch, 0 )

STATIC FUNCTION PatchColumnOffset( cPatch, nColumn )
RETURN LumpLong( cPatch, 8 + nColumn * 4 )

FUNCTION R_DrawColumnInCache( cPatch, nPatchOff, cCache, nOriginY, nCacheHeight )
    LOCAL nTop
    LOCAL nCount
    LOCAL nPosition
    LOCAL nSource
    LOCAL nCopy

    DO WHILE nPatchOff < Len( cPatch )
        nTop := LumpByte( cPatch, nPatchOff )
        IF nTop == 0xFF
            EXIT
        ENDIF

        nCount := LumpByte( cPatch, nPatchOff + 1 )
        nSource := nPatchOff + 3
        nPosition := nOriginY + nTop

        IF nPosition < 0
            nSource := nSource - nPosition
            nCount := nCount + nPosition
            nPosition := 0
        ENDIF
        IF nPosition + nCount > nCacheHeight
            nCount := nCacheHeight - nPosition
        ENDIF
        IF nCount > 0
            nCopy := Min( nCount, Len( cPatch ) - nSource )
            IF nCopy > 0
                cCache := Stuff( cCache, nPosition + 1, nCopy, ;
                                 SubStr( cPatch, nSource + 1, nCopy ) )
            ENDIF
        ENDIF

        nPatchOff := nPatchOff + LumpByte( cPatch, nPatchOff + 1 ) + 4
    ENDDO
RETURN cCache

FUNCTION R_GenerateComposite( nTexNum )
    LOCAL oTexture
    LOCAL oPatch
    LOCAL cRealPatch
    LOCAL aColumns
    LOCAL nX
    LOCAL nX1
    LOCAL nX2
    LOCAL i
    LOCAL nPatchCol

    oTexture := textures[ nTexNum + 1 ]
    aColumns := BlankArray( oTexture:width, Replicate( Chr( 0 ), oTexture:height ) )

    FOR i := 0 TO oTexture:patchcount - 1
        oPatch := oTexture:patches[ i + 1 ]
        cRealPatch := W_CacheLumpNum( oPatch:patch, PU_CACHE )
        nX1 := oPatch:originx
        nX2 := Min( nX1 + PatchWidth( cRealPatch ), oTexture:width )
        nX := Max( nX1, 0 )

        DO WHILE nX < nX2
            IF texturecolumnlump[ nTexNum + 1 ][ nX + 1 ] < 0
                nPatchCol := PatchColumnOffset( cRealPatch, nX - nX1 )
                aColumns[ nX + 1 ] := R_DrawColumnInCache( cRealPatch, nPatchCol, ;
                    aColumns[ nX + 1 ], oPatch:originy, oTexture:height )
            ENDIF
            nX := nX + 1
        ENDDO
    NEXT

    texturecomposite[ nTexNum + 1 ] := aColumns
RETURN NIL

FUNCTION R_GenerateLookup( nTexNum )
    LOCAL oTexture
    LOCAL oPatch
    LOCAL cRealPatch
    LOCAL aPatchCount
    LOCAL aColLump
    LOCAL aColOfs
    LOCAL nX
    LOCAL nX1
    LOCAL nX2
    LOCAL i

    oTexture := textures[ nTexNum + 1 ]
    texturecomposite[ nTexNum + 1 ] := NIL
    texturecompositesize[ nTexNum + 1 ] := 0
    aColLump := texturecolumnlump[ nTexNum + 1 ]
    aColOfs := texturecolumnofs[ nTexNum + 1 ]
    aPatchCount := BlankArray( oTexture:width, 0 )

    FOR i := 0 TO oTexture:patchcount - 1
        oPatch := oTexture:patches[ i + 1 ]
        cRealPatch := W_CacheLumpNum( oPatch:patch, PU_CACHE )
        nX1 := oPatch:originx
        nX2 := Min( nX1 + PatchWidth( cRealPatch ), oTexture:width )
        nX := Max( nX1, 0 )

        DO WHILE nX < nX2
            aPatchCount[ nX + 1 ] := aPatchCount[ nX + 1 ] + 1
            aColLump[ nX + 1 ] := oPatch:patch
            aColOfs[ nX + 1 ] := PatchColumnOffset( cRealPatch, nX - nX1 ) + 3
            nX := nX + 1
        ENDDO
    NEXT

    FOR nX := 0 TO oTexture:width - 1
        IF aPatchCount[ nX + 1 ] == 0
            OutErr( "R_GenerateLookup: column without a patch (" + ;
                    oTexture:name + ")" + hb_eol() )
        ELSEIF aPatchCount[ nX + 1 ] > 1
            aColLump[ nX + 1 ] := -1
            aColOfs[ nX + 1 ] := texturecompositesize[ nTexNum + 1 ]
            texturecompositesize[ nTexNum + 1 ] := ;
                texturecompositesize[ nTexNum + 1 ] + oTexture:height
        ENDIF
    NEXT
RETURN NIL

FUNCTION R_GetColumn( nTex, nCol )
    LOCAL nLump
    LOCAL nOfs
    LOCAL nHeight
    LOCAL cData
    LOCAL cColumn

    nCol := ( nCol & texturewidthmask[ nTex + 1 ] )
    nLump := texturecolumnlump[ nTex + 1 ][ nCol + 1 ]
    nOfs := texturecolumnofs[ nTex + 1 ][ nCol + 1 ]
    nHeight := textures[ nTex + 1 ]:height

    IF nLump > 0
        cData := W_CacheLumpNum( nLump, PU_CACHE )
        cColumn := Replicate( Chr( 0 ), Max( nHeight, 128 ) )
        cColumn := R_DrawColumnInCache( cData, nOfs - 3, cColumn, 0, nHeight )
        RETURN cColumn
    ENDIF

    IF texturecomposite[ nTex + 1 ] == NIL
        R_GenerateComposite( nTex )
    ENDIF
RETURN texturecomposite[ nTex + 1 ][ nCol + 1 ]

FUNCTION R_GetColumnPosts( nTex, nCol )
    LOCAL nLump
    LOCAL nOfs
    LOCAL nHeight
    LOCAL cData
    LOCAL cColumn

    nCol := ( nCol & texturewidthmask[ nTex + 1 ] )
    nLump := texturecolumnlump[ nTex + 1 ][ nCol + 1 ]
    nOfs := texturecolumnofs[ nTex + 1 ][ nCol + 1 ]
    nHeight := textures[ nTex + 1 ]:height

    IF nLump > 0
        cData := W_CacheLumpNum( nLump, PU_CACHE )
        RETURN SubStr( cData, ( nOfs - 3 ) + 1 )
    ENDIF

    IF texturecomposite[ nTex + 1 ] == NIL
        R_GenerateComposite( nTex )
    ENDIF
    cColumn := texturecomposite[ nTex + 1 ][ nCol + 1 ]
    nHeight := Len( cColumn )
RETURN Chr( 0 ) + Chr( ( nHeight & 0xFF ) ) + Chr( 0 ) + cColumn + Chr( 0 ) + Chr( 255 )

STATIC PROCEDURE GenerateTextureHashTable()
    LOCAL i
    LOCAL nKey
    LOCAL nRover
    LOCAL oTexture

    textures_hashtable := BlankArray( numtextures, NIL )
    FOR i := 0 TO numtextures - 1
        oTexture := textures[ i + 1 ]
        oTexture:index := i
        oTexture:next := NIL
        nKey := W_LumpNameHash( oTexture:name ) % numtextures

        IF textures_hashtable[ nKey + 1 ] == NIL
            textures_hashtable[ nKey + 1 ] := i
        ELSE
            nRover := textures_hashtable[ nKey + 1 ]
            DO WHILE textures[ nRover + 1 ]:next != NIL
                nRover := textures[ nRover + 1 ]:next
            ENDDO
            textures[ nRover + 1 ]:next := i
        ENDIF
    NEXT
RETURN

STATIC FUNCTION ParseMapTexture( cMapTex, nOffset, aPatchLookup )
    LOCAL oMapTexture
    LOCAL oMapPatch
    LOCAL oTexture
    LOCAL oPatch
    LOCAL nPatchOff
    LOCAL nPatchNum
    LOCAL j

    oMapTexture := maptexturedef_t():New()
    oMapTexture:name := Name8( cMapTex, nOffset )
    oMapTexture:masked := LumpLong( cMapTex, nOffset + 8 )
    oMapTexture:width := LumpShort( cMapTex, nOffset + 12 )
    oMapTexture:height := LumpShort( cMapTex, nOffset + 14 )
    oMapTexture:obsolete := LumpLong( cMapTex, nOffset + 16 )
    oMapTexture:patchcount := LumpShort( cMapTex, nOffset + 20 )

    oTexture := texture_t():New()
    oTexture:name := oMapTexture:name
    oTexture:width := oMapTexture:width
    oTexture:height := oMapTexture:height
    oTexture:patchcount := oMapTexture:patchcount

    FOR j := 0 TO oMapTexture:patchcount - 1
        nPatchOff := nOffset + MAPTEXTURE_HEADER_SIZE + j * MAPPATCH_SIZE
        oMapPatch := mappatch_t():New()
        oMapPatch:originx := LumpShort( cMapTex, nPatchOff )
        oMapPatch:originy := LumpShort( cMapTex, nPatchOff + 2 )
        oMapPatch:patch := LumpShort( cMapTex, nPatchOff + 4 )
        oMapPatch:stepdir := LumpShort( cMapTex, nPatchOff + 6 )
        oMapPatch:colormap := LumpShort( cMapTex, nPatchOff + 8 )
        AAdd( oMapTexture:patches, oMapPatch )

        nPatchNum := oMapPatch:patch
        IF nPatchNum < 0 .OR. nPatchNum >= Len( aPatchLookup )
            I_Error( "R_InitTextures: bad patch index in texture %s", oTexture:name )
        ENDIF
        oPatch := texpatch_t():New()
        oPatch:originx := oMapPatch:originx
        oPatch:originy := oMapPatch:originy
        oPatch:patch := aPatchLookup[ nPatchNum + 1 ]
        IF oPatch:patch == -1
            I_Error( "R_InitTextures: Missing patch in texture %s", oTexture:name )
        ENDIF
        AAdd( oTexture:patches, oPatch )
    NEXT
RETURN oTexture

FUNCTION R_InitTextures()
    LOCAL cNames
    LOCAL nMapPatches
    LOCAL aPatchLookup
    LOCAL cName
    LOCAL cMapTex1
    LOCAL cMapTex2
    LOCAL nTexture1Lump
    LOCAL nTexture2Lump
    LOCAL nTextures1
    LOCAL nTextures2
    LOCAL cMapTex
    LOCAL nLocalIndex
    LOCAL nOffset
    LOCAL oTexture
    LOCAL nMask
    LOCAL i
    MEMVAR textureheight
    MEMVAR texturetranslation

    cNames := W_CacheLumpName( "PNAMES", PU_STATIC )
    nMapPatches := LumpLong( cNames, 0 )
    aPatchLookup := {}
    FOR i := 0 TO nMapPatches - 1
        cName := Name8( cNames, 4 + i * 8 )
        AAdd( aPatchLookup, W_CheckNumForName( cName ) )
    NEXT
    W_ReleaseLumpName( "PNAMES" )

    nTexture1Lump := W_GetNumForName( "TEXTURE1" )
    cMapTex1 := W_CacheLumpNum( nTexture1Lump, PU_STATIC )
    nTextures1 := LumpLong( cMapTex1, 0 )
    nTexture2Lump := W_CheckNumForName( "TEXTURE2" )
    IF nTexture2Lump != -1
        cMapTex2 := W_CacheLumpNum( nTexture2Lump, PU_STATIC )
        nTextures2 := LumpLong( cMapTex2, 0 )
    ELSE
        cMapTex2 := ""
        nTextures2 := 0
    ENDIF

    numtextures := nTextures1 + nTextures2
    textures := {}
    texturecolumnlump := {}
    texturecolumnofs := {}
    texturecomposite := BlankArray( numtextures, NIL )
    texturecompositesize := BlankArray( numtextures, 0 )
    texturewidthmask := {}
    textureheight := {}

    FOR i := 0 TO numtextures - 1
        IF i < nTextures1
            cMapTex := cMapTex1
            nLocalIndex := i
        ELSE
            cMapTex := cMapTex2
            nLocalIndex := i - nTextures1
        ENDIF
        nOffset := LumpLong( cMapTex, 4 + nLocalIndex * 4 )
        IF nOffset < 0 .OR. nOffset + MAPTEXTURE_HEADER_SIZE > Len( cMapTex )
            I_Error( "R_InitTextures: bad texture directory" )
        ENDIF

        oTexture := ParseMapTexture( cMapTex, nOffset, aPatchLookup )
        AAdd( textures, oTexture )
        AAdd( texturecolumnlump, BlankArray( oTexture:width, 0 ) )
        AAdd( texturecolumnofs, BlankArray( oTexture:width, 0 ) )

        nMask := 1
        DO WHILE nMask * 2 <= oTexture:width
            nMask := nMask * 2
        ENDDO
        AAdd( texturewidthmask, nMask - 1 )
        AAdd( textureheight, oTexture:height * FRACUNIT )
    NEXT

    W_ReleaseLumpNum( nTexture1Lump )
    IF nTexture2Lump != -1
        W_ReleaseLumpNum( nTexture2Lump )
    ENDIF

    FOR i := 0 TO numtextures - 1
        R_GenerateLookup( i )
    NEXT
    texturetranslation := {}
    FOR i := 0 TO numtextures - 1
        AAdd( texturetranslation, i )
    NEXT
    AAdd( texturetranslation, numtextures )
    GenerateTextureHashTable()
RETURN NIL

FUNCTION R_InitFlats()
    LOCAL i
    MEMVAR firstflat
    MEMVAR flattranslation
    MEMVAR numflats

    firstflat := W_GetNumForName( "F_START" ) + 1
    lastflat := W_GetNumForName( "F_END" ) - 1
    numflats := lastflat - firstflat + 1
    flattranslation := {}
    FOR i := 0 TO numflats - 1
        AAdd( flattranslation, i )
    NEXT
    AAdd( flattranslation, numflats )
RETURN NIL

FUNCTION R_InitSpriteLumps()
    LOCAL cPatch
    LOCAL i
    MEMVAR firstspritelump
    MEMVAR lastspritelump
    MEMVAR numspritelumps
    MEMVAR spriteoffset
    MEMVAR spritetopoffset
    MEMVAR spritewidth

    firstspritelump := W_GetNumForName( "S_START" ) + 1
    lastspritelump := W_GetNumForName( "S_END" ) - 1
    numspritelumps := lastspritelump - firstspritelump + 1
    spritewidth := {}
    spriteoffset := {}
    spritetopoffset := {}

    FOR i := 0 TO numspritelumps - 1
        cPatch := W_CacheLumpNum( firstspritelump + i, PU_CACHE )
        AAdd( spritewidth, LumpShort( cPatch, 0 ) * FRACUNIT )
        AAdd( spriteoffset, LumpShort( cPatch, 4 ) * FRACUNIT )
        AAdd( spritetopoffset, LumpShort( cPatch, 6 ) * FRACUNIT )
    NEXT
RETURN NIL

FUNCTION R_InitColormaps()
    LOCAL nLump
    MEMVAR colormaps

    nLump := W_GetNumForName( "COLORMAP" )
    colormaps := W_CacheLumpNum( nLump, PU_STATIC )
RETURN NIL

FUNCTION R_InitData()
    R_InitTextures()
    OutStd( "." )
    R_InitFlats()
    OutStd( "." )
    R_InitSpriteLumps()
    OutStd( "." )
    R_InitColormaps()
RETURN NIL

FUNCTION R_FlatNumForName( cName )
    LOCAL i
    MEMVAR firstflat

    i := W_CheckNumForName( cName )
    IF i == -1
        I_Error( "R_FlatNumForName: %s not found", NameKey( cName ) )
    ENDIF
RETURN i - firstflat

FUNCTION R_CheckTextureNumForName( cName )
    LOCAL nKey
    LOCAL nTexture
    LOCAL cKey

    cKey := NameKey( cName )
    IF Left( cKey, 1 ) == "-"
        RETURN 0
    ENDIF
    IF numtextures == 0
        RETURN -1
    ENDIF

    nKey := W_LumpNameHash( cKey ) % numtextures
    nTexture := textures_hashtable[ nKey + 1 ]
    DO WHILE nTexture != NIL
        IF NameKey( textures[ nTexture + 1 ]:name ) == cKey
            RETURN textures[ nTexture + 1 ]:index
        ENDIF
        nTexture := textures[ nTexture + 1 ]:next
    ENDDO
RETURN -1

FUNCTION R_TextureNumForName( cName )
    LOCAL i

    i := R_CheckTextureNumForName( cName )
    IF i == -1
        I_Error( "R_TextureNumForName: %s not found", NameKey( cName ) )
    ENDIF
RETURN i

STATIC FUNCTION ThinkName( oThinker )
    LOCAL x

    IF oThinker == NIL
        RETURN ""
    ENDIF
    IF __objHasMsg( oThinker, "THINKFN" ) .AND. ;
       ValType( oThinker:thinkfn ) == "C"
        RETURN oThinker:thinkfn
    ENDIF
    IF __objHasMsg( oThinker, "THINKER" ) .AND. oThinker:thinker != NIL
        IF __objHasMsg( oThinker:thinker, "THINKFN" ) .AND. ;
           ValType( oThinker:thinker:thinkfn ) == "C"
            RETURN oThinker:thinker:thinkfn
        ENDIF
    ENDIF
    IF __objHasMsg( oThinker, "FUNCTION" ) .AND. ;
       oThinker:function != NIL .AND. ValType( oThinker:function ) == "O"
        x := oThinker:function:acp1
        IF ValType( x ) == "C"
            RETURN x
        ENDIF
    ENDIF
RETURN ""

STATIC FUNCTION ThinkerMobj( oThinker )
    IF oThinker == NIL
        RETURN NIL
    ENDIF
    IF __objHasMsg( oThinker, "SPRITE" )
        RETURN oThinker
    ENDIF
    IF __objHasMsg( oThinker, "OWNER" ) .AND. oThinker:owner != NIL
        IF __objHasMsg( oThinker:owner, "SPRITE" )
            RETURN oThinker:owner
        ENDIF
    ENDIF
RETURN NIL

FUNCTION R_PrecacheLevel()
    LOCAL aFlatPresent
    LOCAL aTexturePresent
    LOCAL aSpritePresent
    LOCAL oTexture
    LOCAL oThinker
    LOCAL oMobj
    LOCAL oFrame
    LOCAL nLump
    LOCAL i
    LOCAL j
    LOCAL k
    MEMVAR demoplayback
    MEMVAR firstflat
    MEMVAR firstspritelump
    MEMVAR lumpinfo
    MEMVAR numflats
    MEMVAR numsectors
    MEMVAR numsides
    MEMVAR sectors
    MEMVAR sides
    MEMVAR skytexture
    MEMVAR sprites
    MEMVAR thinkercap

    IF demoplayback
        RETURN NIL
    ENDIF

    aFlatPresent := BlankArray( numflats, .F. )
    FOR i := 0 TO numsectors - 1
        IF sectors[ i + 1 ]:floorpic >= 0 .AND. sectors[ i + 1 ]:floorpic < numflats
            aFlatPresent[ sectors[ i + 1 ]:floorpic + 1 ] := .T.
        ENDIF
        IF sectors[ i + 1 ]:ceilingpic >= 0 .AND. sectors[ i + 1 ]:ceilingpic < numflats
            aFlatPresent[ sectors[ i + 1 ]:ceilingpic + 1 ] := .T.
        ENDIF
    NEXT
    flatmemory := 0
    FOR i := 0 TO numflats - 1
        IF aFlatPresent[ i + 1 ]
            nLump := firstflat + i
            flatmemory := flatmemory + lumpinfo[ nLump + 1 ]:size
            W_CacheLumpNum( nLump, PU_CACHE )
        ENDIF
    NEXT

    aTexturePresent := BlankArray( numtextures, .F. )
    FOR i := 0 TO numsides - 1
        IF sides[ i + 1 ]:toptexture >= 0 .AND. sides[ i + 1 ]:toptexture < numtextures
            aTexturePresent[ sides[ i + 1 ]:toptexture + 1 ] := .T.
        ENDIF
        IF sides[ i + 1 ]:midtexture >= 0 .AND. sides[ i + 1 ]:midtexture < numtextures
            aTexturePresent[ sides[ i + 1 ]:midtexture + 1 ] := .T.
        ENDIF
        IF sides[ i + 1 ]:bottomtexture >= 0 .AND. sides[ i + 1 ]:bottomtexture < numtextures
            aTexturePresent[ sides[ i + 1 ]:bottomtexture + 1 ] := .T.
        ENDIF
    NEXT
    IF skytexture >= 0 .AND. skytexture < numtextures
        aTexturePresent[ skytexture + 1 ] := .T.
    ENDIF

    texturememory := 0
    FOR i := 0 TO numtextures - 1
        IF aTexturePresent[ i + 1 ]
            oTexture := textures[ i + 1 ]
            FOR j := 0 TO oTexture:patchcount - 1
                nLump := oTexture:patches[ j + 1 ]:patch
                texturememory := texturememory + lumpinfo[ nLump + 1 ]:size
                W_CacheLumpNum( nLump, PU_CACHE )
            NEXT
        ENDIF
    NEXT

    aSpritePresent := BlankArray( numsprites, .F. )
    IF thinkercap != NIL
        oThinker := thinkercap:next
        DO WHILE oThinker != NIL .AND. !( oThinker == thinkercap )
            IF ThinkName( oThinker ) == "P_MobjThinker"
                oMobj := ThinkerMobj( oThinker )
                IF oMobj != NIL .AND. oMobj:sprite >= 0 .AND. oMobj:sprite < numsprites
                    aSpritePresent[ oMobj:sprite + 1 ] := .T.
                ENDIF
            ENDIF
            oThinker := oThinker:next
        ENDDO
    ENDIF

    spritememory := 0
    FOR i := 0 TO numsprites - 1
        IF aSpritePresent[ i + 1 ]
            FOR j := 0 TO sprites[ i + 1 ]:numframes - 1
                oFrame := sprites[ i + 1 ]:spriteframes[ j + 1 ]
                FOR k := 0 TO 7
                    nLump := firstspritelump + oFrame:lump[ k + 1 ]
                    spritememory := spritememory + lumpinfo[ nLump + 1 ]:size
                    W_CacheLumpNum( nLump, PU_CACHE )
                NEXT
            NEXT
        ENDIF
    NEXT
RETURN NIL
