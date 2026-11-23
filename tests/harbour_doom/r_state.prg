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

#include "r_state.ch"

INIT PROCEDURE init_r_state
    PUBLIC textureheight
    PUBLIC spritewidth
    PUBLIC spriteoffset
    PUBLIC spritetopoffset
    PUBLIC colormaps
    PUBLIC viewwidth
    PUBLIC scaledviewwidth
    PUBLIC viewheight
    PUBLIC firstflat
    PUBLIC flattranslation
    PUBLIC texturetranslation
    PUBLIC firstspritelump
    PUBLIC lastspritelump
    PUBLIC numspritelumps
    PUBLIC numsprites
    PUBLIC sprites
    PUBLIC viewx
    PUBLIC viewy
    PUBLIC viewz
    PUBLIC viewangle
    PUBLIC viewplayer
    PUBLIC clipangle
    PUBLIC viewangletox
    PUBLIC xtoviewangle
    PUBLIC rw_distance
    PUBLIC rw_normalangle
    PUBLIC rw_angle1
    PUBLIC sscount
    PUBLIC floorplane
    PUBLIC ceilingplane
    PUBLIC skyflatnum

    textureheight := {}
    spritewidth := {}
    spriteoffset := {}
    spritetopoffset := {}
    colormaps := {}
    viewwidth := 0
    scaledviewwidth := 0
    viewheight := 0
    firstflat := 0
    flattranslation := {}
    texturetranslation := {}
    firstspritelump := 0
    lastspritelump := 0
    numspritelumps := 0
    numsprites := 0
    sprites := {}
    viewx := 0
    viewy := 0
    viewz := 0
    viewangle := 0
    viewplayer := NIL
    clipangle := 0
    viewangletox := {}
    xtoviewangle := {}
    rw_distance := 0
    rw_normalangle := 0
    rw_angle1 := 0
    sscount := 0
    floorplane := NIL
    ceilingplane := NIL
    skyflatnum := 0
RETURN
