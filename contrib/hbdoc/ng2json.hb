#!/usr/bin/env hbmk2
/*
 * Norton Guide (.ng) to .json converter
 *
 * Copyright 2016 Viktor Szakats (vszakats.net/harbour)
 *
 * .ng parsing logic based on:
 *   Expert Guide - A Text Mode Norton Guide Reader
 *   Copyright (C) 1997-2015 David A Pearson
 *   https://github.com/davep/eg
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their website at https://www.gnu.org/).
 *
 */

/* Keeping it tidy */
#pragma -w3
#pragma -es2

/* Optimizations */
#pragma -km+
#pragma -ko+

#include 'directry.ch'

procedure main( name )

  if empty( name )
    for each name in hb_vfdirectory( '*.NG' )
      conv_one( name[ F_NAME ] )
    next
  else
    conv_one( name )
  endif

static procedure conv_one( name )

  local a, p, ic, t1, t2, mt, mc, tmp
  local r := { => }

  a := hb_memoread( name )
  a := hb_bleft( a, 378 ) + hb_strxor( hb_bsubstr( a, 378 + 1 ), 0x1a )  // "decrypt"

  if hb_lefteq( a, 'NG' ) .or. ;
     hb_lefteq( a, 'EH' )  // untested

    mc := bin2w( hb_bsubstr( a, 7, 2 ) )  // menu count

    r[ 'title' ] := substrz( a, 9, 40 )
    for each tmp in r[ 'copy' ] := array( 5 )
      tmp := substrz( a, 0x31 + ( tmp:__enumindex() - 1 ) * 66, 66 )
    next
    while ! empty( r[ 'copy' ] ) .and. atail( r[ 'copy' ] ) == ''
      asize( r[ 'copy' ], len( r[ 'copy' ] ) - 1 )
    enddo
    r[ 'menu' ] := { => }

    p := 0x17a + 1
    while mc-- >= 0
      mt := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // menu type
      do case
      case mt == 0 .or. ;
           mt == 1
        tmp := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2
        p += tmp + 22
        loop
      case mt == 2
        p += 2
        ic := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // item count
        p += 20
        for each tmp in t1 := array( ic - 1 )
          tmp := readentry( a, bin2l( hb_bsubstr( a, p, 4 ) ) ); p += 4  // item pos
        next
        p += ic * 8
        t2 := r[ 'menu' ][ substrz( a, @p, 40 ) ] := { => }
        for tmp := 1 to ic - 1
          t2[ substrz( a, @p, 50 ) ] := t1[ tmp ]
        next
        p++
      otherwise
        exit
      endcase
    enddo
  endif

  hb_memowrit( hb_fnameextset( lower( name ), '.json' ), hb_jsonencode( r, .t. ) )

  return

static function readentry( a, p )

  local r, ec, sc, t1, t2, p_entry, d1, d2, tmp

  if p > 0
    p_entry := p

    p++

    switch d1 := hb_bcode( hb_bsubstr( a, d2 := p, 1 ) )
    case 0  // short entry

      p += 2

      p += 2  // unknown
      ec := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // entry count
      p += 2  // unknown
      p += 2  // parent line
      p += 4  // parent entry
      p += 12

      for each tmp in t1 := array( ec )
        p += 2  // unknown
        tmp := readentry( a, bin2l( hb_bsubstr( a, p, 4 ) ) ); p += 4  // entry offset (may be zero) -> entry (may be nil or duplicate)
      next

      t2 := readentrytext( a, p, ec )

      r := array( ec )
      for tmp := 1 to ec
        if hb_ishash( t1[ tmp ] ) .and. 'content' $ t1[ tmp ]
          r[ tmp ] := t1[ tmp ]
          r[ tmp ][ 'title' ] := t2[ tmp ]
        else
          r[ tmp ] := { ;
            'title' => t2[ tmp ], ;
            'content' => t1[ tmp ] }
        endif
      next

      return r

    case 1  // long entry

      p += 2

      p += 2  // unknown
      ec := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // entry count
      sc := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // seealso any?
      p += 2  // parent line
      p += 4  // parent entry
      p += 4
      p += 2
      p += 6

      r := { ;
        'title' =>, ;
        'id' => p_entry, ;
        'content' => readentrytext( a, @p, ec ) }

      if sc > 0
        sc := bin2w( hb_bsubstr( a, p, 2 ) ); p += 2  // seealso count
        for each tmp in r[ 'seealso' ] := array( sc )
          tmp := { 'link' => bin2l( hb_bsubstr( a, p, 4 ) ) }; p += 4  // entry offset
        next
        for each tmp in r[ 'seealso' ]
          tmp[ 'title' ] := substrz( a, @p, 70 )
        next
      endif

      return r

    otherwise
      ? '!', 'unrecognized entry type', hb_numtohex( d1 ), hb_numtohex( d2 )
    endswitch
  endif

  return nil

static function readentrytext( a, /* @ */ p, nCount )

  local aResult, tmp

  for each tmp in aResult := array( nCount )
    tmp := substrz( a, @p, 1024 )
  next

  return aResult

static function substrz( a, /* @ */ p, nLen )

  local cResult, cChar, tmp

  a := hb_bsubstr( a, p, nLen )
  if ( tmp := hb_bat( hb_bchar( 0 ), a ) ) > 0
    a := hb_bleft( a, tmp - 1 )
  endif

  p += hb_blen( a ) + 1

  cResult := ''
  for tmp := 1 to hb_blen( a )
    cChar := hb_bsubstr( a, tmp, 1 )
    cResult += iif( cChar == hb_bchar( 0xff ), space( hb_bcode( hb_bsubstr( a, ++tmp, 1 ) ) ), cChar )
  next

  while ( tmp := hb_bat( '^C', cResult ) ) > 0 .or. ;
        ( tmp := hb_bat( '^c', cResult ) ) > 0
    cResult := hb_bleft( cResult, tmp - 1 ) + hextostr( hb_bsubstr( cResult, tmp + 2, 2 ) ) + hb_bsubstr( cResult, tmp + 4 )
  enddo

  return hb_translate( cResult, 'cp437', 'UTF8' )

static function hextostr( hex )

  hex := hb_hextostr( hex )

  return iif( hex == '^', '^^', hex )
