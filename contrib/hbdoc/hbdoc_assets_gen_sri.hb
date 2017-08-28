#!/usr/bin/env hbrun
/*
 * Recalculate SRI hashes for hbdoc_assets.yml
 *
 * Copyright 2017 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * (or visit their website at https://www.gnu.org/licenses/).
 *
 */

/* Requires: curl */

procedure main( fn )

  local r, pkg, file, url, body

  fn := hb_defaultvalue( fn, 'hbdoc_assets.yml' )

  for each pkg in hb_defaultValue( r := hb_yaml_decode( hb_memoread( fn ) ), { => } )
    for each file in pkg[ 'files' ]
      ? url := ;
        pkg[ 'root' ] + ;
        iif( 'ver' $ pkg, pkg[ 'ver' ] + '/', '' ) + ;
        file[ 'name' ]
      body := dl( url )
      if body == ''
        ? 'Wrong URL:', url
        loop
      endif
      if 'sri' $ file
        file[ 'sri' ] := 'sha384-' + hb_base64encode( hb_sha384( body, .t. ) )
      endif
    next
  next

  hb_memowrit( fn, hb_yaml_encode( r ) )

  return

static function dl( url )

  local stdout

  if hb_processrun( hb_strformat( 'curl -fsS -L --proto-redir =https "%1$s"', url ),, @stdout ) != 0
    stdout := ''
  endif

  return stdout
