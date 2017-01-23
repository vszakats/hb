#!/usr/bin/env hbmk2
/*
 * Norton Guide JSON to HBDOC/NFDOC (.txt) format converter
 *
 * Copyright 2016 Viktor Szakats (vszakats.net/harbour)
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

/* Caveat:

   - This tool was created for and tested with certain specific .ng files
     and therefore makes certain assumptions on the doc content. It means
     it may work with other content, but the results may well be wrong.

   Usage:

   1. create directory `<harbour-root>/contrib/<name>/en/doc` and make it the current one
   2. get original .ng file(s) and copy them to the directory
   3. run `./ng2json.hb`
   4. run this script
   5. done
 */

/* Keeping it tidy */
#pragma -w3
#pragma -es2

/* Optimizations */
#pragma -km+
#pragma -ko+

#include 'directry.ch'

procedure main()

  local f, aEntry := {}, savename

  for each f in hb_vfdirectory( './*.json' )
    hb_default( @savename, f[ F_NAME ] )
    loadngjson( f[ F_NAME ], aEntry, savename )
  next

  hb_memowrit( savename := hb_fnameextset( savename, '.txt' ), __hbdoc_ToSource( aEntry ) )

  ? '! running hbmk2 -fixcase' ; hb_run( 'hbmk2 -fixcase ' + savename )

  return

static procedure loadngjson( fn, aEntry, savename )

  static s_lcopy := .f.

  local ng := hb_jsondecode( hb_memoread( fn ) )

  local name, cred, menu, cat, subcat, line, e

  /* add copyright entry */
  if ! s_lcopy
    name := ng[ 'title' ]
    cred := ''
    for each line in ng[ 'copy' ]
      if ! hb_isnull( cred )
        cred += chr( 10 )
      endif
      cred += alltrim( line )
    next

    if '»' $ name
      name := rtrim( hb_bleft( name, hb_bat( '»', name ) - 1 ) )
    endif

    e := { => }
    e[ '_DOCSOURCE' ] := fn
    e[ '_LANG' ] := 'en'
    e[ 'NAME' ] := name
    e[ 'TEMPLATE' ] := 'Document'
    e[ 'ONELINER' ] := e[ 'CATEGORY' ] := 'Copyright'
    e[ 'DESCRIPTION' ] := cred
    e[ 'PLATFORMS' ] := 'DOS'
    aadd( aEntry, entrysort( e ) )

    s_lcopy := .t.
  endif

  /* deal with some content pecularities */
  for each menu in ng[ 'menu' ]
    for each cat in menu
      if hb_isarray( cat )
        for each subcat in cat descend
          if right( subcat[ 'title' ], len( 'continued...' ) ) == 'continued...' .and. ;  /* join multi-part entries. MemoEdit(), @...GET, #command */
             subcat:__enumindex() > 1
            for each line in subcat[ 'content' ]
              aadd( cat[ subcat:__enumindex() - 1 ][ 'content' ], line )
            next
            hb_adel( cat, subcat:__enumindex(), .t. )
          elseif 'SET MESSAGE' $ subcat[ 'title' ] .and. 'SET MEMOBLOCK' $ subcat[ 'content' ][ 1 ]  /* fix typo */
            subcat[ 'content' ][ 1 ] := ' ' + 'SET MESSAGE'
          elseif ! empty( subcat[ 'content' ] ) .and. hb_isstring( subcat[ 'content' ][ 1 ] ) .and. ! hb_lefteq( subcat[ 'content' ][ 1 ], ' ' )  /* fix overflown title. ORDSETRELATION(), WORD()* */
            subcat[ 'title' ] += ' ' + subcat[ 'content' ][ 1 ]
            hb_adel( subcat[ 'content' ], 1, .t. )
          endif
        next
      else
        /* hack: add some separators to help getting tables detected right */
        if hb_isarray( cat[ 'content' ] ) .and. ;
           len( cat[ 'content' ] ) >= 1 .and. ;
           hb_isstring( cat[ 'content' ][ 1 ] )
          for each line in cat[ 'content' ] descend
            if hb_lefteq( line, ' ^BSay Picture Template Symbols^b' ) .or. ;
               hb_lefteq( line, '^b Get Picture Functions^b' ) .or. ;
               hb_lefteq( line, '^b Get Picture Template Symbols ^b' )
              hb_ains( cat[ 'content' ], line:__enumindex(), replicate( '─', 77 ), .t. )
            endif
          next
        endif
      endif
    next
  next

  /* convert entries to NFDOC format */
  // ? name
  for each menu in ng[ 'menu' ]
    // ? "", menu:__enumkey()
    for each cat in menu
      // ? "", "", cat:__enumkey()
      if hb_isarray( cat )
        for each subcat in cat
          if ! empty( subcat[ 'content' ] )
            // ? "", "", "", subcat[ 'title' ]
            tonf( aEntry, subcat, cat:__enumkey(), menu:__enumkey(), fn, savename )
          endif
        next
      else
        tonf( aEntry, cat, cat:__enumkey(), menu:__enumkey(), fn, savename )
      endif
    next
  next

  return

static function tonf( aEntry, nge, cat, menu, fn, savename )

  local line, line_raw
  local hEntry := { => }
  local cName := '', nTable := 0, lDoc := .f., lClass := .f.
  local tmp
  local nIndent
  local container
  local nskip := 0
  local title

  if nge[ 'title' ] == nil
    hEntry[ 'TEMPLATE' ] := 'Document'
    hEntry[ 'NAME' ] := hEntry[ 'ONELINER' ] := title := cat
    hEntry[ 'CATEGORY' ] := menu
    hEntry[ cName := 'DESCRIPTION' ] := ''
    lDoc := .t.
  elseif hb_isstring( nge[ 'content' ][ 1 ] )
    if hb_isstring( nge[ 'title' ] )
      hEntry[ 'NAME' ] := rtrim( cln( substr( nge[ 'content' ][ 1 ], 2 ) ) )
      if hb_isnull( nge[ 'title' ] )
        hEntry[ 'ONELINER' ] := cat
      else
        title := allTrim( left( nge[ 'title' ], iif( 'tools' $ savename, 13, 16 ) ) )
        if ! empty( tmp := rtrim( cln( substr( nge[ 'content' ][ 2 ], 2 ) ) ) )
          nskip := 2
          hEntry[ 'ONELINER' ] := tmp
          if ! hb_lefteq( nge[ 'content' ][ 3 ], '────────' )
            hEntry[ 'ONELINER' ] += nge[ 'content' ][ 3 ]
            nskip++
          endif
        else
          hEntry[ 'ONELINER' ] := allTrim( nge[ 'title' ] )
        endif
      endif
    else
      title := allTrim( left( nge[ 'title' ], iif( 'tools' $ savename, 13, 16 ) ) )
      hEntry[ 'NAME' ] := rtrim( cln( substr( nge[ 'content' ][ 1 ], 2 ) ) )
      hEntry[ 'ONELINER' ] := rtrim( cln( substr( nge[ 'content' ][ 2 ], 2 ) ) )
      nskip := 2
      if ! hb_lefteq( nge[ 'content' ][ 3 ], '────────' )
        hEntry[ 'ONELINER' ] += nge[ 'content' ][ 3 ]
        nskip++
      endif
    endif
  else
    lClass := .t.
    hEntry[ 'NAME' ] := title := allTrim( left( nge[ 'title' ], 18 ) )
    hEntry[ 'ONELINER' ] := allTrim( substr( nge[ 'title' ], 18 + 1 ) )
  endif
  if lower( hEntry[ 'NAME' ] ) == 'introduction'
    hEntry[ 'ONELINER' ] := hEntry[ 'NAME' ]
    hEntry[ 'NAME' ] := cat
    hEntry[ 'TEMPLATE' ] := 'Document'
    hEntry[ 'CATEGORY' ] := 'Intro'
    hEntry[ cName := 'DESCRIPTION' ] := ''
    lDoc := .t.
  endif

  if hb_isstring( nge[ 'content' ][ 1 ] )
    container := { nge }
  else
    lClass := .t.
    cat := nge[ 'title' ]
    container := nge[ 'content' ]
    hEntry[ 'TEMPLATE' ] := 'Class'
    hEntry[ 'ONELINER' ] := rtrim( cln( substr( nge[ 'content' ][ 1 ][ 'content' ][ 2 ], 2 ) ) )
    nskip := 2
    if ! hb_lefteq( nge[ 'content' ][ 1 ][ 'content' ][ 3 ], '────────' )
      hEntry[ 'ONELINER' ] += ' ' + rtrim( cln( substr( nge[ 'content' ][ 1 ][ 'content' ][ 3 ], 2 ) ) )
      nskip++
    endif
  endif

  for each nge in container

    if cname == 'METHODSLINK' .and. ;
       ! hb_lefteq( nge[ 'title' ], '────────' ) .and. ! hb_isnull( nge[ 'title' ] )
      if ! cName $ hEntry
        hEntry[ cName ] := ''
      endif
      if ! hb_isnull( hEntry[ cName ] )
        hEntry[ cName ] += chr( 10 ) + chr( 10 )
      endif
      hEntry[ cName ] += '*' + cln( alltrim( nge[ 'title' ] ) ) + '*' + chr( 10 ) + '---'
      nIndent := nil
    endif

    if nge[ 'content' ] == nil
      loop
    endif

    for each line in nge[ 'content' ]

      line := cln( line_raw := line )

      do case
      case line:__enumindex() <= nskip
        /* nop */
      case ! empty( substr( line, 1, 2 ) ) .and. ! ldoc
        tmp := alltrim( line )
        if hb_lefteqi( tmp, 'FILES ' )
          hEntry[ 'FILES' ] := cln_files( substr( line, 10 ) )
          cName := ''
        elseif hb_lefteqi( tmp, 'FILES:' )
          hEntry[ 'FILES' ] := cln_files( substr( line, 13 ) )
          cName := ''
        elseif cname == 'METHODSLINK'
          hEntry[ cName ] += chr( 10 ) + conv( alltrim( line_raw ) )
          nIndent := nil
        elseif hb_lefteq( tmp, '────────' )
          /* nop */
        elseif ! substr( line_raw, 2, 1 ) == '^'  // ordName()
          sectadd( hEntry, cName, tmp )
        elseif ! upper( tmp ) $ '|TEMPLATE|NAME|ONELINER|CATEGORY|SUBCATEGORY|SYNTAX|ARGUMENTS|RETURNS|DESCRIPTION|NOTES|DATALINK|DATANOLINK|METHODSLINK|METHODSNOLINK|EXAMPLES|TESTS|STATUS|COMPLIANCE|PLATFORMS|FILES|TAGS|'
          cName := 'DESCRIPTION'
          if hb_isnull( tmp )
            sectadd( hEntry, cName, tmp )
          else
            if nIndent == nil
              nIndent := 1
            endif
            if nTable == 0
              sectadd( hEntry, cName, '*' + tmp + '*' )
            else
              sectadd( hEntry, cName, ' ' + tmp )
            endif
          endif
        else
          if nTable > 0 .and. ! empty( cName )
            hEntry[ cName ] += chr( 10 ) + '</table>'
          endif
          tmp := upper( tmp )
          cName := hb_hgetdef( { ;
            'ARGUMENT' => 'ARGUMENTS', ;
            'EXAMPLE'  => 'EXAMPLES', ;
            'FILE'     => 'FILES', ;
            'NOTE'     => 'NOTES' }, tmp, tmp )
          hEntry[ cName ] := ''
          nTable := 0
        endif
      case ! empty( cName )
        if empty( substr( line, 1, 4 ) ) .and. nTable == 0
          line := substr( line, 5 )
        else
          if nTable > 0
            line := substr( line, 2 )
          else
            line := conv( substr( line_raw, 2 ) )

            if lDoc
              if hb_lefteqi( tmp := cln( substr( line_raw, 2 ) ), 'appendix ' )
                hEntry[ 'NAME' ] := 'Appendix' + ' ' + hEntry[ 'NAME' ]
                hEntry[ 'TEMPLATE' ] := 'Document'
                hEntry[ 'CATEGORY' ] := 'Appendix'
                loop
              elseif lower( tmp ) == 'introduction'
                loop
              endif
            endif

            if hb_lefteq( line, '<b>' ) .and. ! right( line, 4 ) == '</b>'
              line += '</b>'
            endif
          endif
        endif
        if ! hb_isnull( hEntry[ cName ] )
          hEntry[ cName ] += chr( 10 )
        endif
        if hb_isnull( line )
          tmp := ''
        else
          if nIndent == nil
            nIndent := len( line ) - len( ltrim( line ) )
          endif
          tmp := rtrim( substr( line, nIndent + 1 ) )
        endif
        if hb_lefteq( ltrim( tmp ), '────────' )
          if nTable > 0 .or. ! 'tools' $ savename .or. ! hb_isnull( line:__enumbase()[ line:__enumindex() + 1 ] )
            do case
            case nTable == 0
              tmp := space( len( tmp ) - len( ltrim( tmp ) ) ) + '<table>'
              nTable++
            case nTable == 1
              tmp := ''
              nTable++
            case nTable == 2
              tmp := space( len( tmp ) - len( ltrim( tmp ) ) ) + '</table>'
              nTable := 0
            endcase
          else
            tmp := space( len( tmp ) - len( ltrim( tmp ) ) ) + '---'
          endif
        endif
        hEntry[ cName ] += iif( nTable > 0 .and. ! ltrim( tmp ) == '<table>', ' ', '' ) + tmp
      endcase
    next

    if lClass
      cName := 'METHODSLINK'
      nskip := 0
    endif
  next

  if nTable > 0 .and. ! empty( cName )
    hEntry[ cName ] += chr( 10 ) + '</table>'
  endif

  if 'seealso' $ container[ 1 ] .and. ! empty( container[ 1 ][ 'seealso' ] )
    hEntry[ 'SEEALSO' ] := ''
    for each line in container[ 1 ][ 'seealso' ]
      if ! hb_isnull( hEntry[ 'SEEALSO' ] )
        hEntry[ 'SEEALSO' ] += ', '
      endif
      hEntry[ 'SEEALSO' ] += line[ 'title' ]
    next
  endif

  addentry( aEntry, hEntry, fn, cat, title )

  return nil

static procedure sectadd( hEntry, cName, line )

  if ! cName $ hEntry
     hEntry[ cName ] := ''
  endif
  if ! hb_isnull( hEntry[ cName ] )
    hEntry[ cName ] += chr( 10 )
  endif
  hEntry[ cName ] += line

  return

static procedure addentry( aEntry, e, fn, cat, cAlias )

  local tmp

  if ! empty( e )
    if ! 'DESCRIPTION' $ e .and. ! 'ARGUMENTS' $ e .and. ! 'EXAMPLES' $ e
      return
    endif
    if 'ONELINER' $ e .and. hb_lefteq( e[ 'ONELINER' ], '────────' )
      e[ 'ONELINER' ] := e[ 'NAME' ]
    endif
    if 'NAME' $ e
      switch e[ 'NAME' ]
      case '<> != #'
        cAlias := '<>'
        exit
      case '#xcommand | #xtranslate'
        cAlias := '#xcommand'
        exit
      case 'CLEAR ALL*'
        cAlias := 'CLEAR'
        exit
      endswitch
      if cAlias != nil .and. ! cAlias == e[ 'NAME' ]
        e[ 'ALIAS' ] := cAlias
      endif
    endif
    if ! 'NAME' $ e
      /* Fill basic fields if those weren't provided in
         doc source. It happens with 'tables' in cl5 docs. */
      e[ 'NAME' ] := cat  // hb_hgetdef( s, hb_fnamename( fn ), '' )
      e[ 'TEMPLATE' ] := 'Document'
      e[ 'CATEGORY' ] := 'Table'
      e[ 'ONELINER' ] := e[ 'NAME' ]
    elseif ! 'TEMPLATE' $ e
      if '|' + upper( e[ 'NAME' ] ) + '|' $ ;
         '|ANNOUNCE|BEGIN SEQUENCE|DECLARE*|DO*|DO CASE|DO WHILE|EXIT' + ;
         '|PROCEDURE|EXTERNAL*|FIELD|FOR|FUNCTION|IF|INIT PROCEDURE' + ;
         '|LOCAL|MEMVAR|PARAMETERS|PRIVATE|PROCEDURE|PUBLIC|REQUEST|RETURN|STATIC|'
        e[ 'TEMPLATE' ] := 'Statement'
      elseif ( upper( e[ 'NAME' ] ) == e[ 'NAME' ] .and. ;
           ( ! '(' $ e[ 'NAME' ] .or. e[ 'NAME' ] == '( )' ) ) .or. ;
         hb_lefteq( e[ 'NAME' ], '= ' )  // f.e. "= (assign)"
        e[ 'TEMPLATE' ] := 'Command'
      elseif 'METHODSLINK' $ e
        e[ 'TEMPLATE' ] := 'Class'
        e[ 'NAME' ] += ' class'
      elseif ( tmp := hb_bat( "#", e[ 'NAME' ] ) ) > 0 .AND. hb_asciiisalpha( hb_bsubStr( e[ 'NAME' ], tmp + 1, 1 ) )
        // directive
      elseif 'RETURNS' $ e .or. 'SYNTAX' $ e .or. '()' $ e[ 'NAME' ]
        e[ 'TEMPLATE' ] := 'Function'
      else
        e[ 'TEMPLATE' ] := 'Document'
      endif
    endif
    if e[ 'NAME' ] == 'Installing CA-Clipper Tools' .or. ;
       e[ 'NAME' ] == 'Using CA-Clipper Tools'
      e[ 'CATEGORY' ] := 'Intro'
    endif
    e[ '_DOCSOURCE' ] := fn
    e[ '_LANG' ] := 'en'
    if 'TEMPLATE' $ e .and. e[ 'TEMPLATE' ] == 'Document' .and. 'ALIAS' $ e
      hb_hdel( e, 'ALIAS' )
    endif
    e[ 'PLATFORMS' ] := 'DOS'
    aadd( aEntry, entrysort( e ) )
  endif

  return

static function cln( s )  /* strip formatting markers */

  static t := { ;
    '^B' => '', ;
    '^b' => '', ;
    '^N' => '', ;
    '^n' => '', ;
    '^R' => '', ;
    '^r' => '', ;
    '^U' => '', ;
    '^u' => '', ;
    '^^' => '^', ;
    hb_bchar( 0x00 ) => '?', ;
    hb_bchar( 0x01 ) => '?', ;
    hb_bchar( 0x02 ) => '?', ;
    hb_bchar( 0x03 ) => '?', ;
    hb_bchar( 0x04 ) => '?', ;
    hb_bchar( 0x05 ) => '?', ;
    hb_bchar( 0x06 ) => '?', ;
    hb_bchar( 0x07 ) => '?', ;
    hb_bchar( 0x08 ) => '?', ;
    hb_bchar( 0x09 ) => '?', ;
    hb_bchar( 0x0a ) => '?', ;
    hb_bchar( 0x0b ) => '?', ;
    hb_bchar( 0x0c ) => '?', ;
    hb_bchar( 0x0d ) => '?', ;
    hb_bchar( 0x0e ) => '?', ;
    hb_bchar( 0x0f ) => '?', ;
    hb_bchar( 0x10 ) => '?', ;
    hb_bchar( 0x11 ) => '?', ;
    hb_bchar( 0x12 ) => '?', ;
    hb_bchar( 0x13 ) => '?', ;
    hb_bchar( 0x14 ) => '?', ;
    hb_bchar( 0x15 ) => '?', ;
    hb_bchar( 0x16 ) => '?', ;
    hb_bchar( 0x17 ) => '?', ;
    hb_bchar( 0x18 ) => '?', ;
    hb_bchar( 0x19 ) => '?', ;
    hb_bchar( 0x1a ) => '?', ;
    hb_bchar( 0x1b ) => '?', ;
    hb_bchar( 0x1c ) => '?', ;
    hb_bchar( 0x1d ) => '?', ;
    hb_bchar( 0x1e ) => '?', ;
    hb_bchar( 0x1f ) => '?', ;
    hb_bchar( 0x7f ) => '?' }

  local tmp

  while ( tmp := hb_bat( '^A', s ) ) > 0 .or. ;
        ( tmp := hb_bat( '^a', s ) ) > 0
    s := hb_bleft( s, tmp - 1 ) + hb_bsubstr( s, tmp + 4 )
  enddo

  return hb_strreplace( s, t )

static function conv( s )  /* convert formatting markers to HBDOC ones */

  static t := { ;
    '^N' => '', ;
    '^n' => '', ;
    '^R' => '', ;
    '^r' => '', ;
    '^U' => '', ;
    '^u' => '', ;
    '^^' => '^', ;
    hb_bchar( 0x00 ) => '?', ;
    hb_bchar( 0x01 ) => '?', ;
    hb_bchar( 0x02 ) => '?', ;
    hb_bchar( 0x03 ) => '?', ;
    hb_bchar( 0x04 ) => '?', ;
    hb_bchar( 0x05 ) => '?', ;
    hb_bchar( 0x06 ) => '?', ;
    hb_bchar( 0x07 ) => '?', ;
    hb_bchar( 0x08 ) => '?', ;
    hb_bchar( 0x09 ) => '?', ;
    hb_bchar( 0x0a ) => '?', ;
    hb_bchar( 0x0b ) => '?', ;
    hb_bchar( 0x0c ) => '?', ;
    hb_bchar( 0x0d ) => '?', ;
    hb_bchar( 0x0e ) => '?', ;
    hb_bchar( 0x0f ) => '?', ;
    hb_bchar( 0x10 ) => '?', ;
    hb_bchar( 0x11 ) => '?', ;
    hb_bchar( 0x12 ) => '?', ;
    hb_bchar( 0x13 ) => '?', ;
    hb_bchar( 0x14 ) => '?', ;
    hb_bchar( 0x15 ) => '?', ;
    hb_bchar( 0x16 ) => '?', ;
    hb_bchar( 0x17 ) => '?', ;
    hb_bchar( 0x18 ) => '?', ;
    hb_bchar( 0x19 ) => '?', ;
    hb_bchar( 0x1a ) => '?', ;
    hb_bchar( 0x1b ) => '?', ;
    hb_bchar( 0x1c ) => '?', ;
    hb_bchar( 0x1d ) => '?', ;
    hb_bchar( 0x1e ) => '?', ;
    hb_bchar( 0x1f ) => '?', ;
    hb_bchar( 0x7f ) => '?' }

  local tmp, flag := .f.

  while ( tmp := hb_bat( '^B', s ) ) > 0 .or. ;
        ( tmp := hb_bat( '^b', s ) ) > 0
    s := hb_bleft( s, tmp - 1 ) + iif( flag := ! flag, '<b>', '</b>' ) + hb_bsubstr( s, tmp + 2 )
  enddo

  return hb_strreplace( s, t )

static function cln_files( s )
  return strtran( s, '\', '/' )

static function entrysort( e )

  static s_a := { ;
    "TEMPLATE"      , ;
    "NAME"          , ;
    "ALIAS"         , ;
    "ONELINER"      , ;
    "CATEGORY"      , ;
    "SUBCATEGORY"   , ;
    "SYNTAX"        , ;
    "ARGUMENTS"     , ;
    "RETURNS"       , ;
    "DESCRIPTION"   , ;
    "NOTES"         , ;
    "DATALINK"      , ;
    "DATANOLINK"    , ;
    "METHODSLINK"   , ;
    "METHODSNOLINK" , ;
    "EXAMPLES"      , ;
    "TESTS"         , ;
    "STATUS"        , ;
    "COMPLIANCE"    , ;
    "PLATFORMS"     , ;
    "FILES"         , ;
    "TAGS"          , ;
    "SEEALSO"        }

  local n := { => }, tmp

  for each tmp in s_a
    if tmp $ e
      n[ tmp ] := e[ tmp ]
    endif
  next

  return n
