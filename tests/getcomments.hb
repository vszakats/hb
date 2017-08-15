#!/usr/bin/env hbmk2

/* Copyright 2013-2017 Viktor Szakats (vszakats.net/harbour) */

/* Update copyright headers */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

PROCEDURE Main()

   LOCAL aFile
   LOCAL aHit, aHits
   LOCAL cFile
   LOCAL cExt
   LOCAL cOut := "all_comments.txt"

   LOCAL hReplace := { ;
      _in0() =>, ;
      _in1() =>, ;
      _in2() => }

   LOCAL cComments := ""
   LOCAL pComments := { => }
   LOCAL cType

   LOCAL hSkipFile := { cOut =>, "ChangeLog.txt" => }

   IF Empty( pComments[ "c" ] := hb_regexComp( "(?s)//.*?$|/\*.*?\*/",, .T. ) ) .OR. ;
      Empty( pComments[ "h" ] := hb_regexComp( "\#.*?$",, .T. ) )
      ? "Error in regexp"
      RETURN
   ENDIF

   FOR EACH aFile IN hb_DirScan( ".", hb_osFileMask() )
      cExt := hb_FNameExt( aFile[ F_NAME ] )

      IF ! hb_FNameNameExt( aFile[ F_NAME ] ) $ hSkipFile .AND. ;
         ! SkipDir( "./" + hb_FNameDir( aFile[ F_NAME ] ) ) .AND. ;
         "|" + cExt + "|" $ "||.c|.h|.hb|.prg|.hbm|.hbp|.hbc|.ini|.bat|.sh|.vbs|.def|.api|.ch|.txt|.mk|"

         IF "|" + cExt + "|" $ "||.hbm|.hbp|.hbc|.ini|.sh|.mk|"
            cType := "h"
         ELSE
            cType := "c"
         ENDIF

         cFile := hb_StrReplace( hb_MemoRead( aFile[ F_NAME ] ), hReplace )

         IF ! Empty( aHits := hb_regexAll( pComments[ cType ], cFile,, .T. ) )
            cComments += hb_eol() + "::" + " " + aFile[ F_NAME ] + " " + "::" + hb_eol() + hb_eol()
            FOR EACH aHit IN aHits
               cComments += aHit[ 1 ] + hb_eol()
            NEXT
         ENDIF
      ENDIF
   NEXT

   hb_MemoWrit( cOut, cComments )

   RETURN

STATIC FUNCTION SkipDir( cDir )

   cDir := StrTran( cDir, "\", "/" )

   RETURN ;
      "/doc/en/" $ cDir .OR. ;
      "/doc/pt_BR/" $ cDir

STATIC FUNCTION _in0()
#pragma __cstream | RETURN %s
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software Foundation,
Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
(or visit their website at https://www.gnu.org/licenses/).
#pragma __endtext

STATIC FUNCTION _in1()
#pragma __cstream | RETURN %s
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
#pragma __endtext

STATIC FUNCTION _in2()
#pragma __cstream | RETURN %s
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
#pragma __endtext
