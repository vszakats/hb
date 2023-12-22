#!/usr/bin/env hbmk2
/*
 * Commit preparer and source checker/fixer
 *
 * Copyright 2012-2017 Viktor Szakats
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

#define _CONFIGFIL_ ".hbcommit"
#define _CONFIGENV_ "HBCOMMIT_USER"

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"
#include "hbgtinfo.ch"

PROCEDURE Main()

   LOCAL cVCS
   LOCAL cVCSDir
   LOCAL cLocalRoot
   LOCAL aFiles
   LOCAL aChanges
   LOCAL cLogName

   IF "-c" $ cli_Options()
      CheckFileList( iif( Empty( cli_Values() ),, cli_Values() ) )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   cVCS := VCSDetect( @cVCSDir, @cLocalRoot )

   IF cVCS == "git"
      InstallHook( cVCSDir, "pre-commit"        , hb_StrFormat( "exec %1$s %2$s --check-only", hb_FNameNameExt( hbshell_ProgName() ), CommitScript() ) )
#if 0
      InstallHook( cVCSDir, "prepare-commit-msg", hb_StrFormat( "exec %1$s %2$s $1 --prepare-commit", hb_FNameNameExt( hbshell_ProgName() ), CommitScript() )
#endif
   ENDIF

   aFiles := {}
   aChanges := DoctorChanges( cVCS, Changes( cVCS ), aFiles )

   IF Empty( aChanges )
      OutStd( hb_ProgName() + ": " + "no changes" + hb_eol() )
      ErrorLevel( 0 )
      RETURN
   ENDIF

   IF CheckFileList( aFiles, cLocalRoot, .F. ) .OR. ;
      "-f" $ cli_Options() .OR. ;
      "--force" $ cli_Options()

      IF ! "--check-only" $ cli_Options() .AND. ;
         ! "--prepare-commit" $ cli_Options()

         hb_MemoWrit( cLogName := cLocalRoot + "_commit.txt", MakeEntry( aChanges ) )

         OutStd( hb_ProgName() + ": " + hb_StrFormat( "Edit %1$s and commit", cLogName ) + hb_eol() )
#if 0
         LaunchCommand( GitEditor(), cLogName )
#endif
      ENDIF

      ErrorLevel( 0 )
   ELSE
      OutStd( hb_ProgName() + ": " + "Please correct errors listed above and re-run" + hb_eol() )
      ErrorLevel( 1 )
   ENDIF

   RETURN

STATIC FUNCTION cli_Options()

   THREAD STATIC t_hOptions

   LOCAL tmp
   LOCAL nArg

   IF t_hOptions == NIL
      t_hOptions := { => }
      nArg := 1
      FOR tmp := 1 TO hb_argc()
         IF hb_LeftEq( hb_argv( tmp ), "-" )
            t_hOptions[ hb_argv( tmp ) ] := nArg
         ELSE
            ++nArg
         ENDIF
      NEXT
   ENDIF

   RETURN t_hOptions

STATIC FUNCTION cli_Values()

   THREAD STATIC t_aValues

   LOCAL tmp

   IF t_aValues == NIL
      t_aValues := {}
      FOR tmp := 1 TO hb_argc()
         IF ! hb_LeftEq( hb_argv( tmp ), "-" )
            AAdd( t_aValues, hb_argv( tmp ) )
         ENDIF
      NEXT
   ENDIF

   RETURN t_aValues

STATIC FUNCTION CommitScript()

   LOCAL cBaseName := hb_FNameName( hb_ProgName() ) + ".hb"
   LOCAL cName

   IF hb_vfExists( cName := hb_DirSepToOS( "bin/" ) + cBaseName )
      RETURN cName
   ENDIF

   RETURN cBaseName

STATIC FUNCTION InstallHook( cDir, cHookName, cCommand )

   LOCAL cName := hb_DirSepAdd( cDir ) + hb_DirSepToOS( "hooks/" ) + cHookName
   LOCAL cFile := hb_MemoRead( cName )

   cCommand := StrTran( cCommand, "\", "/" )

   IF cCommand $ cFile
      RETURN .T.
   ENDIF

   IF cFile == ""
      cFile += "#!/bin/sh" + Chr( 10 )
   ENDIF

   RETURN hb_MemoWrit( cName, cFile + Chr( 10 ) + cCommand + Chr( 10 ) )

STATIC FUNCTION GetLastEntry( cLog, /* @ */ nStart, /* @ */ nEnd )

   LOCAL cLogHeaderExp := "\n[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9] [0-2][0-9]:[0-6][0-9] [\S ]*"

   LOCAL cOldCP := hb_cdpSelect( "cp437" )
   LOCAL cHit

   nEnd := 0

   IF Empty( cHit := hb_AtX( cLogHeaderExp, cLog ) )
      cHit := ""
   ENDIF

   IF ( nStart := At( AllTrim( cHit ), cLog ) ) > 0

      IF Empty( cHit := hb_AtX( cLogHeaderExp, cLog,, nStart + Len( cHit ) ) )
         cHit := ""
      ENDIF

      IF ( nEnd := At( AllTrim( cHit ), cLog ) ) == 0
         nEnd := Len( cLog )
      ENDIF

      cLog := RTrimEOL( SubStr( cLog, nStart, nEnd - nStart ) )
   ELSE
      cLog := ""
   ENDIF

   hb_cdpSelect( cOldCP )

   RETURN cLog

STATIC FUNCTION MakeEntry( aChanges )

   LOCAL cLog := "module: edit my changes" + hb_eol() + hb_eol()
   LOCAL cLine

   FOR EACH cLine IN aChanges
      cLog += cLine + hb_eol()
   NEXT

   RETURN cLog

STATIC FUNCTION VCSDetect( /* @ */ cVCSDir, /* @ */ cLocalRoot )

   DO CASE
   CASE hb_vfDirExists( ".svn" )
      cVCSDir := hb_DirSepToOS( "./.svn/" )
      cLocalRoot := hb_DirSepToOS( "./" )
      RETURN "svn"
   CASE hb_vfDirExists( ".git" )
      cVCSDir := hb_DirSepToOS( "./.git/" )
      cLocalRoot := hb_DirSepToOS( "./" )
      RETURN "git"
   CASE GitDetect( @cVCSDir )
      cVCSDir := cVCSDir
      cLocalRoot := GitLocalRoot()
      RETURN "git"
   ENDCASE

   cVCSDir := ""

   RETURN ""

STATIC FUNCTION GitDetect( /* @ */ cGitDir )

   LOCAL cStdOut, cStdErr
   LOCAL nResult := hb_processRun( "git rev-parse --is-inside-work-tree",, @cStdOut, @cStdErr )

   IF nResult == 0 .AND. hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) == "true"
      hb_processRun( "git rev-parse --git-dir",, @cGitDir )
      cGitDir := hb_DirSepAdd( hb_DirSepToOS( hb_StrReplace( cGitDir, Chr( 13 ) + Chr( 10 ) ) ) )
      RETURN .T.
   ENDIF

   RETURN .F.

STATIC FUNCTION GitLocalRoot()

   LOCAL cStdOut, cStdErr
   LOCAL nResult := hb_processRun( "git rev-parse --show-toplevel",, @cStdOut, @cStdErr )

   RETURN iif( nResult == 0, hb_DirSepAdd( hb_DirSepToOS( hb_StrReplace( cStdOut, Chr( 13 ) + Chr( 10 ) ) ) ), "" )

STATIC FUNCTION GitFileList()

   LOCAL cStdOut
   LOCAL nResult := hb_processRun( "git ls-files",, @cStdOut )
   LOCAL aList := iif( nResult == 0, hb_ATokens( cStdOut, .T. ), {} )
   LOCAL cItem

   FOR EACH cItem IN aList DESCEND
      IF cItem == ""
         hb_ADel( aList, cItem:__enumIndex(), .T. )
      ELSE
         cItem := hb_DirSepToOS( cItem )
      ENDIF
   NEXT

   RETURN aList

STATIC FUNCTION GitEditor()

   LOCAL cValue

   hb_processRun( Shell() + " " + CmdEscape( "git config --global core.editor" ),, @cValue )

   cValue := hb_StrReplace( cValue, Chr( 10 ) + Chr( 13 ) )

   IF Left( cValue, 1 ) == "'" .AND. Right( cValue, 1 ) == "'"
      cValue := hb_StrShrink( SubStr( cValue, 2 ) )
   ENDIF

   IF Lower( cValue ) == "notepad.exe"  /* banned, use notepad2.exe or else */
      cValue := ""
   ENDIF

   RETURN cValue

STATIC FUNCTION DoctorChanges( cVCS, aChanges, aFiles )

   LOCAL cLine
   LOCAL cStart
   LOCAL aNew := {}

   LOCAL cFile
   LOCAL tmp

   ASort( aChanges )

   SWITCH cVCS
   CASE "svn"

      FOR EACH cLine IN aChanges
         IF ! Empty( cLine ) .AND. SubStr( cLine, 8, 1 ) == " "
            cStart := Left( cLine, 1 )
            SWITCH cStart
            CASE "M"
            CASE " "  ; cStart := "*" ; EXIT  /* modified props */
            CASE "A"  ; cStart := "+" ; EXIT
            CASE "D"  ; cStart := "-" ; EXIT
            CASE "X"  ; cStart := "" ; EXIT
            OTHERWISE ; cStart := "?"
            ENDSWITCH
            IF ! cStart == ""
               AAdd( aNew, "  " + cStart + " " + StrTran( SubStr( cLine, 8 + 1 ), "\", "/" ) )
               IF ! cStart == "-"
                  AAdd( aFiles, SubStr( cLine, 8 + 1 ) )
               ENDIF
            ENDIF
         ENDIF
      NEXT
      EXIT

   CASE "git"

      FOR EACH cLine IN aChanges
         IF ! Empty( cLine ) .AND. SubStr( cLine, 3, 1 ) == " "
            cStart := Left( cLine, 1 )
            IF Empty( Left( cLine, 1 ) )
               cStart := SubStr( cLine, 2, 1 )
            ENDIF
            SWITCH cStart
            CASE " "
            CASE "?"  ; cStart := "" ; EXIT
            CASE "M"
            CASE "R"
            CASE "T"
            CASE "U"  ; cStart := "*" ; EXIT
            CASE "A"
            CASE "C"  ; cStart := "+" ; EXIT
            CASE "D"  ; cStart := "-" ; EXIT
            OTHERWISE ; cStart := "?"
            ENDSWITCH
            IF ! cStart == ""
               AAdd( aNew, "  " + cStart + " " + StrTran( SubStr( cLine, 3 + 1 ), "\", "/" ) )
               IF ! cStart == "-"
                  cFile := SubStr( cLine, 3 + 1 )
                  IF ( tmp := At( " -> ", cFile ) ) > 0
                     cFile := SubStr( cFile, tmp + Len( " -> " ) )
                  ENDIF
                  AAdd( aFiles, cFile )
               ENDIF
            ENDIF
         ENDIF
      NEXT
      EXIT

   ENDSWITCH

   RETURN aNew

STATIC FUNCTION Shell()

   LOCAL cShell

#if defined( __PLATFORM__UNIX )
   cShell := GetEnv( "SHELL" )
#else
   cShell := GetEnv( "COMSPEC" )
#endif

   IF ! cShell == ""
#if defined( __PLATFORM__UNIX )
      cShell += " -c"
#else
      cShell += " /c"
#endif
   ENDIF

   RETURN cShell

STATIC FUNCTION CmdEscape( cCmd )

#if defined( __PLATFORM__UNIX )
   cCmd := '"' + cCmd + '"'
#endif

   RETURN cCmd

STATIC FUNCTION Changes( cVCS )

   LOCAL cStdOut

   SWITCH cVCS
   CASE "svn" ; hb_processRun( Shell() + " " + CmdEscape( "svn status -q" ),, @cStdOut ) ; EXIT
   /* FIXME: This will inconveniently (for us) return all files as changed, where
             automatic EOL conversion is going to be done by Git on commit. */
   CASE "git" ; hb_processRun( Shell() + " " + CmdEscape( "git status --porcelain" ),, @cStdOut ) ; EXIT
   OTHERWISE ; cStdOut := ""
   ENDSWITCH

   RETURN hb_ATokens( cStdOut, .T. )

#if 0
STATIC FUNCTION LaunchCommand( cCommand, cArg )

   IF cCommand == ""
      RETURN -1
   ENDIF

#if defined( __PLATFORM__WINDOWS )
   IF hb_osIsWinNT()
      cCommand := 'start "" "' + cCommand + '"'
   ELSE
      cCommand := "start " + cCommand
   ENDIF
#elif defined( __PLATFORM__OS2 )
   cCommand := 'start "" "' + cCommand + '"'
#endif

   RETURN hb_run( cCommand + " " + cArg )
#endif

/* ---- */

#define _HBROOT_  hb_PathNormalize( hb_DirSepToOS( hb_DirBase() + "../" ) )  /* must end with dirsep */

STATIC FUNCTION CheckFileList( xName, cLocalRoot, lRebase )

   LOCAL lPassed := .T.

   LOCAL aErr, s
   LOCAL file
   LOCAL tmp

   LOCAL lApplyFixes := "--fixup" $ cli_Options()

   hb_default( @lRebase, .T. )

   IF HB_ISSTRING( xName )
      xName := { xName }
   ENDIF

   IF Empty( xName ) .OR. HB_ISARRAY( xName )
      IF ! HB_ISARRAY( xName )
         IF GitDetect()
            xName := GitFileList()
            lRebase := .F.
         ELSE
            s := hb_cwd( _HBROOT_ )
            xName := my_DirScan( hb_osFileMask() )
            hb_cwd( s )
         ENDIF
         lApplyFixes := .F.  /* do not allow to mass fix all files */
      ENDIF
      IF "--fixup-case" $ cli_Options()
         tmp := ""
         FOR EACH file IN xName
            IF "|" + hb_FNameExt( file ) + "|" $ "|.c|.cpp|.h|.api|.ch|.hb|.po|.prg|.md|.txt|" .AND. ;
               FixFuncCaseFilter( file )
               tmp += file + " "
            ENDIF
         NEXT
         IF ! tmp == ""
            hb_run( hbshell_ProgName() + " -fixcase " + tmp )
         ENDIF
      ELSE
         FOR EACH file IN xName
            IF ! CheckFile( file, @aErr, lApplyFixes, hb_defaultValue( cLocalRoot, "" ), lRebase )
               lPassed := .F.
               FOR EACH s IN aErr
                  OutStd( file + ": " + s + hb_eol() )
               NEXT
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN lPassed

STATIC FUNCTION CheckFile( cName, /* @ */ aErr, lApplyFixes, cLocalRoot, lRebase )

   LOCAL cFile
   LOCAL tmp
   LOCAL cEOL

   LOCAL lProcess
   LOCAL lReBuild
   LOCAL lRemoveEndingWhitespace

   /* FIXME: Harbour repo specific */
   LOCAL aCanBeUpper := { ;
      "Makefile", ;
      "ChangeLog.*", ;
      "CONTRIBUTING.*", ;
      "DEPRECATED.*", ;
      "LICENSE.*", ;
      "README.*", ;
      "WARNING.*", ;
      "*/RELNOTES.*", ;
      "*/doc/*/*.txt", ;
      "*.po", ;
      "*.md" }

   LOCAL aCanHaveNoExtension := { ;
      ".*", ;
      "Makefile", ;
      "debian/*" }

   LOCAL aCanHaveTab := { ;
      "Makefile", ;
      "debian/rules", ;
      "*.mk", ;
      "*.go", ;
      "*.diff", ;
      "*.patch" }

   LOCAL aCanHaveSpaceAtEol := { ;
      "*.diff", ;
      "*.patch", ;
      "*.md" }

   LOCAL aCanHaveAnyEncoding := { ;
      "*.diff", ;
      "*.patch" }

   LOCAL aForcedCRLF := { ;
      "*.bat" }

   LOCAL aForcedLF := { ;
      "*.sh" }

   /* FIXME: Harbour repo specific */
   LOCAL aNoProc := { ;
      "contrib/hbhpdf/tests/files/*" }

   /* FIXME: Harbour repo specific */
   LOCAL aNoCopyrightOk := { ;
      "tests/*", ;
      "*/*/tests/*", ;
      "src/codepage/*", ;
      "src/lang/*" }

   LOCAL nLines

   /* TODO: extend as you go */
   /* FIXME: Harbour repo specific */
   LOCAL hDoNotProcess := { ;
      ".c" => { "3rd", "include", "dlmalloc", "hvm", "sha1", "sha2" }, ;
      ".h" => { "3rd", "include" } }

   hb_default( @lApplyFixes, .F. )

   aErr := {}

   cName := hb_DirSepToOS( cName )

   IF hb_vfExists( iif( lRebase, _HBROOT_, "" ) + cName ) .AND. ;
      ! FNameExc( cName, LoadGitignore( cLocalRoot + ".gitignore" ) )

      /* filename checks */

      IF ! hb_DirSepToOS( "/3rd/" ) $ cName
         IF hb_FNameExt( cName ) == "" .AND. ! FNameExc( cName, aCanHaveNoExtension )
            AAdd( aErr, "filename: missing extension" )
         ENDIF

         IF ! cName == Lower( cName ) .AND. ! FNameExc( cName, aCanBeUpper )
            AAdd( aErr, "filename: non-lowercase" )
         ENDIF
      ENDIF

      IF ! IsASCII7( cName )
         AAdd( aErr, "filename: non-ASCII-7" )
      ENDIF

      cFile := hb_MemoRead( iif( lRebase, _HBROOT_, "" ) + cName )

      IF ! IsBinary( cFile )

         IF hb_FileMatch( cName, "ChangeLog.txt" ) .AND. hb_BLen( cFile ) > 32768 .AND. ! lApplyFixes
            cFile := RTrimEOL( hb_BLeft( cFile, 16384 ) ) + LTrim( hb_BRight( cFile, 16384 ) )
         ENDIF

         lReBuild := .F.

         /* FIXME: Harbour repo specific */
         IF ! hb_DirSepToOS( "/3rd/" ) $ cName .OR. ;
            hb_FNameName( cName ) == "Makefile" .OR. ;
            hb_FNameExt( cName ) == ".hbc" .OR. ;
            hb_FNameExt( cName ) == ".hbp"

            /* text content checks */

            IF ! FNameExc( cName, aCanHaveTab ) .AND. e"\t" $ cFile
               AAdd( aErr, "content: has tab" )
            ENDIF

            IF hb_BLeft( cFile, hb_BLen( UTF8_BOM() ) ) == UTF8_BOM()
               AAdd( aErr, "content: has BOM" )
               IF lApplyFixes
                  cFile := hb_BSubStr( cFile, hb_BLen( UTF8_BOM() ) + 1 )
               ENDIF
            ENDIF

            IF hb_BRight( cFile, 1 ) == Chr( 26 )
               AAdd( aErr, "content: has legacy EOF char" )
               IF lApplyFixes
                  cFile := hb_StrShrink( cFile )
               ENDIF
            ENDIF

            cEOL := EOLDetect( cFile, @nLines )

            IF cEOL == ""
               AAdd( aErr, "content: has mixed EOL types" )
               IF lApplyFixes
                  lReBuild := .T.
               ENDIF
            ENDIF

            IF FNameExc( cName, aForcedCRLF ) .AND. ! cEOL == Chr( 13 ) + Chr( 10 )
               AAdd( aErr, "content: must use CRLF EOL for file type" )
               IF lApplyFixes
                  cFile := StrTran( StrTran( cFile, Chr( 13 ) ), Chr( 10 ), cEOL := Chr( 13 ) + Chr( 10 ) )
               ENDIF
            ENDIF

            IF FNameExc( cName, aForcedLF ) .AND. ! cEOL == Chr( 10 )
               AAdd( aErr, "content: must use LF EOL for file type" )
               IF lApplyFixes
                  cFile := StrTran( cFile, Chr( 13 ) )
                  cEOL := Chr( 10 )
               ENDIF
            ENDIF

            IF ! FNameExc( cName, aCanHaveSpaceAtEol ) .AND. EndingWhitespace( cFile )
               AAdd( aErr, "content: has ending whitespace" )
               IF lApplyFixes
                  lRemoveEndingWhitespace := .T.
                  lReBuild := .T.
               ENDIF
            ENDIF

            IF lReBuild
               cFile := RemoveEndingWhitespace( cFile, iif( cEOL == "", hb_eol(), cEOL ), lRemoveEndingWhitespace )
            ENDIF

            IF ! hb_BRight( cFile, Len( Chr( 10 ) ) ) == Chr( 10 )
               AAdd( aErr, "content: has no EOL at EOF" )
               IF lApplyFixes
                  cFile += iif( cEOL == "", hb_eol(), cEOL )
               ENDIF
            ENDIF

            IF Right( cFile, Len( Chr( 10 ) ) * 2 ) == Replicate( Chr( 10 ), 2 )
               AAdd( aErr, "content: has multiple EOL at EOF" )
               IF lApplyFixes
                  DO WHILE Right( cFile, Len( Chr( 10 ) ) * 2 ) == Replicate( Chr( 10 ), 2 )
                     cFile := hb_StrShrink( cFile, Len( Chr( 10 ) ) )
                  ENDDO
               ENDIF
            ELSEIF Right( cFile, Len( Chr( 13 ) + Chr( 10 ) ) * 2 ) == Replicate( Chr( 13 ) + Chr( 10 ), 2 )
               AAdd( aErr, "content: has multiple EOL at EOF" )
               IF lApplyFixes
                  DO WHILE Right( cFile, Len( Chr( 13 ) + Chr( 10 ) ) * 2 ) == Replicate( Chr( 13 ) + Chr( 10 ), 2 )
                     cFile := hb_StrShrink( cFile, Len( Chr( 13 ) + Chr( 10 ) ) )
                  ENDDO
               ENDIF
            ENDIF

            IF ! FNameExc( cName, aCanHaveAnyEncoding )
               tmp := -1
               IF ! IsASCII7( cFile, @tmp ) .AND. ! hb_StrIsUTF8( cFile )
                  AAdd( aErr, hb_StrFormat( "content: is non-UTF-8/ASCII-7: %1$d", tmp ) )
               ENDIF
            ENDIF

            IF "$" + "Id" $ cFile
               AAdd( aErr, "content: has " + "$" + "Id" )
            ENDIF

            /* FIXME: Harbour repo specific */
            IF "|" + hb_FNameExt( cName ) + "|" $ "|.c|.h|.api|.prg|.hb|.ch|" .AND. ;
               nLines > 20 .AND. ;
               ! FNameExc( cName, aNoCopyrightOk ) .AND. ;
               ! "public domain" $ Lower( cFile ) .AND. ;
               ! "copyright" $ Lower( cFile ) .AND. ;
               ! "license" $ Lower( cFile )
               AAdd( aErr, "content: source code missing copyright/license" )
            ENDIF

            IF "|" + hb_FNameExt( cName ) + "|" $ "|.c|.h|.api|"
               IF "//" $ StripCStrings( StripCComments( cFile ) )
                  AAdd( aErr, "content: C file with C++ comment" )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF lApplyFixes
         lProcess := .T.
         FOR EACH tmp IN hb_HGetDef( hDoNotProcess, hb_FNameExt( cName ), {} )
            IF tmp $ cName
               lProcess := .F.
               EXIT
            ENDIF
         NEXT
         IF lProcess .AND. ! FNameExc( cName, aNoProc )
            OutStd( cName + ": " + "content: processing" + hb_eol() )
            ProcFile( cName )
         ENDIF
      ENDIF
   ENDIF

   RETURN Empty( aErr )

STATIC FUNCTION IsBinary( cFile )
   RETURN Chr( 0 ) $ cFile .OR. ! Chr( 10 ) $ cFile

STATIC FUNCTION RTrimEOL( cFile )

   DO WHILE Right( cFile, 1 ) $ Chr( 13 ) + Chr( 10 )
      cFile := hb_StrShrink( cFile )
   ENDDO

   RETURN cFile

STATIC FUNCTION IsASCII7( cString, /* @ */ nChar )

   LOCAL tmp

   FOR EACH tmp IN cString
      nChar := hb_BCode( tmp )
      IF ( nChar < 32 .OR. nChar > 126 ) .AND. ;
         nChar != 10 .AND. nChar != 13 .AND. nChar != 9 .AND. nChar != 12
         RETURN .F.
      ENDIF
   NEXT

   RETURN .T.

STATIC FUNCTION EOLDetect( cFile, /* @ */ nLines )

   LOCAL nCR := 0
   LOCAL nLF := 0
   LOCAL tmp

   FOR EACH tmp IN cFile
      SWITCH tmp
      CASE Chr( 13 )
         ++nCR
         EXIT
      CASE Chr( 10 )
         ++nLF
         EXIT
      ENDSWITCH
   NEXT

   DO CASE
   CASE nCR > 0 .AND. nLF == 0
      nLines := nCR
      RETURN Chr( 13 )
   CASE nCR == 0 .AND. nLF > 0
      nLines := nLF
      RETURN Chr( 10 )
   CASE nCR == 0 .AND. nLF == 0
      nLines := 0
      RETURN "binary"
   CASE nCR == nLF
      nLines := nCR
      RETURN Chr( 13 ) + Chr( 10 )
   ENDCASE

   nLines := -1

   RETURN ""

STATIC FUNCTION EndingWhitespace( cFile )

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( cFile, .T. )
      IF Right( cLine, 1 ) == " "
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC FUNCTION RemoveEndingWhitespace( cFile, cEOL, lRTrim )

   LOCAL cResult := ""
   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( cFile, .T. )
      cResult += iif( lRTrim, RTrim( cLine ), cLine )
      IF ! cLine:__enumIsLast()
         cResult += cEOL
      ENDIF
   NEXT

   RETURN cResult

/* retains positions in file */
STATIC FUNCTION StripCStrings( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp

   DO WHILE ( tmp := hb_BAt( '"', cFile, nPos ) ) > 0
      /* FIXME: imprecise escaped char detection */
      IF ( ! hb_BSubStr( cFile, tmp - 1, 1 ) == "\" .OR. ;
         hb_BSubStr( cFile, tmp - 2, 2 ) == "\\" ) .AND. ;
         ! hb_BSubStr( cFile, tmp - 1, 1 ) + hb_BSubStr( cFile, tmp + 1, 1 ) == "''"
         AAdd( aHits, tmp )
      ENDIF
      nPos := tmp + 1
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   FOR tmp := 1 TO Len( aHits ) STEP 2
      cFile := hb_BLeft( cFile, aHits[ tmp ] ) + Replicate( " ", aHits[ tmp + 1 ] - aHits[ tmp ] - 1 ) + hb_BSubStr( cFile, aHits[ tmp + 1 ] )
   NEXT

   RETURN cFile

/* retains positions in file */
STATIC FUNCTION StripCComments( cFile )

   LOCAL nPos := 1
   LOCAL aHits := {}
   LOCAL tmp
   LOCAL tmp1
   LOCAL lStart := .T.

   /* bare bones */
   DO WHILE ( tmp := hb_BAt( iif( lStart, "/*", "*/" ), cFile, nPos ) ) > 0
      AAdd( aHits, tmp + iif( lStart, 0, 2 ) )
      nPos := tmp
      lStart := ! lStart
   ENDDO

   /* unbalanced */
   IF Len( aHits ) % 2 != 0
      AAdd( aHits, hb_BLen( cFile ) )
   ENDIF

   FOR tmp := 1 TO Len( aHits ) STEP 2
      FOR tmp1 := aHits[ tmp ] TO aHits[ tmp + 1 ]
         IF ! hb_BSubStr( cFile, tmp1, 1 ) $ Chr( 13 ) + Chr( 10 )
            hb_BPoke( @cFile, tmp1, hb_BCode( " " ) )
         ENDIF
      NEXT
   NEXT

   RETURN cFile

STATIC FUNCTION FNameExc( cName, aList )

   LOCAL tmp, tmp1

   FOR EACH tmp IN aList
      IF ! hb_LeftEq( tmp, "!" ) .AND. ;
         ( hb_FileMatch( cName, hb_DirSepToOS( tmp ) ) .OR. hb_FileMatch( hb_FNameNameExt( cName ), hb_DirSepToOS( tmp ) ) )
         FOR EACH tmp1 IN aList
            IF hb_LeftEq( tmp1, "!" ) .AND. hb_FileMatch( cName, hb_DirSepToOS( SubStr( tmp1, 2 ) ) )
               RETURN .F.
            ENDIF
         NEXT
         RETURN .T.
      ENDIF
   NEXT

   RETURN .F.

STATIC PROCEDURE ProcFile( cFileName )

   /* FIXME: bin/hb-uncrustify.cfg is in Harbour's bin dir, not in current project's */
   LOCAL hProc := { ;
      ".png" => { "advpng -z -4 %1$s", "optipng -o7 %1$s" }, ;
      ".jpg" => { "jpegoptim --strip-all %1$s" }, ;
      ".c"   => { hb_StrFormat( "uncrustify -c %1$s %%1$s", hb_DirSepToOS( _HBROOT_ + "bin/hb-uncrustify.cfg" ) ), @FixFuncCase() }, ;
      ".cpp" => ".c", ;
      ".h"   => ".c", ;
      ".api" => ".c", ;
      ".go"  => { "go fmt %1$s" }, ;
      ".txt" => { @FixFuncCase() }, ;
      ".md"  => ".txt", ;
      ".po"  => ".txt", ;
      ".prg" => { @FixFuncCase() /*, "hbformat %1$s" */ }, ;  /* NOTE: hbformat has bugs which make it unsuitable for unattended use */
      ".hb"  => ".prg", ;
      ".ch"  => ".prg" }

   LOCAL aProc := hb_FNameExt( cFileName )
   LOCAL xCmd

   DO WHILE HB_ISSTRING( aProc := hb_HGetDef( hProc, aProc, NIL ) )
   ENDDO

   IF HB_ISARRAY( aProc )
      FOR EACH xCmd IN aProc
         DO CASE
         CASE HB_ISSTRING( xCmd )
            hb_run( hb_StrFormat( hb_DirSepToOS( xCmd ), '"' + _HBROOT_ + cFileName + '"' ) )
         CASE HB_ISEVALITEM( xCmd )
            Eval( xCmd, cFileName )
         ENDCASE
      NEXT
   ENDIF

   RETURN

STATIC FUNCTION UTF8_BOM()
   RETURN ;
      hb_BChar( 0xEF ) + ;
      hb_BChar( 0xBB ) + ;
      hb_BChar( 0xBF )

STATIC FUNCTION LoadGitignore( cFileName )

   THREAD STATIC t_aIgnore

   LOCAL cLine

   IF t_aIgnore == NIL

      /* FIXME: Harbour repo specific */
      t_aIgnore := { ;
         "*/3rd/*", ;
         "!*/3rd/*/*.hbc", ;
         "!*/3rd/*/*.hbp", ;
         "!*/3rd/*/Makefile" }

      FOR EACH cLine IN hb_ATokens( hb_MemoRead( cFileName ), .T. )
         IF ! Empty( cLine ) .AND. ! hb_LeftEq( cLine, "#" )
            /* TODO: clean this */
            AAdd( t_aIgnore, ;
               iif( Left( cLine, 1 ) $ "?*/!", "", "*/" ) + ;
               cLine + ;
               iif( Right( cLine, 1 ) == "/", "*", ;
               iif( hb_FNameExt( cLine ) == "" .AND. ! Right( cLine, 2 ) == "*/", "/*", "" ) ) )
            IF ! ATail( t_aIgnore ) == cLine
               IF hb_LeftEq( ATail( t_aIgnore ), "*/" )
                  AAdd( t_aIgnore, SubStr( ATail( t_aIgnore ), 3 ) )
               ENDIF
               AAdd( t_aIgnore, cLine )
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN t_aIgnore

STATIC FUNCTION my_DirScan( cMask )
   RETURN my_DirScanWorker( cMask, {} )

STATIC FUNCTION my_DirScanWorker( cMask, aList )

   LOCAL file

   IF ! hb_vfExists( hb_FNameDir( cMask ) + ".git" )  /* skip Git submodules */
      FOR EACH file IN hb_vfDirectory( cMask, "D" )
         IF file[ F_NAME ] == "." .OR. file[ F_NAME ] == ".."
         ELSEIF "D" $ file[ F_ATTR ]
            my_DirScanWorker( hb_FNameDir( cMask ) + file[ F_NAME ] + hb_ps() + hb_FNameNameExt( cMask ), aList )
         ELSE
            AAdd( aList, hb_FNameDir( cMask ) + file[ F_NAME ] )
         ENDIF
      NEXT
   ENDIF

   RETURN aList

STATIC FUNCTION FixFuncCaseFilter( cFileName )

   /* FIXME: Harbour repo specific */
   STATIC sc_hFileExceptions := { ;
      "c_std.txt"   =>, ;  /* C level doc */
      "locks.txt"   =>, ;  /* C level doc */
      "pcode.txt"   =>, ;  /* C level doc */
      "tracing.txt" => }   /* C level doc */

   /* FIXME: Harbour repo specific */
   STATIC sc_aMaskExceptions := { ;
      "*/3rd/*" }  /* foreign code */

   RETURN ;
      ! hb_FNameExt( cFileName ) == "" .AND. ;
      ! hb_FNameNameExt( cFileName ) $ sc_hFileExceptions .AND. ;
      AScan( sc_aMaskExceptions, {| tmp | hb_FileMatch( cFileName, hb_DirSepToOS( tmp ) ) } ) == 0

STATIC PROCEDURE FixFuncCase( cFileName )

   IF FixFuncCaseFilter( cFileName )
      hb_run( hbshell_ProgName() + " -fixcase " + cFileName )
   ENDIF

   RETURN
