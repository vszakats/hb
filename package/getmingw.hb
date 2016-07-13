#!/usr/bin/env hbmk2

/* Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour) */

#include "hbver.ch"

PROCEDURE Main()

   LOCAL pkg, cFileName, tmp, cDst, cTarget, cCBase, cCOpt
   LOCAL cDirBase := hb_FNameDir( hbshell_ScriptName() )
   LOCAL cOldDir := hb_cwd( cDirBase )

   LOCAL aPkg := {}

   IF hb_vfExists( cDirBase + "harbour.exe" )

      cCBase := "https://downloads.sourceforge.net"; cCOpt := "-L"

      IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw.txt" )
         SWITCH hb_Version( HB_VERSION_BITWIDTH )
         CASE 32
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 32-bit hosted dual-target (multilib) mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.1.0/threads-posix/sjlj/i686-6.1.0-release-posix-sjlj-rt_v5-rev0.7z", ;
               "sum" => "f3ce910465f72b0a6180b7255f3f1c6ae10855454b10939a8608ddb9b1f2aa52", ;
               "fil" => "mingw" } )
            EXIT
         CASE 64
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 64-bit hosted dual-target (multilib) mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.1.0/threads-posix/sjlj/x86_64-6.1.0-release-posix-sjlj-rt_v5-rev0.7z", ;
               "sum" => "39edf7d7938c891b45b06e8f0879aef0b366a63f519725a8af3f5b6a651c2849", ;
               "fil" => "mingw" } )
            EXIT
         ENDSWITCH
      ELSE
         IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw32.txt" )
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 32-bit hosted 32-bit-target mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/6.1.0/threads-posix/dwarf/i686-6.1.0-release-posix-dwarf-rt_v5-rev0.7z", ;
               "sum" => "2b6fae2b7247e7d4ae4e821de1bc126457a74991e991da4c2d55df3595eebbb1", ;
               "fil" => "mingw32" } )
         ENDIF
         IF hb_vfExists( cDirBase + ".." + hb_ps() + "BUILD-mingw64.txt" )
            AAdd( aPkg, { ;
               "dsc" => "! Downloading 64-bit hosted 64-bit-target mingw...", ;
               "url" => cCBase + "/mingw-w64/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/6.1.0/threads-posix/seh/x86_64-6.1.0-release-posix-seh-rt_v5-rev0.7z", ;
               "sum" => "026d119a5fe5db15867cca24894447bf3f0a7b216226af7fb406bf33ed7eb855", ;
               "fil" => "mingw64" } )
         ENDIF
      ENDIF

      FOR EACH pkg IN aPkg

         IF HB_ISSTRING( pkg[ "url" ] )

            OutStd( pkg[ "dsc" ] + hb_eol() )

            hb_vfClose( hb_vfTempFile( @cFileName,,, ".7z" ) )

            IF hb_processRun( "curl" + ;
                  " -fsS" + ;
                  " -o " + FNameEscape( cFileName ) + ;
                  " " + cCOpt + ;
                  " " + pkg[ "url" ] ) == 0

               tmp := hb_MemoRead( cFileName )

               IF ! hb_LeftEqI( tmp, "<!DOCTYPE" ) .AND. ;
                  ! hb_LeftEqI( tmp, "<html" )

                  IF Empty( pkg[ "sum" ] ) .OR. hb_SHA256( tmp ) == pkg[ "sum" ]

                     IF Empty( pkg[ "sum" ] )
                        OutStd( "! Warning: Checksum not verified." + hb_eol() )
                     ELSE
                        OutStd( "! Checksum OK." + hb_eol() )
                     ENDIF

                     cDst := hb_PathNormalize( cTarget := cDirBase + ".." + hb_ps() + "comp" )

                     OutStd( hb_StrFormat( "! Unpacking to '%1$s'...", cDst ) + hb_eol() )

                     IF hb_processRun( "7za" + ;
                           " x -y" + ;
                           " " + FNameEscape( "-o" + cDst ) + ;
                           " " + FNameEscape( cFileName ),, @tmp, @tmp ) != 0

                        hb_vfCopyFile( cFileName, tmp := pkg[ "fil" ] + ".7z" )
                        OutStd( hb_StrFormat( "! Error: Unpacking. Please unpack '%1$s' manually to '%2$s'.", tmp, cTarget ) + hb_eol() )
                     ENDIF
                  ELSE
                     OutStd( "! Error: Checksum mismatch - corrupted download. Please retry." + hb_eol() )
                  ENDIF
               ELSE
                  OutStd( "! Error: Downloading MinGW. Please retry." + hb_eol() )
               ENDIF
            ELSE
               OutStd( "! Error: Downloading MinGW." + hb_eol() )
            ENDIF

            hb_vfErase( cFileName )
         ENDIF
      NEXT
   ENDIF

   IF Empty( aPkg )
      OutStd( ;
         "! Error: This script has to be run from a Harbour binary installation." + hb_eol() + ;
         "         Download from:" + hb_eol() + ;
         "            https://github.com/vszakats/harbour-core/releases/tag/v_HB_VF_DEF_" + hb_eol() )
      Inkey( 0 )
      ErrorLevel( 1 )
   ENDIF

   hb_cwd( cOldDir )

   RETURN

STATIC FUNCTION FNameEscape( cFileName )
   RETURN '"' + cFileName + '"'
