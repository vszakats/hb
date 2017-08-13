/*
 * Windows Service API test code
 *
 * Copyright 2010 Jose Luis Capel <jlcapel at hotmail . com>
 *
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
 *
 */

#if ! defined( __HBSCRIPT__HBSHELL )

#define _SERVICE_NAME  "Harbour_Test_Service"

PROCEDURE Main( cMode )

   SWITCH Lower( hb_defaultValue( cMode, "" )
   CASE "-install"

      IF win_serviceInstall( _SERVICE_NAME, ;
                             "Harbour Windows Test Service", ;
                             Chr( 34 ) + hb_ProgName() + Chr( 34 ) + " -service" )
         ? "Service has been successfully installed"
      ELSE
         ? "Error installing Service:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
      ENDIF
      EXIT

   CASE "-uninstall"

      IF win_serviceDelete( _SERVICE_NAME )
         ? "Service has been deleted"
      ELSE
         ? "Error deleting Service:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
      ENDIF
      EXIT

   CASE "-service"

      /* NOTE: Used when starting up as Service.
               Do not invoke the executable from the command-line with this option. */

      IF win_serviceStart( _SERVICE_NAME, @SrvMain() )
         ? "Service has started OK"
      ELSE
         ? "Service has had some problems:", hb_ntos( wapi_GetLastError() ), win_ErrorDesc()
      ENDIF
      EXIT

   OTHERWISE

      ? "This is a Windows Service app and cannot be directly run from the command-line."
      ?
      ? "Options:"
      ?
      ? "  -install    install as a Service"
      ? "  -uninstall  uninstall Service"
      ? "  -service    run as Service"

   ENDSWITCH

   RETURN

#include "fileio.ch"

STATIC PROCEDURE SrvMain( cParam1, cParam2 )

   LOCAL n := 0
   LOCAL hFile := hb_vfOpen( hb_FNameExtSet( hb_ProgName(), ".out" ), FO_CREAT + FO_TRUNC + FO_WRITE + FO_DENYNONE )
   LOCAL cParam

   hb_vfWrite( hFile, ;
      "Startup" + hb_eol() + ;
      "|" + hb_CmdLine() + "|" + hb_eol() + ;
      "|" + hb_defaultValue( cParam1, "" ) + "|" + hb_defaultValue( cParam2, "" ) + "|" + hb_eol() )

   FOR EACH cParam IN hb_AParams()
      hb_vfWrite( hFile, "Parameter " + hb_ntos( cParam:__enumIndex() ) + " >" + cParam + "<" + hb_eol() )
   NEXT

   DO WHILE win_serviceGetStatus() == WIN_SERVICE_RUNNING
      hb_vfWrite( hFile, "Work in progress " + hb_ntos( ++n ) + hb_eol() )
      hb_idleSleep( 0.5 )
   ENDDO

   hb_vfWrite( hFile, "Exiting..." + hb_eol() )
   hb_vfClose( hFile )

   win_serviceSetExitCode( 0 )
   win_serviceStop()

   RETURN

#else

PROCEDURE Main()

   ? "Cannot be used in script mode."

   RETURN

#endif
