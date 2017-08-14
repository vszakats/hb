/*
 * xHarbour compatible disk type/status functions
 *
 * Copyright 2017 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

#define _HB_MOUNT_UNKNOWN      0
#define _HB_MOUNT_NO_ROOT_DIR  1
#define _HB_MOUNT_REMOVABLE    2
#define _HB_MOUNT_FIXED        3
#define _HB_MOUNT_REMOTE       4
#define _HB_MOUNT_CDROM        5
#define _HB_MOUNT_RAMDISK      6

static int s_mount_type( void )
{
   int iType;
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   UINT uiType;
   PHB_ITEM pRootName = hb_param( 1, HB_IT_STRING );

   LPCTSTR lpDrive;
   LPTSTR lpFree;

   if( pRootName )
   {
      char szRoot[ 4 ];
      const char * pszDrive;

      if( hb_itemGetCLen( pRootName ) > 0 &&
          hb_itemGetCLen( pRootName ) < 3 )
      {
         szRoot[ 0 ] = hb_itemGetCPtr( pRootName )[ 0 ];
         szRoot[ 1 ] = ':';
         szRoot[ 2 ] = '\\';
         szRoot[ 3 ] = '\0';
         pszDrive = szRoot;
      }
      else
         pszDrive = hb_itemGetCPtr( pRootName );

      lpDrive = HB_FSNAMECONV( pszDrive, &lpFree );
   }
   else
   {
      lpDrive = NULL;  /* Root of the current directory */
      lpFree = NULL;
   }

   hb_vmUnlock();
   uiType = GetDriveType( lpDrive );
   hb_vmLock();

   switch( uiType )
   {
      case DRIVE_NO_ROOT_DIR:
         iType = _HB_MOUNT_NO_ROOT_DIR;
         break;
      case DRIVE_REMOVABLE:
         iType = _HB_MOUNT_REMOVABLE;
         break;
      case DRIVE_FIXED:
         iType = _HB_MOUNT_FIXED;
         break;
      case DRIVE_REMOTE:
         iType = _HB_MOUNT_REMOTE;
         break;
      case DRIVE_CDROM:
         iType = _HB_MOUNT_CDROM;
         break;
      case DRIVE_RAMDISK:
         iType = _HB_MOUNT_RAMDISK;
         break;
      default:
         iType = _HB_MOUNT_UNKNOWN;
   }

   if( lpFree )
      hb_xfree( lpFree );
#else
   iType = _HB_MOUNT_UNKNOWN;
#endif
   return iType;
}

HB_FUNC( HB_ISREMOTEDISK )
{
   hb_retl( s_mount_type() == _HB_MOUNT_REMOTE );
}

HB_FUNC( HB_ISRAMDISK )
{
   hb_retl( s_mount_type() == _HB_MOUNT_RAMDISK );
}

HB_FUNC( HB_ISHARDDISK )
{
   hb_retl( s_mount_type() == _HB_MOUNT_FIXED );
}

HB_FUNC( HB_ISCDROM )
{
   hb_retl( s_mount_type() == _HB_MOUNT_CDROM );
}

HB_FUNC( HB_ISDISKETTE )
{
   hb_retl( s_mount_type() == _HB_MOUNT_REMOVABLE );
}
