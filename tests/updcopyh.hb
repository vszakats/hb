#!/usr/bin/env hbmk2

/* Copyright 2013-2017 Viktor Szakats (vszakats.net/harbour) */

/* Update copyright headers */

#pragma -w3
#pragma -km+
#pragma -ko+

#include "directry.ch"

PROCEDURE Main()

   LOCAL aFile
   LOCAL nUpd := 0
   LOCAL cOri, cNew

   LOCAL cGPL1Old := ;
      "along with this program; if not, write to the Free Software" + hb_eol() + ;
      "Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit" + hb_eol() + ;
      "their website at https://www.gnu.org/)." + hb_eol()

   LOCAL cGPL1New := ;
      "along with this program; if not, write to the Free Software Foundation," + hb_eol() + ;
      "Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA." + hb_eol() + ;
      "(or visit their website at https://www.gnu.org/licenses/)." + hb_eol()

   LOCAL cGPL2Old := ;
      " * along with this program; if not, write to the Free Software" + hb_eol() + ;
      " * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit" + hb_eol() + ;
      " * their website at https://www.gnu.org/)." + hb_eol()

   LOCAL cGPL2New := ;
      " * along with this program; if not, write to the Free Software Foundation," + hb_eol() + ;
      " * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA." + hb_eol() + ;
      " * (or visit their website at https://www.gnu.org/licenses/)." + hb_eol()

   LOCAL cGPH1Old := ;
      "along with this software; see the file LICENSE.txt.  If not, write to" + hb_eol() + ;
      "the Free Software Foundation, Inc., 59 Temple Place, Suite 330," + hb_eol() + ;
      "Boston, MA 02111-1307 USA (or visit the website https://www.gnu.org/)." + hb_eol()

   LOCAL cGPH1Old := ;
      "along with this program; see the file LICENSE.txt.  If not, write to" + hb_eol() + ;
      "the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor," + hb_eol() + ;
      "Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/)." + hb_eol()

   LOCAL cGPH2Old := ;
      " * along with this software; see the file COPYING.txt.  If not, write to" + hb_eol() + ;
      " * the Free Software Foundation, Inc., 59 Temple Place, Suite 330," + hb_eol() + ;
      " * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/)." + hb_eol()

   LOCAL cGPH2New := ;
      " * along with this program; see the file LICENSE.txt.  If not, write to" + hb_eol() + ;
      " * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor," + hb_eol() + ;
      " * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/)." + hb_eol()

   LOCAL hReplace := { ;
      cGPL1Old => cGPL1New, ;
      cGPL2Old => cGPL2New, ;
      cGPH1Old => cGPH1New, ;
      cGPH2Old => cGPH2New }

   FOR EACH aFile IN hb_DirScan( ".", hb_osFileMask() )

      cNew := hb_StrReplace( cOri := hb_MemoRead( aFile[ F_NAME ] ), hReplace )

      IF ! cNew == cOri
         IF hb_MemoWrit( aFile[ F_NAME ], cNew )
            ++nUpd
         ELSE
            ? "Error saving:", aFile[ F_NAME ]
         ENDIF
      ENDIF
   NEXT

   ? "File(s) updated:", hb_ntos( nUpd )

   RETURN
