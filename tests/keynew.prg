/* Copyright 2016 Viktor Szakats (vszakats.net/harbour) */

#include "inkey.ch"

#define HB_INKEY_EXT_BIT            0x40000000
#define HB_INKEY_EXT_KEY            0x01000000
#define HB_INKEY_EXT_VALBITS        16

#translate TEST <a>, <e> => TestKey( <a>, <e> )

PROCEDURE Main()

   TEST hb_keyNew( HB_KX_UP )   , hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_UP )

   TEST hb_keyNew( HB_KX_UP )   , hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_UP )
   TEST hb_keyNew( HB_KX_DOWN ) , hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_DOWN )
   TEST hb_keyNew( HB_KX_ENTER ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_ENTER )
   TEST hb_keyNew( HB_KX_RIGHT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_RIGHT )

   TEST hb_keyNew( HB_KX_F9 , HB_KF_SHIFT + HB_KF_ALT ), 0x41050009
   TEST hb_keyNew( HB_KX_F10, HB_KF_SHIFT + HB_KF_ALT ), 0x4105000A

   TEST hb_keyNew( "C", HB_KF_CTRL ), 0x41020043
   TEST hb_keyNew( "V", HB_KF_CTRL ), 0x41020056

   TEST hb_keyNew( "W", HB_KF_ALT  ), _keyMake( "W", HB_KF_ALT  )
   TEST hb_keyNew( "A", HB_KF_CTRL ), _keyMake( "A", HB_KF_CTRL )
   TEST hb_keyNew( "B", HB_KF_CTRL ), _keyMake( "B", HB_KF_CTRL )
   TEST hb_keyNew( "C", HB_KF_CTRL ), _keyMake( "C", HB_KF_CTRL )
   TEST hb_keyNew( "V", HB_KF_CTRL ), _keyMake( "V", HB_KF_CTRL )
   TEST hb_keyNew( "X", HB_KF_CTRL ), _keyMake( "X", HB_KF_CTRL )

   TEST hb_keyNew( HB_KX_INS  , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_INS  , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_DOWN , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_DOWN , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_UP   , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_UP   , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_DEL  , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_DEL  , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_RIGHT, HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_RIGHT, hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_LEFT , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_LEFT , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_END  , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_END  , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )
   TEST hb_keyNew( HB_KX_HOME , HB_KF_SHIFT ), hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, HB_KX_HOME , hb_bitShift( HB_KF_SHIFT, HB_INKEY_EXT_VALBITS ) )

   RETURN

STATIC PROCEDURE TestKey( nActual, nExpected )

   ? nActual == nExpected, hb_NumToHex( nActual ), hb_NumToHex( nExpected )

   RETURN

STATIC FUNCTION _keyMake( cChar, nMod )
   RETURN hb_bitOr( HB_INKEY_EXT_BIT, HB_INKEY_EXT_KEY, ;
      hb_bitShift( hb_defaultValue( nMod, 0 ), HB_INKEY_EXT_VALBITS ), ;
      Asc( hb_defaultValue( cChar, "" ) ) )
