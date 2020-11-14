/* Copyright 2014-2016 Viktor Szakats (vsz.me/hb) */

#require "hbmac"

PROCEDURE Main()

   LOCAL cFile := hb_DirBase() + "test.txt"

   hb_MemoWrit( cFile, "test" )

   ? mac_MoveToTrash( cFile )

   RETURN
