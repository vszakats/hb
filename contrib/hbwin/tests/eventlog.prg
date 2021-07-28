/* Copyright 2010 Viktor Szakats */

#require "hbwin"

PROCEDURE Main()

   ? win_ReportEvent( , "Application", WIN_EVENTLOG_SUCCESS, 0, 0, "hello" )
   ? win_ReportEvent( , "Application", WIN_EVENTLOG_SUCCESS, 0, 0, { "hello", "world" } )

   RETURN
