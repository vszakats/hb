/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbicu"

PROCEDURE Main()

    LOCAL source
    LOCAL pattern := "yyyy-MM-dd'T'HH:mm:ssxxx" // ISO 8601
    LOCAL patternNoTZ := "yyyy-MM-dd'T'HH:mm:ss"
    LOCAL language := "it-IT"
    LOCAL timeZone := "Europe/Rome"
    LOCAL nUnixEpoch, aUnixEpoch
    LOCAL dateTime, status, formatted

    ? u_errorName( 0 /* U_ZERO_ERROR */ )
    ? hb_u_getVersion()

    ? "language=", language, "timeZone=", timeZone, "pattern=", pattern
    ? "***PARSE TEST***"
    ?

    source := "2016-03-01T09:10:00+01:00"    // NO DST

    ?
    ? "parsing", source, "NO DST"
    IF ( hb_icu_parse_datetime( source, pattern, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF

    source := "2016-06-05T19:15:00+02:00"    // DST

    ?
    ? "parsing", source
    IF ( hb_icu_parse_datetime( source, pattern, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime, "DST"
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF

    source := "2015-06-25T09:26:45+02:00"    // DST

    ?
    ? "parsing", source
    IF ( hb_icu_parse_datetime( source, pattern, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime, "DST"
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF

    source := "123456"    // ERR

    ?
    ? "parsing", source
    IF ( hb_icu_parse_datetime( source, pattern, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF

    source := "2016-03-01T09:10:00Z"    // NO DST

    ?
    ? "parsing", source, "NO DST, Z"
    IF ( hb_icu_parse_datetime( source, pattern, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF

    source := "2016-03-01T09:10:00"    // NO DST

    ?
    ? "parsing", source, "NO DST, NO TZ"
    IF ( hb_icu_parse_datetime( source, patternNoTZ, language, timeZone, @dateTime, @status ) )
        ? "Parsed", dateTime
    ELSE
        ? "Parse error", u_errorName( status )
    ENDIF


    ? "***FORMAT TEST***"
    ?
    dateTime := hb_DateTime()

    ?
    ? "formatting", dateTime
    IF ( hb_icu_format_datetime( dateTime, pattern, language, timeZone, @formatted, @status ) )
        ? "formatted", formatted
    ELSE
        ? "format error", u_errorName( status )
    ENDIF

    dateTime := hb_CToT( "2016-03-01 15:10:20", "yyyy-mm-dd", "hh:mm:ss" )

    ?
    ? "formatting", dateTime, "DST"
    IF ( hb_icu_format_datetime( dateTime, pattern, language, timeZone, @formatted, @status ) )
        ? "formatted", formatted
    ELSE
        ? "format error", u_errorName( status )
    ENDIF

    dateTime := hb_CToT( "2016-05-01 09:10:20", "yyyy-mm-dd", "hh:mm:ss" )

    ?
    ? "formatting", dateTime
    IF ( hb_icu_format_datetime( dateTime, pattern, language, timeZone, @formatted, @status ) )
        ? "formatted", formatted
    ELSE
        ? "format error", u_errorName( status )
    ENDIF

    dateTime := Date()

    ?
    ? "formatting", dateTime, "DATE"
    IF ( hb_icu_format_datetime( dateTime, pattern, language, timeZone, @formatted, @status ) )
        ? "formatted", formatted
    ELSE
        ? "format error", u_errorName( status )
    ENDIF

    dateTime := CToD( "01/02/2016" )

    ?
    ? "formatting", dateTime, "DATE DST"
    IF ( hb_icu_format_datetime( dateTime, pattern, language, timeZone, @formatted, @status ) )
        ? "formatted", formatted
    ELSE
        ? "format error", u_errorName( status )
    ENDIF


    ?
    ? "***UnixEpoch_To_DateTime***"
    ?

    aUnixEpoch = { 1440288000000, 1428710400000, 1440288000000,  1503446399000 }

    FOR EACH nUnixEpoch in aUnixEpoch
        IF UnixEpoch_To_DateTime( nUnixEpoch, timeZone, @dateTime, @status )
            ? "nUnixEpoch=", nUnixEpoch, "===>", dateTime
        ELSE
            ? "epoch error", u_errorName( status )
        ENDIF
    NEXT

   RETURN
