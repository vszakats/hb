---
layout: default
title: "Contribs"
---
<div markdown="1" class="components">

# Harbour Contribs

Contribs are libraries or tools, which are independent from the language core,
but bundled with the main Harbour distribution. These components are part of
the Harbour source package.

Common to all contribs is that you can download them freely, but please note
that [licensing terms]({{ site.baseurl }}/terms) may vary for each contrib.

See respective authors inside the [source code](https://github.com/{{ site.repo_slug }}/tree/master/contrib).

## gtwvg

This library can be used for pure console applications instead of `gtwvt`.
But if used with GUI extensions, an appealing Windows screens without
sacrificing the Clipper syntax. Additionally `gtwvg` has a nice set of
`Wvt*()` classes which employ the common event loop.

You can create high performance dialogs with multiple TBrowser, Reads,
Bitmaps, Buttons, i.e. all GUI elements you can think of.

## hbct

This library provides functions compatible with the famous Clipper Tools for
CA-Clipper (`CT.LIB`).

## hbcurl

libcurl bindings for Harbour. libcurl is a free and easy-to-use client-side
URL transfer library, supporting SSL, HTTP GET/PUT/POST/forms, HTTP2, SCP,
FTPS, SFTP, FTP, LDAP, LDAPS, SMTP, SMTPS, IMAP, IMAPS, POP3, POP3S, TFTP,
TELNET, FILE/SMB, proxies, cookies, authentication (Basic, Digest, NTLM,
Negotiate, Kerberos), file transfer resume, http proxy tunneling and more.
<span class="readmore-md">[Read more](https://curl.haxx.se/libcurl/)</span>

## hbfbird

Firebird/Interbase RDBMS API bindings for Harbour, including classes and
functions to work with these RDBMS.

## hbfimage

FreeImage graphic library bindings for Harbour.
FreeImage is an Open Source library project for developers who would like to
support popular graphics image formats like PNG, BMP, JPEG, TIFF and others
as needed by today's multimedia applications.

FreeImage is easy to use, fast, multithreading safe and cross-platform
(works on Windows, Linux and Mac).

## hbgd

Thomas Boutell's GD 2.x library bindings for Harbour. GD Library is a powerful
graphic library, useful expecially under CGI environment. `hbgd` supports
almost all GD functions, plus a set of extra functions and classes that extend
and make it easier to work with this library.
<span class="readmore-md">[Read more](https://libgd.github.io/)</span>

## hbgt

Miscellaneous functions for manipulating strings.

## hbhpdf

Libharu bindings for Harbour. Haru is a free, cross platform, open-sourced
software library for generating PDF written in ANSI C.
<span class="readmore-md">[Read more](https://github.com/libharu/libharu)</span>

## hbmisc

Miscellaneous functions for manipulating strings, numbers, type conversions,
etc.

## hbmysql

MariaDB/MySQL client API bindings and helper classes for Harbour. Includes
a `dbf2mysql.prg` utility to convert `.dbf` files into MariaDB/MySQL tables.

## hbmzip

Minizip API bindings for Harbour, allowing to read and write `.zip` files.

## hbnf

A port to Harbour of the Nanforum Library for Clipper.

## hbodbc

ODBC library for Harbour. Includes bindings and helper classes to work with
various RDBMS via ODBC.

## hbpgsql

Harbour Low Level API for PostgreSQL RDBMS. Includes a `dbf2pg.prg` utility to
convert a `.dbf` file into a PostgreSQL table.

## hbsqlit3

SQLite3 bindings for Harbour. This library allow access an SQLite3
databases using Harbour.

## hbssl

OpenSSL bindings for Harbour. Supports SSL, SSL_CIPHER, SSL_CTX, RAND,
SSL_SESSION (and more) modules.

## hbtip

Class-oriented internet protocol library for Harbour. Supports HTTP, HTTPS,
SMTP, SMTPS, POP3, FTP and more.

## hbtpathy

Telepath(y) emulation library. Telepath(y) is the best serial communication
library for Nantucket/CA-Clipper and this library has a significant
amount of the functionality contained in Telepath(y) in this Harbour port.

## hbvpdf

This is a pure Clipper PDF Library what includes Harbour support and runs
without the need for external files.

## hbwin

This library has functions and classes to access the Windows API. Among
these features are:

* Registry access
* GUI printing
* OLE support
* Services
* DLL handling functions
* …and more

## rddads

`rddads` is an RDD for the Advantage Database Server, an xBase data server by
Extended Systems. With this library your Harbour application can access
a remote database server for a true client/server architecture, or it can use
the "local server" `adsloc32.dll` for stand-alone or even small network
installations.
<span class="readmore-md">[Read more](https://www.sap.com/pc/tech/database/software/advantage-database-server/index.html)</span>

## rddbm

Compatible with core RDDs of Harbour, with bitmap filters and other
extensions.

## rddsql

SQL MIX (Memory Index) Database Driver. This library provides access to
PostgreSQL, SQLite, MariaDB/MySQL, Firebird and ODBC servers.

## xhb

Provides a compatibility layer with the xHarbour fork.

</div>
