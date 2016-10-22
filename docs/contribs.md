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

## gtqtc

Multi-platform QT based GUI console.

## gtwvg (Windows-only)

This library can be used for pure console applications instead of `GTWVT`.
But if used with GUI extensions, an appealing Windows screens without
sacrificing the Clipper syntax. Additionally `GTWVG` has a nice set of
`Wvt*()` classes which employ the common event loop.

You can create high performance dialogs with multiple TBrowser, Reads,
Bitmaps, Buttons, i.e. all GUI elements you can think of.

## gtwvw (Windows-only)

A `GTWVG` alternative with multi-window support.

## hbamf

AMF file format handling

## hbblink

Blinker compatibility

## hbbz2

bz2 bindings (compression)

## hbcairo

Cairo bindings (imaging)

## hbcomm

HBCOMM (xhb.com, MiniGUI) compatibility (serial communication)

{% if site.fork %}
## hbcrypto

Crypto functions

{% endif %}
## hbct

This library provides functions compatible with the famous Clipper Tools for
CA-Clipper (`CT.LIB`).

## hbcups (*NIX-only)

Cups bindings (printing)

## hbcurl

libcurl bindings for Harbour. libcurl is a free and easy-to-use client-side
URL transfer library, supporting SSL, HTTP GET/PUT/POST/forms, HTTP2, SCP,
FTPS, SFTP, FTP, LDAP, LDAPS, SMTP, SMTPS, IMAP, IMAPS, POP3, POP3S, TFTP,
TELNET, FILE/SMB, proxies, cookies, authentication (Basic, Digest, NTLM,
Negotiate, Kerberos), file transfer resume, http proxy tunneling and more.
<span class="readmore-md">[Read more](https://curl.haxx.se/libcurl/)</span>

## hbexpat

libexpat bindings (XML parser)

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

## hbformat

Harbour source code formatter

## hbfoxpro

FoxPro compatibility

## hbfship

FlagShip compatibility

## hbgd

Thomas Boutell's GD 2.x library bindings for Harbour. GD Library is a powerful
graphic library, useful expecially under CGI environment. `hbgd` supports
almost all GD functions, plus a set of extra functions and classes that extend
and make it easier to work with this library.
<span class="readmore-md">[Read more](https://libgd.github.io/)</span>

## hbgt

Miscellaneous functions for manipulating strings.

## hbgs

Ghostscript bindings (imaging)

## hbgt

GT library (string functions)

## hbhpdf

Libharu bindings for Harbour. Haru is a free, cross platform, open-source
library for generating PDF, written in ANSI C.
<span class="readmore-md">[Read more](https://github.com/libharu/libharu)</span>

## hbhttpd

HTTP server

## hbicu

ICU bindings/puller

## hblzf

LZF bindings (compression)

## hbmac (Mac-only)

Apple Mac specific functions

## hbmagic (*NIX-only)

libmagic bindings (file identification)

## hbmisc

Miscellaneous functions for manipulating strings, numbers, type conversions,
etc.

## hbmlzo

minilzo bindings (compression)

## hbmxml

minixml bindings (XML parser/generator)

## hbmysql

MariaDB/MySQL client API bindings and helper classes for Harbour. Includes
a `dbf2mysql.prg` utility to convert `.dbf` files into MariaDB/MySQL tables.

## hbmzip

Minizip API bindings for Harbour, allowing to read and write `.zip` files.

## hbnf

A port of the Nanforum Library for Clipper.

## hbodbc

ODBC library for Harbour. Includes bindings and helper classes to work with
various RDBMS via ODBC.

## hboslib

OSLib (Dave Pearson's) compatibility

## hbpgsql

Harbour Low Level API for PostgreSQL RDBMS. Includes a `dbf2pg.prg` utility to
convert a `.dbf` file into a PostgreSQL table.

## hbsqlit3

SQLite3 bindings for Harbour. This library allow access an SQLite3
databases using Harbour.

## hbsms

SMS handling functions

## hbssl

OpenSSL bindings for Harbour. Supports SSL, SSL_CIPHER, SSL_CTX, RAND,
SSL_SESSION (and more) modules.

## hbtest

Regression test framework

## hbtip

Class-oriented internet protocol library for Harbour. Supports HTTP, HTTPS,
SMTP, SMTPS, POP3, FTP and more.

## hbtpathy

Telepath(y) emulation library. Telepath(y) is the best serial communication
library for Nantucket/CA-Clipper and this library has a significant
amount of the functionality contained in Telepath(y) in this Harbour port.

## hbunix (*NIX-only)

Unix specific functions

## hbwin (Windows-only)

This library has functions and classes to access the Windows API. Among
these features are:

* Registry access
* GUI printing
* OLE support
* Services
* DLL handling functions
* …and more

## hbxdiff

libxdiff bindings (diffing)

## hbxpp

Xbase++ compatibility

## hbzebra

Barcode creating functions

## hbziparc

`HBZIPARC` compatibility (zip compression)

## xhb

xHarbour compatibility

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
PostgreSQL (via `sddpg`), SQLite3 (via `sddsqlt3`), Oracle (via `sddoci`),
MariaDB/MySQL (via `sddmy`), Firebird (via `sddfb`) and ODBC servers
(via `sddodbc`).

## xhb

Provides a compatibility layer with the xHarbour fork.

## hbbz2io

I/O driver for BZIP2 compressed streams

## hbcomio

I/O driver for serial port streams

## hbgzio

I/O driver for GZIP compressed streams

## hbmemio

Memory I/O driver

## hbnetio

Network I/O driver

## hbpipeio

I/O driver for pipe streams

## hbtcpio

I/O driver for TCP streams

</div>
