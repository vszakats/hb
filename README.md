# Welcome to Harbour
[![License](https://img.shields.io/badge/license-GPLv2%20%2B%20Library%20Exception-blue.svg)](LICENSE.txt "License")
[![Download](https://img.shields.io/badge/download-snapshot_3.4.0dev-ff4500.svg)](https://github.com/vszakats/harbour-core/releases "Download snapshot release")
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![PayPal Donate](https://img.shields.io/badge/PayPal-Donate_Now-ff4500.svg?colorA=00457c)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BPSZQYKXMQJYG "Donate Now")
<br />
[![Build Status](https://api.travis-ci.org/vszakats/harbour-core.svg?branch=master)](https://travis-ci.org/vszakats/harbour-core)
[![Build Status](https://ci.appveyor.com/api/projects/status/1kx6w3y6qasymah3/branch/master?svg=true)](https://ci.appveyor.com/project/vsz/harbour-core/branch/master)
[![Coverity Status](https://scan.coverity.com/projects/3208/badge.svg)](https://scan.coverity.com/projects/3208)
[![Average time to resolve an Issue](https://isitmaintained.com/badge/resolution/vszakats/harbour-core.svg)](https://isitmaintained.com/project/vszakats/harbour-core "Average time to resolve an Issue")
[![Percentage of Issues still open](https://isitmaintained.com/badge/open/vszakats/harbour-core.svg)](https://isitmaintained.com/project/vszakats/harbour-core "Percentage of Issues still open")

Harbour is the open/free software implementation of a cross-platform,
multi-threading, object-oriented, scriptable programming language, backwards
compatible with xBase languages. Harbour consists of a compiler and runtime
libraries with multiple UI, database and I/O backends, its own build system
and a collection of libraries and bindings for popular APIs.

# Table of Content

1. [How to Donate](#how-to-donate)
2. [How to Get](#how-to-get)
3. [How to Build](#how-to-build)
4. [How to Do a Partial Build](#how-to-do-a-partial-build)
5. [How to Create Packages for Distribution](#how-to-create-packages-for-distribution)
6. [How to Enable Optional Components](#how-to-enable-optional-components)
7. [Build Options](#build-options)
8. [Build Examples](#build-examples)
9. [Build Your Own Harbour App](#build-your-own-harbour-app)
10. [Debugging Options](#debugging-options)
11. [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)
12. [Platform Matrix](#platform-matrix)
13. [External Links](#external-links)
14. [Harbour Links](#harbour-links)
15. [Guarantees and Liability](#guarantees-and-liability)

---

# How to Donate

  You can donate to fund further maintenance of this fork:

  [![PayPal](https://www.paypalobjects.com/webstatic/i/logo/rebrand/ppcom.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=BPSZQYKXMQJYG)

  Thanks to all who did!

# Maintainer contacts

  * [Twitter](https://twitter.com/vszakats)
  * [GitHub](https://github.com/vszakats)
  * [PGP](https://keybase.io/vszakats) [key](https://keybase.io/vszakats/key.asc)
  * [Homepage](https://vszakats.net/)

# How to Get

## Stable versions (non-fork/mainline)

### Harbour stable binary download

<https://github.com/vszakats/harbour-core/releases/tag/v3.0.0>

> NOTE: It is identical to the mainline stable release, but
> not supported or recommended by this fork.

### Harbour stable source download

<https://github.com/vszakats/harbour-core/archive/v3.0.0.zip>

## Unstable versions

> :bulb: TIP:
> [For](https://groups.google.com/forum/#!msg/harbour-users/2fwUzdKwpKA/32nI4WhZLfYJ)
> [users](https://groups.google.com/forum/#!msg/harbour-users/Ro99f8S6my0/KvfjhCx_jE4J)
> [contributing](.github/CONTRIBUTING.md) to development, it's recommended to follow [commits](https://github.com/vszakats/harbour-core/commits/master) and reading
> [ChangeLog.txt](ChangeLog.txt?raw=true).

### Harbour live source repository

You will need Git version control software installed on your system
and to issue this command:

    git clone https://github.com/vszakats/harbour-core.git harbour-core

You can get subsequent updates using this command:

    git pull

### Harbour unstable sources

Download source archive from any of these URLs and unpack:

* <https://github.com/vszakats/harbour-core/archive/master.zip>
* <https://github.com/vszakats/harbour-core/archive/master.tar.gz>

### Harbour unstable binaries (updated after each commit)

#### Windows (mingw, 64-bit hosted, 32/64-bit targets, 7-zip archive)

* <https://github.com/vszakats/harbour-core/releases/download/v3.4.0dev/harbour-snapshot-win.7z>

#### Mac (using Homebrew :beer:)

    brew install https://raw.githubusercontent.com/vszakats/harbour-core/master/package/harbour.rb --HEAD

### Follow commits using:

* [Web browser](https://github.com/vszakats/harbour-core/commits/master)
* [RSS feed](https://github.com/vszakats/harbour-core/commits/master.atom)
* Any of the Git/GitHub client mobile/desktop apps

# How to Build

For all platforms you will need:

* Supported ANSI C compiler
* GNU Make (3.81 recommended, minimum 3.79 required, see also platform details)
* Harbour sources (2.0.0 or upper)

## on Windows hosts (possible cross-build targets: Windows CE, MS-DOS, OS/2, Linux)

Platform specific prerequisites:

1. Windows 7 or upper system is recommended to *build* Harbour. (64-bit
   edition is also recommended to make things simpler)
2. Make sure to have your C compiler of choice installed in `PATH`. Refer to
   your C compiler installation and setup instructions for details. Make sure
   no tools in your `PATH` belonging to other C compilers are interfering with
   your setup. Also avoid to keep multiple copies of the same compiler, or
   different versions of the same compiler in `PATH` at the same time. For the
   list of supported compilers,
   look up [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers).
3. GNU Make 3.81 or upper is required. A copy of this tool is included in
   the source package, so you don't have to do anything.
   If you want to get it separately, you can find it [here](#external-links).
   Unpack it to your `PATH` or Harbour source root directory, and run it as
   `mingw32-make`.

To build:

    > win-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see `Hello, world!` on screen.

## on Windows hosts with POSIX shells (MSYS2/Cygwin)

> Though you can use these alternative shells to build Harbour on Windows,
> it's recommended to use the native one.

To build:

    > sh -c make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see `Hello, world!` on screen.

## on MS-DOS hosts (possible cross-build targets: Windows, OS/2, Linux)

Make sure to have your C compiler of choice installed in `PATH`.

To prepare:

    1. Get `curl` tool.
    2. Execute shell script `config/dl-tools.sh` to download required build tools.

To build:

    > dos-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see `Hello, world!` on screen.

## on OS/2 hosts (possible cross-build targets: MS-DOS, OS/2, Linux)

To prepare:

    1. Get `curl` tool.
    2. Execute shell script `config/dl-tools.sh` to download required build tools.

To build:

    > os2-make

To test it, type:

    > cd tests
    > ..\bin\<plat>\<comp>\hbmk2 hello.prg
    > hello

You should see `Hello, world!` on screen.

## on Linux hosts (possible cross-build targets: Windows, Windows CE, MS-DOS, OS/2)

To build:

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

## on Darwin (Mac) hosts (possible cross-build targets: Windows, Windows CE, MS-DOS)

Platform specific prerequisite:
   Xcode or Command Line Tools for Xcode installed

To build:

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

> You can override default (host) architecture by adding
> values below to `HB_USER_CFLAGS`, `HB_USER_LDFLAGS` envvars,
> you can use multiple values:<br />
> <br />
> Intel 32-bit: `-arch i386`<br />
> Intel 64-bit: `-arch x86_64`<br />

## on FreeBSD hosts

Platform specific prerequisites:

1. You will need to have the developer tools installed.
2. Then you will need to install gmake and optionally bison.
   If you installed the ports collection, then all you need to do to install
   bison and gmake is to run the following commands, which may require that
   you run su root first to get the correct permissions:

        $ cd /usr/ports/devel/gmake
        $ make
        $ make install
        $ make clean
        $ cd /usr/ports/devel/bison
        $ make
        $ make install
        $ make clean

To build:

    $ gmake

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

## on Minix hosts

Install GNU make from the Minix pkgsrc repository; for details see [here](http://wiki.minix3.org/en/UsersGuide/InstallingBinaryPackages).

Optionally, GCC may also be installed if you wish to use that instead
of Clang, the Minix system compiler.

## on other \*nix hosts (possible cross-build targets: Windows, Windows CE, MS-DOS)

To build:

    $ gmake [HB_PLATFORM=<...>]

Or

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

> For sunpro on Solaris:<br />
> If you have any GNU binutils stuff installed, do make sure `/usr/ccs/bin`
> (the location of the native Sun C compilation system tools) come *before*
> the GNU binutils components in your `$PATH`.


# How to Do a Partial Build

If you want to build only a specific part of Harbour, like one core library
or all core libraries, or all contrib packages, you have to do everything
the same way as for a full build, the only difference is that you first
have to go into the specific source directory you'd like to build. When
starting GNU Make, all components under that directory will be built:

    cd src/rtl
    <make> [clean]

If you want to rebuild one specific contrib package, use this:

    cd contrib
    hbmk2 make.hb <name> [clean] [custom hbmk2 options]


# How to Create Packages for Distribution

## Source .tgz on \*nix

    $ package/mpkg_src.sh

## Binary .tgz on \*nix

    $ export HB_BUILD_PKG=yes
    $ make clean install

## Binary .deb on Linux

    $ fakeroot debian/rules binary

## Binary .rpm on Linux

    $ package/mpkg_rpm.sh

You can fine-tune the build with these options:

    --with static      - link all binaries with static libs
    --with localzlib   - build local copy of zlib library
    --with localpcre2  - build local copy of pcre2 library
    --with localpcre   - build local copy of pcre library
    --without x11      - do not build components dependent on x11 (gtxwc)
    --without curses   - do not build components dependent on curses (gtcrs)
    --without slang    - do not build components dependent on slang (gtsln)
    --without gpllib   - do not build components dependent on GPL 3rd party code
    --without gpm      - build components without gpm support (gttrm, gtsln, gtcrs)

## Binary .rpm on Linux (cross-builds)

### for Windows:

    $ package/mpkg_rpm_win.sh

### for Windows CE:

    $ package/mpkg_rpm_wce.sh

## Binary .7z archive on Windows for all targets (except Linux)

    $ set HB_DIR_7Z=C:\7-zip\
    $ set HB_BUILD_PKG=yes

Then run build as usual with `clean install` options.
See: [How to Build](#how-to-build)


# How to Enable Optional Components

Certain Harbour parts &ndash; typically contrib packages &ndash; depend on
3rd party components. To make these Harbour parts built, you need to tell
Harbour where to find the headers for these 3rd party components.

On \*nix systems most of these 3rd party components will automatically
be used if installed on well-known standard system locations.

You only need to use manual setup if the dependency isn't available on your
platform on a system location, or you wish to use a non-standard location.
Typically, you need to do this on non-\*nix (Windows, MS-DOS, OS/2) systems
for all packages and for a few packages on \*nix which are not available
through official package managers (f.e. ADS Client).

Note that Harbour is tuned to use 3rd party **binary** packages in their
default, unmodified &ndash; "vanilla" &ndash; install layout created by their
official/mainstream install kits. If you manually move, rename, delete, add
files under the 3rd party packages' root directory, or use a source package,
the default Harbour build process (especially Windows implib generation)
might not work as expected.

You can set these environment variables before starting the build. Make sure
to adjust them to your own directories:

    HB_WITH_CURSES= (on *nix systems and DJGPP, auto-detected on both)
    HB_WITH_GPM= (on Linux only)
    HB_WITH_PCRE2=C:\pcre2
    HB_WITH_PCRE=C:\pcre (defaults to locally hosted version if not found)
    HB_WITH_PNG=C:\libpng (defaults to locally hosted version if not found)
    HB_WITH_SLANG= (on *nix systems)
    HB_WITH_WATT= (on MS-DOS systems)
    HB_WITH_X11= (on *nix systems)
    HB_WITH_ZLIB=C:\zlib (defaults to locally hosted version if not found)

To explicitly disable any given components, use the value `no`. This may be
useful to avoid auto-detection of installed packages on \*nix systems. You
may also use the value `local` to force using the locally hosted copy
(inside Harbour source repository) of these packages, where applicable.
`nolocal` will explicitly disable using locally hosted copy.

See contrib-specific dependencies and build notes in the projects' `.hbp`
file and find occasional link notes inside their `.hbc` files.


> NOTES:
>
>    * you need to use path format native to your shell/OS
>    * don't put directory names inside double quotes
>    * use absolute paths

## Darwin (Mac)

1. Install [Homebrew](http://brew.sh/) :beer:
2. Install packages:

        $ brew install valgrind pcre pcre2 s-lang upx uncrustify ack optipng jpegoptim eg
        $ brew tap caskroom/cask

3. Install [X11](https://www.xquartz.org/) (optional, for `gtxwc`)

        $ brew cask install xquartz


## Linux (.deb based distros: Debian, *buntu)

You will need these base packages to build/package/test/use Harbour:

      bash git gcc binutils fakeroot debhelper valgrind upx uncrustify p7zip-full

You will need these packages to compile optional core Harbour features:

      for gtcrs terminal lib:    libncurses-dev
      for gtsln terminal lib:    libslang2-dev OR libslang1-dev
      for gtxwc terminal lib:    libx11-dev
      for console mouse support: libgpm-dev OR libgpmg1-dev

Optional, to override locally hosted sources:

      for zlib support:          zlib1g-dev
      for pcre (regex) support:  libpcre3-dev

## Linux (.rpm based distros: openSUSE, Fedora, CentOS, Mandriva)

You will need these base packages to build/package/test/use Harbour:

      bash git gcc make glibc-devel rpm valgrind upx uncrustify p7zip

You will need these packages to compile optional core Harbour features:

      for gtcrs terminal lib:    ncurses-devel ncurses
      for gtsln terminal lib:    slang-devel slang
      for gtxwc terminal lib:    xorg-x11-devel OR XFree86-devel
      for console mouse support: gpm-devel OR gpm

## pacman based systems (Windows/MSYS2, Arch Linux)

For Windows/MSYS2 environment:

      git base-devel msys2-devel mingw-w64-{x86_64,i686}-toolchain upx uncrustify p7zip

Packages for optional core Harbour features:

      for gtcrs terminal lib:    ncurses
      for gtsln terminal lib:    slang
      for gtxwc terminal lib:    libx11
      for console mouse support: gpm

> NOTES:
>
>   * See [this](https://distrowatch.com/dwres.php?resource=package-management)
>       on package management in various distros.
>   * On openSUSE, if you want to build 32-bit Harbour on a 64-bit host,
>       install above packages with `-32bit` suffix, f.e. `slang-devel-32bit`

## OpenSolaris

    $ pkg install SUNWgit SUNWgcc SUNWgmake

## FreeBSD

If you want to use the `gtsln` library instead of `gtstd` or `gtcrs`, then you
also need to install `libslang`. If you installed the ports collection, then
all you need to do to install `libslang` is to run the following commands,
which may require that you run `su` first to get the correct permissions:

    $ cd /usr/ports/devel/libslang
    $ make
    $ make install
    $ make clean


# Build Options

You can fine-tune Harbour builds with below listed environment variables.
You can add most of these via the GNU Make command-line also, using
`make VARNAME=value` syntax. All of these settings are optional and all
settings are case-sensitive.

## General

   - `HB_INSTALL_PREFIX`

     Target root directory to install Harbour files.
     On \*nix systems the default is set to `/usr/local/`
     or `$(PREFIX)` if specified, and
     `/usr/local/harbour-<arch>-<comp>` for cross-builds.
     It's always set to `./pkg/<arch>/<comp>` when
     `HB_BUILD_PKG` is set to `yes`. On non-\*nix systems,
     you must set it to a valid directory when using
     `install`. Use absolute paths only.
     You have to use path format native to your shell.
     F.e. to specify `C:\dir` on Windows, with Cygwin
     you should use `/cygdrive/c/dir`, with MSYS `/c/dir`.

   - `HB_USER_PRGFLAGS`        User Harbour compiler options
   - `HB_USER_CFLAGS`          User C compiler options
   - `HB_USER_RESFLAGS`        User resource compiler options (on win, wce, os2)
   - `HB_USER_LDFLAGS`         User linker options for executables
   - `HB_USER_AFLAGS`          User linker options for libraries
   - `HB_USER_DFLAGS`          User linker options for dynamic libraries

   Set these only if auto-detection doesn't suit your purpose:

   - `HB_PLATFORM`             Override platform auto-detection
   - `HB_COMPILER`             Override C compiler auto-detection

     See this for possible values:
     [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)
     See also: `HB_CC*` settings.

## Special

   - `HB_BUILD_DYN=no`

     Create Harbour dynamic libraries. Default: `yes`

   - `HB_BUILD_CONTRIB_DYN=yes`

     Create contrib dynamic libraries. Default: `no`,
     except Windows platform, where it's `yes`.

   - `HB_BUILD_PKG=yes`

     Create release package. Default: `no`
     Requires `clean install` in root source dir.

   - `HB_BUILD_SHARED=yes`

     Create Harbour executables in shared mode.
     Default: `yes` when `HB_INSTALL_PREFIX` points
     to a \*nix system location, otherwise `no`.

   - `HB_BUILD_DEBUG=yes`

     Create debug build. Default: `no`

   - `HB_BUILD_STRIP=[all|bin|lib|no]`

     Strip symbols and debug information from binaries.
     Default: `no`

   - `HB_BUILD_OPTIM=no`

     Enable C compiler optimizations. Default: `yes`

   - `HB_BUILD_MODE=[cpp|c]`

     Change default build mode to C++ or C.
     Default: `c`, except for msvc* compilers, where it's `cpp`.

   - `HB_BUILD_PARTS=[all|compiler|lib]`

     Build only specific part of Harbour.

   - `HB_BUILD_NOGPLLIB=yes`

     Disable components dependent on GPL 3rd party code, to allow using
     Harbour for nonfree/proprietary projects. Default: `no`

   - `HB_BUILD_3RDEXT=no`

     Enable auto-detection of 3rd party components on default system
     locations. Default: `yes`

   - `HB_BUILD_CONTRIBS=no [<l>]`

     Do not build any, or space separated `<l>` list of, contrib packages.
     Please note it will not prevent building packages which are dependencies
     of other &ndash; enabled &ndash; packages.

   - `HB_BUILD_CONTRIBS=[<l>]`

     Build space separated `<l>` list of contrib libraries.
     Build all if left empty (default).

   - `HB_BUILD_ADDONS=<l>`

     Build space separated <l> list of additional `.hbp`
     projects.

   - `HB_BUILD_NAME=[<name>]`

     Create named build. This allows keeping multiple builds in parallel for
     any given platform/compiler. F.e. debug / release.

     > In current implementation it's appended to compiler directory name, so
     > all filesystem/platform name rules and limits apply. (Back)slashes will
     > be stripped from the name though.

   - `HB_USER_LIBS=[<list>]`

     Add space separated `<list>` of libs to link process.
     Lib names should be without extension and path.
     You only need this in special cases, like CodeGuard build with win/bcc.

   - `HB_INSTALL_IMPLIB=no`

     Copy import libraries created for external .dll dependencies to the
     library install directory in `install` build phase. Default: `yes`<br />
     For Windows and OS/2 targets only. Please note that this feature doesn't
     work with all possible binary distributions of 3rd party packages.
     We test only the official/mainstream ones. Also note that the generated
     implibs will require .dlls compatible with the ones used at build time.

   - `HB_INSTALL_3RDDYN=yes`

     Copy dynamic libraries of external .dll dependencies to the dynamic
     library directory in `install` build phase. Default: `no`

   - `HB_SRC_ROOTPATH=<dir>`

     When using GNU Make older than 3.81, you shall set the root directory
     of Harbour source tree as an absolute path. If not set, some build
     functionality may fail, like detection of 3rd party packages with
     locally hosted sources.
     With newer make versions, this variable is ignored.

   - `HB_REBUILD_EXTERN=yes`

     Rebuild extern headers. It is meant for developers doing Harbour code
     modifications and releases. Default: `no`

   - `HB_REBUILD_PARSER=yes`

     Rebuild language parser sources. You only need this if your are Harbour
     core developer modifying grammar rules (.y). Requires GNU Bison 1.28 or
     upper in `PATH`. Default: `no`

   - `HB_CCPATH=[<dir>/]`

     Used with non-\*nix gcc family compilers (and sunpro) to specify path
     to compiler/linker/archive tool to help them run from \*nix hosts as
     cross-build tools. Ending slash must be added.

   - `HB_CCPREFIX=[<prefix>]`

     Used with gcc compiler family to specify compiler/linker/archive tool
     name prefix.

   - `HB_CCSUFFIX=[<suffix>]`

     Used with gcc compiler family to specify compiler/linker tool name
     suffix &ndash; usually version number.

   - `HB_BUILD_POSTRUN_HOST=[<l>]`

     Run space separated `<l>` list of commands after successfully finishing
     a build. Commands will be run in the host binary directory.

   - `HB_BUILD_POSTRUN=[<l>]`

     Run space separated `<l>` list of commands after successfully finishing
     a build. Commands will be run in the target binary directory if possible
     to run on the host platform.

## Cross-builds

You can build Harbour for target platforms different from host platform. F.e.
you can create Windows build on \*nix systems, Linux builds on Windows systems,
etc. It's also possible to build targets for different from host CPU
architectures. F.e. you can create Windows 64-bit build on 32-bit Windows
platform, or Linux x86-64 build on x86 hosts, or Linux MIPS build on x86 host,
etc.

Point this envvar to the directory where native Harbour executables for your
host platform can be found:

      HB_HOST_BIN=<path-to-harbour-native-build>\bin

If you leave this value empty, the make system will try to auto-detect it, so
in practice all you have to do is to create a native build first (no `install`
required), then create the cross-build. If you set this value manually, it may
be useful to know that `harbour`, `hbpp` and `hbmk2` executables are required
for a cross-build process to succeed.


# Build Examples

## on Windows x86-64 (64-bit) hosts

> NOTES:
>
> - All code below should be copied to batch files or typed at command-line.
> - Naturally, you will need to adapt pathnames to valid ones on your system.
> - You can use additional `clean`, `install` or `clean install` make
>   parameters depending on what you want to do.
> - To redirect all output to a log file, append this after the make command:
>   `> log.txt 2>&1`

```batchfile
:: MinGW GCC for Windows x86
set PATH=C:\mingw\bin;%PATH%
win-make
```

```batchfile
:: MinGW GCC for Windows x86-64
set PATH=C:\mingw64\bin;%PATH%
win-make
```

```batchfile
:: clang (alpha)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set PATH=%ProgramFiles(x86)%\LLVM 3.6.svn;%PATH%
win-make
```

```batchfile
:: MSVC 2015 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2015 for Windows x86-64
:: (requires preceding build for native target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
:: MSVC 2010 and Windows SDK 7.1 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2010 (Professional or above) and Windows SDK 7.1 for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" amd64
win-make
```

```batchfile
:: Windows SDK 7 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
win-make
```

```batchfile
:: Windows SDK 7 for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars64.bat"
win-make
```

```batchfile
:: MSVC 2008 for Windows x86
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2008 (Standard or above) for Windows x86-64
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" amd64
win-make
```

```batchfile
:: MSVC 2008 (Team Suite) for Windows IA-64 Itanium
:: (requires preceding build for native target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_ia64
win-make
```

```batchfile
:: Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT64;%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
win-make
```

## on Windows 32-bit hosts

Same as 64-bit Windows, with the difference that you will have to change
`%ProgramFiles(x86)%` to `%ProgramFiles%` for 32-bit and mixed tools.
Building 64-bit targets requires a preceding 32-bit build and to do
a cross-build. It's recommended to use a 64-bit environment for Windows
development.

```batchfile
:: clang (alpha)
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set PATH=%ProgramFiles%\LLVM 3.6.svn;%PATH%
win-make
```

```batchfile
:: MSVC 2015
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2015 for Windows x86-64
:: (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
:: MSVC 2010 and Windows SDK 7.1
call "%ProgramFiles%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2010 (Professional or above) and Windows SDK 7.1 for Windows x86-64
:: (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 10.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
:: Windows SDK 7
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
win-make
```

```batchfile
:: Windows SDK 7 for Windows x86-64
:: (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\bin\vcvarsx86_amd64.bat"
win-make
```

```batchfile
:: MSVC 2008 + SDK
set WindowsSDKDir=%ProgramFiles%\Microsoft SDKs\Windows\v6.0A\
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2008
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2008 (Standard or above) for Windows x86-64
:: (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_amd64
win-make
```

```batchfile
:: MSVC 2008 (Team Suite) for Windows IA-64 Itanium
:: (requires preceding build for native target)
call "%ProgramFiles%\Microsoft Visual Studio 9.0\VC\vcvarsall.bat" x86_ia64
win-make
```

```batchfile
:: MSVC 2008 for Windows CE ARM
:: (requires preceding build for native target)
set INCLUDE=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\include;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Include\Armv4i
set LIB=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\lib\armv4i;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Lib\ARMV4I
set PATH=%ProgramFiles%\Microsoft Visual Studio 9.0\VC\ce\bin\x86_arm;%ProgramFiles%\Microsoft Visual Studio 9.0\Common7\IDE;%PATH%
win-make
```

```batchfile
:: MSVC 2005
call "%ProgramFiles%\Microsoft Visual Studio 8\VC\vcvarsall.bat"
win-make
```

```batchfile
:: MSVC 2005 for Windows CE ARM
:: (requires preceding build for native target)
set INCLUDE=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\include;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Include\Armv4i
set LIB=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\lib\armv4i;%ProgramFiles%\Windows Mobile 5.0 SDK R2\PocketPC\Lib\ARMV4I
set PATH=%ProgramFiles%\Microsoft Visual Studio 8\VC\ce\bin\x86_arm;%ProgramFiles%\Microsoft Visual Studio 8\Common7\IDE;%PATH%
win-make
```

```batchfile
:: MSVC .NET 2003 (untested)
call "%ProgramFiles%\Microsoft Visual Studio .NET 2003\VC7\vcvarsall.bat"
win-make
```

```batchfile
:: MinGW GCC
set PATH=C:\mingw\bin;%PATH%
win-make
```

```batchfile
:: MinGW GCC using MSYS2 shell
set PATH=C:\msys64\usr\bin;C:\msys64\mingw64\bin;%PATH%
sh -c make
```

```batchfile
:: MinGW GCC for Windows x86-64
:: (requires preceding build for native target)
set PATH=C:\mingw64\bin;%PATH%
win-make
```

```batchfile
:: MinGW GCC for Windows CE ARM
:: (requires Cygwin + preceding build for native target)
set PATH=C:\mingwce\opt\mingw32ce\bin;C:\cygwin\bin;%PATH%
:: optional:
set CYGWIN=nodosfilewarning
win-make
```

```batchfile
:: Intel(R) C++
call "%ProgramFiles%\Intel\Compiler\C++\10.1.014\IA32\Bin\iclvars.bat"
win-make
```

```batchfile
:: Intel(R) C++ for Windows IA-64 Itanium
:: (requires preceding build for native target)
call "%ProgramFiles%\Intel\Compiler\C++\10.1.025\Itanium\Bin\iclvars.bat"
win-make
```

```batchfile
:: Delorie GNU C for MS-DOS
set DJGPP=C:\djgpp\djgpp.env
set PATH=C:\djgpp\bin;%PATH%
win-make
```

```batchfile
:: Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
win-make
```

```batchfile
:: Open Watcom C/C++ for MS-DOS
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H
win-make
```

```batchfile
:: Open Watcom C/C++ for OS/2
:: (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2
set BEGINLIBPATH=%WATCOM%\BINP\DLL
win-make
```

```batchfile
:: Open Watcom C/C++ for Linux
:: (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\LH
win-make
```

```batchfile
:: VxWorks GCC x86
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
win-make
```

```batchfile
:: VxWorks GCC ARM
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
set HB_CPU=arm
set HB_BUILD_NAME=arm
win-make
```

```batchfile
:: VxWorks Wind River Compiler x86
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=diab
win-make
```

```batchfile
:: Cygwin GCC using Cygwin shell
set PATH=C:\cygwin\bin
sh -c make
```

## on MS-DOS hosts

```batchfile
rem Delorie GNU C
set DJGPP=C:\djgpp\djgpp.env
set PATH=C:\djgpp\bin;%PATH%
dos-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H
dos-make
```

## on OS/2 hosts

```batchfile
rem GCC 3.3.4 and GCC 3.3.5
C:\usr\bin\gccenv.cmd
os2-make
```

```batchfile
rem GCC 4.x
C:\usr\local433\gcc440.cmd
set HB_COMPILER=gccomf
os2-make
```

```batchfile
rem Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINP;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2
set BEGINLIBPATH=%WATCOM%\BINP\DLL
os2-make
```

## on Linux hosts

```sh
# Open Watcom C/C++ for OS/2
# (requires preceding build for Linux target)
export WATCOM="/opt/lng/watcom"
export PATH="${WATCOM}/binl:$PATH"
export INCLUDE="${WATCOM}/h:${WATCOM}/h/os2"
export HB_BUILD_3RDEXT=no
make
```

## on Darwin (Mac) hosts

```sh
# To create "Universal" binaries, compatible with pre-Lion 32-bit Intel systems
export HB_USER_LDFLAGS="-arch x86_64 -arch i386"
export HB_USER_CFLAGS="$HB_USER_LDFLAGS"
export HB_COMPILER=gcc
make
```

## on \*nix hosts in general

```sh
make
```

```sh
# MinGW GCC for Windows x86
make HB_PLATFORM=win
```

```sh
# MinGW GCC for Windows CE ARM
make HB_PLATFORM=wce
```


# Build Your Own Harbour App

For all platforms you will need two things:

* Harbour binaries

    Either a Harbour binary distribution or a local Harbour build will be okay.
    If you're reading this text, it's likely you have one of these already.

* Supported ANSI C compiler

    Your compiler of choice has to be placed in the `PATH` &ndash; and
    configured appropriately according to instructions.
    If you use official Harbour binary distribution on Windows, you already
    have MinGW compiler embedded in the installation, which will automatically
    be used, so you don't have to make any extra steps here.

Use `hbmk2` to build your app from source. It's recommended to put it in the
`PATH` (f.e. by using `set PATH=C:\hb\bin;%PATH%` on Windows).

See `hbmk2` [documentation, with examples](utils/hbmk2/doc/hbmk2.en.md).


# Debugging Options

## Tracing

Build Harbour with:

    HB_BUILD_DEBUG=yes

Run app with:

    HB_TR_LEVEL=debug
    # to override default stderr output:
    HB_TR_OUTPUT=<filename>
    # to enable additional system specific logging output,
    # OutputDebugString() on Windows, syslog() on *nix systems:
    HB_TR_SYSOUT=yes

## Memory statistics/tracking

Build Harbour with:

    HB_USER_CFLAGS=-DHB_FM_STATISTICS

## Valgrind (on linux and darwin targets)

Build Harbour with:

    HB_BUILD_DEBUG=yes

Build app with:

    $ hbmk2 myapp -debug

Run app with:

    $ valgrind --tool=memcheck --leak-check=yes --num-callers=16 -v ./myapp 2> myapp.log

## CodeGuard (on win/bcc target only)

Build Harbour with:

    HB_USER_CFLAGS=-vG
    HB_USER_LIBS=cg32

## Harbour Debugger

Build app with:

    $ hbmk2 myapp -b -run

or run script with:

    $ hbrun myapp --hb:debug

Press `<Alt+D>` in the app.


# Supported Platforms and C Compilers

## You can override target platform auto-detection with these `HB_PLATFORM` values:

* linux    - Linux
* darwin   - macOS / iOS / tvOS
* bsd      - *BSD
* android  - Android
* win      - MS Windows (Win9x deprecated)
* wce      - MS Windows CE
* dos      - MS-DOS (32-bit protected mode only)
             (MS-DOS compatible systems also work, like dosemu)
* os2      - OS/2 Warp 4 / eComStation
* aix      - IBM AIX
* hpux     - HP-UX
* sunos    - Sun Solaris / OpenSolaris
* beos     - BeOS / Haiku (experimental)
* qnx      - QNX (experimental)
* vxworks  - VxWorks (experimental)
* minix    - Minix 3 (tested on 3.2.1; earlier releases will not work, experimental)
* symbian  - Symbian OS (deprecated)
* cygwin   - Cygwin (experimental)

## You can override C compiler auto-detection with these `HB_COMPILER` values:

### linux
* gcc      - GNU C
* clang    - Clang
* watcom   - Open Watcom C/C++
* icc      - Intel(R) C/C++
* sunpro   - Sun Studio C/C++
* open64   - Open64 C/C++

### darwin
* gcc      - GNU C
* clang    - Clang
* icc      - Intel(R) C/C++

### bsd
* gcc      - GNU C
* clang    - Clang
* pcc      - Portable C Compiler (experimental)

### android
* gcc      - GNU C x86
* gccarm   - GNU C ARM

### win
* mingw    - MinGW GNU C 3.4.2 and above
* mingw64  - MinGW GNU C x86-64
* msvc     - Microsoft Visual C++
* msvc64   - Microsoft Visual C++ x86-64
* msvcia64 - Microsoft Visual C++ IA-64 (Itanium)

### win (partial support, some features may be missing)
* clang    - Clang
* watcom   - Open Watcom C/C++
* icc      - Intel(R) C/C++
* iccia64  - Intel(R) C/C++ IA-64 (Itanium)

### win (deprecated)
* bcc      - Borland/CodeGear/Embarcadero C++ 5.5 and above
* bcc64    - Embarcadero C++ 6.5 and above
* pocc     - Pelles C 4.5 and above
* pocc64   - Pelles C x86-64 5.0 and above
* xcc      - Pelles C for xhb

### wce
* mingw    - MinGW GNU C x86
* mingwarm - MinGW GNU C ARM (CEGCC 0.55 and above)
* msvcarm  - Microsoft Visual C++ ARM
* poccarm  - Pelles C ARM 5.0 and above

### dos
* djgpp    - Delorie GNU C
* watcom   - Open Watcom C/C++

### os2
* gcc      - EMX GNU C 3.3.5 or lower
* gccomf   - EMX GNU C 3.3.5 or upper
* watcom   - Open Watcom C/C++

### aix
* gcc      - GNU C

### hpux
* gcc      - GNU C

### sunos
* gcc      - GNU C
* sunpro   - Sun Studio C/C++

### beos (experimental)
* gcc      - GNU C

### qnx (experimental)
* gcc      - GNU C

### vxworks (experimental)
* gcc      - GNU C
* diab     - Wind River Compiler

### minix (experimental)
* clang    - Clang
* gcc      - GNU C

### cygwin (experimental)
* gcc      - GNU C

### symbian (deprecated)
* gcc      - GNU C


# Platform Matrix

 &nbsp;| host<br />platform | target<br />platform/compiler | target cpu
 :---- | :------- | :---------------- | :---------------------------------------
       | linux    | linux/gcc         | (CPU cross-builds possible)
       | linux    | linux/clang       | (CPU cross-builds possible)
       | linux    | linux/icc         | (CPU cross-builds possible: x86, x86-64, ia64)
       | linux    | linux/sunpro      | (CPU cross-builds possible: x86, x86-64)
       | linux    | linux/open64      | (CPU cross-builds possible: x86-64, ia64, ...)
     x | linux    | wce/mingwarm      | arm
     x | linux    | wce/mingw         | x86
     x | linux    | win/mingw         | x86
     x | linux    | win/mingw64       | x86-64
     x | linux    | win/watcom        | x86
     x | linux    | win/bcc           | x86 (requires WINE)
     x | linux    | win/bcc64         | x86-64 (requires WINE)
     x | linux    | os2/watcom        | x86
     x | linux    | dos/watcom        | x86
     x | linux    | dos/djgpp         | x86
     x | linux    | android/gcc       | x86
     x | linux    | android/gccarm    | arm
     x | linux    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | linux    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
       | win      | win/bcc           | x86
       | win      | win/bcc64         | x86-64
       | win      | win/clang         | x86
       | win      | win/gcc           | x86
       | win      | win/icc           | x86
       | win      | win/icc64         | x86-64 (not supported yet)
       | win      | win/iccia64       | ia64
       | win      | win/mingw         | x86
       | win      | win/mingw64       | x86-64
       | win      | win/msvc          | x86
       | win      | win/msvc64        | x86-64
       | win      | win/msvcia64      | ia64
       | win      | win/pocc          | x86
       | win      | win/pocc64        | x86-64
       | win      | win/watcom        | x86
       | win      | win/xcc           | x86
     x | win      | wce/mingwarm      | arm
     x | win      | wce/mingw         | x86   (not fully supported yet)
     x | win      | wce/poccarm       | arm
     x | win      | wce/msvcarm       | arm
     x | win      | wce/msvcmips      | mips  (not supported yet)
     x | win      | wce/msvcsh        | sh    (not supported yet)
     x | win      | wce/msvc          | x86   (not supported yet)
     x | win      | dos/djgpp         | x86   (on Windows x86 hosts only)
     x | win      | dos/watcom        | x86
     x | win      | os2/watcom        | x86
     x | win      | linux/watcom      | x86
     x | win      | android/gcc       | x86
     x | win      | android/gccarm    | arm
     x | win      | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | win      | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
     x | win      | symbian/gcc       | arm
     x | win      | cygwin/gcc        | x86
       | os2      | os2/gcc           | x86
       | os2      | os2/watcom        | x86
     x | os2      | win/watcom        | x86
     x | os2      | dos/watcom        | x86
     x | os2      | linux/watcom      | x86
       | dos      | dos/djgpp         | x86
       | dos      | dos/watcom        | x86
     x | dos      | win/watcom        | x86
     x | dos      | os2/watcom        | x86
     x | dos      | linux/watcom      | x86
       | darwin   | darwin/clang      | (CPU cross-builds possible: x86, x86-64, unibin)
       | darwin   | darwin/gcc        | (CPU cross-builds possible: x86, x86-64, ppc, ppc64, unibin)
       | darwin   | darwin/icc        | (CPU cross-builds possible: x86, x86-64)
     x | darwin   | wce/mingwarm      | arm
     x | darwin   | wce/mingw         | x86
     x | darwin   | win/mingw         | x86
     x | darwin   | win/mingw64       | x86-64
     x | darwin   | dos/djgpp         | x86
     x | darwin   | android/gcc       | x86
     x | darwin   | android/gccarm    | arm
       | bsd      | bsd/gcc           | (CPU cross-builds possible)
       | bsd      | bsd/clang         | (CPU cross-builds possible)
       | bsd      | bsd/pcc           | (experimental)
     x | bsd      | wce/mingwarm      | arm
     x | bsd      | wce/mingw         | x86
     x | bsd      | win/mingw         | x86
     x | bsd      | dos/djgpp         | x86
       | hpux     | hpux/gcc          | (CPU cross-builds possible)
       | qnx      | qnx/gcc           | (CPU cross-builds possible - not tested)
       | beos     | beos/gcc          | x86
     x | hpux     | wce/mingwarm      | arm
     x | hpux     | wce/mingw         | x86
     x | hpux     | win/mingw         | x86
     x | hpux     | dos/djgpp         | x86
       | minix    | minix/clang       | x86
       | minix    | minix/gcc         | x86
       | aix      | aix/gcc           | (CPU cross-builds possible: ppc, ppc64)
       | sunos    | sunos/gcc         | (CPU cross-builds possible)
       | sunos    | sunos/sunpro      | (CPU cross-builds possible: x86, x86-64, sparc32, sparc64)
     x | sunos    | wce/mingwarm      | arm
     x | sunos    | wce/mingw         | x86
     x | sunos    | win/mingw         | x86
     x | sunos    | dos/djgpp         | x86
     x | sunos    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
     x | sunos    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)

> Leading **x** marks cross-platform scenarios.

Supported shells per host platforms:

* \*nix / POSIX shell
* win  / NT shell (cmd.exe)
* win  / POSIX shell (MSYS2 or Cygwin sh.exe)
* win  / MS-DOS shell (command.com)
* dos  / MS-DOS shell (command.com)
* dos  / POSIX shell (bash.exe)
* os/2 / OS/2 shell (cmd.exe)
* os/2 / POSIX shell (bash.exe)


# External Links

* C/C++ Compilers/Shells:

     * MinGW/MinGW-64 [win, \*nix, free software, open-source]
        * <https://mingw-w64.org/>, <https://duckduckgo.com/?q=mingw-w64> (recommended, look for MSYS2 or niXman builds)
          * Dual-target (aka _multilib_) for x86-64 and x86 hosts (select non-native target with `HB_CPU=x86` or `HB_CPU=x86_64`):
            * 32-bit hosted, posix, sjlj
            * 64-bit hosted, posix, sjlj
          * x86:
            * 32-bit hosted, posix, dwarf-2
          * x86-64:
            * 64-bit hosted, posix, seh
     * MinGW CEGCC [win, \*nix, free software, open-source]
        * <https://web.archive.org/web/sourceforge.net/projects/cegcc/files/cegcc/>
          * To use this package, you will also need Cygwin package
            installed and be in `PATH` for the Cygwin runtime (`cygwin1.dll`).
          * Unpack using these commands:

            `bzip2 -d cegcc_mingw32ce_cygwin1.7_r1399.tar.bz2`<br />
            `tar -xvf cegcc_mingw32ce_cygwin1.7_r1399.tar -h`

          * Compiler will be in the `opt\mingw32ce` subdirectory.
     * MSYS2 [win, free software, open-source]
        * <https://msys2.github.io/>
     * Clang [multi-platform, free software, open-source]
        * <http://releases.llvm.org/>
     * Cygwin [win, free software, open-source]
        * <https://cygwin.com/>
     * OS/2 GCC [os2, free software, open-source]
        * <http://os2ports.smedley.id.au/index.php?page=tools-utilities>
     * DJGPP [\*nix, dos, free software, open-source]
        * <http://www.delorie.com/djgpp/>
     * Open Watcom [multi-platform, free software, open-source]
        * <https://github.com/open-watcom/open-watcom-v2>, <https://open-watcom.github.io/open-watcom/>
     * Xcode / Command Line Tools for Xcode [darwin, zero price, proprietary with open-source components]
        * <https://itunes.apple.com/us/app/xcode/id497799835>
        * <https://developer.apple.com/downloads/>
     * MS Windows SDK [zero price, proprietary]
        * <https://developer.microsoft.com/windows/downloads/sdk-archive/><br />
         ([7.x](https://www.microsoft.com/download/details.aspx?id=8279) includes compilers for x86, x86-64 and IA-64)
     * MS Windows Mobile SDK [wce, zero price, proprietary]
        * <https://www.microsoft.com/download/details.aspx?id=42>
     * MS Visual C++ Build Tools [win, zero price, proprietary]
        * <https://go.microsoft.com/fwlink/?LinkId=691126>
     * MS Visual Studio Community [win, zero price, proprietary]
        * <https://www.visualstudio.com/vs/visual-studio-express/>
     * MS Visual Studio [win, commercial, proprietary]
        * <https://www.visualstudio.com/>
     * Intel Compiler [mult-platform, commercial, proprietary]
        * <https://software.intel.com/c-compilers>

* Libraries:

     * HB_WITH_PCRE2, HB_WITH_PCRE - Perl Compatible Regular Expressions [multi-platform, free software, open-source]
        * <http://pcre.org/>
     * HB_WITH_PNG - libpng [multi-platform, free software, open-source]
        * <https://github.com/glennrp/libpng>
     * HB_WITH_WATT - Watt-32 (TCP/IP sockets) [dos, free software, open-source]
        * <http://www.watt-32.net/>
     * HB_WITH_ZLIB - zlib [multi-platform, free software, open-source]
        * <http://zlib.net/>

* Tools:

     * Git (1.7 or upper) [multi-platform, free software, open-source]
        * <https://git-scm.com/>
        * Windows binaries:
           * <https://git-for-windows.github.io/>
     * GitHub Desktop [multi-platform, zero price, proprietary]
        * <https://desktop.github.com/>
     * Travis CI [continuous integration, web service, free plan available]
        * <https://travis-ci.org/>
     * AppVeyor CI [continuous integration, web service, free plan available]
        * <https://www.appveyor.com/>
     * Read the Docs [online documentation creator, web service, free software]
        * <https://readthedocs.org/>
     * GNU Bison (grammar parser generator) [multi-platform, free software, open-source]
        * Windows binary:
           * <https://git-for-windows.github.io/>
     * Cppcheck (static analysis) [multi-platform, free software, open-source]
        * <https://github.com/danmar/cppcheck>
     * Valgrind (dynamic executable analysis tool) [linux, darwin, free software, open-source]
        * <http://valgrind.org/>
     * Uncrustify (source formatter) [multi-platform, free software, open-source]
        * <https://github.com/uncrustify/uncrustify>
     * UPX (executable compressor) [multi-platform, free software, open-source]
        * <https://upx.github.io/>
     * 7-Zip [multi-platform, free software, open-source]
        * <http://7-zip.org/>
     * Chocolatey and NuGet (Windows package managers) [free software, open-source]
        * <https://chocolatey.org/>
        * <https://www.nuget.org/>
     * GNU Make [multi-platform, free software, open-source]
        * <https://www.gnu.org/software/make/>

* Documentation:

     * [Netiquette Guidelines](https://tools.ietf.org/html/rfc1855)
     * [Setting Up Git](https://help.github.com/articles/set-up-git)
     * [Pro Git](https://git-scm.com/book) [free book]
     * [GitHub Training Kit & Multi-language Cheat Sheet](https://training.github.com/kit/)
     * Using gettext (.po files)
       * <https://docs.transifex.com/formats/gettext>
       * <http://heiner-eichmann.de/autotools/using_gettext.html>
     * [GitHub Guides](https://guides.github.com/)
     * [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown)
     * [A Practical Git Introduction](http://marc.helbling.fr/2014/09/practical-git-introduction)

* Community forums:

  * Generic:
     * [English](https://groups.google.com/forum/#!forum/harbour-users)
     * [Italian](https://groups.google.com/forum/#!forum/harbourita)
     * [Portuguese](http://pctoledo.com.br/forum/viewforum.php?f=4)
     * [Russian](http://clipper.borda.ru/?0-4)

  * Social media:
     * [Facebook](https://www.facebook.com/groups/harbour.project/)
     * [Twitter](https://twitter.com/harbourproject)

  * Product-oriented:
     * [Harbour mainline development](https://groups.google.com/forum/#!forum/harbour-devel)
     * [HMG (GUI)](http://hmgforum.com/viewforum.php?f=7)
     * [hbqt (GUI)](https://groups.google.com/forum/#!forum/qtcontribs)
     * [hwgui (GUI)](https://sourceforge.net/p/hwgui/mailman/hwgui-developers/)
     * [xHarbour fork](https://groups.google.com/forum/#!forum/comp.lang.xharbour)

  * Translators:
     * [Bing Translator](https://www.bing.com/translator/)
     * [Google Translate](https://translate.google.com/)


# Harbour Links

  * [Homepage](https://vszakats.github.io/harbour-core/)
  * [How to contribute](.github/CONTRIBUTING.md)
  * [Source code](https://github.com/vszakats/harbour-core)
  * [Issues](https://github.com/vszakats/harbour-core/issues)
  * [Localization](https://www.transifex.com/projects/p/harbour/) (Resource [hbmk2-vszakats](https://www.transifex.com/projects/p/harbour/resource/hbmk2-vszakats/))
  * Documents:
     * [hbmk2 documentation](utils/hbmk2/doc/hbmk2.en.md)
     * [hbrun documentation](contrib/hbrun/doc/hbrun.en.md)
     * [ChangeLog](ChangeLog.txt?raw=true)
     * Comparing [Harbour with xHarbour](doc/xhb-diff.txt?raw=true)
     * CA-Cl*pper 5.3 [online documentation](https://harbour.github.io/ng/c53g01c/menu.html)
     * Harbour [online documentation](https://harbour.github.io/doc/)
     * Harbour [internal documents](doc/)
     * [Wikipedia](https://en.wikipedia.org/wiki/Harbour_compiler)
     * [Stack Overflow](https://stackoverflow.com/questions/tagged/clipper)


# Guarantees and Liability

   This document and all other parts of Harbour are distributed in the
   hope they will be useful, but WITHOUT GUARANTEE that they are complete,
   accurate, non-infringing or usable for any purpose whatsoever.
   Contributors are NOT LIABLE for any damages that result from using
   Harbour in any ways. For more legal details, see [LICENSE](LICENSE.txt).

   If you feel you can make Harbour better: contribute.
   [See how](.github/CONTRIBUTING.md).

   Information in this document is subject to change without notice and does
   not represent any future commitment by the participants of the project.

   This and related documents use the term "recommended" for practices and
   tools *tested most*, *focused on*, *used and deployed* by the
   maintainer/developer of this fork. While this is strongly believed to result
   in the best Harbour experience for most situations, it's ultimately
   a subjective decision. If you don't like it, use what fits you best.

---
This document Copyright &copy;&nbsp;2009&ndash;2017 Viktor Szakáts (vszakats.net/harbour)<br />
[![Creative Commons Attribution-ShareAlike 4.0](https://cdn.rawgit.com/cc-icons/cc-icons/master/fonts/cc-icons-svg/small.by-sa.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
