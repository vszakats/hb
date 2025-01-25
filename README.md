# Welcome to Harbour
[![License](https://img.shields.io/badge/license-GPLv2%20%2B%20Library%20Exception-blue.svg)](LICENSE.txt "License")
[![Download](https://img.shields.io/badge/download-snapshot_binary_(3.4)-blue.svg)](https://github.com/vszakats/hb/releases "Download snapshot release")
<br>
[![Build status](https://ci.appveyor.com/api/projects/status/4rd806hk73q83qo4/branch/main?svg=true)](https://ci.appveyor.com/project/vsz/hb/branch/main)
[![Coverity Status](https://scan.coverity.com/projects/3208/badge.svg)](https://scan.coverity.com/projects/3208)

Harbour is the open/free software implementation of a cross-platform,
multi-threading, object-oriented, scriptable programming language, backwards
compatible with xBase languages. Harbour consists of a compiler and runtime
libraries with multiple UI, database and I/O backends, its own build system
and a collection of libraries and bindings for popular APIs.

# Table of Contents

1. [How to Get](#how-to-get)
2. [How to Build](#how-to-build)
3. [How to Do a Partial Build](#how-to-do-a-partial-build)
4. [How to Create Packages for Distribution](#how-to-create-packages-for-distribution)
5. [How to Enable Optional Components](#how-to-enable-optional-components)
6. [Build Options](#build-options)
7. [Build Examples](#build-examples)
8. [Build Your Own Harbour App](#build-your-own-harbour-app)
9. [Debugging Options](#debugging-options)
10. [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)
11. [Platform Matrix](#platform-matrix)
12. [External Links](#external-links)
13. [Harbour Links](#harbour-links)
14. [Guarantees and Liability](#guarantees-and-liability)

---

# How to Get

## Stable versions (non-fork/mainline)

### Harbour stable binary download

<https://github.com/vszakats/hb/releases/tag/v3.0.0>

> NOTE: It is identical to the mainline stable release, but
> not supported or recommended by this fork.

### Harbour stable source download

<https://github.com/vszakats/hb/archive/v3.0.0.zip>

## Unstable versions

> TIP:
> [For](https://groups.google.com/forum/#!msg/harbour-users/2fwUzdKwpKA/32nI4WhZLfYJ)
> [users](https://groups.google.com/forum/#!msg/harbour-users/Ro99f8S6my0/KvfjhCx_jE4J)
> [contributing](CONTRIBUTING.md) to development, it's recommended to follow
> [commits](https://github.com/vszakats/hb/commits/main) and reading
> [ChangeLog.txt](ChangeLog.txt?raw=true).

### Harbour live source repository

You will need Git version control software installed on your system
and use this command (remove the `--depth` option to clone the whole
history &mdash; useful for development):

    git clone --depth=10 https://github.com/vszakats/hb.git

You can get subsequent updates using this command:

    git pull

### Harbour unstable sources

Download source archive from any of these URLs and unpack:

* <https://github.com/vszakats/hb/archive/main.zip>
* <https://github.com/vszakats/hb/archive/main.tar.gz>

### Harbour unstable binaries (updated after each commit)

#### Windows (MinGW, 64-bit hosted, 32/64-bit targets, compressed archive)

* <https://github.com/vszakats/hb/releases>

#### Mac (using Homebrew)

    brew install https://raw.githubusercontent.com/vszakats/hb/main/package/hb@3.4.rb --HEAD

### Follow commits using:

* [Web browser](https://github.com/vszakats/hb/commits/main)
* [RSS feed](https://github.com/vszakats/hb/commits/main.atom)
* Any compatible mobile/desktop client app

# How to Build

For all platforms you will need:

* Supported ANSI C89 compiler
* GNU Make (3.81 or upper)
* Harbour sources

## on Windows hosts

Platform specific prerequisites:

1. Windows 7 or upper system is recommended to *build* Harbour. (64-bit
   edition is recommended.)
2. Consider using the binary release. On Windows, it is recommended for most
   people. You can still rebuild specific contribs this way.
3. Make sure to have your C compiler of choice installed in `PATH`. Refer to
   your C compiler installation and setup instructions for details. Make sure
   no tools in your `PATH` belonging to other C compilers are interfering with
   your setup. Also avoid to keep multiple copies of the same compiler, or
   different versions of the same compiler in `PATH` at the same time. For the
   list of supported compilers,
   look up [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers).
4. A native build of GNU Make is required. It is usually named
   `mingw32-make.exe`. It's distributed in MSYS2, mingw-w64 packages. You can
   find download links [here](#external-links).
   Unpack it to your `PATH` or Harbour source root directory, and run it as
   `mingw32-make`.

To build:

    $ mingw32-make

To test it, type:

    $ cd tests
    $ ..\bin\<plat>\<comp>\hbmk2 hello.prg
    $ hello

You should see `Hello, world!` on screen.

## on Windows hosts with POSIX shells (MSYS2)

To build:

    $ sh -c make

To test it, type:

    $ cd tests
    $ ..\bin\<plat>\<comp>\hbmk2 hello.prg
    $ hello

You should see `Hello, world!` on screen.

## on Linux hosts (possible cross-build targets: Windows, Windows CE, MS-DOS, OS/2)

To build:

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

## on Darwin (Mac) hosts (possible cross-build targets: Windows)

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
> you can use multiple values:<br>
> <br>
> ARM64: `-arch arm64`<br>
> Intel 64-bit: `-arch x86_64`<br>

## on FreeBSD hosts

Platform specific prerequisites:

1. You will need to have the developer tools installed.
2. Then you will need to install gmake and optionally bison:

        $ pkg install gmake bison

To build:

    $ gmake

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

## on Minix hosts

Install GNU make from the Minix pkgsrc repository; for details see
[here](https://wiki.minix3.org/doku.php?id=usersguide:installingbinarypackages).

You may also install GCC, if you wish to use that instead of Clang, the Minix
system compiler.

## on other \*nix hosts

To build:

    $ gmake [HB_PLATFORM=<...>]

Or

    $ make [HB_PLATFORM=<...>]

To test it, type:

    $ cd tests
    $ ../bin/<plat>/<comp>/hbmk2 hello.prg
    $ ./hello

You should see `Hello, world!` on screen.

> For sunpro on Solaris:<br>
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

## On \*nix systems

    $ cd contrib/<name>
    $ make.hb [clean] [custom hbmk2 options]

## On Windows

    $ cd contrib/<name>
    $ hbmk2 make.hb [clean] [custom hbmk2 options]

> Where `make.hb` and `hbmk2` must be in `PATH`.

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
    --with localpcre1  - build local copy of pcre1 library
    --without x11      - do not build components dependent on x11 (gtxwc)
    --without curses   - do not build components dependent on curses (gtcrs)
    --without slang    - do not build components dependent on slang (gtsln)
    --without gpllib   - do not build components dependent on GPL 3rd party code
    --without gpm      - build components without gpm support (gttrm, gtsln, gtcrs)

## Binary .rpm on Linux (cross-builds)

### for Windows

    $ package/mpkg_rpm_win.sh

### for Windows CE

    $ package/mpkg_rpm_wce.sh

## Binary .7z archive on Windows for all targets (except Linux)

    $ set HB_DIR_7Z=C:\7-zip\
    $ set HB_BUILD_PKG=yes

Then run build as usual with `clean install` options.
See: [How to Build](#how-to-build)


# How to Enable Optional Components

Certain Harbour parts &mdash; typically contrib packages &mdash; depend on
3rd party components. To make these Harbour parts built, you need to tell
Harbour where to find the headers for these 3rd party components.

On \*nix systems most of these 3rd party components will automatically
be used if installed on well-known standard system locations.

You only need to use manual setup if the dependency isn't available on your
platform on a system location, or you wish to use a non-standard location.
Typically, you need to do this on non-\*nix (e.g. Windows) systems for all
packages and for a few packages on \*nix that are not available via
official package managers (e.g. ADS Client).

Note that Harbour will use 3rd party **binary** packages in their default,
unmodified &mdash; "vanilla" &mdash; install layout created by their
official/mainstream install kits. If you manually move, rename, remove, add
files under the 3rd party packages' root directory, or use a source package,
the default Harbour build process (especially Windows implib generation)
might not work as expected.

You can set these environment variables before starting the build. Make sure
to adjust them to your own directories:

    HB_WITH_CURSES= (on \*nix systems and DJGPP, auto-detected on both)
    HB_WITH_GPM= (on Linux only)
    HB_WITH_PCRE2=C:\pcre2 (defaults to vendored copy if not found)
    HB_WITH_PCRE=C:\pcre (defaults to vendored copy if not found)
    HB_WITH_PNG=C:\libpng (defaults to vendored copy if not found)
    HB_WITH_SLANG= (on \*nix systems)
    HB_WITH_WATT= (on MS-DOS systems)
    HB_WITH_X11= (on \*nix systems)
    HB_WITH_ZLIB=C:\zlib (defaults to vendored copy if not found)

To explicitly disable any given components, use the value `no`. This may be
useful to avoid auto-detection of installed packages on \*nix systems. You
may also use the value `local` to force using the vendored copy (foreign
sources hosted inside the Harbour source repository) of these packages,
where applicable. `nolocal` will explicitly disable using the vendored
copy.

See contrib-specific dependencies and build notes in the projects' `.hbp`
file and find occasional link notes inside their `.hbc` files.


> NOTES:
>
>    * you need to use path format native to your shell/OS
>    * don't put directory names inside double quotes
>    * use absolute paths

## Darwin (Mac)

1. Install [Homebrew](https://brew.sh/)
2. Install packages:

        $ brew install valgrind pcre pcre2 s-lang upx uncrustify optipng jpegoptim

3. Install [X11](https://www.xquartz.org/) (optional, for `gtxwc`)

        $ brew cask install xquartz


## Linux (.deb based distros: Debian, \*buntu)

You will need these base packages to build/package/test/use Harbour:

      bash git gcc binutils fakeroot debhelper valgrind upx uncrustify p7zip-full

You will need these packages to compile optional core Harbour features:

      for gtcrs terminal lib:    libncurses-dev
      for gtsln terminal lib:    libslang2-dev OR libslang1-dev
      for gtxwc terminal lib:    libx11-dev
      for console mouse support: libgpm-dev OR libgpmg1-dev

Optional, to override vendored sources:

      for zlib support:          zlib1g-dev
      for pcre2 (regex) support: libpcre2-dev
      for pcre1 (regex) support: libpcre3-dev

## Linux (.rpm based distros: openSUSE, Fedora, CentOS)

You will need these base packages to build/package/test/use Harbour:

      bash git gcc make glibc-devel rpm-build valgrind upx uncrustify p7zip

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
>       install above packages with `-32bit` suffix, e.g. `slang-devel-32bit`

## OpenSolaris

    $ pkg install SUNWgit SUNWgcc SUNWgmake

## FreeBSD

If you want to use the `gtsln` library instead of `gtstd` or `gtcrs`, then you
also need to install `libslang`. If you installed the ports collection, then
all you need to do to install `libslang` is to run the following command,
which may require that you run `su` first to get the right permissions:

    $ pkg install libslang2


# Build Options

You can fine-tune Harbour builds with below listed environment variables.
You can add most of these via the GNU Make command-line also, using
`make VARNAME=value` syntax. These settings are optional and all settings
are case-sensitive.

## General

   - `HB_BUILD_VERBOSE=yes`

     Enable verbose build output. Redirect it to file by appending this to
     the build command: `> log.txt 2>&1`<br>
     Default: `no`

   - `HB_PLATFORM`             Override platform auto-detection
   - `HB_COMPILER`             Override C compiler auto-detection

     Set these only if auto-detection doesn't suit your purpose.

     See this for possible values:
     [Supported Platforms and C Compilers](#supported-platforms-and-c-compilers)<br>
     See also: `HB_CC*` settings.

   - `HB_BUILD_CONTRIBS=no [<l>]`

     Do not build any, or space separated `<l>` list of, contrib packages.
     Please note that packages which are dependencies of other &mdash;
     enabled &mdash; packages will still build, unless their dependents
     get disabled as well.

   - `HB_BUILD_CONTRIBS=[<l>]`

     Build space separated `<l>` list of contrib libraries.
     Build all if left empty (default).

   - `HB_BUILD_STRIP=[all|bin|lib|no]`

     Strip symbols and debug information from binaries.
     Default: `no`

   - `HB_BUILD_3RDEXT=no`

     Enable auto-detection of 3rd party components on default system
     locations. Default: `yes`

   - `HB_BUILD_NOGPLLIB=yes`

     Disable components dependent on GPL 3rd party code, to allow using
     Harbour for nonfree/proprietary projects. Default: `no`

   - `HB_CCPATH=[<dir>/]`

     Used with non-\*nix gcc family compilers (and sunpro) to specify path
     to compiler/linker/archive tool to help them run from \*nix hosts as
     cross-build tools. Always add an ending slash.

   - `HB_CCPREFIX=[<prefix>]`

     Used with gcc compiler family to specify compiler/linker/archive tool
     name prefix.

   - `HB_CCSUFFIX=[<suffix>]`

     Used with gcc/clang compiler families to specify compiler/linker tool
     name suffix &mdash; usually version number.

   - `HB_INSTALL_PREFIX`

     Target root directory to install Harbour files.
     On \*nix systems the default is `/usr/local/` or `$(PREFIX)` if
     specified, and `/usr/local/harbour-<arch>-<comp>` for cross-builds.
     It's always set to `./pkg/<arch>/<comp>` when `HB_BUILD_PKG` is `yes`.
     On non-\*nix systems, you must set it to a valid directory when using
     `install`. Use absolute paths only.
     You have to use path format native to your shell.
     E.g. to specify `C:\dir` on Windows.

     > WARNING:
     >
     > Harbour is fully functional on all platforms, without installing it
     > to any other directory. On \*nix systems, if you must install, please
     > use a stable installer package instead.

## For developing Harbour itself

   - `HB_USER_PRGFLAGS`        User Harbour compiler options
   - `HB_USER_CFLAGS`          User C compiler options
   - `HB_USER_DCFLAGS`         User C compiler options (for dynamic libraries only)
   - `HB_USER_RESFLAGS`        User resource compiler options (on win, wce, os2)
   - `HB_USER_LDFLAGS`         User linker options for executables
   - `HB_USER_AFLAGS`          User linker options for libraries
   - `HB_USER_DFLAGS`          User linker options for dynamic libraries

   - `HB_BUILD_DEBUG=yes`

     Create debug build. Default: `no`

   - `HB_BUILD_OPTIM=no`

     Enable C compiler optimizations. Default: `yes`

   - `HB_BUILD_PKG=yes`

     Create release package. Default: `no`
     Requires `clean install` in root source dir.

   - `HB_BUILD_CONTRIB_DYN=yes`

     Create contrib dynamic libraries (in addition to static).
     Default: `no`,
     except Windows and darwin platforms, where it's `yes`.

   - `HB_BUILD_3RD_DYN=yes`

     Create dynamic libraries of vendored 3rd party libraries
     (in addition to static). Default: `no`

   - `HB_BUILD_SHARED=yes`

     Create Harbour executables in shared mode.
     Default: `yes` on non-\*nix platforms that support
     is and on \*nix when `HB_INSTALL_PREFIX` points to
     a system location, otherwise `no`.

   - `HB_BUILD_PARTS=[all|compiler|lib]`

     Build only specific part of Harbour.

   - `HB_BUILD_NAME=[<name>]`

     Create named build. This allows keeping multiple builds in parallel for
     any given platform/compiler. E.g. debug / release.

     > It is appended to compiler directory name, so all file-system/platform
     > name rules and limits apply. (Back)slashes will be stripped from the
     > name though.

   - `HB_USER_LIBS=[<list>]`

     Add space separated `<list>` of libs to link process.
     Lib names should be without extension and path.

   - `HB_BUILD_LIBPATH`

     Use extra library path when building contrib packages. It will be passed
     to `hbmk2` via its `-L` option, _after_ any other custom option.

   - `HB_INSTALL_IMPLIB=no`

     Copy import libraries created for external .dll dependencies to the
     library install directory in `install` build phase. Default: `yes`<br>
     For Windows and OS/2 targets only. Please note that this feature isn't
     supported with all possible binary distributions of 3rd party packages.
     We test only the official/mainstream ones. Also note that the created
     implibs will require .dlls compatible with the ones used at build time.

   - `HB_INSTALL_3RDDYN=yes`

     Copy dynamic libraries of external .dll dependencies to the dynamic
     library directory in `install` build phase. Default: `no`

   - `HB_REBUILD_EXTERN=yes`

     Rebuild extern headers. For developers doing Harbour code updates
     and releases. Default: `no`

   - `HB_REBUILD_PARSER=yes`

     Rebuild language parser sources. You only need this if your are Harbour
     core developer updating grammar rules (.y). Requires GNU Bison 1.28 or
     upper in `PATH`. Default: `no`

   - `HB_BUILD_MODE=[cpp|c]`

     Set default build mode to C++ or C. Default: `c`

     This serves only to test Harbour code base for issues revealed by
     stricter C++ compiler rules and/or to ensure C/C++ interoperability.
     C++ mode is deprecated and not supported for production use.

   - `HB_BUILD_POSTRUN_HOST=[<l>]`

     Run space separated `<l>` list of commands after successfully finishing
     a build. Commands will run in the host binary directory.

   - `HB_BUILD_POSTRUN=[<l>]`

     Run space separated `<l>` list of commands after successfully finishing
     a build. Commands will run in the target binary directory if possible
     to run on the host platform.

## Cross-builds

You can build Harbour for target platforms different from host platform. E.g.
you can create Windows build on \*nix systems, Linux builds on Windows systems,
etc. It's also possible to build targets for different from host CPU
architectures. E.g. you can create Windows 64-bit build on 32-bit Windows
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

## on Windows 64-bit hosts

> NOTES:
>
> - Copy code below to batch files or type it at the command-line.
> - Naturally, you will need to adapt path names to valid ones on your system.
> - You can use `clean`, `install` or `clean install` make parameters
>   depending on what you want to do.
> - To redirect all output to a log file, append this after the make command:
>   `> log.txt 2>&1`

```batchfile
:: MinGW-w64 LLVM/Clang via MSYS2 (x86 target)
set PATH=C:\msys64\mingw32\bin;C:\msys64\usr\bin;%PATH%
set HB_COMPILER=clang
mingw32-make
```

```batchfile
:: MinGW-w64 LLVM/Clang via MSYS2 (x64 target)
set PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
set HB_COMPILER=clang64
mingw32-make
```

```batchfile
:: MinGW-w64 GCC via MSYS2 (x86 target)
set PATH=C:\msys64\mingw32\bin;C:\msys64\usr\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC via MSYS2 (x64 target)
set PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC (x86 target)
set PATH=C:\mingw\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC (x64 target)
set PATH=C:\mingw64\bin;%PATH%
mingw32-make
```

```batchfile
:: MSVC 2017 or upper
:: For configuration, see:
::   https://docs.microsoft.com/cpp/build/setting-the-path-and-environment-variables-for-command-line-builds
:: Then:
mingw32-make
```

```batchfile
:: MSVC 2015 (x86 target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
mingw32-make
```

```batchfile
:: MSVC 2015 (x64 target)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64
mingw32-make
```

```batchfile
:: Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT64;%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
mingw32-make
```

```batchfile
:: LLVM/Clang-cl (pre-experimental)
call "%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set PATH=%ProgramFiles(x86)%\LLVM 3.6.svn;%PATH%
mingw32-make
```

```batchfile
:: Intel(R) C++ (x86 target)
call "%ProgramFiles(x86)%\Intel\Compiler\11.1\054\bin\ia32\iclvars_ia32.bat"
mingw32-make
```

```batchfile
:: Intel(R) C++ (x64 target)
call "%ProgramFiles(x86)%\Intel\Compiler\11.1\054\bin\intel64\iclvars_intel64.bat"
mingw32-make
```

## on Windows 32-bit hosts

Same as 64-bit Windows, with the difference that you will have to change
`%ProgramFiles(x86)%` to `%ProgramFiles%` for 32-bit and mixed tools.
Building 64-bit targets requires a preceding 32-bit build and to do
a cross-build. It's recommended to use a 64-bit environment for Windows
development.

```batchfile
:: MinGW-w64 LLVM/Clang via MSYS2 (x86 target)
set PATH=C:\msys64\mingw32\bin;C:\msys64\usr\bin;%PATH%
set HB_COMPILER=clang
mingw32-make
```

```batchfile
:: MinGW-w64 LLVM/Clang via MSYS2 (x64 target)
:: (requires preceding build for x86 target)
set PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
set HB_COMPILER=clang64
mingw32-make
```

```batchfile
:: MinGW-w64 GCC via MSYS2 (x86 target)
set PATH=C:\msys64\mingw32\bin;C:\msys64\usr\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC via MSYS2 (x64 target)
:: (requires preceding build for x86 target)
set PATH=C:\msys64\mingw64\bin;C:\msys64\usr\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC (x86 target)
set PATH=C:\mingw\bin;%PATH%
mingw32-make
```

```batchfile
:: MinGW-w64 GCC (x64 target)
:: (requires preceding build for x86 target)
set PATH=C:\mingw64\bin;%PATH%
mingw32-make
```

```batchfile
:: MSVC 2017 or upper
:: For configuration, see:
::   https://docs.microsoft.com/cpp/build/setting-the-path-and-environment-variables-for-command-line-builds
:: Then:
mingw32-make
```

```batchfile
:: MSVC 2015 (x86 target)
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
mingw32-make
```

```batchfile
:: MSVC 2015 (x64 target)
:: (requires preceding build for x86 target)
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64
mingw32-make
```

```batchfile
:: MinGW GCC (Windows CE ARM target)
:: (requires Cygwin + preceding build for x86 target)
set PATH=C:\mingwce\opt\mingw32ce\bin;C:\cygwin\bin;%PATH%
:: optional:
set CYGWIN=nodosfilewarning
mingw32-make
```

```batchfile
:: Delorie GNU C for MS-DOS
set DJGPP=C:\djgpp\djgpp.env
set PATH=C:\djgpp\bin;%PATH%
mingw32-make
```

```batchfile
:: Open Watcom C/C++
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\NT;%WATCOM%\H\NT\DIRECTX;%WATCOM%\H\NT\DDK;%INCLUDE%
mingw32-make
```

```batchfile
:: Open Watcom C/C++ for MS-DOS
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%PATH%
set INCLUDE=%WATCOM%\H
mingw32-make
```

```batchfile
:: Open Watcom C/C++ for OS/2
:: (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\H;%WATCOM%\H\OS2
set BEGINLIBPATH=%WATCOM%\BINP\DLL
mingw32-make
```

```batchfile
:: Open Watcom C/C++ for Linux
:: (requires preceding build for Windows target)
set WATCOM=C:\watcom
set PATH=%WATCOM%\BINNT;%WATCOM%\BINW;%PATH%
set INCLUDE=%WATCOM%\LH
mingw32-make
```

```batchfile
:: LLVM/Clang-cl (pre-experimental)
call "%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set PATH=%ProgramFiles%\LLVM 3.6.svn;%PATH%
mingw32-make
```

```batchfile
:: VxWorks GCC x86
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
mingw32-make
```

```batchfile
:: VxWorks GCC ARM
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=gcc
set HB_CPU=arm
set HB_BUILD_NAME=arm
mingw32-make
```

```batchfile
:: VxWorks Wind River Compiler x86
:: (requires preceding build for Windows target)
wrenv -p vxworks-6.8
set HB_COMPILER=diab
mingw32-make
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
export WATCOM='/opt/lng/watcom'
export PATH="${WATCOM}/binl:$PATH"
export INCLUDE="${WATCOM}/h:${WATCOM}/h/os2"
export HB_BUILD_3RDEXT=no
make
```

## on Mac hosts

```sh
# To create "Universal 2" binaries, compatible with Apple Silicon (ARM64) and Intel (x86-64)
# Requires: Xcode 12.2, SDK MacOSX11.0, MACOSX_DEPLOYMENT_TARGET=11.00
export HB_USER_LDFLAGS='-arch arm64 -arch x86_64'
export HB_USER_DFLAGS="${HB_USER_LDFLAGS}"
export HB_USER_CFLAGS="${HB_USER_LDFLAGS}"
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

For all platforms you will need:

* Harbour binaries

    Either a Harbour binary distribution or a local Harbour build will be okay.
    If you're reading this text, it's likely you have one of these already.

* Supported ANSI C89 compiler

    You need to add your compiler of choice to the `PATH` &mdash; and
    configure it according to its instructions.
    If you use the official Harbour binary distribution on Windows, you already
    have the MinGW C compiler embedded in the installation, which will
    automatically be used, so you don't have to make extra steps here.

Use `hbmk2` to build your app from source. It's recommended to put it in the
`PATH` (e.g. by using `set PATH=C:\hb\bin;%PATH%` on Windows).

See `hbmk2` [documentation, with examples](utils/hbmk2/doc/hbmk2.en.md).


# Debugging Options

## Tracing

Build Harbour with:

    HB_BUILD_DEBUG=yes

Run app with:

    HB_TR_LEVEL=debug
    # to override default STDERR output:
    HB_TR_OUTPUT=<filename>
    # to enable additional system specific logging output,
    # OutputDebugString() on Windows, syslog() on \*nix systems:
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

Press <kbd>Alt</kbd>+<kbd>D</kbd> in the app.


# Supported Platforms and C Compilers

## You can override target platform auto-detection with these `HB_PLATFORM` values:

* linux    - Linux
* darwin   - macOS / iOS and derivatives
* bsd      - \*BSD
* android  - Android
* win      - MS Windows (Win9x deprecated)
* wce      - MS Windows CE
* dos      - MS-DOS (32-bit protected mode only)
             (MS-DOS compatible systems also work, like dosemu)
* os2      - OS/2 Warp 4 / eComStation
* aix      - IBM AIX
* hpux     - HP-UX
* sunos    - Sun Solaris / OpenSolaris
* qnx      - QNX (experimental)
* vxworks  - VxWorks (experimental)
* minix    - Minix 3 (experimental, tested on 3.2.1; earlier releases will not work)
* cygwin   - Cygwin (experimental)
* beos     - BeOS / Haiku (deprecated)

## You can override C compiler auto-detection with these `HB_COMPILER` values:

### linux
* gcc      - GNU C
* clang    - LLVM/Clang
* watcom   - Open Watcom C/C++
* icc      - Intel(R) C/C++
* sunpro   - Sun Studio C/C++
* open64   - Open64 C/C++

### darwin
* clang    - Apple LLVM/Clang
* gcc      - GNU C
* icc      - Intel(R) C/C++

### bsd
* gcc      - GNU C
* clang    - LLVM/Clang
* pcc      - Portable C Compiler (experimental)

### android
* gcc      - GNU C x86
* gccarm   - GNU C ARM

### win
* clang    - LLVM/Clang (5.0.0 and above)
* clang64  - LLVM/Clang x86-64 (5.0.0 and above)
* mingw    - MinGW GNU C (4.4.0 and above, 6.x or newer recommended)
* mingw64  - MinGW GNU C x86-64
* msvc     - Microsoft Visual C++ (2013 and above)
* msvc64   - Microsoft Visual C++ x86-64 (2013 and above)

### win (experimental)
* clang-cl - LLVM/Clang-cl
* clang-cl64 - LLVM/Clang-cl x86-64
* watcom   - Open Watcom C/C++
* icc      - Intel(R) C/C++
* icc64    - Intel(R) C/C++ x86-64

### win (deprecated)
* bcc      - Borland/CodeGear/Embarcadero C++ 5.5 and above
* bcc64    - Embarcadero C++ 6.5 and above
* pocc     - Pelles C 4.5 and above
* pocc64   - Pelles C x86-64 5.0 and above
* iccia64  - Intel(R) C/C++ IA-64 (Itanium)
* msvcia64 - Microsoft Visual C++ IA-64 (Itanium)

### wce
* mingw    - MinGW GNU C x86
* mingwarm - MinGW GNU C ARM (CEGCC 0.55 and above)
* msvcarm  - Microsoft Visual C++ ARM
* poccarm  - Pelles C ARM 5.0 and above (deprecated)

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

### qnx (experimental)
* gcc      - GNU C

### vxworks (experimental)
* gcc      - GNU C
* diab     - Wind River Compiler

### minix (experimental)
* clang    - LLVM/Clang
* gcc      - GNU C

### cygwin (experimental)
* gcc      - GNU C

### beos (deprecated)
* gcc      - GNU C


# Platform Matrix

 host<br>platform | target<br>platform/compiler | target CPU
 :------- | :---------------- | :---------------------------------------
 linux    | linux/gcc         | (CPU cross-builds possible)
 linux    | linux/clang       | (CPU cross-builds possible)
 linux    | linux/icc         | (CPU cross-builds possible: x86, x86-64, ia64)
 linux    | linux/sunpro      | (CPU cross-builds possible: x86, x86-64)
 linux    | linux/open64      | (CPU cross-builds possible: x86-64, ia64, ...)
 linux    | wce/mingwarm      | arm
 linux    | wce/mingw         | x86
 linux    | win/mingw         | x86
 linux    | win/mingw64       | x86-64
 linux    | win/watcom        | x86
 linux    | os2/watcom        | x86
 linux    | dos/watcom        | x86
 linux    | dos/djgpp         | x86
 linux    | android/gcc       | x86
 linux    | android/gccarm    | arm
 linux    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
 linux    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
 win      | win/clang         | x86
 win      | win/clang64       | x86-64
 win      | win/mingw         | x86
 win      | win/mingw64       | x86-64
 win      | win/msvc          | x86
 win      | win/msvc64        | x86-64
 win      | wce/mingwarm      | arm
 win      | wce/msvcarm       | arm
 win      | dos/djgpp         | x86    (on Windows x86 hosts only)
 win      | dos/watcom        | x86
 win      | os2/watcom        | x86
 win      | linux/watcom      | x86
 win      | android/gcc       | x86
 win      | android/gccarm    | arm
 win      | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
 win      | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)
 win      | cygwin/gcc        | x86
 win      | win/clang-cl      | x86    (experimental)
 win      | win/clang-cl64    | x86-64 (experimental)
 win      | win/icc           | x86    (experimental)
 win      | win/icc64         | x86-64 (experimental)
 win      | win/watcom        | x86    (experimental)
 win      | win/bcc           | x86    (deprecated)
 win      | win/bcc64         | x86-64 (deprecated)
 win      | win/iccia64       | ia64   (deprecated)
 win      | win/msvcia64      | ia64   (deprecated)
 win      | win/pocc          | x86    (deprecated)
 win      | win/pocc64        | x86-64 (deprecated)
 win      | wce/poccarm       | arm    (deprecated)
 os2      | os2/gcc           | x86
 os2      | os2/watcom        | x86
 os2      | win/watcom        | x86
 os2      | dos/watcom        | x86
 os2      | linux/watcom      | x86
 darwin   | darwin/clang      | (CPU cross-builds possible: x86, x86-64, unibin)
 darwin   | darwin/gcc        | (CPU cross-builds possible: x86, x86-64, ppc, ppc64, unibin)
 darwin   | darwin/icc        | (CPU cross-builds possible: x86, x86-64)
 darwin   | wce/mingwarm      | arm
 darwin   | wce/mingw         | x86
 darwin   | win/mingw         | x86
 darwin   | win/mingw64       | x86-64
 darwin   | dos/djgpp         | x86
 darwin   | android/gcc       | x86
 darwin   | android/gccarm    | arm
 bsd      | bsd/gcc           | (CPU cross-builds possible)
 bsd      | bsd/clang         | (CPU cross-builds possible)
 bsd      | bsd/pcc           | (experimental)
 bsd      | wce/mingwarm      | arm
 bsd      | wce/mingw         | x86
 bsd      | win/mingw         | x86
 bsd      | dos/djgpp         | x86
 hpux     | hpux/gcc          | (CPU cross-builds possible)
 qnx      | qnx/gcc           | (CPU cross-builds possible - not tested)
 beos     | beos/gcc          | x86
 hpux     | wce/mingwarm      | arm
 hpux     | wce/mingw         | x86
 hpux     | win/mingw         | x86
 hpux     | dos/djgpp         | x86
 minix    | minix/clang       | x86
 minix    | minix/gcc         | x86
 aix      | aix/gcc           | (CPU cross-builds possible: ppc, ppc64)
 sunos    | sunos/gcc         | (CPU cross-builds possible)
 sunos    | sunos/sunpro      | (CPU cross-builds possible: x86, x86-64, sparc32, sparc64)
 sunos    | wce/mingwarm      | arm
 sunos    | wce/mingw         | x86
 sunos    | win/mingw         | x86
 sunos    | dos/djgpp         | x86
 sunos    | vxworks/gcc       | (CPU cross-builds possible: x86, arm, mips, ppc)
 sunos    | vxworks/diab      | (CPU cross-builds possible: x86, arm, mips, ppc, sparc)

Supported shells per host platforms:

* \*nix / POSIX shell
* win  / NT shell (`cmd.exe`)
* win  / POSIX shell (MSYS2 `sh.exe`)
* win  / MS-DOS shell (`command.com`)
* dos  / MS-DOS shell (`command.com`)
* dos  / POSIX shell (`bash.exe`)
* os/2 / OS/2 shell (`cmd.exe`)
* os/2 / POSIX shell (`bash.exe`)


# External Links

* C/C++ Compilers/Shells:

     * [LLVM/Clang](https://releases.llvm.org/) [multi-platform, free software, open source]
     * LLVM/Clang via [MSYS2](https://www.msys2.org/) (recommended) [win, multi-platform, free software, open source]
        * MinGW-w64 below + `pacman --sync mingw-w64-{i686,x86_64}-clang`
        * <https://stackoverflow.com/questions/25019057/how-are-msys-msys2-and-msysgit-related-to-each-other>
     * MinGW-w64 via [MSYS2](https://www.msys2.org/) [win, free software, open source]
        * `pacman --sync git base-devel msys2-devel mingw-w64-{i686,x86_64}-toolchain`
     * [MinGW-w64](https://sourceforge.net/projects/mingw-w64/) ([more](https://en.wikipedia.org/wiki/MinGW#MinGW-w64)) [win, \*nix, free software, open source]
        * [64-bit: threads-posix, seh](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win64/Personal%20Builds/mingw-builds/)
        * [32-bit: threads-posix, dwarf-2](https://sourceforge.net/projects/mingw-w64/files/Toolchains%20targetting%20Win32/Personal%20Builds/mingw-builds/)
     * [Dr. Mingw](https://github.com/jrfonseca/drmingw) Just-in-Time debugger [win, free software, open source]
        * MSYS2 package: mingw-w64-{i686,x86_64}-drmingw
     * [Xcode](https://apps.apple.com/app/xcode/id497799835) / [Command Line Tools for Xcode](https://developer.apple.com/downloads/) [darwin, zero price, proprietary with open source components]
     * [MS Windows SDK](https://developer.microsoft.com/windows/downloads/sdk-archive/) [zero price, proprietary]
     * [MS Visual C++ Build Tools](https://go.microsoft.com/fwlink/?LinkId=691126) [win, zero price, proprietary]
     * [MS Visual Studio Community](https://visualstudio.microsoft.com/vs/express/) [win, zero price, proprietary]
     * [MS Windows Mobile SDK](https://www.microsoft.com/download/details.aspx?id=42) [wce, zero price, proprietary]
     * [MinGW CEGCC](https://sourceforge.net/projects/cegcc/files/cegcc/) [win, \*nix, free software, open source]
     * [Open Watcom](https://github.com/open-watcom) [multi-platform, free software, open source]
     * [Intel Compiler](https://software.intel.com/c-compilers) [multi-platform, commercial, proprietary]
     * [Cygwin](https://cygwin.com/) [win, free software, open source]

* Libraries:

     * `HB_WITH_PCRE2`, `HB_WITH_PCRE` - [Perl Compatible Regular Expressions](https://pcre.org/) [multi-platform, free software, open source]
     * `HB_WITH_PNG` - [libpng](https://github.com/glennrp/libpng) [multi-platform, free software, open source]
     * `HB_WITH_WATT` - Watt-32 (TCP/IP sockets) [dos, free software, open source, vendored]
     * `HB_WITH_ZLIB` - [zlib](https://zlib.net/) [multi-platform, free software, open source]

* Tools:

     * [Git](https://git-scm.com/) (2.2.0 or upper) [multi-platform, free software, open source]
        * on Windows:
           * <https://gitforwindows.org/>
           * via Windows Subsystem for Linux on Windows 10 Anniversary Update
     * [AppVeyor CI](https://www.appveyor.com/) [continuous integration, web service, free plan available]
     * GNU Bison (grammar parser generator) [multi-platform, free software, open source]
        * Windows binary: See at Git or MSYS2.
     * [Cppcheck](https://github.com/danmar/cppcheck) (static analysis) [multi-platform, free software, open source]
     * [Valgrind](https://valgrind.org/) (dynamic executable analysis tool) [linux, darwin, free software, open source]
     * [Uncrustify](https://github.com/uncrustify/uncrustify) (source formatter) [multi-platform, free software, open source]
     * [UPX](https://upx.github.io/) (executable compressor) [multi-platform, free software, open source]
     * [GNU Make](https://www.gnu.org/software/make/) [multi-platform, free software, open source]

* Package searches

     * [Repology](https://repology.org/) (General)
     * deb ([Debian](https://packages.debian.org/search))
     * deb ([Ubuntu](https://packages.ubuntu.com/))
     * rpm ([Fedora](https://apps.fedoraproject.org/packages/))
     * pacman ([Arch Linux](https://www.archlinux.org/packages/))
     * [pkgng](https://www.freebsd.org/ports/), [ports](https://www.freshports.org/) (FreeBSD)
     * [Homebrew](https://formulae.brew.sh/) (macOS)
     * [MSYS2](https://github.com/Alexpux/MINGW-packages) (Windows)

* Documentation:

     * [Netiquette Guidelines](https://tools.ietf.org/html/rfc1855)
     * [Getting Started with Git](https://git-scm.com/book/en/Getting-Started-First-Time-Git-Setup)
     * [Pro Git](https://git-scm.com/book) [free book]
     * Using gettext (`.po` files)
       * <https://docs.transifex.com/formats/gettext>
       * <https://web.archive.org/web/20160427125642/heiner-eichmann.de/autotools/using_gettext.html>
     * [Markdown](https://daringfireball.net/projects/markdown/syntax)

* Community forums:

  * General:
     * [English](https://groups.google.com/forum/#!forum/harbour-users)
     * [Italian](https://groups.google.com/forum/#!forum/harbourita)
     * [Portuguese](https://pctoledo.websiteseguro.com/forum/viewforum.php?f=4)
     * [Russian](https://clipper.borda.ru/?0-4)

  * Product-oriented:
     * [Harbour mainline development](https://groups.google.com/forum/#!forum/harbour-devel)
     * [hbqt (GUI)](https://groups.google.com/forum/#!forum/qtcontribs)
     * [hwgui (GUI)](https://sourceforge.net/p/hwgui/mailman/hwgui-developers/)
     * [xHarbour fork](https://groups.google.com/forum/#!forum/comp.lang.xharbour)

# Harbour Links

  * [Homepage](https://vszakats.github.io/hb/)
  * [How to contribute](CONTRIBUTING.md)
  * [Source code](https://github.com/vszakats/hb)
  * [Issues](https://github.com/vszakats/hb/issues)
  * [Localization](https://www.transifex.com/harbour/harbour/) (Resource [hbmk2-vsz](https://www.transifex.com/harbour/harbour/hbmk2-vsz/) (requires login))
  * Documents:
     * [hbmk2 documentation](utils/hbmk2/doc/hbmk2.en.md)
     * [hbrun documentation](contrib/hbrun/doc/hbrun.en.md)
     * [ChangeLog](ChangeLog.txt?raw=true)
     * Comparing [Harbour with xHarbour](doc/xhb-diff.txt?raw=true)
     * CA-Cl*pper 5.3 [online documentation](https://harbour.github.io/ng/c53g01c/menu.html)
     * Harbour [online documentation](https://harbour.github.io/doc/)
     * Harbour [internal documents](doc/)
     * [Harbour for Beginners](https://www.kresin.ru/en/hrbfaq_3.html) &mdash; by Alexander Kresin
     * [Harbour Wiki](https://github.com/Petewg/V-harbour-core/wiki) &mdash; by Pete D
     * [Harbour Magazine](https://medium.com/harbour-magazine) &mdash; by José Luis Sánchez
     * [Wikipedia](https://en.wikipedia.org/wiki/Harbour_compiler)
     * [Stack Overflow](https://stackoverflow.com/questions/tagged/clipper)


# Guarantees and Liability

   This document and all other parts of Harbour are distributed in the
   hope they will be useful, but WITHOUT GUARANTEE that they are complete,
   accurate, non-infringing or usable for any purpose whatsoever.
   Contributors are NOT LIABLE for any damages that result from using
   Harbour in any ways. For more legal details, see [LICENSE](LICENSE.txt).

   If you feel you can make Harbour better: contribute.
   [See how](CONTRIBUTING.md).

   Information in this document is subject to change without notice and does
   not represent any future commitment by the participants of the project.

   This and related documents use the term "recommended" for practices and
   tools *tested most*, *focused on*, *used and deployed* by the
   maintainer/developer of this fork. While this is strongly believed to
   result in the best Harbour experience for most people, it's a subjective
   decision. If you don't like it, use what fits you best.

---
This document Copyright &copy;&nbsp;2009&ndash;present [Viktor Szakats](https://vsz.me/hb)<br>
[![Creative Commons Attribution-ShareAlike 4.0](https://mirrors.creativecommons.org/presskit/buttons/80x15/svg/by-sa.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
