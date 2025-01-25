# Contributing to this fork of Harbour

# Table of Contents

1. [How to Share](#how-to-share)
1. [How to Get Involved](#how-to-get-involved)
1. [Troubleshooting](#troubleshooting)

---

# How to Share

  Share by **linking** to this project from your forum, blog or Twitter. Link
  to commits, issues or source files, or whatever else you find worthy to pass
  to someone. This is the easiest, most useful and self-updating way of
  referring to this project. It's also the best form of crediting and
  appreciating this work without donating or participating directly.

  Examples:

  * [commit](https://github.com/vszakats/hb/commit/35ffdc113f348fbf10203757073bbee564e4339f)
  * [commit - specific file](https://github.com/vszakats/hb/commit/35ffdc113f348fbf10203757073bbee564e4339f#diff-2)
  * [issue](https://github.com/vszakats/hb/issues/18)
  * [issue comment](https://github.com/vszakats/hb/issues/22#issuecomment-47928889)
  * [source file](tests/hello.prg)
  * [source file - specific revision](https://github.com/vszakats/hb/blob/dd2c3b3e9c0e7db7f1c18be2a079dc92f983122e/tests/hello.prg) ([more information here](https://help.github.com/articles/getting-permanent-links-to-files))
  * [source line](https://github.com/vszakats/hb/blob/dd2c3b3e9c0e7db7f1c18be2a079dc92f983122e/tests/hello.prg#L5)
  * [source section](https://github.com/vszakats/hb/blob/90ce13098244b0e27fc4b8c7af5586f892a09337/src/rtl/chruni.c#L101-L112)
  * [source directory](contrib/hbcurl/)
  * [source - specific revision](https://github.com/vszakats/hb/tree/e46f2fdb75c493ff5b4c777f5a0963d2e7d6f65b)
  * [source - specific revision .zip archive](https://github.com/vszakats/hb/archive/e46f2fdb75c493ff5b4c777f5a0963d2e7d6f65b.zip)


# How to Get Involved

- You can give feedback/suggestions by submitting an [issue](https://github.com/vszakats/hb/issues/new).
- Submit a patch:
  1. Fork the repository
  2. Create a branch: `git switch -c mypatch`
     or `git checkout -b mypatch` (for Git 2.22.x and earlier)
  3. Do commit pre-check and new log entry: `hbrun bin/commit`
  4. Commit your changes: `git commit -am "Add this feature to that module"`
  5. Push to the branch: `git push origin mypatch`
  6. Open a Pull Request
- Make sure to use the same coding/formatting style as you find in the files
  you're updating. The easiest way to achieve this is to use these commands
  to format the sources (use this with care - most sources are well-formatted,
  so make sure to only apply it to newly added or updated code sections)

        $ uncrustify -c <harbour-dir>/bin/harbour.ucf <source{.c|.h}>
        $ <harbour-dir>/bin/hbformat <source{.prg|.hb|.ch}>

- Text editor setting for Harbour files
  - Encoding is either 7-bit ASCII or [UTF-8](https://utf8everywhere.org/),
    without [BOM](https://en.wikipedia.org/wiki/Byte_order_mark)
  - Use spaces (U+0020), never tabs or non-breaking spaces
  - Remove trailing spaces from lines
  - Keep one (not zero or multiple) newline at the end of file
  - Use platform native newline (CRLF or LF)
- In the rare case you need to send something large (> 100 kB), use this
  [free service](https://transfer.sh/).
- See this good guideline on how to contribute:
  <https://github.com/necolas/issue-guidelines/blob/master/CONTRIBUTING.md>
- And these:
  - <https://opensource.guide/how-to-contribute/>
  - <https://github.com/blog/1943-how-to-write-the-perfect-pull-request>
- You can also participate in localization:<br>
  [![Localization Status](https://chart.googleapis.com/chart?chxt=y%2Cr&chd=e%3A....8J4T4T4To8UeP.AAAAAAAAAA&chco=84CCFF%2CBFE4FF%2CF4F6FB&chbh=9&chs=350x196&cht=bhs&chxl=0%3A%7CSerbian+%28Latin%29%7CLithuanian%7CChinese+%28China%29+%28GB2312%29%7CIndonesian%7CRussian%7CHungarian%7CFrench%7CItalian%7CSpanish+%28Latin+America%29%7CSpanish%7CGalician%7CGreek%7CPortuguese+%28Brazil%29%7CEnglish%7C1%3A%7C0%25%7C0%25%7C0%25%7C0%25%7C0%25%7C25%25%7C32%25%7C64%25%7C88%25%7C88%25%7C88%25%7C94%25%7C100%25%7C100%25%7C)](https://www.transifex.com/harbour/harbour/)
- If looking for known pending issues to work on, look for `TODO` and `FIXME`
  markers in the source code/ChangeLog or see this list of issues that need
  further input/contribution:
     * https://github.com/vszakats/hb/issues?q=label%3A%22help+wanted%22
     * https://github.com/harbour/core/issues?q=label%3A%22help+wanted%22


# Troubleshooting

Evaluate these points before reporting an issue:

1.  <a name="trbl-1"></a> Make sure to have carefully read this document.
2.  <a name="trbl-2"></a> Make sure to do a `make clean` before doing
    a build after refreshing the sources.
3.  <a name="trbl-3"></a> If that still fails, make sure to install fresh
    source tree in a new local directory and start over. See
    [How to Get](../README.md#how-to-get) for instructions to get the source.
    In case you installed Harbour into system locations (this used to be the
    case with some \*nix users, albeit mostly unnecessarily or wrongly - e.g.
    for unstable versions), you will need to remember cleaning off Harbour
    from these locations, too.
    Hint: Never install unstable Harbour versions to system locations.
4.  <a name="trbl-4"></a> If you are doing a cross-build, make sure to have
    rebuilt the native Harbour executables for your host platform.
    See `HB_HOST_BIN` build messages to find their location.
5.  <a name="trbl-5"></a> Keep your `PATH` clean from old, mixed compiler
    tools or other Harbour versions when building Harbour. The surest way
    to achieve this is to leave only the C compiler directory in `PATH`:

        set PATH=C:\<c-compiler-bin-dir>

    > If you use Harbour official binary distro on Windows, even above is
    > unnecessary, so avoid it.
6.  <a name="trbl-6"></a> Remove all old, unnecessary environment variables
    (for both Harbour and C compiler) from your environment. Also remove
    any custom settings for your C compiler.
    Use only those documented in this file.
7.  <a name="trbl-7"></a> Remove any Harbour build settings documented in
    [Build Options](../README.md#build-options).
8.  <a name="trbl-8"></a> Do no or only minor updates at once to the examples
    included in [Build Examples](../README.md#build-examples).
    If it doesn't work, fall back to documented examples _as is_.
9.  <a name="trbl-9"></a> If everything fails and you are to report a build
    problem to Harbour developers, make sure to include your OS
    version/language/CPU architecture, Harbour revision, C compiler
    name/release and version, environment variables and verbose log output
    containing **both STDERR and STDOUT in one combined stream**
    (use `make > log.txt 2>&1`). Enable verbose mode using
    `HB_BUILD_VERBOSE=yes` and _do not_ enable multi-threaded (parallel) build.
    Configure your tools to output English language messages using `HB_LANG=en`
    and `LANG=en_GB.UTF-8`.
    Complete log output is rarely necessary, but make sure to include the top
    of the output (lines starting with `!`) and the area where problematic
    behavior occurred _first_. Make sure to not only include a link failure or
    a make tool failure, as it's most often not enough information. Compress
    your log using zip if it is larger than 25 kB (use the extension `.zip`).
    With these, you have much better chance to get useful or any response.
10. <a name="trbl-10"></a> Do not alter the directory layout and files in
    Harbour and 3rd party packages and tools (including C compilers).
11. <a name="trbl-11"></a> If you are to report a build problem with
    a Harbour application, all the above points apply, plus make sure
    to use `hbmk2` with the `-trace` command-line option and redirect its
    output to a file (see above how). Also include your full command-line
    and any referenced build script in your report.
    It is good idea to first remove all manual references to Harbour
    core components from make-files and custom environment. E.g. it's
    common mistake to add C compiler header and/or lib dirs, Harbour core
    header and/or lib dirs, built-in constants to make-files or environment.
    No such thing is necessary as these are automatically handled by `hbmk2`.
    In other words: start simple and don't be over-busy with *fine-tuning*
    your configuration. If you need to, the problem is most probably
    elsewhere. It's also good idea to try with the latest Harbour revision or
    Harbour's mainline branch first.
12. <a name="trbl-12"></a> If you are to report a problem with Harbour itself,
    include self-contained, minimal source code example. Do not use `xhb`
    contrib library (including `hbcompat.ch`), nor any 3rd party Harbour
    libraries.
    The example should reproduce the problem using the latest Harbour revision
    (with _no_ local commits or pending local updates) at the time of the
    report. Do not post links to executables and other binary files. If
    your source contains non-ASCII and non-UTF-8 national, accented, special
    chars, make sure to mark the codepage/encoding used and use
    `Chr()`/`hb_BCode()` calls to form the strings. Use UTF-8 if possible.
    Notice that core developers are likely to run code examples as `hbrun`
    scripts for testing, so it's a good idea to make them work this way.<br>
    Also make sure not to report multiple issues under a single
    GitHub Issue.<br>
    * More on self-contained examples:
      <https://en.wikipedia.org/wiki/Minimal_working_example>
    * More on how to report issues in an effective and useful way:
      <https://www.chiark.greenend.org.uk/~sgtatham/bugs.html>
    * "How To Ask Questions The Smart Way" article by Eric S. Raymond:
      <http://catb.org/~esr/faqs/smart-questions.html>
    * "How to Ask Good Questions" by Julia Evans
      <https://jvns.ca/blog/good-questions/>
    * "Question Checklist" by Jon Skeet
      <https://codeblog.jonskeet.uk/2012/11/24/stack-overflow-question-checklist/>
    * "Does Not Work"
      <https://web.archive.org/web/20180124130721/importblogkit.com/2015/07/does-not-work/>
    * "The XY Problem"
      <https://meta.stackexchange.com/questions/66377/what-is-the-xy-problem/66378#66378>
13. <a name="trbl-13"></a> Please do not report warnings or bugs &mdash; except
    _build errors_ &mdash; in 3rd party components hosted inside the Harbour
    source tree. You can recognize these by their source path, which always
    contain a subdirectory named `/3rd/`. Report these directly to the
    maintainer of the respective component instead.
14. <a name="trbl-14"></a> If your example or report contains human readable
    text, use English only.
15. <a name="trbl-15"></a> If your example involves compatibility components,
    make sure to test it against original implementation (e.g. legacy Cl\*pper
    core language elements against real CA-Cl\*pper 5.2e or 5.3b, or `hbct`
    functions against CT3 library, etc.)
    Notice that Harbour is Cl\*pper Summer '87 compatible exactly as
    much as Cl\*pper 5.2e/5.3b is, meaning: almost, but not completely.
    For tests with Cl\*pper, use this free, open source
    [MS-DOS emulator](https://www.vdos.info/) (requires Windows/Wine).


---
This document Copyright &copy;&nbsp;2009&ndash;present Viktor Szakats<br>
[![Creative Commons Attribution-ShareAlike 4.0](https://mirrors.creativecommons.org/presskit/buttons/80x15/svg/by-sa.svg)](https://creativecommons.org/licenses/by-sa/4.0/)
