## Harbour @HB_VERSION@ (@HB_VER_COMMIT_ID_SHORT@)

Source archives and Windows binaries are available for download:

  <@HB_VER_ORIGIN_URL@releases>

## Release Notes

### Package content

* Harbour tools (requires 64-bit OS)
* Static and dynamic libraries to create 32-bit and 64-bit apps
* Static and dynamic libraries of `libcurl`, `openssl`, `libssh2`, `nghttp2`
  for 32-bit and 64-bit apps
* Example and test sources
* [`getsrc.sh`](https://github.com/vszakats/harbour-core/blob/master/package/getsrc.sh)
  script to download source code
* `BUILD*.txt` with build details in Harbour root directory

### Usage

1. Unpack to any directory <sup>[1](#1)</sup></sup>
2. Install MSYS2 by following steps at <https://msys2.github.io/><br>
   **Make sure you update MSYS2 to its current latest version, as described there.**
3. Install MinGW-w64 and LLVM/Clang C compiler toolchain on the MSYS2 console:
   ```shell
   pacman --noconfirm -S git {base,msys2}-devel mingw-w64-{i686,x86_64}-{clang,toolchain}
   ```
4. Launch the Command Prompt
5. Build a 32-bit test app:
   ```batch
   set PATH=C:\msys64\mingw32\bin;C:\hb\bin;%PATH%
   hbmk2 C:\hb\tests\hello.prg -run
   ```
6. Build a 64-bit test app:
   ```batch
   set PATH=C:\msys64\mingw64\bin;C:\hb\bin;%PATH%
   hbmk2 C:\hb\tests\hello.prg -run
   ```
7. Further hints in section **Build Your Own Harbour App** of
   [`README.md`](https://github.com/vszakats/harbour-core#build-your-own-harbour-app)
8. To customize/build/rebuild Harbour components, run
   `C:\hb\getsrc.sh` to download sources and continue as described in
   section **How to Do a Partial Build** of
   [`README.md`](https://github.com/vszakats/harbour-core#how-to-do-a-partial-build)

<a name="1"><sup>1</sup></a> This document uses `C:\hb`<br>

---
Viktor Szakats
