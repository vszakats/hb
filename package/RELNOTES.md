## Harbour {HB_VERSION} ({HB_VER_COMMIT_ID_SHORT})

Source archives and Windows binaries are available for download:

  <{HB_VER_ORIGIN_URL}releases>

## Release Notes

### Package content

* Harbour tools (requires 64-bit OS)
* Static and dynamic libraries to create 32-bit and 64-bit apps
* Static and dynamic libraries of `libcurl`, `openssl`, `libssh2`, `nghttp2`
  for 32-bit and 64-bit apps
* Example and test sources
* Script to download complete source code
* `BUILD*.txt` in package root with build details

### Usage

1. Unpack to any directory
2. Install MSYS2 by following steps at <https://msys2.github.io/><br>
   **Make sure you update MSYS2 to its current latest version, as described.**
3. Install MinGW-w64 + LLVM/Clang C compiler toolchain on the MSYS2 console:
   ```shell
   pacman --noconfirm -S git {base,msys2}-devel mingw-w64-{i686,x86_64}-{clang,toolchain}
   ```
4. Launch the Command Prompt
5. Build a 32-bit test app:
   ```batch
   set PATH=<path-to-msys2>\mingw32\bin;<path-to-harbour>\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
6. Build a 64-bit test app:
   ```batch
   set PATH=<path-to-msys2>\mingw64\bin;<path-to-harbour>\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
7. Further hints in section **Build Your Own Harbour App** of `README.md`
8. To customize/build/rebuild Harbour components, run
   `<path-to-harbour>/getsrc.sh` to download sources and continue as
   described in section **How to Do a Partial Build** of `README.md`

---
Viktor Szakats
