## Harbour {HB_VERSION} ({HB_VER_COMMIT_ID_SHORT})

Source archives and Windows binaries are available for download:

  <{HB_VER_ORIGIN_URL}releases>

## Release Notes

### Package content

* Harbour tools (requires 64-bit OS)
* static and dynamic libraries to create 32-bit and 64-bit applications
* static and dynamic libraries of `libcurl`, `openssl`, `libssh2`, `nghttp2`
  for 32-bit and 64-bit applications
* example/test sources
* script to download complete source code
* `BUILD*.txt` in package root with build details

### Usage

1. Unpack to any directory.
2. Launch a Command Prompt.
3. Add Harbour to `PATH`:
   ```batch
   set PATH=<path-to-harbour>\bin;%PATH%
   ```
4. Install MSYS2. Follow steps at <https://msys2.github.io/>.<br>
   **Make sure to update MSYS2 to its current latest version, as described there.**
5. Install MinGW-w64 + LLVM/Clang C compiler toolchain using this command
   on the MSYS2 console:
   ```shell
   pacman --noconfirm -S git {base,msys2}-devel mingw-w64-{i686,x86_64}-{clang,toolchain}
   ```
6. Build test 32-bit executable:
   ```batch
   set PATH=<path-to-msys2>\mingw32\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
7. Build test 64-bit executable:
   ```batch
   set PATH=<path-to-msys2>\mingw64\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
8. Further hints in section **Build Your Own Harbour App** of `README.md`.
9. To customize/build/rebuild Harbour components as you like, download
   sources using `getsrc.sh` and continue as described in section
   **How to Do a Partial Build** of `README.md`.

---
Viktor Szakats
