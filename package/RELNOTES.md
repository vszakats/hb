## Harbour {HB_VERSION} ({HB_VER_COMMIT_ID_SHORT})

Source archives and Windows binaries are available for download:

  <{HB_VER_ORIGIN_URL}releases>

## Release Notes

### Package content

* Harbour tools (requires 64-bit OS)
* static/shared libraries for 32-bit and 64-bit targets
* static/shared libraries of libcurl, openssl, libssh2, nghttp2
  for 32-bit and 64-bit targets
* example/test sources
* script to download complete source code
* `BUILD*.txt` in package root with build/package details

### Usage

1. Install/unpack to any directory.
2. Launch a Command Prompt.
3. Add Harbour to `PATH`: `set PATH=<path-to-harbour>\bin;%PATH%`
4. Install MSYS2. Follow steps on <https://msys2.github.io/>. Make sure
   to update MSYS2 to its current latest version, as described.
5. Install MinGW-w64 + LLVM/Clang C compiler toolchain using this command
   on the MSYS2 console:
   `pacman --noconfirm -S git {base,msys2}-devel mingw-w64-{i686,x86_64}-{clang,toolchain}`
6. Build test 32-bit executable:
   ```
   set PATH=<path-to-msys2>\mingw32\bin;<path-to-harbour>\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
7. Build test 64-bit executable:
   ```
   set PATH=<path-to-msys2>\mingw64\bin;<path-to-harbour>\bin;%PATH%
   hbmk2 <path-to-harbour>/tests/hello.prg -run
   ```
8. Further hints in section **Build Your Own Harbour App** of `README.md`.
9. To customize/build/rebuild Harbour components as you like, download
   sources using `getsrc.sh` and continue as described in section
   **How to Do a Partial Build** of `README.md`.

---
Viktor Szakats
