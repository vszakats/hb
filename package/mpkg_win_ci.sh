#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2015-2016 Viktor Szakats (vszakats.net/harbour)
# See LICENSE.txt for licensing terms.
# ---------------------------------------------------------------

[ "${CI}" = 'True' ] || exit

cd "$(dirname "$0")/.." || exit

_BRANCH="${APPVEYOR_REPO_BRANCH}${TRAVIS_BRANCH}${CI_BUILD_REF_NAME}${GIT_BRANCH}"
_BRANC4="$(echo "${_BRANCH}" | cut -c -4)"
_ROOT="$(realpath '.')"

# Don't remove these markers.
#hashbegin
export NGHTTP2_VER='1.14.1'
export NGHTTP2_HASH_32='e7fd2f98646d8d295130e7aa1321a04aa389cc12d43dc6d77ec1a81249334b30'
export NGHTTP2_HASH_64='473eeb5da04ae7566d7b3cd7b6efa87010bd48b44df91308b96177aa56ce3bf0'
export OPENSSL_VER='1.1.0a'
export OPENSSL_HASH_32='d246af40720e92466c8b73d42318f9197255dc7292204304a82e9cff06329a0b'
export OPENSSL_HASH_64='dff22362a4277eee5792e03a6fac4c86a35d7266be32c1d67fabe2c2c5cb6e64'
export LIBSSH2_VER='1.7.0'
export LIBSSH2_HASH_32='da94ba4061c27de3f6616985d7ca736b4ef7a40c3aafbf016e273f4ac9828049'
export LIBSSH2_HASH_64='a0bb79042c8f924e5f9976774e6fe2a36d99697a1d023679291247e492f2ed03'
export CURL_VER='7.50.3'
export CURL_HASH_32='73608a4cea59b9449fc4ef6ae9dd220b4f4391866fa406c435a30ac478492bed'
export CURL_HASH_64='8b7b37a9bb1e4e52a64d90344495b1b1b48bf853e7efb056c9ddfbe3cea57e16'
#hashend

# Install/update MSYS2 packages required for completing the build

pacman --noconfirm --noprogressbar -S --needed p7zip mingw-w64-{i686,x86_64}-{jq,osslsigncode}

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_BUNDLE_3RDLIB='yes'

[ "${_BRANC4}" = 'msvc' ] || "$(dirname "$0")/mpkg_win_dl.sh" || exit

export HB_VF='snapshot'
export HB_RT="${_ROOT}"
export HB_MKFLAGS="HB_VERSION=${HB_VF}"
export HB_BASE='64'
# export HB_DIR_7Z="${HB_RT}/7z/"
# export HB_DIR_UPX="${HB_RT}/upx/"
_ori_path="${PATH}"

if [ -n "${HB_CI_THREADS}" ] ; then
   export HB_MKFLAGS="${HB_MKFLAGS} -j ${HB_CI_THREADS}"
fi

# common settings

[ "${_BRANCH#*prod*}" != "${_BRANCH}" ] && export HB_BUILD_CONTRIBS='hbrun hbformat/utils hbct hbcurl hbhpdf hbmzip hbwin hbtip hbssl hbexpat hbmemio rddsql hbzebra sddodbc hbunix hbmisc hbcups hbtest hbtcpio hbcomio hbcrypto hbnetio hbpipeio hbgzio hbbz2io hbicu'
export HB_BUILD_STRIP='bin'
export HB_BUILD_PKG='yes'
export _HB_BUILD_PKG_ARCHIVE='no'

# can disable to save time/space

[ "${_BRANC4}" = 'msvc' ] || export _HB_BUNDLE_3RDLIB='yes'
export HB_INSTALL_3RDDYN='yes'
export HB_BUILD_CONTRIB_DYN='yes'
export HB_BUILD_POSTRUN='"./hbmk2 --version" "./hbtest -noenv" "./hbspeed --noenv --stdout"'

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_PKG_DEBUG='yes'
# export _HB_BUNDLE_3RDLIB='yes'

# decrypt code signing key

export HB_CODESIGN_KEY="$(realpath './package/vszakats.p12')"
(
   set +x
   if [ -n "${HB_CODESIGN_GPG_PASS}" ] ; then
      gpg --batch --passphrase "${HB_CODESIGN_GPG_PASS}" -o "${HB_CODESIGN_KEY}" -d "${HB_CODESIGN_KEY}.asc"
   fi
)
[ -f "${HB_CODESIGN_KEY}" ] || unset HB_CODESIGN_KEY

# mingw

if [ "${_BRANC4}" != 'msvc' ] ; then

   # LTO is broken as of mingw 6.1.0
#  [ "${_BRANCH#*prod*}" != "${_BRANCH}" ] && _HB_USER_CFLAGS="${_HB_USER_CFLAGS} -flto -ffat-lto-objects"
   [ "${HB_BUILD_MODE}" = 'cpp' ] && export HB_USER_LDFLAGS="${HB_USER_LDFLAGS} -static-libstdc++"

   readonly _msys_mingw32='/mingw32'
   readonly _msys_mingw64='/mingw64'

   export HB_DIR_MINGW="${HB_RT}/mingw64"
   if [ -d "${HB_DIR_MINGW}/bin" ] ; then
      # Use the same toolchain for both targets
      export HB_DIR_MINGW_32="${HB_DIR_MINGW}"
      export HB_DIR_MINGW_64="${HB_DIR_MINGW}"
      _build_info_32='BUILD-mingw.txt'
      _build_info_64=/dev/null
   else
      export HB_DIR_MINGW_32="${_msys_mingw32}"
      export HB_DIR_MINGW_64="${_msys_mingw64}"
      [ "${HB_BASE}" != '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_32}"
      [ "${HB_BASE}"  = '64' ] && HB_DIR_MINGW="${HB_DIR_MINGW_64}"
      _build_info_32='BUILD-mingw32.txt'
      _build_info_64='BUILD-mingw64.txt'
   fi

   export HB_DIR_OPENSSL_32="${HB_RT}/openssl-mingw32/"
   export HB_DIR_OPENSSL_64="${HB_RT}/openssl-mingw64/"
   export HB_DIR_LIBSSH2_32="${HB_RT}/libssh2-mingw32/"
   export HB_DIR_LIBSSH2_64="${HB_RT}/libssh2-mingw64/"
   export HB_DIR_NGHTTP2_32="${HB_RT}/nghttp2-mingw32/"
   export HB_DIR_NGHTTP2_64="${HB_RT}/nghttp2-mingw64/"
   export HB_DIR_CURL_32="${HB_RT}/curl-mingw32/"
   export HB_DIR_CURL_64="${HB_RT}/curl-mingw64/"

   #

   # Disable picking MSYS2 packages for now
   export HB_BUILD_3RDEXT='no'

   export HB_WITH_CURL="${HB_DIR_CURL_32}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_32}include"
   _inc="${_msys_mingw32}/include"
   export HB_WITH_CAIRO="${_inc}/cairo"
   export HB_WITH_FREEIMAGE="${_inc}"
   export HB_WITH_GD="${_inc}"
   export HB_WITH_GS="${_inc}/ghostscript"
   export HB_WITH_GS_BIN="${_inc}/../bin"
   export HB_WITH_ICU="${_inc}"
   export HB_WITH_MYSQL="${_inc}/mysql"
   export HB_WITH_PGSQL="${_inc}"
   export HB_USER_CFLAGS="${_HB_USER_CFLAGS}"
   [ "${HB_BUILD_MODE}" != 'cpp' ] && export HB_USER_CFLAGS="${HB_USER_CFLAGS} -fno-asynchronous-unwind-tables"
   export PATH="${HB_DIR_MINGW_32}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_32}"
   # shellcheck disable=SC2086
   mingw32-make install ${HB_MKFLAGS} HB_COMPILER=mingw HB_CPU=x86 || exit 1

   export HB_WITH_CURL="${HB_DIR_CURL_64}include"
   export HB_WITH_OPENSSL="${HB_DIR_OPENSSL_64}include"
   _inc="${_msys_mingw64}/include"
   export HB_WITH_CAIRO="${_inc}/cairo"
   export HB_WITH_FREEIMAGE="${_inc}"
   export HB_WITH_GD="${_inc}"
   export HB_WITH_GS="${_inc}/ghostscript"
   export HB_WITH_GS_BIN="${_inc}/../bin"
   export HB_WITH_ICU="${_inc}"
   export HB_WITH_MYSQL="${_inc}/mysql"
   export HB_WITH_PGSQL="${_inc}"
   export HB_USER_CFLAGS="${_HB_USER_CFLAGS}"
   export PATH="${HB_DIR_MINGW_64}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_64}"
   # shellcheck disable=SC2086
   mingw32-make clean ${HB_MKFLAGS} HB_COMPILER=mingw64 HB_CPU=x86_64 || exit 1
   mingw32-make install ${HB_MKFLAGS} HB_COMPILER=mingw64 HB_CPU=x86_64 || exit 1
fi

# msvc

if [ "${_BRANC4}" = 'msvc' ] ; then

   export PATH="${_ori_path}"
   export HB_USER_CFLAGS=
   export HB_USER_LDFLAGS=
   export HB_WITH_CURL=
   export HB_WITH_OPENSSL=

 # export _HB_MSVC_ANALYZE='yes'

   export HB_COMPILER_VER

   [ "${_BRANCH}" = 'msvc2008' ] && HB_COMPILER_VER='1500' && _VCVARSALL='9.0'
   [ "${_BRANCH}" = 'msvc2010' ] && HB_COMPILER_VER='1600' && _VCVARSALL='10.0'
   [ "${_BRANCH}" = 'msvc2012' ] && HB_COMPILER_VER='1700' && _VCVARSALL='11.0'
   [ "${_BRANCH}" = 'msvc2013' ] && HB_COMPILER_VER='1800' && _VCVARSALL='12.0'
   [ "${_BRANCH}" = 'msvc2015' ] && HB_COMPILER_VER='1900' && _VCVARSALL='14.0'
   [ "${_BRANCH}" = 'msvc15'   ] && HB_COMPILER_VER='2000' && _VCVARSALL='15.0'

   export _VCVARSALL="%ProgramFiles(x86)%\Microsoft Visual Studio ${_VCVARSALL}\VC\vcvarsall.bat"

   if [ -n "${_VCVARSALL}" ] ; then
      cat << EOF > _make.bat
         call "%_VCVARSALL%" x86
         win-make.exe install %HB_MKFLAGS% HB_COMPILER=msvc
EOF
      ./_make.bat
      rm _make.bat
   fi

   # 64-bit target not supported by these MSVC versions
   [ "${_BRANCH}" = 'msvc2008' ] && _VCVARSALL=
   [ "${_BRANCH}" = 'msvc2010' ] && _VCVARSALL=

   if [ -n "${_VCVARSALL}" ] ; then
      cat << EOF > _make.bat
         call "%_VCVARSALL%" x86_amd64
         win-make.exe clean %HB_MKFLAGS% HB_COMPILER=msvc64
         win-make.exe install %HB_MKFLAGS% HB_COMPILER=msvc64
EOF
      ./_make.bat
      rm _make.bat
   fi
fi

# packaging

[ "${_BRANC4}" = 'msvc' ] || "$(dirname "$0")/mpkg_win.sh"
