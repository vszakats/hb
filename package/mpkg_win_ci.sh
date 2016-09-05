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
export NGHTTP2_VER='1.14.0'
export NGHTTP2_HASH_32='ef04fbb8e4bd0a7e967b14c454de22127f9e652e358eac15c5b80cef2614b429'
export NGHTTP2_HASH_64='58aec187856116747c6ca933b1ddf9c618eb08f45253620b59c00ebbe2bb6d66'
export OPENSSL_VER='1.1.0'
export OPENSSL_HASH_32='7cb9024c156acd268e6a847abe925f185e6ae06e21887c7caf55d7cd73626987'
export OPENSSL_HASH_64='d0622634a14b6cd9afdfc319ef721a91970fa0cd800be7ed8af946afa8e2ea9b'
export LIBSSH2_VER='1.7.0'
export LIBSSH2_HASH_32='661c5604abfca6bd83cd577d3945176e922b04da89d852ca4a74adf6adfd8ca2'
export LIBSSH2_HASH_64='95fb3a5e2e10a0dd0411eaba5052309f80885aea1d91fd38ebdd0f98f8bdd574'
export CURL_VER='7.50.1'
export CURL_HASH_32='fd8856ed8e6e2cc3f8a901354495a18e737d4e609f39d026cc31498ae851d026'
export CURL_HASH_64='b5a1f49112b20584f71367031f04deb338921089a689108952751ddca210355b'
#hashend

# Install/update MSYS2 packages required for completing the build

pacman --noconfirm --noprogressbar -S --needed p7zip mingw-w64-{i686,x86_64}-{jq,oniguruma,osslsigncode}

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
