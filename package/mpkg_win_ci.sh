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
export NGHTTP2_VER='1.13.0'
export NGHTTP2_HASH_32='fe3a65d35c9bf223f713ccea50df96b35d782773a032c15a582fe15301f23f74'
export NGHTTP2_HASH_64='04e4df55f042e4eb953d98cfaefa9ddb8599af08e961305f87462e3e48860d9e'
export OPENSSL_VER='1.0.2h'
export OPENSSL_HASH_32='10bfb57ab559005f6ddd4d96f4156bc7875ccfdd2552b7fb48f52c3616a3bfd9'
export OPENSSL_HASH_64='12fe2d26ec8c028f015781804e35163710badd626c8ff2d4a6260b1b27355081'
export LIBSSH2_VER='1.7.0'
export LIBSSH2_HASH_32='50e84ed923cea3790759dfe8b2e6e81fc59bd452d2cabbd13ba231c6b12f8518'
export LIBSSH2_HASH_64='f14cc91dd461ff41298aaf5b1202bcc1ce3092e511f8592320ca0f87ff215e96'
export CURL_VER='7.50.0'
export CURL_HASH_32='e906775997b643dd7e3b6366ef39cf921562b3aceb849efcde1bd5455c263e6d'
export CURL_HASH_64='368d64f567292d771768379901f8936139eb59525f9f9f2245f057de2b1146a0'
#hashend

# Update/install MSYS2 pacman packages

pacman --noconfirm --noprogressbar -S --needed p7zip mc mingw-w64-{i686,x86_64}-{jq,osslsigncode}

# Dependencies of the default (full) list of contribs
if [ "${_BRANCH#*prod*}" = "${_BRANCH}" ] ; then
   pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{cairo,freeimage,gd,ghostscript,libmariadbclient,postgresql}
#  pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-qt5
fi

pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-icu

# Dependencies of 'prod' builds (we use our own builds instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{curl,openssl}

# Dependencies of 'prod' builds (we use vendored sources instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{bzip2,expat,libharu,lzo2,sqlite3}

# Dependencies of Harbour core (we use vendored sources instead for now)
# pacman --noconfirm --noprogressbar -S --needed mingw-w64-{i686,x86_64}-{libpng,libtiff,pcre,zlib}

# debug

# export HB_BUILD_CONTRIBS='no'
# export HB_MKFLAGS="${HB_MKFLAGS} HB_BUILD_OPTIM=no"
# export HB_BUILD_VERBOSE='yes'
# export _HB_BUNDLE_3RDLIB='yes'

[ "${_BRANC4}" = 'msvc' ] || "$(dirname "$0")/mpkg_win_dl.sh" || exit

export HB_VF='daily'
export HB_RT="${_ROOT}"
export HB_MKFLAGS="HB_VERSION=${HB_VF}"
[ "${_BRANCH#*prod*}" != "${_BRANCH}" ] && export HB_BASE='64'
# [ "${HB_BASE}" != '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All.sfx"
# [ "${HB_BASE}"  = '64' ] && export HB_SFX_7Z="${HB_RT}/7zsfx/7zsd_All_x64.sfx"
export HB_DIR_7Z="${HB_RT}/7z/"
export HB_DIR_UPX="${HB_RT}/upx/"
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

# mingw

if [ "${_BRANC4}" != 'msvc' ] ; then

   # LTO is broken as of mingw 6.1.0
#  [ "${_BRANCH#*prod*}" != "${_BRANCH}" ] && export HB_USER_CFLAGS="${HB_USER_CFLAGS} -flto -ffat-lto-objects"
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
   export HB_BUILD_3RDEXT=no

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
   export PATH="${HB_DIR_MINGW_32}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_32}"
   osslsigncode -v
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
   export PATH="${HB_DIR_MINGW_64}/bin:${_ori_path}"
   gcc -v 2> "${_build_info_64}"
   osslsigncode -v
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
