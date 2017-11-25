ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbzlib

HB_BUILD_WARN := no
HB_BUILD_MODE := c

# Ugly workaround to avoid fatal error when building for pocc64 (6.x)
ifneq ($(HB_COMPILER),pocc64)
   HB_CFLAGS_DYN := -DZLIB_DLL
endif

C_SOURCES := \
   adler32.c \
   compress.c \
   crc32.c \
   deflate.c \
   infback.c \
   inffast.c \
   inflate.c \
   inftrees.c \
   trees.c \
   uncompr.c \
   zutil.c \

ifeq ($(filter $(HB_COMPILER),msvcarm),)
C_SOURCES += \
   gzclose.c \
   gzlib.c \
   gzread.c \
   gzwrite.c \

endif

ifneq ($(HB_HAS_ZLIB_LOCAL),)
   HB_CFLAGS += -DNOBYFOUR
   ifneq ($(filter $(HB_COMPILER),mingw mingw64),)
      # Required to enable 64-bit lseek() since version 1.2.9
      HB_CFLAGS += -D_LARGEFILE64_SOURCE=1 -D_LFS64_LARGEFILE=1
   endif
   ifneq ($(filter $(HB_PLATFORM),wce),)
      HB_CFLAGS += -D_WINCE
   endif
   ifneq ($(filter $(HB_COMPILER),msvcarm),)
      HB_CFLAGS += -DZ_SOLO
   endif
   ifeq ($(filter $(HB_PLATFORM),win wce),)
      HB_CFLAGS += -DHAVE_UNISTD_H
   else
      ifneq ($(filter $(HB_COMPILER),watcom),)
         HB_CFLAGS += -DHAVE_UNISTD_H
      endif
   endif
   ifneq ($(filter $(HB_COMPILER),msvc msvc64 msvcia64 msvcarm),)
      ifeq ($(filter $(__HB_COMPILER_VER),1200 1300 1310 1400 1500 1600),)
         HB_CFLAGS += -DWINAPI_FAMILY=WINAPI_FAMILY_DESKTOP_APP -D_CRT_SECURE_NO_WARNINGS
      endif
   endif
   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := local copy unused
   include $(TOP)$(ROOT)config/none.mk
endif

# URL    https://github.com/madler/zlib/archive/v1.2.11.tar.gz
# DIFF   zlib.diff
#
# MAP    README
# MAP    adler32.c
# MAP    compress.c
# MAP    crc32.c
# MAP    crc32.h
# MAP    deflate.c
# MAP    deflate.h
# MAP    gzclose.c
# MAP    gzguts.h
# MAP    gzlib.c
# MAP    gzread.c
# MAP    gzwrite.c
# MAP    infback.c
# MAP    inffast.c
# MAP    inffast.h
# MAP    inffixed.h
# MAP    inflate.c
# MAP    inflate.h
# MAP    inftrees.c
# MAP    inftrees.h
# MAP    trees.c
# MAP    trees.h
# MAP    uncompr.c
# MAP    zconf.h
# MAP    zlib.h
# MAP    zutil.c
# MAP    zutil.h
