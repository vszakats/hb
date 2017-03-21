ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbpcre2

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   chartabs.c \
   p2autopo.c \
   p2compil.c \
   p2config.c \
   p2contex.c \
   p2dfamat.c \
   p2error.c \
   p2maktab.c \
   p2match.c \
   p2matchd.c \
   p2newlin.c \
   p2ord2ut.c \
   p2patter.c \
   p2serial.c \
   p2string.c \
   p2study.c \
   p2substi.c \
   p2substr.c \
   p2tables.c \
   p2ucd.c \
   p2validu.c \
   p2xclass.c \

ifneq ($(HB_HAS_PCRE2_LOCAL),)

   HB_CFLAGS += -DPCRE2_CODE_UNIT_WIDTH=8
   # only needed for win/wce, for other platforms this is noop
   HB_CFLAGS_STA := -DPCRE2_STATIC

   ifneq ($(filter $(HB_COMPILER),clang gcc mingw mingw64 sunpro),)
      ifeq ($(filter $(HB_PLATFORM)-$(HB_COMPILER),win-clang),)
         HB_CFLAGS += -DHAVE_INTTYPES_H
      endif
   endif

   HB_CFLAGS += -DHAVE_CONFIG_H

   # workaround for problems in sunpro x86 PIC builds exploited by hbpcre library code
   ifeq ($(HB_COMPILER),sunpro)
      ifeq ($(HB_PLATFORM),sunos)
         ifeq ($(findstring sparc,$(shell isalist)),)
            HB_CFLAGS += -xbuiltin=%none
         endif
      else
         HB_CFLAGS += -xbuiltin=%none
      endif
   endif

   # suppress bcc warnings
   ifeq ($(HB_COMPILER),bcc)
      HB_CFLAGS += -w-use -w-csu -w-aus -w-sig
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := local copy unused
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN http://pcre.org/
# URL    https://ftp.pcre.org/pub/pcre/pcre2-10.23.tar.bz2
#
# MAP    LICENCE                               LICENSE.txt
# MAP    src/config.h.generic                  config.h
# MAP    src/pcre2.h.generic                   pcre2.h
# MAP    src/pcre2_internal.h                  pcre2ina.h
# MAP    src/pcre2_intmodedep.h                pcre2mod.h
# MAP    src/pcre2_ucp.h                       pcre2ucp.h
# MAP    src/pcre2_chartables.c.dist           chartabs.c
# MAP    src/pcre2_auto_possess.c              p2autopo.c
# MAP    src/pcre2_compile.c                   p2compil.c
# MAP    src/pcre2_config.c                    p2config.c
# MAP    src/pcre2_context.c                   p2contex.c
# MAP    src/pcre2_dfa_match.c                 p2dfamat.c
# MAP    src/pcre2_error.c                     p2error.c
# MAP    src/pcre2_maketables.c                p2maktab.c
# MAP    src/pcre2_match.c                     p2match.c
# MAP    src/pcre2_match_data.c                p2matchd.c
# MAP    src/pcre2_newline.c                   p2newlin.c
# MAP    src/pcre2_ord2utf.c                   p2ord2ut.c
# MAP    src/pcre2_pattern_info.c              p2patter.c
# MAP    src/pcre2_serialize.c                 p2serial.c
# MAP    src/pcre2_string_utils.c              p2string.c
# MAP    src/pcre2_study.c                     p2study.c
# MAP    src/pcre2_substitute.c                p2substi.c
# MAP    src/pcre2_substring.c                 p2substr.c
# MAP    src/pcre2_tables.c                    p2tables.c
# MAP    src/pcre2_ucd.c                       p2ucd.c
# MAP    src/pcre2_valid_utf.c                 p2validu.c
# MAP    src/pcre2_xclass.c                    p2xclass.c
