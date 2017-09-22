ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := hbpcre2

HB_BUILD_WARN := no
HB_BUILD_MODE := c

C_SOURCES := \
   pcre2_chartables.c \
   pcre2_auto_possess.c \
   pcre2_compile.c \
   pcre2_config.c \
   pcre2_context.c \
   pcre2_dfa_match.c \
   pcre2_error.c \
   pcre2_find_bracket.c \
   pcre2_jit_compile.c \
   pcre2_maketables.c \
   pcre2_match.c \
   pcre2_match_data.c \
   pcre2_newline.c \
   pcre2_ord2utf.c \
   pcre2_pattern_info.c \
   pcre2_serialize.c \
   pcre2_string_utils.c \
   pcre2_study.c \
   pcre2_substitute.c \
   pcre2_substring.c \
   pcre2_tables.c \
   pcre2_ucd.c \
   pcre2_valid_utf.c \
   pcre2_xclass.c \

ifneq ($(HB_HAS_PCRE2_LOCAL),)

   HB_CFLAGS += -DSUPPORT_UNICODE -DPCRE2_CODE_UNIT_WIDTH=8
   # only needed for win/wce, for other platforms this is no-op
   HB_CFLAGS_STA := -DPCRE2_STATIC

   ifneq ($(filter $(HB_COMPILER),gcc clang clang64 mingw mingw64 sunpro),)
      ifeq ($(filter $(HB_PLATFORM)-$(HB_COMPILER),win-clang win-clang64),)
         HB_CFLAGS += -DHAVE_INTTYPES_H
      endif
   endif

   HB_CFLAGS += -DHAVE_CONFIG_H

   # suppress bcc warnings
   ifeq ($(HB_COMPILER),bcc)
      HB_CFLAGS += -w-use -w-csu -w-aus -w-sig
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := local copy unused
   include $(TOP)$(ROOT)config/none.mk
endif

# ORIGIN https://pcre.org/
# URL    https://ftp.pcre.org/pub/pcre/pcre2-10.30.tar.bz2
#
# MAP    LICENCE
# MAP    src/config.h.generic         config.h
# MAP    src/pcre2.h.generic          pcre2.h
# MAP    src/pcre2_internal.h         pcre2_internal.h
# MAP    src/pcre2_intmodedep.h       pcre2_intmodedep.h
# MAP    src/pcre2_ucp.h              pcre2_ucp.h
# MAP    src/pcre2_chartables.c.dist  pcre2_chartables.c
# MAP    src/pcre2_auto_possess.c     pcre2_auto_possess.c
# MAP    src/pcre2_compile.c          pcre2_compile.c
# MAP    src/pcre2_config.c           pcre2_config.c
# MAP    src/pcre2_context.c          pcre2_context.c
# MAP    src/pcre2_dfa_match.c        pcre2_dfa_match.c
# MAP    src/pcre2_error.c            pcre2_error.c
# MAP    src/pcre2_find_bracket.c     pcre2_find_bracket.c
# MAP    src/pcre2_jit_compile.c      pcre2_jit_compile.c
# MAP    src/pcre2_jit_match.c        pcre2_jit_match.c
# MAP    src/pcre2_jit_misc.c         pcre2_jit_misc.c
# MAP    src/pcre2_maketables.c       pcre2_maketables.c
# MAP    src/pcre2_match.c            pcre2_match.c
# MAP    src/pcre2_match_data.c       pcre2_match_data.c
# MAP    src/pcre2_newline.c          pcre2_newline.c
# MAP    src/pcre2_ord2utf.c          pcre2_ord2utf.c
# MAP    src/pcre2_pattern_info.c     pcre2_pattern_info.c
# MAP    src/pcre2_serialize.c        pcre2_serialize.c
# MAP    src/pcre2_string_utils.c     pcre2_string_utils.c
# MAP    src/pcre2_study.c            pcre2_study.c
# MAP    src/pcre2_substitute.c       pcre2_substitute.c
# MAP    src/pcre2_substring.c        pcre2_substring.c
# MAP    src/pcre2_tables.c           pcre2_tables.c
# MAP    src/pcre2_ucd.c              pcre2_ucd.c
# MAP    src/pcre2_valid_utf.c        pcre2_valid_utf.c
# MAP    src/pcre2_xclass.c           pcre2_xclass.c
