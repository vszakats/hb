ROOT := ../../

PRG_SOURCES := \
   hbtest.prg \

C_SOURCES := \
   hbtestc.c \

RC_SOURCES := harbour.rc

PRG_MAIN := hbtest.prg

HB_PRGFLAGS += -DRT_HAS_C

LIBS = $(HB_LIBS_ST_RDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(__HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),gcc mingw mingw64 clang clang64),)
   HB_LDFLAGS += -fno-lto
endif
