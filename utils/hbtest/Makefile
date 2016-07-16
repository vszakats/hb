ROOT := ../../

PRG_SOURCES := \
   hbtest.prg \

C_SOURCES := \
   hbtestc.c \

RC_SOURCES := \
   $(TOP)$(ROOT)package/harbour.rc

PRG_MAIN := hbtest.prg

HB_PRGFLAGS += -DRT_HAS_C

LIBS = $(HB_LIBS_ST_RDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),mingw mingw64 gcc clang),)
   HB_LDFLAGS += -fno-lto
endif
