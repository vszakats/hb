ROOT := ../../

PRG_SOURCES := \
   hbi18n.prg \

RC_SOURCES := harbour.rc

PRG_MAIN := hbi18n.prg

LIBS = $(HB_LIBS_ST_NORDD)

ifneq ($(filter $(HB_PLATFORM),win wce os2),)
   ifneq ($(__HB_BUILD_DYN),no)
      HB_BUILD_SHARED := yes
   endif
endif

include $(TOP)$(ROOT)config/bin.mk

ifneq ($(filter $(HB_COMPILER),gcc mingw mingw64 clang clang64),)
   HB_LDFLAGS += -fno-lto
endif
