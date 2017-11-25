ifneq ($(filter $(HB_COMPILER),clang-cl clang-cl64),)
   ifeq ($(__HB_COMPILER_VER),)
      $(info ! Warning: __HB_COMPILER_VER internal variable empty. If the variable \
         was manually set, it must be removed and if it still/otherwise fails, this \
         may indicate an auto-detection failure and should be reported.)
   endif
endif

OBJ_EXT := .obj
LIB_PREF :=
LIB_EXT := .lib

ifneq ($(filter $(HB_COMPILER),clang-cl clang-cl64),)
   CC := clang-cl.exe
   HB_BUILD_MODE := c
else
   CC := cl.exe
endif
CC_IN :=
CC_OUT := -Fo

CFLAGS += -I. -I$(HB_HOST_INC) -c

CFLAGS += -nologo

# MSVS 2005 SP1 also supports it, but we only enable it for 2008 and upper.
ifeq ($(filter $(__HB_COMPILER_VER),1200 1300 1310 1400),)
   LDFLAGS += -nxcompat -dynamicbase -fixed:no
   DFLAGS += -nxcompat -dynamicbase
endif
# MSVS 2012 and upper
ifeq ($(filter $(__HB_COMPILER_VER),1200 1300 1310 1400 1500 1600),)
   ifneq ($(filter $(HB_COMPILER),msvc64 msvcia64),)
      LDFLAGS += -highentropyva
      DFLAGS += -highentropyva
   endif
   ifneq ($(HB_BUILD_WARN),no)
      CFLAGS += -sdl
   endif
endif
# enable this only for users of MSVS 2013 and upper
ifeq ($(filter $(__HB_COMPILER_VER),1200 1300 1310 1400 1500 1600 1700),)
   ifeq ($(filter $(HB_COMPILER),clang-cl clang-cl64),)
      ifeq ($(_HB_MSVC_ANALYZE),yes)
         CFLAGS += -analyze
      endif
   endif
endif

ifeq ($(HB_BUILD_MODE),c)
   CFLAGS += -TC
endif
ifeq ($(HB_BUILD_MODE),cpp)
   CFLAGS += -TP
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W4 -wd4127
else
   CFLAGS += -W2
endif

# LLVM mode not functional
ifeq ($(__HB_COMPILER_LLVM),yes)
   ifneq ($(HB_BUILD_WARN),no)
      CFLAGS += -Weverything
      CFLAGS += -Wno-padded -Wno-cast-align -Wno-float-equal -Wno-missing-prototypes
      CFLAGS += -Wno-disabled-macro-expansion -Wno-undef -Wno-unused-macros -Wno-variadic-macros -Wno-documentation
      CFLAGS += -Wno-switch-enum
      ifeq ($(filter $(__HB_COMPILER_VER),0305),)
         CFLAGS += -Wno-reserved-id-macro
      endif
      # These are potentially useful. -Wsign-conversion would require proper HB_SIZE/HB_ISIZ cleanup.
      CFLAGS += -Wno-sign-conversion -Wno-shorten-64-to-32 -Wno-conversion -Wno-bad-function-cast
      CFLAGS += -Wno-language-extension-token
   else
      CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
      ifneq ($(HB_BUILD_MODE),cpp)
         CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
      endif
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   ifneq ($(filter $(__HB_COMPILER_VER),1200 1300 1310),)
      CFLAGS += -Ogt2yb1p -GX- -G6
   else
      CFLAGS += -O2
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -Zi
   LDFLAGS += -debug
   DFLAGS += -debug
endif

ifneq ($(filter $(__HB_COMPILER_VER),1200 1300 1310),)
   ifeq ($(HB_BUILD_DEBUG),yes)
      CFLAGS += -MTd
   else
      CFLAGS += -MT
   endif
endif

RC := rc.exe
RC_OUT := -fo$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC) -c65001

# # NOTE: -GA flag should be disabled when building MT _.dlls_,
# #       as it creates bad code according to MS docs [vszakats].
# ifneq ($(filter $(__HB_COMPILER_VER),1200),)
#    CFLAGS += -GA
# endif

# lld.exe crashes
# ifneq ($(filter $(HB_COMPILER),clang-cl clang-cl64),)
#    LD := lld.exe -flavor link
# else
LD := link.exe
# endif
LD_OUT := -out:

LIBPATHS := $(foreach dir,$(LIB_DIR),-libpath:$(dir))
LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

LDFLAGS += -nologo -subsystem:console $(LIBPATHS)

AR := lib.exe
AR_RULE = $(AR) $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) \
   -nologo -out:$(LIB_DIR)/$@ $(^F)

DY := $(LD)
DFLAGS += -nologo -dll -subsystem:console $(LIBPATHS)
DY_OUT := $(LD_OUT)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),$(lib)$(LIB_EXT))

# NOTE: The empty line directly before 'endef' HAS TO exist!
define dynlib_object
   @$(ECHO) $(ECHOQUOTE)$(file)$(ECHOQUOTE) >> __dyn__.tmp

endef
define create_dynlib
   $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
   $(foreach file,$^,$(dynlib_object))
   $(DY) $(DFLAGS) $(HB_USER_DFLAGS) \
      $(DY_OUT)"$(subst /,$(DIRSEP),$(DYN_DIR)/$@)" \
      -implib:"$(IMP_FILE)" @__dyn__.tmp -def:$(DEF_FILE) $(DLIBS)
endef

DY_RULE = $(create_dynlib)

include $(TOP)$(ROOT)config/rules.mk
