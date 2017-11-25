ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := g++
else
   # for old Darwin systems (having GCC 2.95) this may need to be
   # redefined to 'cc'.
   HB_CMP := gcc
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

CC := $(HB_CCACHE) $(HB_CCPREFIX)$(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
# NOTE: The ending space after -o is important, please preserve it.
#       Now solved with '$(subst x,x, )' expression.
CC_OUT := -o$(subst x,x, )

CFLAGS += -I. -I$(HB_HOST_INC) -c

CFLAGS += -D_FORTIFY_SOURCE=2
ifeq ($(filter $(__HB_COMPILER_VER),0209 0304 0400 0401 0402 0403 0404 0405 0406 0407 0408),)
   CFLAGS += -fstack-protector-strong
endif

# -no-cpp-precomp prevents from using buggy precompiled headers
# CFLAGS += -no-cpp-precomp

# -fno-common enables building .dylib files
CFLAGS += -fno-common

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Wall
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

#CFLAGS += -D_FORTIFY_SOURCE=2

ifneq ($(HB_BUILD_OPTIM),no)
   CFLAGS += -O3
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(CC)
LD_OUT := -o$(subst x,x, )

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))

LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(LIBPATHS)

DY := $(CC)
DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))
DFLAGS += $(LIBPATHS)

include $(TOP)$(ROOT)config/rules.mk
