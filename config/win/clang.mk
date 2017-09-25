ifeq ($(HB_COMPILER),clang64)
   RCFLAGS += --target=pe-x86-64
else
   RCFLAGS += --target=pe-i386
endif

RC := $(HB_CCPATH)$(HB_CCPREFIX)windres
RC_OUT := -o$(subst x,x, )
RCFLAGS += -I. -I$(HB_HOST_INC) -O coff -c65001

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP := -s
   DYSTRIP := -s
endif

include $(TOP)$(ROOT)config/common/clang.mk
