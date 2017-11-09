ROOT := ../../../

include $(TOP)$(ROOT)config/global.mk

LIBNAME := gtcrs

C_SOURCES := \
   gtcrs.c \

ifneq ($(HB_HAS_CURSES),)

   HB_CFLAGS += $(QUOTE)-I$(HB_HAS_CURSES)$(QUOTE)

   ifneq ($(HB_HAS_GPM),)
      HB_CFLAGS += -DHB_HAS_GPM
   endif

   include $(TOP)$(ROOT)config/lib.mk
else
   HB_SKIP_REASON := component not found
   include $(TOP)$(ROOT)config/none.mk
endif
