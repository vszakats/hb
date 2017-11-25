ifeq ($(HB_BUILD_MODE),cpp)
   ifneq ($(findstring clang$(subst x, ,x)version$(subst x, ,x)1,$(shell clang --version)),)
      HB_BUILD_MODE := c
   endif
endif

ifeq ($(HB_BUILD_MODE),cpp)
   HB_CMP := clang++
else
   HB_CMP := clang
endif

OBJ_EXT := .o
LIB_PREF := lib
LIB_EXT := .a

ifeq ($(filter $(HB_PLATFORM),darwin win),)
   HB_DYN_COPT := -DHB_DYNLIB -fPIC
endif

CC := $(HB_CCACHE) $(HB_CMP)$(HB_CCSUFFIX)
CC_IN :=
CC_OUT := -o

CFLAGS += -I. -I$(HB_HOST_INC)
ifeq ($(filter --analyze, $(HB_USER_CFLAGS)),)
   CFLAGS += -c
endif

CFLAGS += -D_FORTIFY_SOURCE=2
ifeq ($(filter $(__HB_COMPILER_VER),0304),)
   ifneq ($(HB_PLATFORM),win)
      CFLAGS += -fstack-protector-strong
   endif
endif

ifeq ($(HB_PLATFORM),win)
   LDFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
   DFLAGS += -Wl,--nxcompat -Wl,--dynamicbase
   ifeq ($(HB_COMPILER),clang64)
      LDFLAGS += -Wl,--pic-executable,-e,mainCRTStartup
      LDFLAGS += -Wl,--high-entropy-va -Wl,--image-base,0x140000000
      DFLAGS += -Wl,--high-entropy-va -Wl,--image-base,0x180000000
   else
      LDFLAGS += -Wl,--pic-executable,-e,_mainCRTStartup
   endif
   # '--no-insert-timestamp' has a bug failing to properly
   # reset timestamp in many (apparently random) cases as
   # of binutils 2.25, so disable for now.
   #LDFLAGS += -Wl,--no-insert-timestamp
   # This has potential risks for .dlls:
   #    https://sourceware.org/bugzilla/show_bug.cgi?id=16887
   #DFLAGS += -Wl,--no-insert-timestamp
endif

ifneq ($(HB_BUILD_WARN),no)
   CFLAGS += -W -Weverything
   CFLAGS += -Wno-padded -Wno-cast-align -Wno-float-equal -Wno-missing-prototypes
   CFLAGS += -Wno-disabled-macro-expansion -Wno-undef -Wno-unused-macros -Wno-variadic-macros -Wno-documentation
   CFLAGS += -Wno-switch-enum
   ifeq ($(HB_PLATFORM),darwin)
      ifeq ($(filter $(__HB_COMPILER_VER),0304 0305 0306),)
         CFLAGS += -Wno-reserved-id-macro
      endif
   else
      ifeq ($(filter $(__HB_COMPILER_VER),0304 0305),)
         CFLAGS += -Wno-reserved-id-macro
      endif
   endif
   CFLAGS += -Wno-empty-translation-unit
   # These are potentially useful. -Wsign-conversion would require proper HB_SIZE/HB_ISIZ cleanup.
   CFLAGS += -Wno-sign-conversion -Wno-shorten-64-to-32 -Wno-conversion -Wno-bad-function-cast
else
   CFLAGS += -Wmissing-braces -Wreturn-type -Wformat
   ifneq ($(HB_BUILD_MODE),cpp)
      CFLAGS += -Wimplicit-int -Wimplicit-function-declaration
   endif
endif

ifneq ($(HB_BUILD_OPTIM),no)
   ifeq ($(HB_BUILD_DEBUG),yes)
      ifeq ($(filter $(__HB_COMPILER_VER),0304 0305 0306 0307 0308 0309),)
         CFLAGS += -Og
      else
         CFLAGS += -O1
      endif
   else
      CFLAGS += -O3
   endif
endif

ifeq ($(HB_BUILD_DEBUG),yes)
   CFLAGS += -g
endif

LD := $(CC)
LD_OUT := -o

LIBPATHS := $(foreach dir,$(LIB_DIR) $(SYSLIBPATHS),-L$(dir))

LDLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))
LDFLAGS += $(LIBPATHS)

DY := $(CC)

ifeq ($(HB_PLATFORM),darwin)
   DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))
   DFLAGS += $(LIBPATHS)
else
   AR_RULE = ( $(AR) rcs $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) \
      $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) \
      || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

   DFLAGS += -shared $(LIBPATHS)
   DY_OUT := -o$(subst x,x, )

   ifeq ($(HB_PLATFORM),win)
#     AR := llvm-ar
      AR := $(HB_CCPATH)$(HB_CCPREFIX)ar

      LDFLAGS += -static-libgcc
      DFLAGS += -static-libgcc

      ifneq ($(HB_CODESIGN_KEY),)
         define create_exe_signed
            $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) \
               $(LD_OUT)$(subst /,$(DIRSEP),$(BIN_DIR)/$@) $(^F) $(LDLIBS) $(LDSTRIP)
            @$(ECHO) $(ECHOQUOTE)! Code signing: $(subst /,$(DIRSEP),$(BIN_DIR)/$@)$(ECHOQUOTE)
            @osslsigncode sign -h sha256 -pkcs12 $(HB_CODESIGN_KEY) \
               -pass "$(HB_CODESIGN_KEY_PASS)" -ts $(HB_SIGN_TIMEURL) \
               -in $(subst /,$(DIRSEP),$(BIN_DIR)/$@) \
               -out $(subst /,$(DIRSEP),$(BIN_DIR)/$@)-signed
            @$(CP) $(subst /,$(DIRSEP),$(BIN_DIR)/$@)-signed $(subst /,$(DIRSEP),$(BIN_DIR)/$@)
            @$(RM) $(subst /,$(DIRSEP),$(BIN_DIR)/$@)-signed
         endef
         LD_RULE = $(create_exe_signed)
      endif

      DLIBS := $(foreach lib,$(HB_USER_LIBS) $(LIBS) $(SYSLIBS),-l$(lib))

      # NOTE: The empty line directly before 'endef' HAS TO exist!
      define dynlib_object
         @$(ECHO) $(ECHOQUOTE)INPUT($(subst \,/,$(file)))$(ECHOQUOTE) >> __dyn__.tmp

      endef
      ifneq ($(HB_CODESIGN_KEY),)
         define create_dynlib_signed
            $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
            $(foreach file,$^,$(dynlib_object))
            $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp \
               $(DEF_FILE) $(DLIBS) \
               -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def \
               -Wl,--major-image-version,$(HB_VER_MAJOR) \
               -Wl,--minor-image-version,$(HB_VER_MINOR) $(DYSTRIP)
            @$(ECHO) $(ECHOQUOTE)! Code signing: $(DYN_DIR)/$@$(ECHOQUOTE)
            @osslsigncode sign -h sha256 -pkcs12 $(HB_CODESIGN_KEY) \
               -pass $(HB_CODESIGN_KEY_PASS) -ts $(HB_SIGN_TIMEURL) \
               -in $(DYN_DIR)/$@ \
               -out $(DYN_DIR)/$@-signed
            @$(CP) $(DYN_DIR)/$@-signed $(DYN_DIR)/$@
            @$(RM) $(DYN_DIR)/$@-signed
         endef
         DY_RULE = $(create_dynlib_signed)
      else
         define create_dynlib
            $(if $(wildcard __dyn__.tmp),@$(RM) __dyn__.tmp,)
            $(foreach file,$^,$(dynlib_object))
            $(DY) $(DFLAGS) $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ __dyn__.tmp \
               $(DEF_FILE) $(DLIBS) \
               -Wl,--out-implib,$(IMP_FILE),--output-def,$(DYN_DIR)/$(basename $@).def \
               -Wl,--major-image-version,$(HB_VER_MAJOR) \
               -Wl,--minor-image-version,$(HB_VER_MINOR) $(DYSTRIP)
         endef
         DY_RULE = $(create_dynlib)
      endif
   else
      AR := ar

      DLIBS := $(foreach lib,$(HB_USER_LIBS) $(SYSLIBS),-l$(lib))

      DY_RULE = $(DY) $(DFLAGS) -Wl,-soname,$(DYN_NAME_CPT) $(HB_USER_DFLAGS) \
         $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) \
         && $(LN) $(@F) $(DYN_FILE_NVR) \
         && $(LN) $(@F) $(DYN_FILE_CPT)
   endif
endif

include $(TOP)$(ROOT)config/rules.mk
