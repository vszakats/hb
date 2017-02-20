ROOT := ../

include $(ROOT)config/global.mk

ifneq ($(HB_NO_HBSCRIPT),yes)

first clean install::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbmk2$(HB_HOST_BIN_EXT)),$(HB_HOST_BIN_DIR)/hbmk2$(HB_HOST_BIN_EXT) $(TOP)$(ROOT)bin/make.hb $@,@$(ECHO) $(ECHOQUOTE)! Warning: hbmk2 not found, bin/make.hb skipped.$(ECHOQUOTE))

endif
