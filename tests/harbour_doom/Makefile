.PHONY: all clean

.SUFFIXES:

SHELL := cmd.exe

ifeq ($(V),1)
	VB=
else
	VB=@
endif

MINGW_ROOT?=c:/mingw-4.8.1
CC=$(MINGW_ROOT)/bin/gcc

CFLAGS=-std=gnu99 -O2 -DNDEBUG -DALLEGRO_STATICLINK
CFLAGS+=-I. -I$(MINGW_ROOT)/include
LDFLAGS=-L$(MINGW_ROOT)/lib
LIBS=-lalleg -lwinmm -lgdi32 -luser32 -lole32 -ldinput -ldxguid -lddraw -ldsound

OBJDIR=build
OUTPUT=Doom_hb.exe

SRCS = boot.c main.c \
	dummy.c am_map.c doomdef.c doomstat.c dstrings.c \
	d_event.c d_items.c d_iwad.c d_loop.c d_main.c d_mode.c d_net.c \
	f_finale.c f_wipe.c g_game.c hu_lib.c hu_stuff.c info.c \
	i_cdmus.c i_endoom.c i_joystick.c i_scale.c i_sound.c i_system.c i_timer.c \
	memio.c m_argv.c m_bbox.c m_cheat.c m_config.c m_controls.c m_fixed.c \
	m_menu.c m_misc.c m_random.c \
	p_ceilng.c p_doors.c p_enemy.c p_floor.c p_inter.c p_lights.c \
	p_map.c p_maputl.c p_mobj.c p_plats.c p_pspr.c p_saveg.c p_setup.c \
	p_sight.c p_spec.c p_switch.c p_telept.c p_tick.c p_user.c \
	r_bsp.c r_data.c r_draw.c r_main.c r_plane.c r_segs.c r_sky.c r_things.c \
	sha1.c sounds.c statdump.c st_lib.c st_stuff.c s_sound.c tables.c \
	v_video.c wi_stuff.c w_checksum.c w_file.c w_main.c w_wad.c z_zone.c \
	w_file_stdc.c i_input.c i_video.c \
	doomgeneric_allegro.c mus2mid.c i_allegromusic.c i_allegrosound.c

OBJS = $(addprefix $(OBJDIR)/,$(SRCS:.c=.o))

all: $(OUTPUT)

clean:
	if exist $(OBJDIR) rmdir /s /q $(OBJDIR)
	if exist $(OUTPUT) del /q $(OUTPUT)

$(OUTPUT): $(OBJS)
	@echo [Linking $@]
	$(VB)$(CC) $(CFLAGS) $(LDFLAGS) $(OBJS) -o $(OUTPUT) $(LIBS)

$(OBJS): | $(OBJDIR)

$(OBJDIR):
	if not exist $(OBJDIR) mkdir $(OBJDIR)

$(OBJDIR)/%.o: %.c
	@echo [Compiling $<]
	$(VB)$(CC) $(CFLAGS) -c $< -o $@
