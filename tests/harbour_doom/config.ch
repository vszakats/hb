/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#ifndef __CONFIG_H__
#define __CONFIG_H__

#undef HAVE_DEV_ISA_SPKRIO_H
#undef HAVE_DEV_SPEAKER_SPEAKER_H
#define HAVE_INTTYPES_H 1
#undef HAVE_IOPERM
#undef HAVE_LIBAMD64
#undef HAVE_LIBI386
#undef HAVE_LIBM
#undef HAVE_LIBPNG
#undef HAVE_LIBSAMPLERATE
#undef HAVE_LIBZ
#undef HAVE_LINUX_KD_H
#undef HAVE_MEMORY_H
#undef HAVE_MMAP
#undef HAVE_SCHED_SETAFFINITY
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#undef HAVE_SYS_STAT_H
#define HAVE_SYS_TYPES_H 1
#undef HAVE_UNISTD_H

#define PACKAGE "Doom"
#undef PACKAGE_BUGREPORT
#define PACKAGE_NAME "doom_hb"
#define PACKAGE_STRING "doom_hb 0.1"
#define PACKAGE_TARNAME "doomgeneric.tar"
#define PACKAGE_URL ""
#define PACKAGE_VERSION 0.1
#define PROGRAM_PREFIX "doom_hb"
#define STDC_HEADERS 1
#define VERSION 0.1

#undef ORIGCODE

#define FILES_DIR "."

#ifndef DOOMGENERIC_RESX
#define DOOMGENERIC_RESX 320
#endif
#ifndef DOOMGENERIC_RESY
#define DOOMGENERIC_RESY 200
#endif
#ifndef CMAP256
#define CMAP256 1
#endif

#endif
