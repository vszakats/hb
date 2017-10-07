/*
 * "$Id: config.h.in 451 2014-01-04 21:50:06Z msweet $"
 *
 * Configuration file for Mini-XML, a small XML-like file parsing library.
 *
 * Copyright 2003-2014 by Michael R Sweet.
 *
 * These coded instructions, statements, and computer programs are the
 * property of Michael R Sweet and are protected by Federal copyright
 * law.  Distribution and use rights are outlined in the file "COPYING"
 * which should have been included with this file.  If this file is
 * missing or damaged, see the license at:
 *
 *     http://www.msweet.org/projects.php/Mini-XML
 */

/*
 * Include necessary headers...
 */

#include "hbapi.h"
#include "hb_io.h"

#if defined ( _MSC_VER )
#define close      _close
#define open       _open
#define read       _read
#undef snprintf
#define snprintf   _snprintf
#define strdup     _strdup
#undef vsnprintf
#define vsnprintf  _vsnprintf
#define write      _write
#endif

#ifdef HB_USE_CORE_PRINTF
#  define HAVE_SNPRINTF  1
#  undef snprintf
#  define snprintf  hb_snprintf

#  define HAVE_VSNPRINTF  1
#  undef vsnprintf
#  define vsnprintf  hb_vsnprintf
#endif

/*
 * Version number...
 */

#define MXML_VERSION	""


/*
 * Inline function support...
 */

#if 0
#define inline
#endif


/*
 * Long long support...
 */

#if 0
#undef HAVE_LONG_LONG
#endif


/*
 * Do we have the snprintf() and vsnprintf() functions?
 */

#if 0
#undef HAVE_SNPRINTF
#undef HAVE_VSNPRINTF
#endif


/*
 * Do we have the strXXX() functions?
 */

#if 0
#undef HAVE_STRDUP
#endif


/*
 * Do we have threading support?
 */

#if 0
#undef HAVE_PTHREAD_H
#endif


/*
 * Define prototypes for string functions as needed...
 */

#  ifndef HAVE_STRDUP
extern char	*_mxml_strdup(const char *);
#    undef strdup
#    define strdup _mxml_strdup
#  endif /* !HAVE_STRDUP */

extern char	*_mxml_strdupf(const char *, ...);
extern char	*_mxml_vstrdupf(const char *, va_list);

#  ifndef HAVE_SNPRINTF
extern int	_mxml_snprintf(char *, size_t, const char *, ...);
#    undef snprintf
#    define snprintf _mxml_snprintf
#  endif /* !HAVE_SNPRINTF */

#  ifndef HAVE_VSNPRINTF
extern int	_mxml_vsnprintf(char *, size_t, const char *, va_list);
#    undef vsnprintf
#    define vsnprintf _mxml_vsnprintf
#  endif /* !HAVE_VSNPRINTF */

/*
 * End of "$Id: config.h.in 451 2014-01-04 21:50:06Z msweet $".
 */
