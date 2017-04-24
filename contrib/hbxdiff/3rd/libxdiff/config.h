#if ! defined( CONFIG_H )
#define CONFIG_H

#define PACKAGE_VERSION       "0.23"

#include "hbdefs.h"
#include "hb_io.h"        /* include unistd.h */

/* Define to 1 if you have the `memchr'&Co function. */
#define HAVE_MEMCHR           1
#define HAVE_MEMCMP           1
#define HAVE_MEMCPY           1
#define HAVE_MEMSET           1
#define HAVE_STRLEN           1

/* for BCC 5.5 */
#if ( defined( __BORLANDC__ ) && ( __BORLANDC__ >= 0x0550 ) && ( __BORLANDC__ < 0x0578 ) ) || ( defined( _MSC_VER ) && ( _MSC_VER <= 1200 ) && ! defined( __POCC__ ) )
#  define XRABPLY_TYPE64        __int64
#  define XV64( v ) ( ( xply_word ) v##ui64 )
#endif

#endif /* CONFIG_H */
