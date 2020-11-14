/* This is an original work from 2012 by Viktor Szakats (vsz.me/hb)
   and is placed in the public domain. */

FUNCTION ft_Adapter()
   RETURN iif( IsColor(), 3 /* VGA */, 0 /* monochrome */ )
