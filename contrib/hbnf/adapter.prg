/* This is an original work from 2012 by Viktor Szakats
   and is placed in the public domain. */

FUNCTION ft_Adapter()
   RETURN iif( IsColor(), 3 /* VGA */, 0 /* monochrome */ )
