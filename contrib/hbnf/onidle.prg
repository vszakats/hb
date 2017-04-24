/* This is an original work from 2016 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

THREAD STATIC t_hIdle

PROCEDURE ft_OnIdle( bBlock )

   IF ! Empty( t_hIdle )
      hb_idleDel( t_hIdle )
      t_hIdle := NIL
   ENDIF

   IF HB_ISEVALITEM( bBlock )
      t_hIdle := hb_idleAdd( bBlock )
   ENDIF

   RETURN
