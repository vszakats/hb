/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

#include "doomtype.ch"
#include "m_argv.ch"


FUNCTION M_CheckParmWithArgs( check, num_args )
    LOCAL i
    MEMVAR myargc
    MEMVAR myargv

    FOR i := 1 TO myargc - num_args - 1
        IF hb_stricmp( check, myargv[ i + 1 ] ) == 0
            RETURN i
        ENDIF
    NEXT
RETURN 0

FUNCTION M_ParmExists( check )
RETURN M_CheckParm( check ) != 0

FUNCTION M_CheckParm( check )
RETURN M_CheckParmWithArgs( check, 0 )

STATIC PROCEDURE LoadResponseFile( argv_index )
    HB_SYMBOL_UNUSED( argv_index )
RETURN

FUNCTION M_FindResponseFile()
    LOCAL i
    MEMVAR myargc
    MEMVAR myargv

    FOR i := 1 TO myargc - 1
        IF Left( myargv[ i + 1 ], 1 ) == "@"
            LoadResponseFile( i )
        ENDIF
    NEXT
RETURN NIL

FUNCTION M_GetExecutableName()
    LOCAL nSep
    MEMVAR myargv

    nSep := RAt( DIR_SEPARATOR_S, myargv[ 1 ] )
    IF nSep == 0
        RETURN myargv[ 1 ]
    ENDIF
RETURN SubStr( myargv[ 1 ], nSep + 1 )
