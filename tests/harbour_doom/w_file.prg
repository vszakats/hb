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

#include "w_file.ch"

CLASS wad_file_class_t
    DATA OpenFile
    DATA CloseFile
    DATA Read
    METHOD New()
ENDCLASS
CLASS wad_file_t
    DATA file_class
    DATA mapped
    DATA length
    DATA fstream
    METHOD New()
ENDCLASS

METHOD New() CLASS wad_file_class_t
    ::OpenFile := NIL
    ::CloseFile := NIL
    ::Read := NIL
RETURN Self

METHOD New() CLASS wad_file_t
    ::file_class := NIL
    ::mapped := NIL
    ::length := 0
    ::fstream := NIL
RETURN Self

FUNCTION W_OpenFile( path )
    MEMVAR stdc_wad_file
RETURN IfaceCall( stdc_wad_file:OpenFile, path )

PROCEDURE W_CloseFile( wad )
    IF wad != NIL .AND. wad:file_class != NIL
        IfaceCall( wad:file_class:CloseFile, wad )
    ENDIF
RETURN

FUNCTION W_Read( wad, offset, buffer_len )
    IF wad == NIL .OR. wad:file_class == NIL
        RETURN ""
    ENDIF
RETURN IfaceCall( wad:file_class:Read, wad, offset, buffer_len )

STATIC FUNCTION IfaceCall( xFun, x1, x2, x3 )
    LOCAL nArgs := PCount() - 1

    IF xFun == NIL
        RETURN NIL
    ENDIF
    IF ValType( xFun ) == "B"
        IF nArgs <= 0
            RETURN Eval( xFun )
        ELSEIF nArgs == 1
            RETURN Eval( xFun, x1 )
        ELSEIF nArgs == 2
            RETURN Eval( xFun, x1, x2 )
        ENDIF
        RETURN Eval( xFun, x1, x2, x3 )
    ENDIF
    IF ValType( xFun ) == "C"
        IF nArgs <= 0
            RETURN &( xFun )()
        ELSEIF nArgs == 1
            RETURN &( xFun )( x1 )
        ELSEIF nArgs == 2
            RETURN &( xFun )( x1, x2 )
        ENDIF
        RETURN &( xFun )( x1, x2, x3 )
    ENDIF
RETURN NIL

INIT PROCEDURE init_w_file
    PUBLIC stdc_wad_file
RETURN
