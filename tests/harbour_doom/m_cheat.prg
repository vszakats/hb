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
#include "m_cheat.ch"

CLASS cheatseq_t
    DATA sequence
    DATA sequence_len
    DATA parameter_chars
    DATA chars_read
    DATA param_chars_read
    DATA parameter_buf
    METHOD New()
ENDCLASS

METHOD New( sequence, sequence_len, parameter_chars ) CLASS cheatseq_t
    ::sequence         := iif( sequence == NIL, "", sequence )
    ::sequence_len     := iif( sequence_len == NIL, Len( ::sequence ), sequence_len )
    ::parameter_chars  := iif( parameter_chars == NIL, 0, parameter_chars )
    ::chars_read       := 0
    ::param_chars_read := 0
    ::parameter_buf    := ""
RETURN Self

FUNCTION cht_CheckCheat( cht, key )
    LOCAL nKey
    LOCAL nSeqLen

    nKey := iif( ValType( key ) == "C", Asc( key ), key )
    nSeqLen := Len( cht:sequence )

    IF cht:parameter_chars > 0 .AND. nSeqLen < cht:sequence_len
        RETURN .F.
    ENDIF

    IF cht:chars_read < nSeqLen
        IF nKey == Asc( SubStr( cht:sequence, cht:chars_read + 1, 1 ) )
            cht:chars_read := cht:chars_read + 1
        ELSE
            cht:chars_read := 0
        ENDIF
        cht:param_chars_read := 0
    ELSEIF cht:param_chars_read < cht:parameter_chars
        cht:parameter_buf := Left( cht:parameter_buf, cht:param_chars_read ) + Chr( nKey )
        cht:param_chars_read := cht:param_chars_read + 1
    ENDIF

    IF cht:chars_read >= nSeqLen .AND. cht:param_chars_read >= cht:parameter_chars
        cht:chars_read := 0
        cht:param_chars_read := 0
        RETURN .T.
    ENDIF
RETURN .F.

FUNCTION cht_GetParam( cht, buffer )
    buffer := Left( cht:parameter_buf, cht:parameter_chars )
RETURN buffer
