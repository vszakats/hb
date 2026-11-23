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
#include "fileio.ch"

FUNCTION W_StdC_OpenFile( path )
    LOCAL result
    LOCAL fstream
    MEMVAR stdc_wad_file

    fstream := FOpen( path, FO_READ )
    IF fstream == F_ERROR
        RETURN NIL
    ENDIF

    result := wad_file_t():New()
    result:file_class := stdc_wad_file
    result:mapped := NIL
    result:length := M_FileLength( fstream )
    result:fstream := fstream
RETURN result

PROCEDURE W_StdC_CloseFile( wad )
    IF wad != NIL .AND. wad:fstream != NIL
        FClose( wad:fstream )
        wad:fstream := NIL
    ENDIF
RETURN

FUNCTION W_StdC_Read( wad, offset, buffer_len )
    LOCAL cBuf
    LOCAL nRead

    IF wad == NIL .OR. wad:fstream == NIL .OR. buffer_len <= 0
        RETURN ""
    ENDIF
    FSeek( wad:fstream, offset, FS_SET )
    cBuf := Replicate( Chr( 0 ), buffer_len )
    nRead := FRead( wad:fstream, @cBuf, buffer_len )
    IF nRead < buffer_len
        cBuf := Left( cBuf, nRead )
    ENDIF
RETURN cBuf

INIT PROCEDURE init_w_file_stdc
    PUBLIC stdc_wad_file
    stdc_wad_file := wad_file_class_t():New()
    stdc_wad_file:OpenFile := {| p | W_StdC_OpenFile( p ) }
    stdc_wad_file:CloseFile := {| w | W_StdC_CloseFile( w ) }
    stdc_wad_file:Read := {| w, o, n | W_StdC_Read( w, o, n ) }
RETURN
