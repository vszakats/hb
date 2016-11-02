
FUNCTION Main( )

    Local cEncode1, cEncode2
    Local cPwd := "XvAQqcma9PqAQjJb"
    
    cEncode1 :=  HASHIDS_ENCODE( 5,cPwd, 6)
    cEncode2 :=  HASHIDS_ENCODE( { 2016, 123456789 }, cPwd, 6 )

     ? cEncode1
     ? cEncode2
     ? hb_ValToExp( HASHIDS_DECODE( cEncode1,cPwd, 8))
     ? hb_ValToExp( HASHIDS_DECODE( cEncode2,cPwd, 8 ))
    
RETURN NIL


