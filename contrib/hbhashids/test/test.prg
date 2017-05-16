PROCEDURE Main( )

   LOCAL cPwd := "XvAQqcma9PqAQjJb"

   LOCAL cEncode1 := hashids_encode( 5, cPwd, 6)
   LOCAL cEncode2 := hashids_encode( { 2016, 123456789 }, cPwd, 6 )

   ? cEncode1
   ? cEncode2
   ? hb_ValToExp( hashids_decode( cEncode1, cPwd, 8 ) )
   ? hb_ValToExp( hashids_decode( cEncode2, cPwd, 8 ) )
    
   RETURN
