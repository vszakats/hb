#require "hbwin"

PROCEDURE Main()

   LOCAL aPrn
   LOCAL nPrn := 1
   LOCAL cDocName := "Raw printing test"
   LOCAL cFileName := Space( 256 )
   LOCAL GetList := {}

   IF Empty( aPrn := win_printerList() )
      Alert( "No printers installed - Cannot continue" )
   ELSE
      DO WHILE nPrn > 0

         CLS
         @ 0, 0 SAY "Raw printing test. Choose a printer to test"
         @ 1, 0 SAY "File name:" GET cFileName PICTURE "@KS40"
         READ
         @ 2, 0 TO MaxRow(), MaxCol()

         IF ( nPrn := AChoice( 3, 1, MaxRow() - 1, MaxCol() - 1, aPrn, .T.,, nPrn ) ) > 0

            IF Empty( cFileName )
               Alert( "win_PrintDataRaw() returned: " + ;
                  hb_ntos( win_PrintDataRaw( aPrn[ nPrn ], "Hello World!" + hb_BChar( 12 ), cDocName ) ) )
            ELSE
               Alert( "win_PrintFileRaw() returned: " + ;
                  hb_ntos( win_PrintFileRaw( aPrn[ nPrn ], cFileName, cDocName ) ) )
            ENDIF
         ENDIF
      ENDDO
   ENDIF

   RETURN
