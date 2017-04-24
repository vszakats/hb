#define CONDITION  1
#define BODY       2

PROCEDURE Main()

   LOCAL Program := Array( 2 )
   LOCAL Counter := 1, TheEnd := 2000000
   LOCAL stop, start

   Program[ CONDITION ] := {|| Counter == TheEnd }
   Program[ BODY      ] := {|| Counter++ }

   ? start := Second()
   DO WHILE ! Program[ CONDITION ]:Eval()
      Program[ BODY ]:Eval()
   ENDDO
   ? stop := Second()
   ? "---"
   ? stop - start

   RETURN
