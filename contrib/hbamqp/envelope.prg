/* AMQP envelope */

#include "hbclass.ch"

#include "hbamqp.ch"

CREATE CLASS AMQPEnvelope

   METHOD FromPtr( pEnvelope )
   METHOD GetPtr()

   METHOD Destroy()

   METHOD GetMessageBody()

   METHOD GetDeliveryTag()
   METHOD GetExchange()
   METHOD GetRoutingKey()

   PROTECTED:

   VAR pEnvelope

END CLASS

METHOD FromPtr( pEnvelope ) CLASS AMQPEnvelope

   ::Super:New()

   IF Empty( pEnvelope )
      ::Throw( "Invalid Envelope" )
   ENDIF

   ::pEnvelope := pEnvelope

   RETURN Self

METHOD GetPtr() CLASS AMQPEnvelope
   RETURN ::pEnvelope

METHOD Destroy() CLASS AMQPEnvelope

   IF ! Empty( ::pEnvelope )
      amqp_destroy_envelope( ::pEnvelope )
      ::pEnvelope := NIL
   ENDIF

   RETURN NIL

METHOD GetMessageBody() CLASS AMQPEnvelope

   IF ! Empty( ::pEnvelope )
      RETURN amqp_envelope_getmessagebody( ::pEnvelope )
   ENDIF

   RETURN NIL

METHOD GetDeliveryTag() CLASS AMQPEnvelope

   IF ! Empty( ::pEnvelope )
      RETURN amqp_envelope_getdeliverytag( ::pEnvelope )
   ENDIF

   RETURN NIL

METHOD GetExchange() CLASS AMQPEnvelope

   IF ! Empty( ::pEnvelope )
      RETURN amqp_envelope_getexchange( ::pEnvelope )
   ENDIF

   RETURN NIL

METHOD GetRoutingKey() CLASS AMQPEnvelope

   IF ! Empty( ::pEnvelope )
      RETURN amqp_envelope_getroutingkey( ::pEnvelope )
   ENDIF

   RETURN NIL
