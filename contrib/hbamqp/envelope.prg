/* AMQP envelope */

#include "hbclass.ch"

CREATE CLASS AMQPEnvelope

   METHOD New()
   METHOD GetPtr()

   METHOD GetMessageBody()

   METHOD GetDeliveryTag()
   METHOD GetExchange()
   METHOD GetRoutingKey()

   PROTECTED:

   VAR pEnvelope AS POINTER

END CLASS

METHOD New() CLASS AMQPEnvelope

   ::pEnvelope := amqp_envelope_new()

   RETURN Self

METHOD GetPtr() CLASS AMQPEnvelope
   RETURN ::pEnvelope

METHOD GetMessageBody() CLASS AMQPEnvelope

   IF Empty( ::pEnvelope )
      RETURN NIL
   ENDIF

   RETURN amqp_envelope_getmessagebody( ::pEnvelope )

METHOD GetDeliveryTag() CLASS AMQPEnvelope

   IF Empty( ::pEnvelope )
      RETURN NIL
   ENDIF

   RETURN amqp_envelope_getdeliverytag( ::pEnvelope )

METHOD GetExchange() CLASS AMQPEnvelope

   IF Empty( ::pEnvelope )
      RETURN NIL
   ENDIF

   RETURN amqp_envelope_getexchange( ::pEnvelope )

METHOD GetRoutingKey() CLASS AMQPEnvelope

   IF Empty( ::pEnvelope )
      RETURN NIL
   ENDIF

   RETURN amqp_envelope_getroutingkey( ::pEnvelope )
