/* AMQP envelope */

#include "hbclass.ch"

#include "hbamqp.ch"

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
