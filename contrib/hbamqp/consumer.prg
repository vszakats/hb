/* AMQP Consumer */

#include "hbclass.ch"

#include "hbamqp.ch"

CREATE CLASS AMQPConsumer

   METHOD New( connection AS OBJECT )

   METHOD SetChannel( nChannel )      INLINE ::channel   := nChannel
   METHOD SetQueueName( cQueueName )  INLINE ::queueName := cQueueName
   METHOD SetTimeoutMS( nTimeoutMS )  INLINE ::timeoutMS := nTimeoutMS
   METHOD SetAction( bAction )

   METHOD OpenChannel()
   METHOD ConsumeAndDispatch()
   METHOD CloseChannel()

   PROTECTED:

   METHOD DumpAction( hEnvelope )

   VAR connection  AS OBJECT     /* AMQPConnection */
   VAR channel     AS NUMERIC    INIT 1
   VAR timeOutMS   AS NUMERIC    INIT AMQP_TIMEOUT_INFINITE
   VAR queueName   AS CHARACTER  INIT "default"
   VAR action

END CLASS

METHOD New( connection AS OBJECT ) CLASS AMQPConsumer

   IF Empty( connection )
      hb_traceLog( "Connection Error" )
   ENDIF

   ::connection := connection
   ::action := {| hEnvelope | ::DumpAction( hEnvelope ) }

   RETURN Self

METHOD OpenChannel() CLASS AMQPConsumer
   RETURN ;
      ::connection:OpenChannel( ::channel ) == AMQP_RESPONSE_NORMAL .AND. ;
      ::connection:BasicConsume( ::channel, ::queueName ) == AMQP_RESPONSE_NORMAL

METHOD CloseChannel() CLASS AMQPConsumer
   RETURN ::connection:CloseChannel( ::channel ) == AMQP_RESPONSE_NORMAL

METHOD PROCEDURE ConsumeAndDispatch() CLASS AMQPConsumer

   LOCAL oEnvelope := AMQPEnvelope():New()
   LOCAL hEnvelope
   LOCAL result

   ::connection:MaybeReleaseBuffers()

   IF ( result := ::connection:ConsumeMessage( oEnvelope, ::timeOutMS ) ) == AMQP_RESPONSE_NORMAL

      // build envelope

      hEnvelope := { ;
         "exchange"    => oEnvelope:GetExchange(), ;
         "routingKey"  => oEnvelope:GetRoutingKey(), ;
         "deliveryTag" => oEnvelope:GetDeliveryTag(), ;
         "messageBody" => oEnvelope:GetMessageBody() }

      // call action

      DO CASE
      CASE HB_ISEVALITEM( ::action )

         result := Eval( ::action, hEnvelope )

      CASE HB_ISSTRING( ::action )

         result := hb_ExecFromArray( { ::action, hEnvelope } )

      OTHERWISE
         hb_traceLog( "ConsumeAndDispatch action error" )
      ENDCASE

      result := hb_default( @result, .F. )

      // action returns .T. -> ack message
      IF result
         IF ::connection:BasicAck( ::channel, oEnvelope:GetDeliveryTag() ) != 0
            hb_traceLog( "AMQPConsumer", "basicAck error" )
         ELSE
            hb_traceLog( "AMQPConsumer", "basicAck OK" )
         ENDIF
      ELSE
         hb_traceLog( "AMQPConsumer", "action block error" )
      ENDIF
   ELSE
      hb_traceLog( "AMQPConsumer", hb_StrFormat( "consume Error (result=%1$d, response=%2$s)", result, ::connection:GetResponse() ) )
   ENDIF

   oEnvelope:Destroy()

   RETURN

// Raw message dump
METHOD PROCEDURE DumpAction( hEnvelope ) CLASS AMQPConsumer

   hb_traceLog( "AMQPConsumer", "Raw message:", hb_ValToExp( hEnvelope ) )

   RETURN

METHOD PROCEDURE SetAction( bAction ) CLASS AMQPConsumer

   DO CASE
   CASE HB_ISEVALITEM( bAction )
      /* do nothing */

   CASE HB_ISSTRING( bAction )

      IF ! hb_IsFunction( bAction )
         hb_traceLog( "action is not a function" )
      ENDIF

   OTHERWISE
      hb_traceLog( "Invalid action" )
   ENDCASE

   ::action = bAction

   RETURN
