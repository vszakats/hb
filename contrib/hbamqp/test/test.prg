/* Basic Publish and Consume API Test */

#include "inkey.ch"

STATIC s_cHost := "localhost"
STATIC s_nPort := NIL
STATIC s_lSSL := .F.
STATIC s_cVirtualHost := NIL
STATIC s_cUser := "guest"
STATIC s_cPassword := "guest"

PROCEDURE Main( cCommand )

   ? amqp_version()

   SWITCH hb_asciiLower( hb_defaultValue( cCommand, "" ) )
   CASE "p"
      Publish()
      EXIT
   CASE "c"
      Consume()
      EXIT
   OTHERWISE
      ? "Pass 'p' for publish or 'c' for consume as parameter"
   ENDSWITCH

   RETURN

STATIC FUNCTION NewConnection()

   LOCAL oConn := AMQPConnection():New()

   oConn:SetHost( s_cHost )
   oConn:SetPort( s_nPort )
   oConn:SetSSL( s_lSSL )
   oConn:SetAuth( s_cUser, s_cPassword )
   oConn:SetVirtualHost( s_cVirtualHost )

   RETURN oConn

STATIC PROCEDURE Publish()

   LOCAL oConn := NewConnection()

   LOCAL cData := "Hello, world!"
   LOCAL cExchange := "amq.direct"
   LOCAL cRoutingKey := "test-routingkey"

   IF oConn:Connect() != AMQP_STATUS_OK
      ? "Connect status:", amqp_error_string2( oConn:GetStatus() )
      RETURN
   ENDIF

   IF oConn:Login() != AMQP_RESPONSE_NORMAL
      ? "Login response:", oConn:GetResponse()
      RETURN
   ENDIF

   IF oConn:OpenChannel( 1 ) != AMQP_RESPONSE_NORMAL
      ? "OpenChannel response:", oConn:GetResponse()
      RETURN
   ENDIF

   IF oConn:BasicPublish( cData, 1, cExchange, cRoutingKey ) != AMQP_STATUS_OK
      ? "Publish status:", amqp_error_string2( oConn:GetStatus() )
      RETURN
   ENDIF

   ? "Message Published"

   IF oConn:CloseChannel() != AMQP_RESPONSE_NORMAL
      ? "CloseChannel status:", oConn:GetResponse()
      RETURN
   ENDIF

   oConn:Close()

   RETURN

STATIC PROCEDURE Consume()

   LOCAL oConn := NewConnection()

   LOCAL cQueueName := "test-queue"
   LOCAL oEnvelope

   IF oConn:Connect() != AMQP_STATUS_OK
      ? "Connect status:", amqp_error_string2( oConn:GetStatus() )
      RETURN
   ENDIF

   IF oConn:Login() != AMQP_RESPONSE_NORMAL
      ? "Login response:", oConn:GetResponse()
      RETURN
   ENDIF

   IF oConn:OpenChannel( 1 ) != AMQP_RESPONSE_NORMAL
      ? "OpenChannel response:", oConn:GetResponse()
      RETURN
   ENDIF

   IF oConn:BasicConsume( 1, cQueueName ) != AMQP_RESPONSE_NORMAL
      ? "BasicConsume status:", oConn:GetResponse()
      RETURN
   ENDIF

   DO WHILE Inkey() != Asc( "q" )

      oEnvelope := AMQPEnvelope():New()

      oConn:MaybeReleaseBuffers()

      IF oConn:ConsumeMessage( oEnvelope, AMQP_TIMEOUT_INFINITE ) != AMQP_RESPONSE_NORMAL
         ? "ConsumeMessage status:", oConn:GetResponse()
         EXIT
      ELSE
         ? "GetExchange:", oEnvelope:GetExchange()
         ? "GetRoutingKey:", oEnvelope:GetRoutingKey()
         ? "GetMessageBody:", oEnvelope:GetMessageBody()

         IF oConn:BasicAck( 1, oEnvelope:GetDeliveryTag() ) != 0
            ? "BasicAck error"
         ENDIF
      ENDIF
   ENDDO

   IF oConn:CloseChannel() != AMQP_RESPONSE_NORMAL
      ? "CloseChannel status:", oConn:GetResponse()
      RETURN
   ENDIF

   oConn:Close()

   RETURN
