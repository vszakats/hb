/* Copyright 2017 Viktor Szakats (vszakats.net/harbour) */
/* Copyright 2015 https://github.com/emazv72 */

/* Basic Publish and Consume API Test */

#require "hbamqp"

PROCEDURE Main( cCommand, cURL )  /* amqps://guest:guest@localhost:5671/vhost */

   LOCAL aConn

   LOCAL cCACert := iif( hb_vfExists( "curl-ca-bundle.crt" ), "curl-ca-bundle.crt", )

   ? "librabbitmq version:", amqp_version(), "0x" + hb_NumToHex( amqp_version_number() )

   IF ( HB_ISSTRING( cURL ) .AND. ! cURL == "" ) .OR. ;
      ! ( cURL := hb_MemoRead( hb_FNameExtSet( __FILE__, ".url" ) ) ) == ""
      amqp_parse_url( cURL, @aConn )
   ELSE
      aConn := amqp_default_connection_info()
   ENDIF

   ? "Connection:", hb_ValToExp( hb_AIns( ADel( AClone( aConn ), HB_AMQP_CONN_cPASSWORD ), HB_AMQP_CONN_cPASSWORD, "*" ) )

   SWITCH hb_asciiLower( hb_defaultValue( cCommand, "" ) )
   CASE "p"
      Publish( aConn, cCACert )
      EXIT
   CASE "c"
      Consume( aConn, cCACert )
      EXIT
   OTHERWISE
      ? "Pass 'p' for publish or 'c' for consume as parameter. Doing a publish-consume cycle now."
      Publish( aConn, cCACert )
      Consume( aConn, cCACert )
   ENDSWITCH

   RETURN

STATIC PROCEDURE Publish( aConn, cCACert )

   LOCAL oConn := AMQPConnection():New( aConn )

   LOCAL cData := "Hello, world!"
   LOCAL cExchange := ""  /* amq.direct */
   LOCAL cRoutingKey := "hello"

   IF oConn:Connect( cCACert ) != AMQP_STATUS_OK
      ? "Connect status:", oConn:GetStatusStr()
      RETURN
   ENDIF

   IF oConn:Login() != AMQP_RESPONSE_NORMAL
      ? "Login response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   IF oConn:OpenChannel( 1 ) != AMQP_RESPONSE_NORMAL
      ? "OpenChannel response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   IF oConn:BasicPublish( cData, 1, cExchange, cRoutingKey ) != AMQP_STATUS_OK
      ? "Publish status:", oConn:GetStatusStr()
      RETURN
   ENDIF

   ? "Message Published"

   IF oConn:CloseChannel() != AMQP_RESPONSE_NORMAL
      ? "CloseChannel response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   oConn:Close()

   RETURN

STATIC PROCEDURE Consume( aConn, cCACert )

   LOCAL oConn := AMQPConnection():New( aConn )

   LOCAL cQueueName := "hello"
   LOCAL pEnvelope

   IF oConn:Connect( cCACert ) != AMQP_STATUS_OK
      ? "Connect status:", oConn:GetStatusStr()
      RETURN
   ENDIF

   IF oConn:Login() != AMQP_RESPONSE_NORMAL
      ? "Login response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   IF oConn:OpenChannel( 1 ) != AMQP_RESPONSE_NORMAL
      ? "OpenChannel response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   IF oConn:BasicConsume( 1, cQueueName ) != AMQP_RESPONSE_NORMAL
      ? "BasicConsume response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   DO WHILE .T.

      pEnvelope := amqp_envelope_new()

      oConn:MaybeReleaseBuffers()

      IF oConn:ConsumeMessage( pEnvelope, 2000 ) == AMQP_RESPONSE_NORMAL
         ? "Response:", hb_ValToExp( oConn:GetResponseDetails() )
         ? "Exchange:", amqp_envelope_getexchange( pEnvelope )
         ? "RoutingKey:", amqp_envelope_getroutingkey( pEnvelope )
         ? "MessageBody:", amqp_envelope_getmessagebody( pEnvelope )

         IF oConn:BasicAck( 1, amqp_envelope_getdeliverytag( pEnvelope ) ) != 0
            ? "BasicAck error"
         ENDIF
      ELSE
         ? "ConsumeMessage response:", oConn:GetResponseStr()
         EXIT
      ENDIF
   ENDDO

   IF oConn:CloseChannel() != AMQP_RESPONSE_NORMAL
      ? "CloseChannel response:", oConn:GetResponseStr()
      RETURN
   ENDIF

   oConn:Close()

   RETURN
