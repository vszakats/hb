/* AMQP Connection */

#include "hbclass.ch"

#include "hbamqp.ch"

CREATE CLASS AMQPConnection

   METHOD New()

   METHOD SetAuth( cUser, cPassword, nMethod )
   METHOD SetHost( cHost )  INLINE ::host := cHost
   METHOD SetPort( nPort )  INLINE ::port := hb_defaultValue( nPort, 5672 )
   METHOD SetSSL( lSSL )    INLINE ::ssl := hb_defaultValue( lSSL, .T. )

   METHOD SetVirtualHost( cVirtualHost )  INLINE ::virtualHost := cVirtualHost
   METHOD SetFramSize( nFrameSize )       INLINE ::frameSize   := nFrameSize

   METHOD Connect( cCACert, cCert, cKey, lNoVerifyPeer, lNoVerifyHost )

   METHOD Login()

   METHOD OpenChannel( nChannel )
   METHOD CloseChannel( nChannel )

   METHOD SetMandatory( nMandatory )  INLINE ::mandatory := nMandatory
   METHOD SetImmediate( nImmediate )  INLINE ::immediate := nImmediate

   METHOD SetMessageProperties( hMessageProperties )  INLINE ::hMessageProperties := hMessageProperties

   METHOD BasicPublish( cData, nChannel, cExchange, cRoutingKey )

   METHOD ExchangeDeclare( nChannel, cExchange, cExchangeType, lPassive, lDurable, lAutoDelete, lInternal )

   METHOD BasicConsume( nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive )

   METHOD CreateEnvelope()
   METHOD ConsumeMessage( envelope AS OBJECT, nTimeOutMS )
   METHOD BasicAck( nChannel, nDeliveryTag, lMultiple )

   METHOD MaybeReleaseBuffers()

   METHOD Close()

   METHOD GetStatus()    INLINE ::status
   METHOD GetResponse()  INLINE ::response

   PROTECTED:

   VAR pConn       AS OBJECT         /* PHB_MQCONN * */
   VAR pSocket     AS OBJECT         /* amqp_socket_t * */

   VAR status      AS NUMERIC        /* AMQP_STATUS_* */
   VAR response    AS NUMERIC        /* AMQP_RESPONSE_* */

   VAR user        AS CHARACTER      INIT "guest"
   VAR password    AS CHARACTER      INIT "guest"
   VAR loginMethod AS NUMERIC        INIT AMQP_SASL_METHOD_PLAIN

   VAR virtualHost AS CHARACTER      INIT ""

   VAR host        AS CHARACTER      INIT "localhost"
   VAR port        AS NUMERIC        INIT 5672
   VAR ssl         AS LOGICAL        INIT .F.
   VAR frameSize   AS NUMERIC        INIT 0x20000

   VAR exchange    AS CHARACTER      INIT ""
   VAR routingKey  AS CHARACTER      INIT ""
   VAR mandatory   AS NUMERIC        INIT 1
   VAR immediate   AS NUMERIC        INIT 0  /* NOT_IMPLEMENTED - immediate=true */

   VAR hMessageProperties AS OBJECT  INIT { "content_type" => "text", "delivery_mode" => AMQP_DELIVERY_PERSISTENT }

END CLASS

METHOD New() CLASS AMQPConnection

   ::pConn := amqp_new_connection()

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF

   RETURN Self

METHOD Connect( cCACert, cCert, cKey, lNoVerifyPeer, lNoVerifyHost ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF

   ::pSocket := iif( ::ssl, ;
      amqp_ssl_socket_new( ::pConn ), ;
      amqp_tcp_socket_new( ::pConn ) )

   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   IF ::ssl
      IF HB_ISSTRING( cCACert )
         amqp_ssl_socket_set_cacert( ::pSocket, cCACert )
      ENDIF
      IF HB_ISSTRING( cCert ) .AND. HB_ISSTRING( cKey )
         amqp_ssl_socket_set_key( ::pSocket, cCert, cKey )
      ENDIF
      IF hb_defaultValue( lNoVerifyPeer, .F. )
         amqp_ssl_socket_set_verify_peer( ::pSocket, .F. )  /* Insecure - For testing only */
      ENDIF
      IF hb_defaultValue( lNoVerifyHost, .F. )
         amqp_ssl_socket_set_verify_hostname( ::pSocket, .F. )  /* Insecure - For testing only */
      ENDIF
   ENDIF

   RETURN ::status := amqp_socket_open( ::pSocket, ::host, ::port )

METHOD PROCEDURE Close() CLASS AMQPConnection

   IF ! Empty( ::pConn )
      amqp_connection_close( ::pConn )
      amqp_destroy_connection( ::pConn )
   ENDIF

   RETURN

METHOD PROCEDURE SetAuth( cUser, cPassword, nMethod ) CLASS AMQPConnection

   ::user := cUser
   ::password := cPassword

   IF HB_ISNUMERIC( nMethod )
      ::loginMethod := nMethod
   ENDIF

   RETURN

METHOD Login() CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::response := amqp_login( ::pConn, ::virtualHost, ::frameSize, ::loginMethod, ::user, ::password )

METHOD OpenChannel( nChannel ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::response := amqp_channel_open( ::pConn, nChannel )

METHOD CloseChannel( nChannel ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   hb_default( @nChannel, 1 )

   RETURN ::response := amqp_channel_close( ::pConn, nChannel )

METHOD BasicPublish( cData, nChannel, cExchange, cRoutingKey ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   IF cData == NIL
      hb_traceLog( "BasicPublish", "data error (NIL)" )
      cData := ""
   ENDIF

   RETURN ::status := amqp_basic_publish( ::pConn, nChannel, cExchange, cRoutingKey, ::mandatory, ::immediate, ::hMessageProperties, cData )

METHOD ExchangeDeclare( nChannel, cExchange, cExchangeType, lPassive, lDurable, lAutoDelete, lInternal ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::response := amqp_exchange_declare( ::pConn, nChannel, cExchange, cExchangeType, lPassive, lDurable, lAutoDelete, lInternal )

METHOD BasicConsume( nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF
   IF ! HB_ISSTRING( cQueueName )
      hb_traceLog( "Queue Name Error" )
   ENDIF

   RETURN ::status := amqp_basic_consume( ::pConn, nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive )

METHOD CreateEnvelope() CLASS AMQPConnection
   RETURN AMQPEnvelope():FromPtr( amqp_envelope_new() )

METHOD ConsumeMessage( envelope AS OBJECT, nTimeOutMS ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF
   IF Empty( envelope )
      hb_traceLog( "Envelope Error" )
   ENDIF

   RETURN ::status := amqp_consume_message( ::pConn, envelope:GetPtr(), nTimeOutMS )

METHOD BasicAck( nChannel, nDeliveryTag, lMultiple ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF
   IF ! HB_ISNUMERIC( nDeliveryTag )
      hb_traceLog( "Delivery Tag Error" )
   ENDIF

   RETURN ::status := amqp_basic_ack( ::pConn, nChannel, nDeliveryTag, lMultiple )

METHOD PROCEDURE MaybeReleaseBuffers() CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF

   amqp_maybe_release_buffers( ::pConn )

   RETURN
