/* AMQP Connection */

#include "hbclass.ch"

#include "hbamqp.ch"

CREATE CLASS AMQPConnection

   METHOD New( aConn )

   METHOD SetAuth( cUser, cPassword, nMethod )
   METHOD SetHost( cHost ) INLINE ::host := cHost
   METHOD SetPort( nPort ) INLINE ::port := hb_defaultValue( nPort, 5672 )
   METHOD SetSSL( lSSL )   INLINE ::ssl := hb_defaultValue( lSSL, .T. )

   METHOD SetVirtualHost( cVirtualHost ) INLINE ::virtualHost := hb_defaultValue( cVirtualHost, "/" )
   METHOD SetFramSize( nFrameSize )      INLINE ::frameSize   := nFrameSize

   METHOD Connect( cCACert, cCert, cKey, lNoVerifyPeer, lNoVerifyHost )

   METHOD Login()

   METHOD OpenChannel( nChannel )
   METHOD CloseChannel( nChannel )

   METHOD SetMandatory( lMandatory ) INLINE ::mandatory := lMandatory
   METHOD SetImmediate( lImmediate ) INLINE ::immediate := lImmediate

   METHOD SetMessageProperties( hMessageProperties ) INLINE ::hMessageProperties := hMessageProperties

   METHOD BasicPublish( cData, nChannel, cExchange, cRoutingKey )

   METHOD ExchangeDeclare( nChannel, cExchange, cExchangeType, lPassive, lDurable, lAutoDelete, lInternal )

   METHOD BasicConsume( nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive )

   METHOD ConsumeMessage( xEnvelope, nTimeoutMS )
   METHOD BasicAck( nChannel, nDeliveryTag, lMultiple )

   METHOD MaybeReleaseBuffers()

   METHOD Close()

   METHOD GetStatus()      INLINE ::status
   METHOD GetResponse()    INLINE ::response
   METHOD GetStatusStr()   INLINE hb_StrFormat( "%1$d: %2$s (%3$s)", ::status, hb_amqp_status_string( ::status ), amqp_error_string2( ::status ) )
   METHOD GetResponseStr() INLINE hb_StrFormat( "%1$d: %2$s", ::response, hb_amqp_response_string( ::response ) )

   PROTECTED:

   VAR pConn
   VAR pSocket     AS POINTER

   VAR status      AS NUMERIC  /* AMQP_STATUS_* */
   VAR response    AS NUMERIC  /* AMQP_RESPONSE_* */

   VAR user        AS CHARACTER
   VAR password    AS CHARACTER
   VAR loginMethod AS NUMERIC   INIT AMQP_SASL_METHOD_PLAIN

   VAR virtualHost AS CHARACTER

   VAR host        AS CHARACTER
   VAR port        AS NUMERIC
   VAR ssl         AS LOGICAL
   VAR frameSize   AS NUMERIC   INIT 0x20000

   VAR exchange    AS CHARACTER INIT ""
   VAR routingKey  AS CHARACTER INIT ""
   VAR mandatory   AS LOGICAL   INIT .T.
   VAR immediate   AS LOGICAL   INIT .F.

   VAR hMessageProperties AS HASH INIT { "content_type" => "text", "delivery_mode" => AMQP_DELIVERY_PERSISTENT }

END CLASS

METHOD New( aConn ) CLASS AMQPConnection

   IF ! HB_ISARRAY( aConn ) .OR. Len( aConn ) < HB_AMQP_CONN_LEN
      aConn := amqp_default_connection_info()
   ENDIF

   ::SetHost( aConn[ HB_AMQP_CONN_cHOST ] )
   ::SetPort( aConn[ HB_AMQP_CONN_nPORT ] )
   ::SetSSL( aConn[ HB_AMQP_CONN_lSSL ] )
   ::SetAuth( aConn[ HB_AMQP_CONN_cUSER ], aConn[ HB_AMQP_CONN_cPASSWORD ] )
   ::SetVirtualHost( aConn[ HB_AMQP_CONN_cVHOST ] )

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
      ::pConn := NIL
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

   RETURN ::response := amqp_login( ::pConn, ::virtualHost, 0, ::frameSize, 0, ::loginMethod, ::user, ::password )

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

   RETURN ::response := amqp_channel_close( ::pConn, nChannel )

METHOD BasicPublish( cData, nChannel, cExchange, cRoutingKey ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
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

   RETURN ::status := amqp_basic_consume( ::pConn, nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive )

METHOD ConsumeMessage( xEnvelope, nTimeoutMS ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   DO CASE
   CASE HB_ISOBJECT( xEnvelope )
      xEnvelope := xEnvelope:GetPtr()
   CASE ! HB_ISPOINTER( xEnvelope ) .OR. Empty( xEnvelope )
      hb_traceLog( "Envelope Error" )
   ENDCASE

   RETURN ::status := amqp_consume_message( ::pConn, xEnvelope, nTimeoutMS )

METHOD BasicAck( nChannel, nDeliveryTag, lMultiple ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::status := amqp_basic_ack( ::pConn, nChannel, nDeliveryTag, lMultiple )

METHOD PROCEDURE MaybeReleaseBuffers() CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF

   amqp_maybe_release_buffers( ::pConn )

   RETURN
