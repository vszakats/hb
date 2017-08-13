/*
 * AMQP Connection
 *
 * Copyright 2017 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2015 https://github.com/emazv72
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

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

   METHOD Close( nReason )

   METHOD GetStatus()      INLINE ::status
   METHOD GetResponse()    INLINE ::response
   METHOD GetStatusStr()   INLINE hb_StrFormat( "%1$d: %2$s (%3$s)", ::status, hb_amqp_status_string( ::status ), amqp_error_string2( ::status ) )
   METHOD GetResponseStr() INLINE hb_StrFormat( "%1$d: %2$s: %3$s", ::response, hb_amqp_response_string( ::response ), hb_ValToExp( ::hResponse ) )

   METHOD GetResponseDetails() INLINE ::hResponse

   PROTECTED:

   VAR pConn
   VAR pSocket     AS POINTER

   VAR status      AS NUMERIC  /* AMQP_STATUS_* */
   VAR response    AS NUMERIC  /* AMQP_RESPONSE_* */
   VAR hResponse   AS HASH

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

METHOD PROCEDURE Close( nReason ) CLASS AMQPConnection

   IF ! Empty( ::pConn )
      amqp_connection_close( ::pConn, nReason, @::hResponse )
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

   RETURN ::response := amqp_login( ::pConn, ::virtualHost, 0, ::frameSize, 0, ::loginMethod, ::user, ::password, @::hResponse )

METHOD OpenChannel( nChannel ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::response := amqp_channel_open( ::pConn, nChannel, @::hResponse )

METHOD CloseChannel( nChannel ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::response := amqp_channel_close( ::pConn, nChannel, @::hResponse )

METHOD BasicPublish( cData, nChannel, cExchange, cRoutingKey ) CLASS AMQPConnection

   IF Empty( ::pConn )
      hb_traceLog( "Connection Error" )
   ENDIF
   IF Empty( ::pSocket )
      hb_traceLog( "Socket Error" )
   ENDIF

   RETURN ::status := amqp_basic_publish( ::pConn, nChannel, cExchange, cRoutingKey, ::mandatory, ::immediate, ::hMessageProperties, cData, @::hResponse )

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

   RETURN ::status := amqp_basic_consume( ::pConn, nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive, @::hResponse )

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

   RETURN ::status := amqp_consume_message( ::pConn, xEnvelope, nTimeoutMS, @::hResponse )

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
