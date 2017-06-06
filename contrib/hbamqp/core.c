/* Requires: rabbitmq-c 0.4.0 or upper */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#include <amqp.h>
#include <amqp_framing.h>
#include <amqp_tcp_socket.h>
#include <amqp_ssl_socket.h>

#define HB_AMQP_VERS( ma, mi, mu )  \
   ( AMQP_VERSION_MAJOR > ma || \
   ( AMQP_VERSION_MAJOR == ma && \
   ( AMQP_VERSION_MINOR > mi || \
   ( AMQP_VERSION_MINOR == mi && \
     AMQP_VERSION_PATCH >= mu ) ) ) )

typedef struct _HB_MQCONN
{
   amqp_connection_state_t conn;
} HB_MQCONN, * PHB_MQCONN;

typedef amqp_envelope_t * PHB_ENVELOPE;

static amqp_response_type_enum s_decode_reply( amqp_rpc_reply_t x, char const * context )
{
   switch( x.reply_type )
   {
      case AMQP_RESPONSE_NORMAL:
         break;

      case AMQP_RESPONSE_NONE:
         HB_TRACE( HB_TR_ERROR, ( "amqp - %s: missing RPC reply type!", context ) );
         break;

      case AMQP_RESPONSE_LIBRARY_EXCEPTION:
         HB_TRACE( HB_TR_ERROR, ( "amqp - %s: %d %s", context, x.library_error, amqp_error_string2( x.library_error ) ) );
         break;

      case AMQP_RESPONSE_SERVER_EXCEPTION:
         switch( x.reply.id )
         {
            case AMQP_CONNECTION_CLOSE_METHOD:
            {
               amqp_connection_close_t * m = ( amqp_connection_close_t * ) x.reply.decoded;
               HB_TRACE( HB_TR_ERROR, ( "amqp - %s: server connection error %d, message: %.*s",
                                        context,
                                        m->reply_code,
                                        ( int ) m->reply_text.len, ( const char * ) m->reply_text.bytes ) );

               break;
            }
            case AMQP_CHANNEL_CLOSE_METHOD:
            {
               amqp_channel_close_t * m = ( amqp_channel_close_t * ) x.reply.decoded;

               HB_TRACE( HB_TR_ERROR, ( "amqp - %s: server channel error %d, message: %.*s",
                                        context,
                                        m->reply_code,
                                        ( int ) m->reply_text.len, ( const char * ) m->reply_text.bytes ) );
               break;
            }
            default:
               HB_TRACE( HB_TR_ERROR, ( "amqp - %s: unrecognized server error, method id 0x%08X", context, x.reply.id ) );
               break;
         }
         break;
#if 0
      default:
         HB_TRACE( HB_TR_ERROR, ( "amqp - %s: unrecognized reply type %d", context, x.reply_type ) );
         break;
#endif
   }

   return x.reply_type;
}

static int s_decode_status( int status, char const * context )
{
   if( status != AMQP_STATUS_OK )
   {
      HB_TRACE( HB_TR_ERROR, ( "amqp - %s status=%d (%s)", context, status, amqp_error_string2( status ) ) );
   }

   return status;
}

/* Creates a new amqp_connection_state_t object.
   amqp_connection_state_t objects created with this function should be freed with amqp_destroy_connection()
   amqp_new_connection() --> pConn */
HB_FUNC( AMQP_NEW_CONNECTION )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_xgrab( sizeof( HB_MQCONN ) );

   if( pConn )
      pConn->conn = amqp_new_connection();

   hb_retptr( pConn );
}

/* amqp_connection_close( pConn ) --> nResponse */
HB_FUNC( AMQP_CONNECTION_CLOSE )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
      hb_retni( s_decode_reply(
                   amqp_connection_close( pConn->conn, AMQP_REPLY_SUCCESS ),
                   "amqp_connection_close()" ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_destroy_connection( pConn ) */
HB_FUNC( AMQP_DESTROY_CONNECTION )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_destroy_connection( pConn->conn );
      hb_xfree( pConn );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_tcp_socket_new( pConn ) --> pSocket */
HB_FUNC( AMQP_TCP_SOCKET_NEW )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_socket_t * pSocket = amqp_tcp_socket_new( pConn->conn );
      hb_retptr( pSocket );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_set_initialize_ssl_library( lDoInitialize ) */
HB_FUNC( AMQP_SET_INITIALIZE_SSL_LIBRARY )
{
   amqp_set_initialize_ssl_library( ( amqp_boolean_t ) hb_parl( 1 ) /* do_initialize */ );
}

/* amqp_ssl_socket_new( pConn ) --> pSocket */
HB_FUNC( AMQP_SSL_SOCKET_NEW )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_socket_t * pSocket = amqp_ssl_socket_new( pConn->conn );
      hb_retptr( pSocket );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_ssl_socket_set_cacert( pSocket, cCACertFileName_PEM ) --> nStatus */
HB_FUNC( AMQP_SSL_SOCKET_SET_CACERT )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
      hb_retni( amqp_ssl_socket_set_cacert( pSocket, hb_parcx( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_ssl_socket_set_key( pSocket, cClientCertFileName_PEM, cClientKeyFileName_PEM ) --> nStatus */
HB_FUNC( AMQP_SSL_SOCKET_SET_KEY )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
      hb_retni( amqp_ssl_socket_set_key( pSocket, hb_parcx( 2 ), hb_parcx( 3 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_ssl_socket_set_ssl_versions( pSocket, nVersionMin, nVersionMax ) */
HB_FUNC( AMQP_SSL_SOCKET_SET_SSL_VERSIONS )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
#if HB_AMQP_VERS( 0, 8, 0 )
      hb_retni( amqp_ssl_socket_set_ssl_versions( pSocket,
                                                 ( amqp_tls_version_t ) hb_parni( 2 ),
                                                 ( amqp_tls_version_t ) hb_parni( 3 ) ) );
#else
      hb_retni( AMQP_STATUS_OK );
#endif
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_ssl_socket_set_verify_peer( pSocket, lVerify ) */
HB_FUNC( AMQP_SSL_SOCKET_SET_VERIFY_PEER )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
#if HB_AMQP_VERS( 0, 8, 0 )
      amqp_ssl_socket_set_verify_peer( pSocket, ( amqp_boolean_t ) hb_parl( 2 ) /* verify */ );
#else
      hb_ret();
#endif
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_ssl_socket_set_verify_hostname( pSocket, lVerify ) */
HB_FUNC( AMQP_SSL_SOCKET_SET_VERIFY_HOSTNAME )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
#if HB_AMQP_VERS( 0, 8, 0 )
      amqp_ssl_socket_set_verify_hostname( pSocket, ( amqp_boolean_t ) hb_parl( 2 ) /* verify */ );
#else
      hb_ret();
#endif
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Attempts to open a socket to hostname on portnumber
   amqp_socket_open( pSocket, cHost, nPort ) --> nStatus */
HB_FUNC( AMQP_SOCKET_OPEN )
{
   amqp_socket_t * pSocket = ( amqp_socket_t * ) hb_parptr( 1 );

   if( pSocket )
      hb_retni( amqp_socket_open( pSocket, hb_parcx( 2 ), hb_parni( 3 ) /* port */ ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Login to the broker.
   After using amqp_open_socket() and amqp_set_sockfd(), call amqp_login() to complete connecting to the broker
   amqp_login( pConn, cVHost, nFrameSize, nMethod, user, pwd ) --> nResponse */
HB_FUNC( AMQP_LOGIN )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
      hb_retni( s_decode_reply(
                   amqp_login(
                      pConn->conn,
                      hb_parcx( 2 ) /* vhost */,
                      0 /* ch max */,
                      hb_parnidef( 3, 0x20000 ) /* frame size */,
                      0 /* heartbeat  - unsupported */,
                      ( amqp_sasl_method_enum ) hb_parni( 4 ) /* AMQP_SASL_METHOD_PLAIN */,
                      hb_parcx( 5 ) /* user */,
                      hb_parcx( 6 ) /* pwd */ ),
                   "amqp_login()" ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_channel_open( pConn, nChannel ) --> nResponse */
HB_FUNC( AMQP_CHANNEL_OPEN )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_channel_open( pConn->conn, ( amqp_channel_t ) hb_parnidef( 2, 1 ) );

      hb_retni( s_decode_reply( amqp_get_rpc_reply( pConn->conn ), "amqp_channel_open()" ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_channel_close( pConn, nChannel ) --> nResponse */
HB_FUNC( AMQP_CHANNEL_CLOSE )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      hb_retni( s_decode_reply(
                   amqp_channel_close(
                      pConn->conn,
                      ( amqp_channel_t ) hb_parnidef( 2, 1 ),
                      AMQP_REPLY_SUCCESS /* code */ ),
                   "amqp_channel_close()" ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_exchange_declare( pConn, nChannel, cExchange, cExchangeType, lPassive, lDurable, lAutoDelete, lInternal ) --> nResponse */
HB_FUNC( AMQP_EXCHANGE_DECLARE )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_exchange_declare( pConn->conn,
                             ( amqp_channel_t ) hb_parnidef( 2, 1 ),  /* channel */
                             amqp_cstring_bytes( hb_parcx( 3 ) ),     /* exchange */
                             amqp_cstring_bytes( hb_parcx( 4 ) ),     /* type */
                             ( amqp_boolean_t ) hb_parl( 5 ),         /* passive */
                             ( amqp_boolean_t ) hb_parl( 6 ),         /* durable */
#if HB_AMQP_VERS( 0, 6, 0 )
                             ( amqp_boolean_t ) hb_parl( 7 ),         /* auto_delete */
                             ( amqp_boolean_t ) hb_parl( 8 ),         /* internal */
#endif
                             amqp_empty_table );

      hb_retni( s_decode_reply(
                   amqp_get_rpc_reply(
                      pConn->conn ),
                   "amqp_exchange_declare()" ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Publish a message to the broker.
   amqp_basic_publish( pConn, nChannel, cExchange, cKey, nMandatory, nImmediate, hProperties, cBody ) --> nStatus
   - hProperties keys: content_type, delivery_mode */
HB_FUNC( AMQP_BASIC_PUBLISH )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn && HB_ISHASH( 7 ) )
   {
      PHB_ITEM pProps = hb_param( 7, HB_IT_HASH );

      amqp_basic_properties_t props;

      props._flags = AMQP_BASIC_CONTENT_TYPE_FLAG |
                     AMQP_BASIC_DELIVERY_MODE_FLAG;

      props.content_type  = amqp_cstring_bytes( hb_itemGetCPtr( hb_hashGetCItemPtr( pProps, "content_type" ) ) );
      props.delivery_mode = hb_itemGetNI( hb_hashGetCItemPtr( pProps, "delivery_mode" ) );  /* AMQP_DELIVERY_* */

      hb_retni( s_decode_status(
                   amqp_basic_publish( pConn->conn,
                                       ( amqp_channel_t ) hb_parnidef( 2, 1 ),  /* channel */
                                       amqp_cstring_bytes( hb_parcx( 3 ) ),     /* exchange */
                                       amqp_cstring_bytes( hb_parcx( 4 ) ),     /* routing_key */
                                       hb_parni( 5 ),                           /* mandatory */
                                       hb_parni( 6 ),                           /* immediate */
                                       &props,                                  /* properties */
                                       amqp_cstring_bytes( hb_parcx( 8 ) ) ),   /* body */
                   "amqp_basic_publish()" ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_basic_consume( pConn, nChannel, cQueueName, cConsumerTag, lNoLocal, lNoAck, lExclusive ) --> nResponse */
HB_FUNC( AMQP_BASIC_CONSUME )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
   {
      amqp_basic_consume( pConn->conn,
                          ( amqp_channel_t ) hb_parnidef( 2, 1 ),  /* channel */
                          amqp_cstring_bytes( hb_parcx( 3 ) ),     /* queuename */
                          amqp_cstring_bytes( hb_parcx( 4 ) ),     /* consumer_tag */
                          ( amqp_boolean_t ) hb_parl( 5 ),         /* no_local */
                          ( amqp_boolean_t ) hb_parl( 6 ),         /* no_ack */
                          ( amqp_boolean_t ) hb_parl( 7 ),         /* exclusive */
                          amqp_empty_table );

      hb_retni( s_decode_reply( amqp_get_rpc_reply( pConn->conn ), "amqp_basic_consume()" ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Acknowledges a message
   amqp_basic_ack( pConn, nChannel, nDeliveryTag, lMultiple ) --> nResponse */
HB_FUNC( AMQP_BASIC_ACK )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
      hb_retni( amqp_basic_ack( pConn->conn /* state */,
                                ( amqp_channel_t ) hb_parnidef( 2, 1 ) /* channel */,
                                hb_parnint( 3 ) /* delivery_tag */,
                                ( amqp_boolean_t ) hb_parl( 4 ) /* multiple */ ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Wait for and consume a message
   amqp_consume_message( pConn, pEnvelope, nTimeoutMs ) --> nResponse */
HB_FUNC( AMQP_CONSUME_MESSAGE )
{
   PHB_MQCONN   pConn     = ( PHB_MQCONN ) hb_parptr( 1 );
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 2 );

   if( pConn && pEnvelope )
   {
      struct timeval * pTimeout;

      if( HB_ISNUM( 3 ) )
      {
         pTimeout = ( struct timeval * ) hb_xgrabz( sizeof( struct timeval ) );
         pTimeout->tv_usec = hb_parni( 3 ) * 1000L;  /* ms to us */
      }
      else
         pTimeout = NULL;  /* infinite */

      hb_retni( s_decode_reply(
         amqp_consume_message(
            pConn->conn,
            pEnvelope,
            pTimeout,
            hb_parni( 4 ) /* flags */ ),
         "amqp_consume_message()" ) );

      if( HB_ISNUM( 3 ) && pTimeout )
         hb_xfree( pTimeout );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( AMQP_ENVELOPE_NEW )
{
   hb_retptr( ( PHB_ENVELOPE ) hb_xgrabz( sizeof( amqp_envelope_t ) ) );
}

/* Frees memory associated with a amqp_envelope_t allocated in amqp_consume_message()
   amqp_destroy_envelope( pEnvelope ) */
HB_FUNC( AMQP_DESTROY_ENVELOPE )
{
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 1 );

   if( pEnvelope )
   {
      amqp_destroy_envelope( pEnvelope );

      hb_xfree( pEnvelope );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_envelope_getmessagebody( pEnvelope ) --> cMessage */
HB_FUNC( AMQP_ENVELOPE_GETMESSAGEBODY )
{
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 1 );

   if( pEnvelope )
      hb_retclen( pEnvelope->message.body.bytes, pEnvelope->message.body.len );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_envelope_getdeliverytag( pEnvelope ) --> nDeliveryTag */
HB_FUNC( AMQP_ENVELOPE_GETDELIVERYTAG )
{
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 1 );

   if( pEnvelope )
      hb_retnll( pEnvelope->delivery_tag );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( AMQP_ENVELOPE_GETROUTINGKEY )
{
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 1 );

   if( pEnvelope )
      hb_retclen( pEnvelope->routing_key.bytes, pEnvelope->routing_key.len );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( AMQP_ENVELOPE_GETEXCHANGE )
{
   PHB_ENVELOPE pEnvelope = ( PHB_ENVELOPE ) hb_parptr( 1 );

   if( pEnvelope )
      hb_retclen( pEnvelope->exchange.bytes, pEnvelope->exchange.len );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_maybe_release_buffers( pConn ) */
HB_FUNC( AMQP_MAYBE_RELEASE_BUFFERS )
{
   PHB_MQCONN pConn = ( PHB_MQCONN ) hb_parptr( 1 );

   if( pConn )
      amqp_maybe_release_buffers( pConn->conn );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* amqp_error_string2( nStatus ) --> cStatus */
HB_FUNC( AMQP_ERROR_STRING2 )
{
   hb_retc_const( amqp_error_string2( hb_parni( 1 ) ) );
}

/* amqp_version() */
HB_FUNC( AMQP_VERSION )
{
   hb_retc( amqp_version() );
}
