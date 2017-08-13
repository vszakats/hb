/*
 * AMQP Consumer
 *
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

CREATE CLASS AMQPConsumer

   METHOD New( oConnection )

   METHOD SetChannel( nChannel )     INLINE ::channel   := nChannel
   METHOD SetQueueName( cQueueName ) INLINE ::queueName := cQueueName
   METHOD SetTimeoutMS( nTimeoutMS ) INLINE ::timeoutMS := nTimeoutMS
   METHOD SetAction( xAction )

   METHOD OpenChannel()
   METHOD ConsumeAndDispatch()
   METHOD CloseChannel()

   PROTECTED:

   METHOD DumpEnvelope( hEnvelope )

   VAR connection AS OBJECT    /* AMQPConnection */
   VAR channel    AS NUMERIC   INIT 1
   VAR timeOutMS  AS NUMERIC   INIT AMQP_TIMEOUT_INFINITE
   VAR queueName  AS CHARACTER INIT "default"
   VAR action

END CLASS

METHOD New( oConnection ) CLASS AMQPConsumer

   IF Empty( oConnection )
      hb_traceLog( "Connection Error" )
   ENDIF

   ::connection := oConnection
   ::action := {| hEnvelope | ::DumpEnvelope( hEnvelope ) }

   RETURN Self

METHOD OpenChannel() CLASS AMQPConsumer
   RETURN ;
      ::connection:OpenChannel( ::channel ) == AMQP_RESPONSE_NORMAL .AND. ;
      ::connection:BasicConsume( ::channel, ::queueName ) == AMQP_RESPONSE_NORMAL

METHOD CloseChannel() CLASS AMQPConsumer
   RETURN ::connection:CloseChannel( ::channel ) == AMQP_RESPONSE_NORMAL

METHOD PROCEDURE ConsumeAndDispatch() CLASS AMQPConsumer

   LOCAL pEnvelope := amqp_envelope_new()
   LOCAL hEnvelope
   LOCAL result

   ::connection:MaybeReleaseBuffers()

   IF ( result := ::connection:ConsumeMessage( pEnvelope, ::timeOutMS ) ) == AMQP_RESPONSE_NORMAL

      // build envelope

      hEnvelope := { ;
         "exchange"    => amqp_envelope_getexchange( pEnvelope ), ;
         "routingKey"  => amqp_envelope_getroutingkey( pEnvelope ), ;
         "deliveryTag" => amqp_envelope_getdeliverytag( pEnvelope ), ;
         "messageBody" => amqp_envelope_getmessagebody( pEnvelope ) }

      // call action

      DO CASE
      CASE HB_ISEVALITEM( ::action )

         result := Eval( ::action, hEnvelope )

      CASE HB_ISSTRING( ::action )

         result := hb_ExecFromArray( { ::action, hEnvelope } )

      OTHERWISE
         hb_traceLog( hb_MethodName(), "action error" )
      ENDCASE

      result := hb_default( @result, .F. )

      // action returns .T. -> ack message
      IF result
         IF ::connection:BasicAck( ::channel, amqp_envelope_getdeliverytag( pEnvelope ) ) != 0
            hb_traceLog( ::className(), "basicAck error" )
         ELSE
            hb_traceLog( ::className(), "basicAck OK" )
         ENDIF
      ELSE
         hb_traceLog( ::className(), "action block error" )
      ENDIF
   ELSE
      hb_traceLog( ::className(), hb_StrFormat( "consume Error (result=%1$d, response=%2$s)", result, ::connection:GetResponse() ) )
   ENDIF

   RETURN

METHOD PROCEDURE DumpEnvelope( hEnvelope ) CLASS AMQPConsumer

   hb_traceLog( ::className(), "Raw message:", hb_ValToExp( hEnvelope ) )

   RETURN

METHOD PROCEDURE SetAction( xAction ) CLASS AMQPConsumer

   DO CASE
   CASE HB_ISEVALITEM( xAction )
      /* do nothing */

   CASE HB_ISSTRING( xAction )

      IF ! hb_IsFunction( xAction )
         hb_traceLog( "action is not a function" )
      ENDIF

   OTHERWISE
      hb_traceLog( "Invalid action" )
   ENDCASE

   ::action = xAction

   RETURN
