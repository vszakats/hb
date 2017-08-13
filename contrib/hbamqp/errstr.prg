/*
 * amqp supplementary functions (error strings)
 *
 * Copyright 2017 Viktor Szakats (vszakats.net/harbour)
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

#include "hbamqp.ch"

FUNCTION hb_amqp_response_string( nResponse )

   IF ! HB_ISNUMERIC( nResponse )
      RETURN "HB_AMQP_RESPONSE_INVALID"
   ENDIF

   SWITCH nResponse
   CASE AMQP_RESPONSE_NONE              ; RETURN "AMQP_RESPONSE_NONE"
   CASE AMQP_RESPONSE_NORMAL            ; RETURN "AMQP_RESPONSE_NORMAL"
   CASE AMQP_RESPONSE_LIBRARY_EXCEPTION ; RETURN "AMQP_RESPONSE_LIBRARY_EXCEPTION"
   CASE AMQP_RESPONSE_SERVER_EXCEPTION  ; RETURN "AMQP_RESPONSE_SERVER_EXCEPTION"
   ENDSWITCH

   RETURN "HB_AMQP_RESPONSE_UNRECOGNIZED_" + hb_ntos( nResponse )

FUNCTION hb_amqp_status_string( nStatus )

   IF ! HB_ISNUMERIC( nStatus )
      RETURN "HB_AMQP_RESPONSE_INVALID"
   ENDIF

   SWITCH nStatus
   CASE AMQP_STATUS_OK                         ; RETURN "AMQP_STATUS_OK"
   CASE AMQP_STATUS_NO_MEMORY                  ; RETURN "AMQP_STATUS_NO_MEMORY"
   CASE AMQP_STATUS_BAD_AMQP_DATA              ; RETURN "AMQP_STATUS_BAD_AMQP_DATA"
   CASE AMQP_STATUS_UNKNOWN_CLASS              ; RETURN "AMQP_STATUS_UNKNOWN_CLASS"
   CASE AMQP_STATUS_UNKNOWN_METHOD             ; RETURN "AMQP_STATUS_UNKNOWN_METHOD"
   CASE AMQP_STATUS_HOSTNAME_RESOLUTION_FAILED ; RETURN "AMQP_STATUS_HOSTNAME_RESOLUTION_FAILED"
   CASE AMQP_STATUS_INCOMPATIBLE_AMQP_VERSION  ; RETURN "AMQP_STATUS_INCOMPATIBLE_AMQP_VERSION"
   CASE AMQP_STATUS_CONNECTION_CLOSED          ; RETURN "AMQP_STATUS_CONNECTION_CLOSED"
   CASE AMQP_STATUS_BAD_URL                    ; RETURN "AMQP_STATUS_BAD_URL"
   CASE AMQP_STATUS_SOCKET_ERROR               ; RETURN "AMQP_STATUS_SOCKET_ERROR"
   CASE AMQP_STATUS_INVALID_PARAMETER          ; RETURN "AMQP_STATUS_INVALID_PARAMETER"
   CASE AMQP_STATUS_TABLE_TOO_BIG              ; RETURN "AMQP_STATUS_TABLE_TOO_BIG"
   CASE AMQP_STATUS_WRONG_METHOD               ; RETURN "AMQP_STATUS_WRONG_METHOD"
   CASE AMQP_STATUS_TIMEOUT                    ; RETURN "AMQP_STATUS_TIMEOUT"
   CASE AMQP_STATUS_TIMER_FAILURE              ; RETURN "AMQP_STATUS_TIMER_FAILURE"
   CASE AMQP_STATUS_HEARTBEAT_TIMEOUT          ; RETURN "AMQP_STATUS_HEARTBEAT_TIMEOUT"
   CASE AMQP_STATUS_UNEXPECTED_STATE           ; RETURN "AMQP_STATUS_UNEXPECTED_STATE"
   CASE AMQP_STATUS_TCP_ERROR                  ; RETURN "AMQP_STATUS_TCP_ERROR"
   CASE AMQP_STATUS_TCP_SOCKETLIB_INIT_ERROR   ; RETURN "AMQP_STATUS_TCP_SOCKETLIB_INIT_ERROR"
   CASE AMQP_STATUS_SSL_ERROR                  ; RETURN "AMQP_STATUS_SSL_ERROR"
   CASE AMQP_STATUS_SSL_HOSTNAME_VERIFY_FAILED ; RETURN "AMQP_STATUS_SSL_HOSTNAME_VERIFY_FAILED"
   CASE AMQP_STATUS_SSL_PEER_VERIFY_FAILED     ; RETURN "AMQP_STATUS_SSL_PEER_VERIFY_FAILED"
   CASE AMQP_STATUS_SSL_CONNECTION_FAILED      ; RETURN "AMQP_STATUS_SSL_CONNECTION_FAILED"
   ENDSWITCH

   RETURN "HB_AMQP_STATUS_UNRECOGNIZED_" + hb_ntos( nStatus )
