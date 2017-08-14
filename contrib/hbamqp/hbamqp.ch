/*
 * AMQP Header
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

#ifndef HBAMQP_CH_
#define HBAMQP_CH_

/* amqp_sasl_method_enum */
#define AMQP_SASL_METHOD_PLAIN                  0        /* the PLAIN SASL method for authentication to the broker */

/* amqp_delivery_mode_enum */
#define AMQP_DELIVERY_NONPERSISTENT             1        /* Non-persistent message */
#define AMQP_DELIVERY_PERSISTENT                2        /* Persistent message */

/* amqp_response_type_enum */
#define AMQP_RESPONSE_NONE                      0        /* the library got an EOF from the socket */
#define AMQP_RESPONSE_NORMAL                    1        /* response normal the RPC completed successfully */
#define AMQP_RESPONSE_LIBRARY_EXCEPTION         2        /* library error an error occurred in the library examine the library_error */
#define AMQP_RESPONSE_SERVER_EXCEPTION          3        /* server exception the broker returned an error check replay */

/* amqp_tls_version_t */
#define AMQP_TLSv1                              1
#define AMQP_TLSv1_1                            2
#define AMQP_TLSv1_2                            3
#define AMQP_TLSvLATEST                         0xfff

/* amqp_status_enum */
#define AMQP_STATUS_OK                          0        /* Operation successful */
#define AMQP_STATUS_NO_MEMORY                   -0x0001  /* Memory allocation failed */
#define AMQP_STATUS_BAD_AMQP_DATA               -0x0002  /* Incorrect or corrupt data was received from the broker. This is a protocol error. */
#define AMQP_STATUS_UNKNOWN_CLASS               -0x0003  /* An unknown AMQP class was received. This is a protocol error. */
#define AMQP_STATUS_UNKNOWN_METHOD              -0x0004  /* An unknown AMQP method was received. This is a protocol error. */
#define AMQP_STATUS_HOSTNAME_RESOLUTION_FAILED  -0x0005  /* Unable to resolve the hostname */
#define AMQP_STATUS_INCOMPATIBLE_AMQP_VERSION   -0x0006  /* The broker advertised an incompatible AMQP version */
#define AMQP_STATUS_CONNECTION_CLOSED           -0x0007  /* The connection to the broker has been closed */
#define AMQP_STATUS_BAD_URL                     -0x0008  /* malformed AMQP URL */
#define AMQP_STATUS_SOCKET_ERROR                -0x0009  /* A socket error occurred */
#define AMQP_STATUS_INVALID_PARAMETER           -0x000A  /* An invalid parameter was passed into the function */
#define AMQP_STATUS_TABLE_TOO_BIG               -0x000B  /* The amqp_table_t object cannot be serialized because the output buffer is too small */
#define AMQP_STATUS_WRONG_METHOD                -0x000C  /* The wrong method was received */
#define AMQP_STATUS_TIMEOUT                     -0x000D  /* Operation timed out */
#define AMQP_STATUS_TIMER_FAILURE               -0x000E  /* The underlying system timer facility failed */
#define AMQP_STATUS_HEARTBEAT_TIMEOUT           -0x000F  /* Timed out waiting for heartbeat */
#define AMQP_STATUS_UNEXPECTED_STATE            -0x0010  /* Unexpected protocol state */

#define AMQP_STATUS_TCP_ERROR                   -0x0100  /* A generic TCP error occurred */
#define AMQP_STATUS_TCP_SOCKETLIB_INIT_ERROR    -0x0101  /* An error occurred trying to initialize the socket library*/

#define AMQP_STATUS_SSL_ERROR                   -0x0200  /* A generic SSL error occurred. */
#define AMQP_STATUS_SSL_HOSTNAME_VERIFY_FAILED  -0x0201  /* SSL validation of hostname against peer certificate failed */
#define AMQP_STATUS_SSL_PEER_VERIFY_FAILED      -0x0202  /* SSL validation of peer certificate failed. */
#define AMQP_STATUS_SSL_CONNECTION_FAILED       -0x0203  /* SSL handshake failed. */

#define AMQP_TIMEOUT_INFINITE                   NIL

/* amqp_parse_url(), amqp_default_connection_info() array positions */
#define HB_AMQP_CONN_cUSER                      1
#define HB_AMQP_CONN_cPASSWORD                  2
#define HB_AMQP_CONN_cHOST                      3
#define HB_AMQP_CONN_cVHOST                     4
#define HB_AMQP_CONN_nPORT                      5
#define HB_AMQP_CONN_lSSL                       6
#define HB_AMQP_CONN_LEN                        6

#endif
