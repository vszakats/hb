/*
 * libcurl 'easy' API - Harbour header.
 *
 * Copyright 2008-present Viktor Szakats
 * originally based on:
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
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

/* NOTE: This file is also used by C code. */

#ifndef HBCURL_CH_
#define HBCURL_CH_

/* curl_easy_setopt() parameters.
   NOTE: The actual values may be different from the libcurl equivalent. */
#define HB_CURLOPT_FILE                       1
#define HB_CURLOPT_URL                        2
#define HB_CURLOPT_PORT                       3
#define HB_CURLOPT_PROXY                      4
#define HB_CURLOPT_USERPWD                    5
#define HB_CURLOPT_PROXYUSERPWD               6
#define HB_CURLOPT_RANGE                      7
#define HB_CURLOPT_INFILE                     9
#define HB_CURLOPT_ERRORBUFFER                10
#define HB_CURLOPT_WRITEFUNCTION              11
#define HB_CURLOPT_READFUNCTION               12
#define HB_CURLOPT_TIMEOUT                    13
#define HB_CURLOPT_INFILESIZE                 HB_CURLOPT_INFILESIZE_LARGE
#define HB_CURLOPT_POSTFIELDS                 15
#define HB_CURLOPT_REFERER                    16
#define HB_CURLOPT_FTPPORT                    17
#define HB_CURLOPT_USERAGENT                  18
#define HB_CURLOPT_LOW_SPEED_LIMIT            19
#define HB_CURLOPT_LOW_SPEED_TIME             20
#define HB_CURLOPT_RESUME_FROM                HB_CURLOPT_RESUME_FROM_LARGE
#define HB_CURLOPT_COOKIE                     22
#define HB_CURLOPT_HTTPHEADER                 23
#define HB_CURLOPT_HTTPPOST                   24  /* deprecated */
#define HB_CURLOPT_SSLCERT                    25
#define HB_CURLOPT_KEYPASSWD                  26
#define HB_CURLOPT_SSLCERTPASSWD              HB_CURLOPT_KEYPASSWD
#define HB_CURLOPT_SSLKEYPASSWD               HB_CURLOPT_KEYPASSWD
#define HB_CURLOPT_CRLF                       27
#define HB_CURLOPT_QUOTE                      28
#define HB_CURLOPT_WRITEHEADER                29
#define HB_CURLOPT_COOKIEFILE                 31
#define HB_CURLOPT_SSLVERSION                 32
#define HB_CURLOPT_TIMECONDITION              33
#define HB_CURLOPT_TIMEVALUE                  34
#define HB_CURLOPT_CUSTOMREQUEST              36
#define HB_CURLOPT_STDERR                     37
#define HB_CURLOPT_POSTQUOTE                  39
#define HB_CURLOPT_WRITEINFO                  40
#define HB_CURLOPT_VERBOSE                    41  /* talk a lot */
#define HB_CURLOPT_HEADER                     42  /* throw the header out too */
#define HB_CURLOPT_NOPROGRESS                 43  /* shut off the progress meter */
#define HB_CURLOPT_NOBODY                     44  /* use HEAD to get http document */
#define HB_CURLOPT_FAILONERROR                45  /* no output on http error codes >= 300 */
#define HB_CURLOPT_UPLOAD                     46  /* this is an upload */
#define HB_CURLOPT_POST                       47  /* HTTP POST method */
#define HB_CURLOPT_DIRLISTONLY                48  /* Use NLST when listing ftp dir */
#define HB_CURLOPT_FTPLISTONLY                HB_CURLOPT_DIRLISTONLY
#define HB_CURLOPT_APPEND                     50  /* Append instead of overwrite on upload! */
#define HB_CURLOPT_FTPAPPEND                  HB_CURLOPT_APPEND
#define HB_CURLOPT_NETRC                      51
#define HB_CURLOPT_FOLLOWLOCATION             52  /* use Location: Luke! */
#define HB_CURLOPT_TRANSFERTEXT               53  /* transfer data in text/ASCII format */
#define HB_CURLOPT_PUT                        54  /* HTTP PUT */
#define HB_CURLOPT_PROGRESSFUNCTION           56
#define HB_CURLOPT_PROGRESSDATA               57
#define HB_CURLOPT_AUTOREFERER                58
#define HB_CURLOPT_PROXYPORT                  59
#define HB_CURLOPT_POSTFIELDSIZE              HB_CURLOPT_POSTFIELDSIZE_LARGE
#define HB_CURLOPT_HTTPPROXYTUNNEL            61
#define HB_CURLOPT_INTERFACE                  62
#define HB_CURLOPT_KRBLEVEL                   63
#define HB_CURLOPT_KRB4LEVEL                  HB_CURLOPT_KRBLEVEL
#define HB_CURLOPT_SSL_VERIFYPEER             64
#define HB_CURLOPT_CAINFO                     65
#define HB_CURLOPT_MAXREDIRS                  68
#define HB_CURLOPT_FILETIME                   69
#define HB_CURLOPT_TELNETOPTIONS              70
#define HB_CURLOPT_MAXCONNECTS                71
#define HB_CURLOPT_CLOSEPOLICY                72
#define HB_CURLOPT_FRESH_CONNECT              74
#define HB_CURLOPT_FORBID_REUSE               75
#define HB_CURLOPT_RANDOM_FILE                76  /* deprecated */
#define HB_CURLOPT_EGDSOCKET                  77  /* deprecated */
#define HB_CURLOPT_CONNECTTIMEOUT             78
#define HB_CURLOPT_HEADERFUNCTION             79
#define HB_CURLOPT_HTTPGET                    80
#define HB_CURLOPT_SSL_VERIFYHOST             81
#define HB_CURLOPT_COOKIEJAR                  82
#define HB_CURLOPT_SSL_CIPHER_LIST            83
#define HB_CURLOPT_HTTP_VERSION               84
#define HB_CURLOPT_FTP_USE_EPSV               85
#define HB_CURLOPT_SSLCERTTYPE                86
#define HB_CURLOPT_SSLKEY                     87
#define HB_CURLOPT_SSLKEYTYPE                 88
#define HB_CURLOPT_SSLENGINE                  89
#define HB_CURLOPT_SSLENGINE_DEFAULT          90
#define HB_CURLOPT_DNS_USE_GLOBAL_CACHE       91  /* Deprecated. It's a no-op. */
#define HB_CURLOPT_DNS_CACHE_TIMEOUT          92
#define HB_CURLOPT_PREQUOTE                   93
#define HB_CURLOPT_DEBUGFUNCTION              94
#define HB_CURLOPT_DEBUGDATA                  95
#define HB_CURLOPT_COOKIESESSION              96
#define HB_CURLOPT_CAPATH                     97
#define HB_CURLOPT_BUFFERSIZE                 98
#define HB_CURLOPT_NOSIGNAL                   99
#define HB_CURLOPT_SHARE                      100
#define HB_CURLOPT_PROXYTYPE                  101
#define HB_CURLOPT_ACCEPT_ENCODING            102
#define HB_CURLOPT_ENCODING                   HB_CURLOPT_ACCEPT_ENCODING
#define HB_CURLOPT_PRIVATE                    103
#define HB_CURLOPT_HTTP200ALIASES             104
#define HB_CURLOPT_UNRESTRICTED_AUTH          105
#define HB_CURLOPT_FTP_USE_EPRT               106
#define HB_CURLOPT_HTTPAUTH                   107
#define HB_CURLOPT_SSL_CTX_FUNCTION           108
#define HB_CURLOPT_SSL_CTX_DATA               109
#define HB_CURLOPT_FTP_CREATE_MISSING_DIRS    110
#define HB_CURLOPT_PROXYAUTH                  111
#define HB_CURLOPT_SERVER_RESPONSE_TIMEOUT    112
#define HB_CURLOPT_FTP_RESPONSE_TIMEOUT       HB_CURLOPT_SERVER_RESPONSE_TIMEOUT  /* deprecated */
#define HB_CURLOPT_IPRESOLVE                  113
#define HB_CURLOPT_MAXFILESIZE                HB_CURLOPT_MAXFILESIZE_LARGE
#define HB_CURLOPT_INFILESIZE_LARGE           115
#define HB_CURLOPT_RESUME_FROM_LARGE          116
#define HB_CURLOPT_MAXFILESIZE_LARGE          117
#define HB_CURLOPT_NETRC_FILE                 118
#define HB_CURLOPT_USE_SSL                    119
#define HB_CURLOPT_FTP_SSL                    HB_CURLOPT_USE_SSL
#define HB_CURLOPT_POSTFIELDSIZE_LARGE        120
#define HB_CURLOPT_TCP_NODELAY                121
#define HB_CURLOPT_SOURCE_USERPWD             123
#define HB_CURLOPT_SOURCE_PREQUOTE            127
#define HB_CURLOPT_SOURCE_POSTQUOTE           128
#define HB_CURLOPT_FTPSSLAUTH                 129
#define HB_CURLOPT_IOCTLFUNCTION              130
#define HB_CURLOPT_IOCTLDATA                  131
#define HB_CURLOPT_SOURCE_URL                 132
#define HB_CURLOPT_SOURCE_QUOTE               133
#define HB_CURLOPT_FTP_ACCOUNT                134
#define HB_CURLOPT_COOKIELIST                 135
#define HB_CURLOPT_IGNORE_CONTENT_LENGTH      136
#define HB_CURLOPT_FTP_SKIP_PASV_IP           137
#define HB_CURLOPT_FTP_FILEMETHOD             138
#define HB_CURLOPT_LOCALPORT                  139
#define HB_CURLOPT_LOCALPORTRANGE             140
#define HB_CURLOPT_CONNECT_ONLY               141
#define HB_CURLOPT_CONV_FROM_NETWORK_FUNCTION 142
#define HB_CURLOPT_CONV_TO_NETWORK_FUNCTION   143
#define HB_CURLOPT_CONV_FROM_UTF8_FUNCTION    144
#define HB_CURLOPT_MAX_SEND_SPEED_LARGE       145
#define HB_CURLOPT_MAX_RECV_SPEED_LARGE       146
#define HB_CURLOPT_FTP_ALTERNATIVE_TO_USER    147
#define HB_CURLOPT_SOCKOPTFUNCTION            148
#define HB_CURLOPT_SOCKOPTDATA                149
#define HB_CURLOPT_SSL_SESSIONID_CACHE        150
#define HB_CURLOPT_SSH_AUTH_TYPES             151
#define HB_CURLOPT_SSH_PUBLIC_KEYFILE         152
#define HB_CURLOPT_SSH_PRIVATE_KEYFILE        153
#define HB_CURLOPT_FTP_SSL_CCC                154
#define HB_CURLOPT_TIMEOUT_MS                 155
#define HB_CURLOPT_CONNECTTIMEOUT_MS          156
#define HB_CURLOPT_HTTP_TRANSFER_DECODING     157
#define HB_CURLOPT_HTTP_CONTENT_DECODING      158
#define HB_CURLOPT_NEW_FILE_PERMS             159
#define HB_CURLOPT_NEW_DIRECTORY_PERMS        160
#define HB_CURLOPT_POST301                    161
#define HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5    162
#define HB_CURLOPT_OPENSOCKETFUNCTION         163
#define HB_CURLOPT_OPENSOCKETDATA             164
#define HB_CURLOPT_COPYPOSTFIELDS             165
#define HB_CURLOPT_PROXY_TRANSFER_MODE        166
#define HB_CURLOPT_SEEKFUNCTION               167
#define HB_CURLOPT_SEEKDATA                   168
#define HB_CURLOPT_CRLFILE                    169
#define HB_CURLOPT_ISSUERCERT                 170
#define HB_CURLOPT_ADDRESS_SCOPE              171
#define HB_CURLOPT_CERTINFO                   172
#define HB_CURLOPT_POSTREDIR                  HB_CURLOPT_POST301
#define HB_CURLOPT_USERNAME                   173
#define HB_CURLOPT_PASSWORD                   174
#define HB_CURLOPT_PROXYUSERNAME              175
#define HB_CURLOPT_PROXYPASSWORD              176
#define HB_CURLOPT_NOPROXY                    177
#define HB_CURLOPT_TFTP_BLKSIZE               178
#define HB_CURLOPT_SOCKS5_GSSAPI_SERVICE      179
#define HB_CURLOPT_SOCKS5_GSSAPI_NEC          180
#define HB_CURLOPT_PROTOCOLS                  181  /* deprecated */
#define HB_CURLOPT_REDIR_PROTOCOLS            182  /* deprecated */
#define HB_CURLOPT_SSH_KNOWNHOSTS             183
#define HB_CURLOPT_MAIL_FROM                  186
#define HB_CURLOPT_MAIL_RCPT                  187
#define HB_CURLOPT_FTP_USE_PRET               188
#define HB_CURLOPT_RTSP_REQUEST               189
#define HB_CURLOPT_RTSP_SESSION_ID            190
#define HB_CURLOPT_RTSP_STREAM_URI            191
#define HB_CURLOPT_RTSP_TRANSPORT             192
#define HB_CURLOPT_RTSP_HEADER                HB_CURLOPT_HTTPHEADER
#define HB_CURLOPT_RTSP_CLIENT_CSEQ           193
#define HB_CURLOPT_RTSP_SERVER_CSEQ           194
#define HB_CURLOPT_WILDCARDMATCH              197
#define HB_CURLOPT_RESOLVE                    198
#define HB_CURLOPT_TRANSFER_ENCODING          199
#define HB_CURLOPT_GSSAPI_DELEGATION          200
#define HB_CURLOPT_DNS_SERVERS                201
#define HB_CURLOPT_ACCEPTTIMEOUT_MS           202
#define HB_CURLOPT_SSL_OPTIONS                203
#define HB_CURLOPT_TCP_KEEPALIVE              204
#define HB_CURLOPT_TCP_KEEPIDLE               205
#define HB_CURLOPT_TCP_KEEPINTVL              206
#define HB_CURLOPT_MAIL_AUTH                  207
#define HB_CURLOPT_XOAUTH2_BEARER             208
#define HB_CURLOPT_LOGIN_OPTIONS              209
#define HB_CURLOPT_EXPECT_100_TIMEOUT_MS      210
#define HB_CURLOPT_SSL_ENABLE_ALPN            211
#define HB_CURLOPT_SSL_ENABLE_NPN             212  /* deprecated */
#define HB_CURLOPT_HEADEROPT                  213
#define HB_CURLOPT_PROXYHEADER                214
#define HB_CURLOPT_PINNEDPUBLICKEY            215
#define HB_CURLOPT_UNIX_SOCKET_PATH           216
#define HB_CURLOPT_SSL_VERIFYSTATUS           217
#define HB_CURLOPT_SSL_FALSESTART             218
#define HB_CURLOPT_PATH_AS_IS                 219
#define HB_CURLOPT_PROXY_SERVICE_NAME         220
#define HB_CURLOPT_SERVICE_NAME               221
#define HB_CURLOPT_PIPEWAIT                   222
#define HB_CURLOPT_DEFAULT_PROTOCOL           223
#define HB_CURLOPT_CONNECT_TO                 224
#define HB_CURLOPT_TCP_FASTOPEN               225
#define HB_CURLOPT_KEEP_SENDING_ON_ERROR      226
#define HB_CURLOPT_PROXY_SSLCERT              227
#define HB_CURLOPT_PROXY_SSLCERTTYPE          228
#define HB_CURLOPT_PROXY_SSLKEY               229
#define HB_CURLOPT_PROXY_SSLKEYTYPE           230
#define HB_CURLOPT_PROXY_KEYPASSWD            231
#define HB_CURLOPT_PROXY_SSLVERSION           232
#define HB_CURLOPT_PROXY_CAINFO               233
#define HB_CURLOPT_PROXY_CAPATH               234
#define HB_CURLOPT_PROXY_CRLFILE              235
#define HB_CURLOPT_PROXY_SSL_VERIFYHOST       236
#define HB_CURLOPT_PROXY_SSL_VERIFYPEER       237
#define HB_CURLOPT_PROXY_SSL_CIPHER_LIST      238
#define HB_CURLOPT_PROXY_SSL_OPTIONS          239
#define HB_CURLOPT_PROXY_PINNEDPUBLICKEY      240
#define HB_CURLOPT_PROXY_TLSAUTH_PASSWORD     241
#define HB_CURLOPT_PROXY_TLSAUTH_TYPE         242
#define HB_CURLOPT_PROXY_TLSAUTH_USERNAME     243
#define HB_CURLOPT_PRE_PROXY                  244
#define HB_CURLOPT_ABSTRACT_UNIX_SOCKET       245
#define HB_CURLOPT_SUPPRESS_CONNECT_HEADERS   246
#define HB_CURLOPT_REQUEST_TARGET             247
#define HB_CURLOPT_SOCKS5_AUTH                248
#define HB_CURLOPT_SSH_COMPRESSION            249
#define HB_CURLOPT_MIMEPOST                   250
#define HB_CURLOPT_TIMEVALUE_LARGE            HB_CURLOPT_TIMEVALUE
#define HB_CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS  251
#define HB_CURLOPT_HAPROXYPROTOCOL            252
#define HB_CURLOPT_DNS_SHUFFLE_ADDRESSES      253
#define HB_CURLOPT_TLS13_CIPHERS              254
#define HB_CURLOPT_PROXY_TLS13_CIPHERS        255
#define HB_CURLOPT_DISALLOW_USERNAME_IN_URL   256
#define HB_CURLOPT_DOH_URL                    257
#define HB_CURLOPT_UPLOAD_BUFFERSIZE          258
#define HB_CURLOPT_UPKEEP_INTERVAL_MS         259
#define HB_CURLOPT_HTTP09_ALLOWED             260
#define HB_CURLOPT_ALTSVC                     261
#define HB_CURLOPT_ALTSVC_CTRL                262
#define HB_CURLOPT_MAXAGE_CONN                263
#define HB_CURLOPT_H3                         264
#define HB_CURLOPT_SASL_AUTHZID               265
#define HB_CURLOPT_MAIL_RCPT_ALLOWFAILS       266
#define HB_CURLOPT_SSLCERT_BLOB               267
#define HB_CURLOPT_PROXY_SSLCERT_BLOB         268
#define HB_CURLOPT_SSLKEY_BLOB                269
#define HB_CURLOPT_PROXY_SSLKEY_BLOB          270
#define HB_CURLOPT_ISSUERCERT_BLOB            271
#define HB_CURLOPT_PROXY_ISSUERCERT           272
#define HB_CURLOPT_PROXY_ISSUERCERT_BLOB      273
#define HB_CURLOPT_SSL_EC_CURVES              274
#define HB_CURLOPT_HSTS                       275
#define HB_CURLOPT_HSTS_CTRL                  276
#define HB_CURLOPT_AWS_SIGV4                  277
#define HB_CURLOPT_DOH_SSL_VERIFYHOST         278
#define HB_CURLOPT_DOH_SSL_VERIFYPEER         279
#define HB_CURLOPT_DOH_SSL_VERIFYSTATUS       280
#define HB_CURLOPT_CAINFO_BLOB                281
#define HB_CURLOPT_PROXY_CAINFO_BLOB          282
#define HB_CURLOPT_TLSAUTH_PASSWORD           283
#define HB_CURLOPT_TLSAUTH_TYPE               284
#define HB_CURLOPT_TLSAUTH_USERNAME           285
#define HB_CURLOPT_SSH_HOST_PUBLIC_KEY_SHA256 286
#define HB_CURLOPT_MAXLIFETIME_CONN           287
#define HB_CURLOPT_MIME_OPTIONS               288
#define HB_CURLOPT_PROTOCOLS_STR              289
#define HB_CURLOPT_REDIR_PROTOCOLS_STR        290
#define HB_CURLOPT_WS_OPTIONS                 291
#define HB_CURLOPT_CA_CACHE_TIMEOUT           292
#define HB_CURLOPT_QUICK_EXIT                 293
#define HB_CURLOPT_SERVER_RESPONSE_TIMEOUT_MS 294
#define HB_CURLOPT_ECH                        295
#define HB_CURLOPT_DOWNLOAD                   1001  /* Harbour special ones */
#define HB_CURLOPT_XFERINFOBLOCK              1002
#define HB_CURLOPT_UL_FILE_SETUP              1003
#define HB_CURLOPT_UL_FILE_CLOSE              1004
#define HB_CURLOPT_DL_FILE_SETUP              1005
#define HB_CURLOPT_DL_FILE_CLOSE              1006
#define HB_CURLOPT_UL_BUFF_SETUP              1007
#define HB_CURLOPT_DL_BUFF_SETUP              1008
#define HB_CURLOPT_DL_BUFF_GET                1009
#define HB_CURLOPT_UL_NULL_SETUP              1010
#define HB_CURLOPT_HTTPPOST_CONTENT           1013  /* deprecated */
#define HB_CURLOPT_HTTPPOST_FORM              1014  /* deprecated */
#define HB_CURLOPT_DEBUGBLOCK                 1015
/* Compatibility ones. Don't use these. */
#define HB_CURLOPT_UL_FHANDLE_SETUP           HB_CURLOPT_UL_FILE_SETUP
#define HB_CURLOPT_SETUPLOADFILE              HB_CURLOPT_UL_FILE_SETUP
#define HB_CURLOPT_CLOSEUPLOADFILE            HB_CURLOPT_UL_FILE_CLOSE
#define HB_CURLOPT_DL_FHANDLE_SETUP           HB_CURLOPT_DL_FILE_SETUP
#define HB_CURLOPT_SETDOWNLOADFILE            HB_CURLOPT_DL_FILE_SETUP
#define HB_CURLOPT_CLOSEDOWNLOADFILE          HB_CURLOPT_DL_FILE_CLOSE
#define HB_CURLOPT_SETPROGRESS                HB_CURLOPT_XFERINFOBLOCK
#define HB_CURLOPT_PROGRESSBLOCK              HB_CURLOPT_XFERINFOBLOCK
#define HB_CURLOPT_MAIL_RCPT_ALLLOWFAILS      HB_CURLOPT_MAIL_RCPT_ALLOWFAILS

/* HB_CURLOPT_MIME_OPTIONS option */
#define HB_CURLMIMEOPT_FORMESCAPE             hb_bitShift( 1, 0 )  /* added in 7.81.0 */

/* HB_CURLOPT_PROXYTYPE option */
#define HB_CURLPROXY_HTTP                     0  /* added in 7.10 */
#define HB_CURLPROXY_HTTP_1_0                 1  /* added in 7.19.4, force to use CONNECT HTTP/1.0 */
#define HB_CURLPROXY_HTTPS                    2  /* added in 7.52.0 */
#define HB_CURLPROXY_HTTPS2                   3  /* added in 8.1.0 */
#define HB_CURLPROXY_SOCKS4                   4  /* support added in 7.15.2, enum existed already in 7.10 */
#define HB_CURLPROXY_SOCKS5                   5  /* added in 7.10 */
#define HB_CURLPROXY_SOCKS4A                  6  /* added in 7.18.0 */
#define HB_CURLPROXY_SOCKS5_HOSTNAME          7  /* Use the SOCKS5 protocol but pass along the host name rather than the IP address. added in 7.18.0 */

/* HB_CURLOPT_NETRC option */
#define HB_CURL_NETRC_IGNORED                 0  /* The .netrc will never be read. */
#define HB_CURL_NETRC_OPTIONAL                1  /* A user:password in the URL will be preferred */
#define HB_CURL_NETRC_REQUIRED                2  /* A user:password in the URL will be ignored. */

/* HB_CURLOPT_SSL_OPTIONS values */
#define HB_CURLSSLOPT_ALLOW_BEAST             hb_bitShift( 1, 0 )
#define HB_CURLSSLOPT_NO_REVOKE               hb_bitShift( 1, 1 )
#define HB_CURLSSLOPT_NO_PARTIALCHAIN         hb_bitShift( 1, 2 )
#define HB_CURLSSLOPT_REVOKE_BEST_EFFORT      hb_bitShift( 1, 3 )
#define HB_CURLSSLOPT_NATIVE_CA               hb_bitShift( 1, 4 )
#define HB_CURLSSLOPT_AUTO_CLIENT_CERT        hb_bitShift( 1, 5 )

/* HB_CURLOPT_HTTPAUTH, HB_CURLOPT_PROXYAUTH, HB_CURLOPT_SOCKS5 options */
#define HB_CURLAUTH_NONE                      0                    /* nothing */
#define HB_CURLAUTH_BASIC                     hb_bitShift( 1, 0 )  /* Basic (default) */
#define HB_CURLAUTH_DIGEST                    hb_bitShift( 1, 1 )  /* Digest */
#define HB_CURLAUTH_NEGOTIATE                 hb_bitShift( 1, 2 )  /* Negotiate */
#define HB_CURLAUTH_NTLM                      hb_bitShift( 1, 3 )  /* NTLM */
#define HB_CURLAUTH_DIGEST_IE                 hb_bitShift( 1, 4 )  /* Digest with IE flavour */
#define HB_CURLAUTH_NTLM_WB                   hb_bitShift( 1, 5 )  /* NTLM delegating to winbind helper */
#define HB_CURLAUTH_BEARER                    hb_bitShift( 1, 6 )  /* HTTP Bearer token authentication */
#define HB_CURLAUTH_AWS_SIGV4                 hb_bitShift( 1, 7 )  /* AWS HTTP v4 Signature */
#define HB_CURLAUTH_ONLY                      hb_bitShift( 1, 31 ) /* used together with a single other type to force no auth or just that single type */
#define HB_CURLAUTH_ANY                       hb_bitNot( 0 )       /* all types set */
#define HB_CURLAUTH_ANYSAFE                   hb_bitNot( hb_bitOr( HB_CURLAUTH_BASIC, HB_CURLAUTH_DIGEST_IE ) )

#define HB_CURLAUTH_GSSNEGOTIATE              HB_CURLAUTH_NEGOTIATE
#define HB_CURLAUTH_GSSAPI                    HB_CURLAUTH_NEGOTIATE

/* HB_CURLOPT_HTTP_VERSION option */
#define HB_CURL_HTTP_VERSION_NONE               0   /* setting this means we don't care, and that we'd like the library to choose the best possible for us! */
#define HB_CURL_HTTP_VERSION_1_0                1   /* please use HTTP 1.0 in the request */
#define HB_CURL_HTTP_VERSION_1_1                2   /* please use HTTP 1.1 in the request */
#define HB_CURL_HTTP_VERSION_2_0                3   /* please use HTTP 2.0 in the request */
#define HB_CURL_HTTP_VERSION_2                  HB_CURL_HTTP_VERSION_2_0
#define HB_CURL_HTTP_VERSION_2TLS               4   /* use version 2 for HTTPS, version 1.1 for HTTP */
#define HB_CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE  5   /* please use HTTP 2 without HTTP/1.1 Upgrade */
#define HB_CURL_HTTP_VERSION_3                  30  /* Use HTTP/3, fallback to HTTP/2 or HTTP/1 if needed. For HTTPS only. For HTTP, this option makes libcurl return error. */
#define HB_CURL_HTTP_VERSION_3ONLY              31  /* Use HTTP/3 without fallback. For HTTPS only. For HTTP, this makes libcurl return error. */

/* HB_CURLOPT_ALTSVC_CTRL option */
#define HB_CURLALTSVC_READONLYFILE            hb_bitShift( 1, 2 )
#define HB_CURLALTSVC_H1                      hb_bitShift( 1, 3 )
#define HB_CURLALTSVC_H2                      hb_bitShift( 1, 4 )
#define HB_CURLALTSVC_H3                      hb_bitShift( 1, 5 )

/* HB_CURLOPT_USE_SSL option */
#define HB_CURLUSESSL_NONE                    0  /* do not attempt to use SSL */
#define HB_CURLUSESSL_TRY                     1  /* try using SSL, proceed anyway otherwise */
#define HB_CURLUSESSL_CONTROL                 2  /* SSL for the control connection or fail */
#define HB_CURLUSESSL_ALL                     3  /* SSL for all communication or fail */

/* HB_CURLOPT_FTPSSLAUTH option */
#define HB_CURLFTPAUTH_DEFAULT                0  /* let libcurl decide */
#define HB_CURLFTPAUTH_SSL                    1  /* use "AUTH SSL" */
#define HB_CURLFTPAUTH_TLS                    2  /* use "AUTH TLS" */

/* HB_CURLOPT_FTP_SSL_CCC option */
#define HB_CURLFTPSSL_CCC_NONE                0  /* do not send CCC */
#define HB_CURLFTPSSL_CCC_PASSIVE             1  /* Let the server initiate the shutdown */
#define HB_CURLFTPSSL_CCC_ACTIVE              2  /* Initiate the shutdown */

/* HB_CURLOPT_FTP_FILEMETHOD option */
#define HB_CURLFTPMETHOD_DEFAULT              0  /* let libcurl pick */
#define HB_CURLFTPMETHOD_MULTICWD             1  /* single CWD operation for each path part */
#define HB_CURLFTPMETHOD_NOCWD                2  /* no CWD at all */
#define HB_CURLFTPMETHOD_SINGLECWD            3  /* one CWD to full dir, then work on file */

/* HB_CURLOPT_FTP_CREATE_MISSING_DIRS option */
#define HB_CURLFTP_CREATE_DIR_NONE            0
#define HB_CURLFTP_CREATE_DIR                 1
#define HB_CURLFTP_CREATE_DIR_RETRY           2

/* HB_CURLOPT_RTSP_REQUEST option */
#define HB_CURL_RTSPREQ_NONE                  0
#define HB_CURL_RTSPREQ_OPTIONS               1
#define HB_CURL_RTSPREQ_DESCRIBE              2
#define HB_CURL_RTSPREQ_ANNOUNCE              3
#define HB_CURL_RTSPREQ_SETUP                 4
#define HB_CURL_RTSPREQ_PLAY                  5
#define HB_CURL_RTSPREQ_PAUSE                 6
#define HB_CURL_RTSPREQ_TEARDOWN              7
#define HB_CURL_RTSPREQ_GET_PARAMETER         8
#define HB_CURL_RTSPREQ_SET_PARAMETER         9
#define HB_CURL_RTSPREQ_RECORD                10
#define HB_CURL_RTSPREQ_RECEIVE               11
#define HB_CURL_RTSPREQ_LAST                  12

/* HB_CURLOPT_TIMECONDITION option */
#define HB_CURL_TIMECOND_NONE                 0
#define HB_CURL_TIMECOND_IFMODSINCE           1
#define HB_CURL_TIMECOND_IFUNMODSINCE         2
#define HB_CURL_TIMECOND_LASTMOD              3

/* HB_CURLOPT_IPRESOLVE option */
#define HB_CURL_IPRESOLVE_WHATEVER            0  /* default, resolves addresses to all IP versions that your system allows */
#define HB_CURL_IPRESOLVE_V4                  1  /* resolve to ipv4 addresses */
#define HB_CURL_IPRESOLVE_V6                  2  /* resolve to ipv6 addresses */

/* HB_CURLOPT_HEADEROPT */
#define HB CURLHEADER_UNIFIED                 0
#define HB_CURLHEADER_SEPARATE                1

/* HB_CURLOPT_SSLVERSION option */
#define HB_CURL_SSLVERSION_DEFAULT            0
#define HB_CURL_SSLVERSION_TLSv1              1
#define HB_CURL_SSLVERSION_SSLv2              2
#define HB_CURL_SSLVERSION_SSLv3              3
#define HB_CURL_SSLVERSION_TLSv1_0            4
#define HB_CURL_SSLVERSION_TLSv1_1            5
#define HB_CURL_SSLVERSION_TLSv1_2            6
#define HB_CURL_SSLVERSION_TLSv1_3            7
#define HB_CURL_SSLVERSION_MAX_NONE           0
#define HB_CURL_SSLVERSION_MAX_DEFAULT        hb_bitShift( HB_CURL_SSLVERSION_TLSv1  , 16 )
#define HB_CURL_SSLVERSION_MAX_TLSv1_0        hb_bitShift( HB_CURL_SSLVERSION_TLSv1_0, 16 )
#define HB_CURL_SSLVERSION_MAX_TLSv1_1        hb_bitShift( HB_CURL_SSLVERSION_TLSv1_1, 16 )
#define HB_CURL_SSLVERSION_MAX_TLSv1_2        hb_bitShift( HB_CURL_SSLVERSION_TLSv1_2, 16 )
#define HB_CURL_SSLVERSION_MAX_TLSv1_3        hb_bitShift( HB_CURL_SSLVERSION_TLSv1_3, 16 )

/* HB_CURLOPT_SSH_AUTH_TYPES option */
#define HB_CURL_CURLSSH_AUTH_ANY              hb_bitNot( 0 )       /* all types supported by the server */
#define HB_CURL_CURLSSH_AUTH_NONE             0                    /* none allowed, silly but complete */
#define HB_CURL_CURLSSH_AUTH_PUBLICKEY        hb_bitShift( 1, 0 )  /* public/private key files */
#define HB_CURL_CURLSSH_AUTH_PASSWORD         hb_bitShift( 1, 1 )  /* password */
#define HB_CURL_CURLSSH_AUTH_HOST             hb_bitShift( 1, 2 )  /* host key files */
#define HB_CURL_CURLSSH_AUTH_KEYBOARD         hb_bitShift( 1, 3 )  /* keyboard interactive */
#define HB_CURL_CURLSSH_AUTH_AGENT            hb_bitShift( 1, 4 )  /* agent (ssh-agent, pageant...) */
#define HB_CURL_CURLSSH_AUTH_GSSAPI           hb_bitShift( 1, 5 )  /* gssapi (kerberos, ...) */
#define HB_CURL_CURLSSH_AUTH_DEFAULT          HB_CURLSSH_AUTH_ANY

/* CURLOPT_*PROTOCOLS options */
#define HB_CURLPROTO_HTTP                     hb_bitShift( 1, 0 )
#define HB_CURLPROTO_HTTPS                    hb_bitShift( 1, 1 )
#define HB_CURLPROTO_FTP                      hb_bitShift( 1, 2 )
#define HB_CURLPROTO_FTPS                     hb_bitShift( 1, 3 )
#define HB_CURLPROTO_SCP                      hb_bitShift( 1, 4 )
#define HB_CURLPROTO_SFTP                     hb_bitShift( 1, 5 )
#define HB_CURLPROTO_TELNET                   hb_bitShift( 1, 6 )
#define HB_CURLPROTO_LDAP                     hb_bitShift( 1, 7 )
#define HB_CURLPROTO_LDAPS                    hb_bitShift( 1, 8 )
#define HB_CURLPROTO_DICT                     hb_bitShift( 1, 9 )
#define HB_CURLPROTO_FILE                     hb_bitShift( 1, 10 )
#define HB_CURLPROTO_TFTP                     hb_bitShift( 1, 11 )
#define HB_CURLPROTO_IMAP                     hb_bitShift( 1, 12 )
#define HB_CURLPROTO_IMAPS                    hb_bitShift( 1, 13 )
#define HB_CURLPROTO_POP3                     hb_bitShift( 1, 14 )
#define HB_CURLPROTO_POP3S                    hb_bitShift( 1, 15 )
#define HB_CURLPROTO_SMTP                     hb_bitShift( 1, 16 )
#define HB_CURLPROTO_SMTPS                    hb_bitShift( 1, 17 )
#define HB_CURLPROTO_RTSP                     hb_bitShift( 1, 18 )
#define HB_CURLPROTO_RTMP                     hb_bitShift( 1, 19 )
#define HB_CURLPROTO_RTMPT                    hb_bitShift( 1, 20 )
#define HB_CURLPROTO_RTMPE                    hb_bitShift( 1, 21 )
#define HB_CURLPROTO_RTMPTE                   hb_bitShift( 1, 22 )
#define HB_CURLPROTO_RTMPS                    hb_bitShift( 1, 23 )
#define HB_CURLPROTO_RTMPTS                   hb_bitShift( 1, 24 )
#define HB_CURLPROTO_GOPHER                   hb_bitShift( 1, 25 )
#define HB_CURLPROTO_SMB                      hb_bitShift( 1, 26 )
#define HB_CURLPROTO_SMBS                     hb_bitShift( 1, 27 )
#define HB_CURLPROTO_MQTT                     hb_bitShift( 1, 28 )
#define HB_CURLPROTO_GOPHERS                  hb_bitShift( 1, 29 )
#define HB_CURLPROTO_ALL                      hb_bitNot( 0 )

/* HB_CURLOPT_H3 bit values */
#define HB_CURLH3_DIRECT                      hb_bitshift( 1, 0 )  /* go QUIC + HTTP/3 directly to the given host + port */

/* curl_easy_pause() parameters. They can be combined with hb_bitOr(). */
#define HB_CURLPAUSE_RECV                     1
#define HB_CURLPAUSE_RECV_CONT                0
#define HB_CURLPAUSE_SEND                     4
#define HB_CURLPAUSE_SEND_CONT                0
#define HB_CURLPAUSE_ALL                      hb_bitOr( HB_CURLPAUSE_RECV, HB_CURLPAUSE_SEND )
#define HB_CURLPAUSE_CONT                     hb_bitOr( HB_CURLPAUSE_RECV_CONT, HB_CURLPAUSE_SEND_CONT )

/* curl_global_init() parameters. */
#define HB_CURL_GLOBAL_SSL                    1
#define HB_CURL_GLOBAL_WIN32                  2
#define HB_CURL_GLOBAL_ALL                    hb_bitOr( HB_CURL_GLOBAL_SSL, HB_CURL_GLOBAL_WIN32 )
#define HB_CURL_GLOBAL_NOTHING                0
#define HB_CURL_GLOBAL_DEFAULT                HB_CURL_GLOBAL_ALL

/* curl_easy_getinfo() parameters.
   NOTE: The actual values may be different from the libcurl equivalent. */
#define HB_CURLINFO_EFFECTIVE_URL             1
#define HB_CURLINFO_RESPONSE_CODE             2
#define HB_CURLINFO_HTTP_CONNECTCODE          3
#define HB_CURLINFO_FILETIME_T                4
#define HB_CURLINFO_TOTAL_TIME                5   /* deprecated */
#define HB_CURLINFO_NAMELOOKUP_TIME           6   /* deprecated */
#define HB_CURLINFO_CONNECT_TIME              7   /* deprecated */
#define HB_CURLINFO_PRETRANSFER_TIME          8   /* deprecated */
#define HB_CURLINFO_STARTTRANSFER_TIME        9   /* deprecated */
#define HB_CURLINFO_REDIRECT_TIME             10  /* deprecated */
#define HB_CURLINFO_REDIRECT_COUNT            11
#define HB_CURLINFO_REDIRECT_URL              12
#define HB_CURLINFO_SIZE_UPLOAD_T             13
#define HB_CURLINFO_SIZE_DOWNLOAD_T           14
#define HB_CURLINFO_SPEED_DOWNLOAD_T          15
#define HB_CURLINFO_SPEED_UPLOAD_T            16
#define HB_CURLINFO_HEADER_SIZE               17
#define HB_CURLINFO_REQUEST_SIZE              18
#define HB_CURLINFO_SSL_VERIFYRESULT          19
#define HB_CURLINFO_SSL_ENGINES               20
#define HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD_T 21
#define HB_CURLINFO_CONTENT_LENGTH_UPLOAD_T   22
#define HB_CURLINFO_CONTENT_TYPE              23
#define HB_CURLINFO_PRIVATE                   24
#define HB_CURLINFO_HTTPAUTH_AVAIL            25
#define HB_CURLINFO_PROXYAUTH_AVAIL           26
#define HB_CURLINFO_OS_ERRNO                  27
#define HB_CURLINFO_NUM_CONNECTS              28
#define HB_CURLINFO_COOKIELIST                29
#define HB_CURLINFO_LASTSOCKET                30  /* deprecated */
#define HB_CURLINFO_FTP_ENTRY_PATH            31
#define HB_CURLINFO_PRIMARY_IP                32
#define HB_CURLINFO_APPCONNECT_TIME           33  /* deprecated */
#define HB_CURLINFO_CERTINFO                  34
#define HB_CURLINFO_CONDITION_UNMET           35
#define HB_CURLINFO_RTSP_SESSION_ID           36
#define HB_CURLINFO_RTSP_CLIENT_CSEQ          37
#define HB_CURLINFO_RTSP_SERVER_CSEQ          38
#define HB_CURLINFO_RTSP_CSEQ_RECV            39
#define HB_CURLINFO_PRIMARY_PORT              40
#define HB_CURLINFO_LOCAL_IP                  41
#define HB_CURLINFO_LOCAL_PORT                42
#define HB_CURLINFO_ACTIVESOCKET              43
#define HB_CURLINFO_HTTP_VERSION              44
#define HB_CURLINFO_PROTOCOL                  45  /* deprecated */
#define HB_CURLINFO_SCHEME                    46
#define HB_CURLINFO_PROXY_SSL_VERIFYRESULT    47
#define HB_CURLINFO_FILETIME                  HB_CURLINFO_FILETIME_T  /* deprecated */
#define HB_CURLINFO_SIZE_DOWNLOAD             HB_CURLINFO_SIZE_DOWNLOAD_T  /* deprecated */
#define HB_CURLINFO_SIZE_UPLOAD               HB_CURLINFO_SIZE_UPLOAD_T  /* deprecated */
#define HB_CURLINFO_SPEED_DOWNLOAD            HB_CURLINFO_SPEED_DOWNLOAD_T  /* deprecated */
#define HB_CURLINFO_SPEED_UPLOAD              HB_CURLINFO_SPEED_UPLOAD_T  /* deprecated */
#define HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD   HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD_T  /* deprecated */
#define HB_CURLINFO_CONTENT_LENGTH_UPLOAD     HB_CURLINFO_CONTENT_LENGTH_UPLOAD_T  /* deprecated */
#define HB_CURLINFO_TOTAL_TIME_T              48
#define HB_CURLINFO_NAMELOOKUP_TIME_T         49
#define HB_CURLINFO_CONNECT_TIME_T            50
#define HB_CURLINFO_PRETRANSFER_TIME_T        51
#define HB_CURLINFO_STARTTRANSFER_TIME_T      52
#define HB_CURLINFO_REDIRECT_TIME_T           53
#define HB_CURLINFO_APPCONNECT_TIME_T         54
#define HB_CURLINFO_RETRY_AFTER               55
#define HB_CURLINFO_EFFECTIVE_METHOD          56
#define HB_CURLINFO_PROXY_ERROR               57
#define HB_CURLINFO_REFERER                   58
#define HB_CURLINFO_CAINFO                    59
#define HB_CURLINFO_CAPATH                    60
#define HB_CURLINFO_XFER_ID                   61
#define HB_CURLINFO_CONN_ID                   62
#define HB_CURLINFO_QUEUE_TIME_T              63

/* HB_CURLINFO_PROXY_ERROR results. */
#define HB_CURLPX_OK                                0
#define HB_CURLPX_BAD_ADDRESS_TYPE                  1
#define HB_CURLPX_BAD_VERSION                       2
#define HB_CURLPX_CLOSED                            3
#define HB_CURLPX_GSSAPI                            4
#define HB_CURLPX_GSSAPI_PERMSG                     5
#define HB_CURLPX_GSSAPI_PROTECTION                 6
#define HB_CURLPX_IDENTD                            7
#define HB_CURLPX_IDENTD_DIFFER                     8
#define HB_CURLPX_LONG_HOSTNAME                     9
#define HB_CURLPX_LONG_PASSWD                       10
#define HB_CURLPX_LONG_USER                         11
#define HB_CURLPX_NO_AUTH                           12
#define HB_CURLPX_RECV_ADDRESS                      13
#define HB_CURLPX_RECV_AUTH                         14
#define HB_CURLPX_RECV_CONNECT                      15
#define HB_CURLPX_RECV_REQACK                       16
#define HB_CURLPX_REPLY_ADDRESS_TYPE_NOT_SUPPORTED  17
#define HB_CURLPX_REPLY_COMMAND_NOT_SUPPORTED       18
#define HB_CURLPX_REPLY_CONNECTION_REFUSED          19
#define HB_CURLPX_REPLY_GENERAL_SERVER_FAILURE      20
#define HB_CURLPX_REPLY_HOST_UNREACHABLE            21
#define HB_CURLPX_REPLY_NETWORK_UNREACHABLE         22
#define HB_CURLPX_REPLY_NOT_ALLOWED                 23
#define HB_CURLPX_REPLY_TTL_EXPIRED                 24
#define HB_CURLPX_REPLY_UNASSIGNED                  25
#define HB_CURLPX_REQUEST_FAILED                    26
#define HB_CURLPX_RESOLVE_HOST                      27
#define HB_CURLPX_SEND_AUTH                         28
#define HB_CURLPX_SEND_CONNECT                      29
#define HB_CURLPX_SEND_REQUEST                      30
#define HB_CURLPX_UNKNOWN_FAIL                      31
#define HB_CURLPX_UNKNOWN_MODE                      32
#define HB_CURLPX_USER_REJECTED                     33

/* curl result codes. */

#define HB_CURLE_ERROR                        -1 /* request not passed to libcurl (libcurl not initialized or unknown parameter) */
#define HB_CURLE_OK                           0
#define HB_CURLE_UNSUPPORTED_PROTOCOL         1  /* */
#define HB_CURLE_FAILED_INIT                  2  /* */
#define HB_CURLE_URL_MALFORMAT                3  /* */
#define HB_CURLE_NOT_BUILT_IN                 4  /* */
#define HB_CURLE_COULDNT_RESOLVE_PROXY        5  /* */
#define HB_CURLE_COULDNT_RESOLVE_HOST         6  /* */
#define HB_CURLE_COULDNT_CONNECT              7  /* */
#define HB_CURLE_WEIRD_SERVER_REPLY           8  /* */
#define HB_CURLE_FTP_WEIRD_SERVER_REPLY       HB_CURLE_WEIRD_SERVER_REPLY
#define HB_CURLE_REMOTE_ACCESS_DENIED         9  /* a service was denied by the server due to lack of access - when login fails this is not returned. */
#define HB_CURLE_OBSOLETE10                   10 /* NOT USED */
#define HB_CURLE_FTP_WEIRD_PASS_REPLY         11 /* */
#define HB_CURLE_OBSOLETE12                   12 /* NOT USED */
#define HB_CURLE_FTP_WEIRD_PASV_REPLY         13 /* */
#define HB_CURLE_FTP_WEIRD_227_FORMAT         14 /* */
#define HB_CURLE_FTP_CANT_GET_HOST            15 /* */
#define HB_CURLE_HTTP2                        16 /* */
#define HB_CURLE_FTP_COULDNT_SET_TYPE         17 /* */
#define HB_CURLE_PARTIAL_FILE                 18 /* */
#define HB_CURLE_FTP_COULDNT_RETR_FILE        19 /* */
#define HB_CURLE_OBSOLETE20                   20 /* NOT USED */
#define HB_CURLE_QUOTE_ERROR                  21 /* quote command failure */
#define HB_CURLE_HTTP_RETURNED_ERROR          22 /* */
#define HB_CURLE_WRITE_ERROR                  23 /* */
#define HB_CURLE_OBSOLETE24                   24 /* NOT USED */
#define HB_CURLE_UPLOAD_FAILED                25 /* failed upload "command" */
#define HB_CURLE_READ_ERROR                   26 /* could open/read from file */
#define HB_CURLE_OUT_OF_MEMORY                27 /* */
#define HB_CURLE_OPERATION_TIMEDOUT           28 /* the timeout time was reached */
#define HB_CURLE_OBSOLETE29                   29 /* NOT USED */
#define HB_CURLE_FTP_PORT_FAILED              30 /* FTP PORT operation failed */
#define HB_CURLE_FTP_COULDNT_USE_REST         31 /* the REST command failed */
#define HB_CURLE_OBSOLETE32                   32 /* NOT USED */
#define HB_CURLE_RANGE_ERROR                  33 /* RANGE "command" didn't work */
#define HB_CURLE_HTTP_POST_ERROR              34 /* */
#define HB_CURLE_SSL_CONNECT_ERROR            35 /* wrong when connecting with SSL */
#define HB_CURLE_BAD_DOWNLOAD_RESUME          36 /* couldn't resume download */
#define HB_CURLE_FILE_COULDNT_READ_FILE       37 /* */
#define HB_CURLE_LDAP_CANNOT_BIND             38 /* */
#define HB_CURLE_LDAP_SEARCH_FAILED           39 /* */
#define HB_CURLE_OBSOLETE40                   40 /* NOT USED */
#define HB_CURLE_FUNCTION_NOT_FOUND           41 /* */
#define HB_CURLE_ABORTED_BY_CALLBACK          42 /* */
#define HB_CURLE_BAD_FUNCTION_ARGUMENT        43 /* */
#define HB_CURLE_OBSOLETE44                   44 /* NOT USED */
#define HB_CURLE_INTERFACE_FAILED             45 /* CURLOPT_INTERFACE failed */
#define HB_CURLE_OBSOLETE46                   46 /* NOT USED */
#define HB_CURLE_TOO_MANY_REDIRECTS           47 /* catch endless re-direct loops */
#define HB_CURLE_UNKNOWN_OPTION               48 /* User specified an unknown option */
#define HB_CURLE_UNKNOWN_TELNET_OPTION        HB_CURLE_UNKNOWN_OPTION
#define HB_CURLE_SETOPT_OPTION_SYNTAX         49 /* Malformed telnet option */
#define HB_CURLE_TELNET_OPTION_SYNTAX         HB_CURLE_SETOPT_OPTION_SYNTAX
#define HB_CURLE_OBSOLETE50                   50 /* NOT USED */
#define HB_CURLE_OBSOLETE51                   51 /* peer's certificate or fingerprint wasn't verified fine */
#define HB_CURLE_GOT_NOTHING                  52 /* when this is a specific error */
#define HB_CURLE_SSL_ENGINE_NOTFOUND          53 /* SSL crypto engine not found */
#define HB_CURLE_SSL_ENGINE_SETFAILED         54 /* cannot set SSL crypto engine as default */
#define HB_CURLE_SEND_ERROR                   55 /* failed sending network data */
#define HB_CURLE_RECV_ERROR                   56 /* failure in receiving network data */
#define HB_CURLE_OBSOLETE57                   57 /* NOT IN USE */
#define HB_CURLE_SSL_CERTPROBLEM              58 /* problem with the local certificate */
#define HB_CURLE_SSL_CIPHER                   59 /* couldn't use specified cipher */
#define HB_CURLE_PEER_FAILED_VERIFICATION     60 /* peer's certificate or fingerprint wasn't verified fine */
#define HB_CURLE_SSL_CACERT                   HB_CURLE_PEER_FAILED_VERIFICATION
#define HB_CURLE_BAD_CONTENT_ENCODING         61 /* Unrecognized transfer encoding */
#define HB_CURLE_OBSOLETE62                   62 /* Invalid LDAP URL */
#define HB_CURLE_LDAP_INVALID_URL             HB_CURLE_OBSOLETE62
#define HB_CURLE_FILESIZE_EXCEEDED            63 /* Maximum file size exceeded */
#define HB_CURLE_USE_SSL_FAILED               64 /* Requested FTP SSL level failed */
#define HB_CURLE_SEND_FAIL_REWIND             65 /* Sending the data requires a rewind that failed */
#define HB_CURLE_SSL_ENGINE_INITFAILED        66 /* failed to initialise ENGINE */
#define HB_CURLE_LOGIN_DENIED                 67 /* user, password or similar was not accepted and we failed to login */
#define HB_CURLE_TFTP_NOTFOUND                68 /* file not found on server */
#define HB_CURLE_TFTP_PERM                    69 /* permission problem on server */
#define HB_CURLE_REMOTE_DISK_FULL             70 /* out of disk space on server */
#define HB_CURLE_TFTP_ILLEGAL                 71 /* Illegal TFTP operation */
#define HB_CURLE_TFTP_UNKNOWNID               72 /* Unknown transfer ID */
#define HB_CURLE_REMOTE_FILE_EXISTS           73 /* File already exists */
#define HB_CURLE_TFTP_NOSUCHUSER              74 /* No such user */
#define HB_CURLE_CONV_FAILED                  75 /* conversion failed */
#define HB_CURLE_CONV_REQD                    76 /* caller must register conversion callbacks using curl_easy_setopt() options CURLOPT_CONV_FROM_NETWORK_FUNCTION, CURLOPT_CONV_TO_NETWORK_FUNCTION, and CURLOPT_CONV_FROM_UTF8_FUNCTION */
#define HB_CURLE_SSL_CACERT_BADFILE           77 /* could not load CACERT file, missing or wrong format */
#define HB_CURLE_REMOTE_FILE_NOT_FOUND        78 /* remote file not found */
#define HB_CURLE_SSH                          79 /* error from the SSH layer, somewhat generic so the error message will be of interest when this has happened */
#define HB_CURLE_SSL_SHUTDOWN_FAILED          80 /* Failed to shut down the SSL connection */
#define HB_CURLE_AGAIN                        81 /* socket is not ready for send/recv, wait till it's ready and try again */
#define HB_CURLE_SSL_CRL_BADFILE              82 /* could not load CRL file, missing or wrong format (Added in 7.19.0) */
#define HB_CURLE_SSL_ISSUER_ERROR             83 /* Issuer check failed. (Added in 7.19.0) */
#define HB_CURLE_FTP_PRET_FAILED              84 /* a PRET command failed */
#define HB_CURLE_RTSP_CSEQ_ERROR              85 /* mismatch of RTSP CSeq numbers */
#define HB_CURLE_RTSP_SESSION_ERROR           86 /* mismatch of RTSP Session Identifiers */
#define HB_CURLE_FTP_BAD_FILE_LIST            87 /* unable to parse FTP file list */
#define HB_CURLE_CHUNK_FAILED                 88 /* chunk callback reported error */
#define HB_CURLE_NO_CONNECTION_AVAILABLE      89 /* No connection available, the session will be queued */
#define HB_CURLE_SSL_PINNEDPUBKEYNOTMATCH     90 /* specified pinned public key did not match */
#define HB_CURLE_SSL_INVALIDCERTSTATUS        91 /* invalid certificate status */
#define HB_CURLE_HTTP2_STREAM                 92 /* stream error in HTTP/2 framing layer */
#define HB_CURLE_RECURSIVE_API_CALL           93 /* an api function was called from inside a callback */
#define HB_CURLE_AUTH_ERROR                   94 /* an authentication function returned an error */
#define HB_CURLE_HTTP3                        95 /* An HTTP/3 layer problem */
#define HB_CURLE_QUIC_CONNECT_ERROR           96 /* QUIC connection error */
#define HB_CURLE_PROXY                        97 /* proxy handshake error */
#define HB_CURLE_SSL_CLIENTCERT               98 /* client-side certificate required */
#define HB_CURLE_UNRECOVERABLE_POLL           99 /* poll/select returned fatal error */
#define HB_CURLE_TOO_LARGE                    100 /* a value/data met its maximum */

#define HB_CURLE_OBSOLETE16                   HB_CURLE_HTTP2

/* curl_version_info() returned array positions. */
#define HB_CURLVERINFO_VERSION                1
#define HB_CURLVERINFO_VERSION_NUM            2
#define HB_CURLVERINFO_HOST                   3
#define HB_CURLVERINFO_FEATURES               4
#define HB_CURLVERINFO_SSLVERSION             5
#define HB_CURLVERINFO_SSLVERSION_NUM         6
#define HB_CURLVERINFO_LIBZ_VERSION           7
#define HB_CURLVERINFO_PROTOCOLS              8  /* Array */
#define HB_CURLVERINFO_ARES                   9
#define HB_CURLVERINFO_ARES_NUM               10
#define HB_CURLVERINFO_LIBIDN                 11
#define HB_CURLVERINFO_ICONV_VER_NUM          12
#define HB_CURLVERINFO_LIBSSH_VERSION         13
#define HB_CURLVERINFO_BROTLI_VER_NUM         14
#define HB_CURLVERINFO_BROTLI_VERSION         15
#define HB_CURLVERINFO_NGHTTP2_VER_NUM        16
#define HB_CURLVERINFO_NGHTTP2_VERSION        17
#define HB_CURLVERINFO_QUIC_VERSION           18
#define HB_CURLVERINFO_CAINFO                 19
#define HB_CURLVERINFO_CAPATH                 20
#define HB_CURLVERINFO_ZSTD_VER_NUM           21
#define HB_CURLVERINFO_ZSTD_VERSION           22
#define HB_CURLVERINFO_HYPER_VERSION          23
#define HB_CURLVERINFO_GSASL_VERSION          24
#define HB_CURLVERINFO_FEATURE_NAMES          25
#define HB_CURLVERINFO_RTMP_VERSION           26
#define HB_CURLVERINFO_LEN                    26

/* HB_CURLVERINFO_FEATURES bit positions. */
#define HB_CURL_VERSION_IPV6                  hb_bitShift( 1, 0 )   /* IPv6-enabled */
#define HB_CURL_VERSION_KERBEROS4             hb_bitShift( 1, 1 )   /* Kerberos V4 auth is supported (deprecated) */
#define HB_CURL_VERSION_SSL                   hb_bitShift( 1, 2 )   /* SSL options are present */
#define HB_CURL_VERSION_LIBZ                  hb_bitShift( 1, 3 )   /* libz features are present */
#define HB_CURL_VERSION_NTLM                  hb_bitShift( 1, 4 )   /* NTLM auth is supported */
#define HB_CURL_VERSION_GSSNEGOTIATE          hb_bitShift( 1, 5 )   /* Negotiate auth is supported (deprecated) */
#define HB_CURL_VERSION_DEBUG                 hb_bitShift( 1, 6 )   /* Built with debug capabilities */
#define HB_CURL_VERSION_ASYNCHDNS             hb_bitShift( 1, 7 )   /* Asynchronous DNS resolves */
#define HB_CURL_VERSION_SPNEGO                hb_bitShift( 1, 8 )   /* SPNEGO auth is supported */
#define HB_CURL_VERSION_LARGEFILE             hb_bitShift( 1, 9 )   /* Supports files larger than 2GB */
#define HB_CURL_VERSION_IDN                   hb_bitShift( 1, 10 )  /* Internationized Domain Names are supported */
#define HB_CURL_VERSION_SSPI                  hb_bitShift( 1, 11 )  /* Built against Windows SSPI */
#define HB_CURL_VERSION_CONV                  hb_bitShift( 1, 12 )  /* Character conversions supported */
#define HB_CURL_VERSION_CURLDEBUG             hb_bitShift( 1, 13 )  /* Debug memory tracking supported */
#define HB_CURL_VERSION_TLSAUTH_SRP           hb_bitShift( 1, 14 )  /* TLS-SRP auth is supported */
#define HB_CURL_VERSION_NTLM_WB               hb_bitShift( 1, 15 )  /* NTLM delegation to winbind helper is supported */
#define HB_CURL_VERSION_HTTP2                 hb_bitShift( 1, 16 )  /* HTTP2 support built-in */
#define HB_CURL_VERSION_GSSAPI                hb_bitShift( 1, 17 )  /* Built against a GSS-API library */
#define HB_CURL_VERSION_KERBEROS5             hb_bitShift( 1, 18 )  /* Kerberos V5 auth is supported */
#define HB_CURL_VERSION_UNIX_SOCKETS          hb_bitShift( 1, 19 )  /* Unix domain sockets support */
#define HB_CURL_VERSION_PSL                   hb_bitShift( 1, 20 )  /* Mozilla's Public Suffix List, used for cookie domain verification */
#define HB_CURL_VERSION_HTTPS_PROXY           hb_bitShift( 1, 21 )  /* HTTPS-proxy support built-in */
#define HB_CURL_VERSION_MULTI_SSL             hb_bitShift( 1, 22 )  /* Multiple SSL backends available */
#define HB_CURL_VERSION_BROTLI                hb_bitShift( 1, 23 )  /* Brotli features are present */
#define HB_CURL_VERSION_ALTSVC                hb_bitShift( 1, 24 )  /* Alt-Svc handling built-in */
#define HB_CURL_VERSION_HTTP3                 hb_bitShift( 1, 25 )  /* HTTP3 support built-in */
#define HB_CURL_VERSION_ZSTD                  hb_bitShift( 1, 26 )  /* zstd features are present */
#define HB_CURL_VERSION_UNICODE               hb_bitShift( 1, 27 )  /* Unicode support on Windows */
#define HB_CURL_VERSION_HSTS                  hb_bitShift( 1, 28 )  /* HSTS is supported */
#define HB_CURL_VERSION_GSASL                 hb_bitShift( 1, 29 )  /* libgsasl is supported */
#define HB_CURL_VERSION_THREADSAFE            hb_bitShift( 1, 30 )  /* libcurl API is thread-safe */

/* HB_CURLOPT_HSTS_CTRL */
#define HB_CURLHSTS_ENABLE                    hb_bitShift( 1, 0 )
#define HB_CURLHSTS_READONLYFILE              hb_bitShift( 1, 1 )

/* HB_CURLOPT_HTTPPOST_FORM type. */
#define HB_CURLOPT_HTTPPOST_FORM_CONTENT      1
#define HB_CURLOPT_HTTPPOST_FORM_FILE         2

/* HB_CURLOPT_DEBUGBLOCK callback modes. */
#define HB_CURLINFO_TEXT                      0  /* Informational text. */
#define HB_CURLINFO_HEADER_IN                 1  /* Header (or header-like) data received from the peer. */
#define HB_CURLINFO_HEADER_OUT                2  /* Header (or header-like) data sent to the peer. */
#define HB_CURLINFO_DATA_IN                   3  /* Protocol data received from the peer. */
#define HB_CURLINFO_DATA_OUT                  4  /* Protocol data sent to the peer. */
#define HB_CURLINFO_SSL_DATA_IN               5  /* SSL/TLS (binary) data received from the peer. */
#define HB_CURLINFO_SSL_DATA_OUT              6  /* SSL/TLS (binary) data sent to the peer. */

/* HB_CURLOPT_WS_OPTIONS flags. */
#define HB_CURLWS_RAW_MODE                    hb_bitShift( 1, 0 )

/* curl_global_sslset() return values. */
#define HB_CURLSSLSET_NOT_IMPLEMENTED         -1  /* Harbour-specific value */
#define HB_CURLSSLSET_OK                      0
#define HB_CURLSSLSET_UNKNOWN_BACKEND         1
#define HB_CURLSSLSET_TOO_LATE                2
#define HB_CURLSSLSET_NO_BACKENDS             3

/* curl_global_sslset() 1st parameter and
   hash key of returned 3rd parameter. */
#define HB_CURLSSLBACKEND_NONE                0
#define HB_CURLSSLBACKEND_OPENSSL             1
#define HB_CURLSSLBACKEND_GNUTLS              2
#define HB_CURLSSLBACKEND_NSS                 3
#define HB_CURLSSLBACKEND_OBSOLETE4           4
#define HB_CURLSSLBACKEND_GSKIT               5
#define HB_CURLSSLBACKEND_POLARSSL            6
#define HB_CURLSSLBACKEND_WOLFSSL             7
#define HB_CURLSSLBACKEND_SCHANNEL            8
#define HB_CURLSSLBACKEND_SECURETRANSPORT     9
#define HB_CURLSSLBACKEND_AXTLS               10
#define HB_CURLSSLBACKEND_MBEDTLS             11
#define HB_CURLSSLBACKEND_LIBRESSL            HB_CURLSSLBACKEND_OPENSSL
#define HB_CURLSSLBACKEND_BORINGSSL           HB_CURLSSLBACKEND_OPENSSL
#define HB_CURLSSLBACKEND_CYASSL              HB_CURLSSLBACKEND_WOLFSSL
#define HB_CURLSSLBACKEND_MESALINK            12
#define HB_CURLSSLBACKEND_DARWINSSL           HB_CURLSSLBACKEND_SECURETRANSPORT

/* the error codes for the URL API */
#define HB_CURLUE_ERROR                       -1 /* request not passed to libcurl (e.g. unknown parameter) */
#define HB_CURLUE_OK                          0
#define HB_CURLUE_BAD_HANDLE                  1
#define HB_CURLUE_BAD_PARTPOINTER             2
#define HB_CURLUE_MALFORMED_INPUT             3
#define HB_CURLUE_BAD_PORT_NUMBER             4
#define HB_CURLUE_UNSUPPORTED_SCHEME          5
#define HB_CURLUE_URLDECODE                   6
#define HB_CURLUE_OUT_OF_MEMORY               7
#define HB_CURLUE_USER_NOT_ALLOWED            8
#define HB_CURLUE_UNKNOWN_PART                9
#define HB_CURLUE_NO_SCHEME                   10
#define HB_CURLUE_NO_USER                     11
#define HB_CURLUE_NO_PASSWORD                 12
#define HB_CURLUE_NO_OPTIONS                  13
#define HB_CURLUE_NO_HOST                     14
#define HB_CURLUE_NO_PORT                     15
#define HB_CURLUE_NO_QUERY                    16
#define HB_CURLUE_NO_FRAGMENT                 17
#define HB_CURLUE_NO_ZONEID                   18
#define HB_CURLUE_BAD_FILE_URL                19
#define HB_CURLUE_BAD_FRAGMENT                20
#define HB_CURLUE_BAD_HOSTNAME                21
#define HB_CURLUE_BAD_IPV6                    22
#define HB_CURLUE_BAD_LOGIN                   23
#define HB_CURLUE_BAD_PASSWORD                24
#define HB_CURLUE_BAD_PATH                    25
#define HB_CURLUE_BAD_QUERY                   26
#define HB_CURLUE_BAD_SCHEME                  27
#define HB_CURLUE_BAD_SLASHES                 28
#define HB_CURLUE_BAD_USER                    29
#define HB_CURLUE_LACKS_IDN                   30

/* URL parts */
#define HB_CURLUPART_URL                      0
#define HB_CURLUPART_SCHEME                   1
#define HB_CURLUPART_USER                     2
#define HB_CURLUPART_PASSWORD                 3
#define HB_CURLUPART_OPTIONS                  4
#define HB_CURLUPART_HOST                     5
#define HB_CURLUPART_PORT                     6
#define HB_CURLUPART_PATH                     7
#define HB_CURLUPART_QUERY                    8
#define HB_CURLUPART_FRAGMENT                 9
#define HB_CURLUPART_ZONEID                   10

/* URL flags */
#define HB_CURLU_DEFAULT_PORT                 hb_bitShift( 1, 0 )   /* return default port number */
#define HB_CURLU_NO_DEFAULT_PORT              hb_bitShift( 1, 1 )   /* act as if no port number was set, if the port number matches the default for the scheme */
#define HB_CURLU_DEFAULT_SCHEME               hb_bitShift( 1, 2 )   /* return default scheme if missing */
#define HB_CURLU_NON_SUPPORT_SCHEME           hb_bitShift( 1, 3 )   /* allow non-supported scheme */
#define HB_CURLU_PATH_AS_IS                   hb_bitShift( 1, 4 )   /* leave dot sequences */
#define HB_CURLU_DISALLOW_USER                hb_bitShift( 1, 5 )   /* no user+password allowed */
#define HB_CURLU_URLDECODE                    hb_bitShift( 1, 6 )   /* URL decode on get */
#define HB_CURLU_URLENCODE                    hb_bitShift( 1, 7 )   /* URL encode on set */
#define HB_CURLU_APPENDQUERY                  hb_bitShift( 1, 8 )   /* append a form style part */
#define HB_CURLU_GUESS_SCHEME                 hb_bitShift( 1, 9 )   /* legacy curl-style guessing */
#define HB_CURLU_NO_AUTHORITY                 hb_bitShift( 1, 10 )  /* Allow empty authority when the scheme is unknown. */
#define HB_CURLU_ALLOW_SPACE                  hb_bitShift( 1, 11 )  /* Allow spaces in the URL */
#define HB_CURLU_PUNYCODE                     hb_bitShift( 1, 12 )  /* get the host name in punycode */
#define HB_CURLU_PUNY2IDN                     hb_bitShift( 1, 13 )  /* punycode => IDN conversion */

/* curl_ws_send()/curl_ws_recv() flags */
#define HB_CURLWS_TEXT                        hb_bitShift( 1, 0 )
#define HB_CURLWS_BINARY                      hb_bitShift( 1, 1 )
#define HB_CURLWS_CONT                        hb_bitShift( 1, 2 )
#define HB_CURLWS_CLOSE                       hb_bitShift( 1, 3 )
#define HB_CURLWS_PING                        hb_bitShift( 1, 4 )
#define HB_CURLWS_OFFSET                      hb_bitShift( 1, 5 )
#define HB_CURLWS_PONG                        hb_bitShift( 1, 6 )

/* This is a return code for the progress callback that, when returned, will
   signal libcurl to continue executing the default progress function */
#define HB_CURL_PROGRESSFUNC_CONTINUE         0x10000001

#endif /* HBCURL_CH_ */
