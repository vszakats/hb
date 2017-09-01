/*
 * YAML API - Harbour header
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

#ifndef HBYAML_CH_
#define HBYAML_CH_

/* token[ "type" ] values */
#define YAML_NO_TOKEN                       0
#define YAML_STREAM_START_TOKEN             1
#define YAML_STREAM_END_TOKEN               2
#define YAML_VERSION_DIRECTIVE_TOKEN        3
#define YAML_TAG_DIRECTIVE_TOKEN            4
#define YAML_DOCUMENT_START_TOKEN           5
#define YAML_DOCUMENT_END_TOKEN             6
#define YAML_BLOCK_SEQUENCE_START_TOKEN     7
#define YAML_BLOCK_MAPPING_START_TOKEN      8
#define YAML_BLOCK_END_TOKEN                9
#define YAML_FLOW_SEQUENCE_START_TOKEN      10
#define YAML_FLOW_SEQUENCE_END_TOKEN        11
#define YAML_FLOW_MAPPING_START_TOKEN       12
#define YAML_FLOW_MAPPING_END_TOKEN         13
#define YAML_BLOCK_ENTRY_TOKEN              14
#define YAML_FLOW_ENTRY_TOKEN               15
#define YAML_KEY_TOKEN                      16
#define YAML_VALUE_TOKEN                    17
#define YAML_ALIAS_TOKEN                    18
#define YAML_ANCHOR_TOKEN                   19
#define YAML_TAG_TOKEN                      20
#define YAML_SCALAR_TOKEN                   21

/* token[ "style" ] values and
   yaml_document_add_scalar() parameter */
#define YAML_ANY_SCALAR_STYLE               0
#define YAML_PLAIN_SCALAR_STYLE             1
#define YAML_SINGLE_QUOTED_SCALAR_STYLE     2
#define YAML_DOUBLE_QUOTED_SCALAR_STYLE     3
#define YAML_LITERAL_SCALAR_STYLE           4
#define YAML_FOLDED_SCALAR_STYLE            5

/* token[ "encoding" ] values and
   yaml_emitter_set_encoding() parameter */
#define YAML_ANY_ENCODING                   0
#define YAML_UTF8_ENCODING                  1
#define YAML_UTF16LE_ENCODING               2
#define YAML_UTF16BE_ENCODING               3

/* event[ "type" ] values */
#define YAML_NO_EVENT                       0
#define YAML_STREAM_START_EVENT             1
#define YAML_STREAM_END_EVENT               2
#define YAML_DOCUMENT_START_EVENT           3
#define YAML_DOCUMENT_END_EVENT             4
#define YAML_ALIAS_EVENT                    5
#define YAML_SCALAR_EVENT                   6
#define YAML_SEQUENCE_START_EVENT           7
#define YAML_SEQUENCE_END_EVENT             8
#define YAML_MAPPING_START_EVENT            9
#define YAML_MAPPING_END_EVENT              10

/* event[ "style" ] for YAML_SEQUENCE_START_EVENT and
   yaml_document_add_sequence() parameter */
#define YAML_ANY_SEQUENCE_STYLE             0
#define YAML_BLOCK_SEQUENCE_STYLE           1
#define YAML_FLOW_SEQUENCE_STYLE            2

/* event[ "style" ] for YAML_MAPPING_START_EVENT and
   yaml_document_add_mapping() parameter */
#define YAML_ANY_MAPPING_STYLE              0
#define YAML_BLOCK_MAPPING_STYLE            1
#define YAML_FLOW_MAPPING_STYLE             2

/* yaml_emitter_set_break() parameter */
#define YAML_ANY_BREAK                      0
#define YAML_CR_BREAK                       1
#define YAML_LN_BREAK                       2
#define YAML_CRLN_BREAK                     3

#endif /* HBYAML_CH_ */
