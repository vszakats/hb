/*
DOOM generic portado para Harbour com interfaces minimas em C para acesso a allegro.

Por Wagner Nunes da Silva

vagucs@bol.com.br
vagucs@vagucs.com.br
vagucs@gmail.com

www.vagucs.com.br
*/
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

STATIC midiheader := ""
STATIC channelvelocities := {}
STATIC queuedtime := 0
STATIC tracksize := 0
STATIC controller_map := {}
STATIC channel_map := {}

#include "doomtype.ch"
#include "i_swap.ch"
#include "memio.ch"
#include "mus2mid.ch"

#define MUS2MID_CHANNELS 16
#define MIDI_PERCUSSION_CHAN 9
#define MUS_PERCUSSION_CHAN  15

#define mus_releasekey         0x00
#define mus_presskey           0x10
#define mus_pitchwheel         0x20
#define mus_systemevent        0x30
#define mus_changecontroller   0x40
#define mus_scoreend           0x60

#define midi_releasekey        0x80
#define midi_presskey          0x90
#define midi_aftertouchkey     0xA0
#define midi_changecontroller  0xB0
#define midi_changepatch       0xC0
#define midi_aftertouchchannel 0xD0
#define midi_pitchwheel        0xE0

CLASS musheader
    DATA id
    DATA scorelength
    DATA scorestart
    DATA primarychannels
    DATA secondarychannels
    DATA instrumentcount
    METHOD New()
ENDCLASS

METHOD New() CLASS musheader
    ::id                := ""
    ::scorelength       := 0
    ::scorestart        := 0
    ::primarychannels   := 0
    ::secondarychannels := 0
    ::instrumentcount   := 0
RETURN Self

INIT PROCEDURE init_mus2mid
    LOCAL i

    midiheader := "MThd" + ;
        Chr( 0x00 ) + Chr( 0x00 ) + Chr( 0x00 ) + Chr( 0x06 ) + ;
        Chr( 0x00 ) + Chr( 0x00 ) + ;
        Chr( 0x00 ) + Chr( 0x01 ) + ;
        Chr( 0x00 ) + Chr( 0x46 ) + ;
        "MTrk" + ;
        Chr( 0x00 ) + Chr( 0x00 ) + Chr( 0x00 ) + Chr( 0x00 )

    channelvelocities := {}
    FOR i := 1 TO MUS2MID_CHANNELS
        AAdd( channelvelocities, 127 )
    NEXT

    queuedtime := 0
    tracksize := 0

    controller_map := { ;
        0x00, 0x20, 0x01, 0x07, 0x0A, 0x0B, 0x5B, 0x5D, ;
        0x40, 0x43, 0x78, 0x7B, 0x7E, 0x7F, 0x79 }

    channel_map := {}
    FOR i := 1 TO MUS2MID_CHANNELS
        AAdd( channel_map, -1 )
    NEXT
RETURN

STATIC FUNCTION WriteTime( nTime, midioutput )
    LOCAL nBuffer
    LOCAL nWriteval

    nBuffer := ( nTime & 0x7F )
    nTime := UShr( nTime, 7 )
    DO WHILE nTime != 0
        nBuffer := nBuffer * 256
        nBuffer := ( nBuffer | ( ( nTime & 0x7F ) | 0x80 ) )
        nTime := UShr( nTime, 7 )
    ENDDO

    DO WHILE .T.
        nWriteval := ( nBuffer & 0xFF )
        IF mem_fwrite( nWriteval, 1, 1, midioutput ) != 1
            RETURN .T.
        ENDIF
        tracksize := tracksize + 1
        IF ( nBuffer & 0x80 ) != 0
            nBuffer := UShr( nBuffer, 8 )
        ELSE
            queuedtime := 0
            RETURN .F.
        ENDIF
    ENDDO
RETURN .F.

STATIC FUNCTION WriteEndTrack( midioutput )
    LOCAL cEnd := Chr( 0xFF ) + Chr( 0x2F ) + Chr( 0x00 )

    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( cEnd, 1, 3, midioutput ) != 3
        RETURN .T.
    ENDIF
    tracksize := tracksize + 3
RETURN .F.

STATIC FUNCTION WritePressKey( channel, key, velocity, midioutput )
    LOCAL nWorking

    nWorking := ( midi_presskey | channel )
    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( key & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( velocity & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    tracksize := tracksize + 3
RETURN .F.

STATIC FUNCTION WriteReleaseKey( channel, key, midioutput )
    LOCAL nWorking

    nWorking := ( midi_releasekey | channel )
    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( key & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := 0
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    tracksize := tracksize + 3
RETURN .F.

STATIC FUNCTION WritePitchWheel( channel, wheel, midioutput )
    LOCAL nWorking

    nWorking := ( midi_pitchwheel | channel )
    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( wheel & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( UShr( wheel, 7 ) & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    tracksize := tracksize + 3
RETURN .F.

STATIC FUNCTION WriteChangePatch( channel, patch, midioutput )
    LOCAL nWorking

    nWorking := ( midi_changepatch | channel )
    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( patch & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    tracksize := tracksize + 2
RETURN .F.

STATIC FUNCTION WriteChangeController_Valued( channel, control, value, midioutput )
    LOCAL nWorking

    nWorking := ( midi_changecontroller | channel )
    IF WriteTime( queuedtime, midioutput )
        RETURN .T.
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := ( control & 0x7F )
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    nWorking := value
    IF ( nWorking & 0x80 ) != 0
        nWorking := 0x7F
    ENDIF
    IF mem_fwrite( nWorking, 1, 1, midioutput ) != 1
        RETURN .T.
    ENDIF
    tracksize := tracksize + 3
RETURN .F.

STATIC FUNCTION WriteChangeController_Valueless( channel, control, midioutput )
RETURN WriteChangeController_Valued( channel, control, 0, midioutput )

STATIC FUNCTION AllocateMIDIChannel()
    LOCAL nResult
    LOCAL nMax
    LOCAL i

    nMax := -1
    FOR i := 0 TO MUS2MID_CHANNELS - 1
        IF channel_map[ i + 1 ] > nMax
            nMax := channel_map[ i + 1 ]
        ENDIF
    NEXT
    nResult := nMax + 1
    IF nResult == MIDI_PERCUSSION_CHAN
        nResult := nResult + 1
    ENDIF
RETURN nResult

STATIC FUNCTION GetMIDIChannel( mus_channel, midioutput )
    IF mus_channel == MUS_PERCUSSION_CHAN
        RETURN MIDI_PERCUSSION_CHAN
    ENDIF
    IF channel_map[ mus_channel + 1 ] == -1
        channel_map[ mus_channel + 1 ] := AllocateMIDIChannel()
        WriteChangeController_Valueless( channel_map[ mus_channel + 1 ], 0x7b, midioutput )
    ENDIF
RETURN channel_map[ mus_channel + 1 ]

STATIC FUNCTION ReadMusHeader( file, header )
    LOCAL lResult
    LOCAL cId := ""
    LOCAL nVal := 0

    lResult := mem_fread( @cId, 1, 4, file ) == 4
    IF ! lResult
        RETURN .F.
    ENDIF
    header:id := cId

    lResult := mem_fread( @nVal, 2, 1, file ) == 1
    IF ! lResult
        RETURN .F.
    ENDIF
    header:scorelength := SHORT( nVal )

    lResult := mem_fread( @nVal, 2, 1, file ) == 1
    IF ! lResult
        RETURN .F.
    ENDIF
    header:scorestart := SHORT( nVal )

    lResult := mem_fread( @nVal, 2, 1, file ) == 1
    IF ! lResult
        RETURN .F.
    ENDIF
    header:primarychannels := SHORT( nVal )

    lResult := mem_fread( @nVal, 2, 1, file ) == 1
    IF ! lResult
        RETURN .F.
    ENDIF
    header:secondarychannels := SHORT( nVal )

    lResult := mem_fread( @nVal, 2, 1, file ) == 1
    IF ! lResult
        RETURN .F.
    ENDIF
    header:instrumentcount := SHORT( nVal )
RETURN .T.

FUNCTION mus2mid( musinput, midioutput )
    LOCAL musfileheader
    LOCAL eventdescriptor := 0
    LOCAL channel
    LOCAL event
    LOCAL key := 0
    LOCAL controllernumber := 0
    LOCAL controllervalue := 0
    LOCAL tracksizebuffer
    LOCAL hitscoreend := 0
    LOCAL working := 0
    LOCAL timedelay

    FOR channel := 0 TO MUS2MID_CHANNELS - 1
        channel_map[ channel + 1 ] := -1
    NEXT

    musfileheader := musheader():New()
    IF ! ReadMusHeader( musinput, musfileheader )
        RETURN 1
    ENDIF

    IF SubStr( musfileheader:id, 1, 1 ) != "M" ;
         .OR. SubStr( musfileheader:id, 2, 1 ) != "U" ;
         .OR. SubStr( musfileheader:id, 3, 1 ) != "S" ;
         .OR. Asc( SubStr( musfileheader:id, 4, 1 ) ) != 0x1A
        RETURN 1
    ENDIF

    IF mem_fseek( musinput, musfileheader:scorestart, MEM_SEEK_SET ) != 0
        RETURN 1
    ENDIF

    mem_fwrite( midiheader, 1, Len( midiheader ), midioutput )
    tracksize := 0

    DO WHILE hitscoreend == 0
        DO WHILE hitscoreend == 0
            IF mem_fread( @eventdescriptor, 1, 1, musinput ) != 1
                RETURN 1
            ENDIF
            channel := GetMIDIChannel( ( eventdescriptor & 0x0F ), midioutput )
            event := ( eventdescriptor & 0x70 )

            SWITCH event
            CASE mus_releasekey
                IF mem_fread( @key, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                IF WriteReleaseKey( channel, key, midioutput )
                    RETURN 1
                ENDIF
                EXIT
            CASE mus_presskey
                IF mem_fread( @key, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                IF ( key & 0x80 ) != 0
                    IF mem_fread( @working, 1, 1, musinput ) != 1
                        RETURN 1
                    ENDIF
                    channelvelocities[ channel + 1 ] := ( working & 0x7F )
                ENDIF
                IF WritePressKey( channel, key, channelvelocities[ channel + 1 ], midioutput )
                    RETURN 1
                ENDIF
                EXIT
            CASE mus_pitchwheel
                IF mem_fread( @key, 1, 1, musinput ) != 1
                    EXIT
                ENDIF
                IF WritePitchWheel( channel, key * 64, midioutput )
                    RETURN 1
                ENDIF
                EXIT
            CASE mus_systemevent
                IF mem_fread( @controllernumber, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                IF controllernumber < 10 .OR. controllernumber > 14
                    RETURN 1
                ENDIF
                IF WriteChangeController_Valueless( channel, controller_map[ controllernumber + 1 ], midioutput )
                    RETURN 1
                ENDIF
                EXIT
            CASE mus_changecontroller
                IF mem_fread( @controllernumber, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                IF mem_fread( @controllervalue, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                IF controllernumber == 0
                    IF WriteChangePatch( channel, controllervalue, midioutput )
                        RETURN 1
                    ENDIF
                ELSE
                    IF controllernumber < 1 .OR. controllernumber > 9
                        RETURN 1
                    ENDIF
                    IF WriteChangeController_Valued( channel, controller_map[ controllernumber + 1 ], controllervalue, midioutput )
                        RETURN 1
                    ENDIF
                ENDIF
                EXIT
            CASE mus_scoreend
                hitscoreend := 1
                EXIT
            OTHERWISE
                RETURN 1
            ENDSWITCH

            IF ( eventdescriptor & 0x80 ) != 0
                EXIT
            ENDIF
        ENDDO

        IF hitscoreend == 0
            timedelay := 0
            DO WHILE .T.
                IF mem_fread( @working, 1, 1, musinput ) != 1
                    RETURN 1
                ENDIF
                timedelay := timedelay * 128 + ( working & 0x7F )
                IF ( working & 0x80 ) == 0
                    EXIT
                ENDIF
            ENDDO
            queuedtime := queuedtime + timedelay
        ENDIF
    ENDDO

    IF WriteEndTrack( midioutput )
        RETURN 1
    ENDIF

    IF mem_fseek( midioutput, 18, MEM_SEEK_SET ) != 0
        RETURN 1
    ENDIF

    tracksizebuffer := Chr( ( UShr( tracksize, 24 ) & 0xFF ) ) + ;
        Chr( ( UShr( tracksize, 16 ) & 0xFF ) ) + ;
        Chr( ( UShr( tracksize, 8 ) & 0xFF ) ) + ;
        Chr( ( tracksize & 0xFF ) )

    IF mem_fwrite( tracksizebuffer, 1, 4, midioutput ) != 4
        RETURN 1
    ENDIF
RETURN 0
