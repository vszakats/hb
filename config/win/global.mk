all : first

RES_EXT := .res
BIN_EXT := .exe
DYN_EXT := .dll

HB_CFLAGS += -DUNICODE

HB_GT_LIBS += gtwvt gtgui gtwin

HB_SIGN_TIMEURL := http://timestamp.digicert.com

# kernel32: needed by some compilers (pocc/watcom)
# user32: *Clipboard*(), GetKeyState(), GetKeyboardState(), SetKeyboardState(), gtwvt
# ws2_32/wsock32: hbsocket
# ws2_32: WSAIoctl()
# iphlpapi: hbsocket->GetAdaptersInfo()
# advapi32: GetUserName()
# gdi32: gtwvt
# winmm: timeGetTime()

SYSLIBS += kernel32 user32 ws2_32 iphlpapi advapi32 gdi32 winmm
