#!/usr/bin/xbasic
' Interface to an Arduino Nano with FloppyDriveController Firmware
' Which then can read raw Track information with an old Floppy disk drive
'
' (c) by Markus Hoffmann 2019 written in X11-Basic
'
'
' Saves each track in a separate file. Checks integrety.
'
' To decode the data (sectors), and then produce a mountable image, 
' there is a separate program (decodetrack.bas)
'
' Note: The Arduino firmware V.1.4 was modified to read HD disks (1.4 MB), 
' but the error rate is still very high.
'

maxtrack=82         ! maximum number of tracks 
maxsector=0

docheck=TRUE
device$="/dev/ttyUSB0"
path$="."

i=1
WHILE LEN(PARAM$(i))
  IF LEFT$(PARAM$(i))="-"
    IF param$(i)="--help" OR PARAM$(i)="-h"
      @intro
      @using
    ELSE IF PARAM$(i)="--version"
      @intro
      QUIT
    ELSE IF PARAM$(i)="--nocheck"
      docheck=FALSE
    ELSE IF PARAM$(i)="--check"
      docheck=TRUE
    ELSE IF PARAM$(i)="-o"
      INC i
      IF LEN(PARAM$(i))
        path$=PARAM$(i)
      ENDIF
    ELSE IF PARAM$(i)="-d" or PARAM$(i)="--device"
      INC i
      IF LEN(PARAM$(i))
        device$=PARAM$(i)
      ENDIF
    ELSE
      collect$=collect$+PARAM$(i)+" "
    ENDIF
  ELSE
  '  inputfile$=PARAM$(i)
  '  IF NOT EXIST(inputfile$)
  '    PRINT "floppydiskreader: "+inputfile$+": file or path not found"
  '    CLR inputfile$
  '  ENDIF
  ENDIF
  INC i
WEND

IF NOT EXIST(path$)
  PRINT "path ";path$;" does not exist."
  IF LEFT$(path$)<>"."
    PRINT "--> ";path$+"/"
    MKDIR path$
  ELSE
    PRINT "ERROR --> QUIT"
    QUIT
  ENDIF
ENDIF

IF NOT EXIST(device$)
  PRINT "ERROR: device ";device$;" does not exist."
  PRINT "Floppy reader is not accessible."
  PRINT "ERROR --> QUIT"
  QUIT
ENDIF

PRINT "<--> ";device$
OPEN "UX:2000000:8:N:1:CTS:DTR",#1,device$
PAUSE 2
@dc("?")   ! Say Hello
PAUSE 0.1
@dc("+")   ! Switch on Motor
PAUSE 0.3
@dc(".")   ! Seek to Track 0 (HOME)
PAUSE 0.5
@dc("M")   ! Measure and let firmware decide about HD/DD
PAUSE 5
' Todo: use the measuement results to decide if it is a HD Disk.
'@dc("D")   ! HD-Disk ("H" or "D")


FOR track=0 TO maxtrack-1
  FOR side=0 TO 1
    name$="track"+STR$(track,3,3,1)+CHR$(ASC("a")+side)+".bin"
    IF NOT EXIST(name$)
      retry=0
      WHILE retry>=0 AND retry<10
        DEC retry
        IF track<10
          @dc("#0"+STR$(track))
        ELSE
          @dc("#"+STR$(track))
        ENDIF
        IF side=1
          @dc("[")
        ELSE
          @dc("]")
        ENDIF
        PAUSE 0.1
        @dc("<"+CHR$(1))
        IF LEN(t$)>9300
           IF docheck
  	     cc=@check_track(t$,track,side)
           ELSE
	     cc=1
	   ENDIF
           IF cc>0
            PRINT COLOR(34,1);"--> ";name$;COLOR(1,0)
            BSAVE name$,VARPTR(t$),LEN(t$)
	    EXIT IF TRUE
	  ELSE
	    PRINT COLOR(31,1);"###### retry / sector";COLOR(1,0)
            si=0
	    REPEAT
  	      badname$=name$+".bad."+STR$(si,3,3,1)
	      INC si
	    UNTIL NOT EXIST(badname$)
            PRINT COLOR(35,1);"--> ";badname$;COLOR(1,0)
            BSAVE badname$,VARPTR(t$),LEN(t$)
	    ADD retry,2
	  ENDIF
        ELSE
 	  PRINT COLOR(31,1);"###### retry / track";COLOR(1,0)
          ADD retry,2
        ENDIF
      WEND
    ENDIF
  NEXT side
NEXT track
PRINT "closing"
s:
@dc(".")   ! Seek HOME
@dc("-")   ! Switch off motor
CLOSE
QUIT

PROCEDURE intro
  PRINT "Floppy Disk reader V.1.27 (c) Markus Hoffmann 2019"
  VERSION
RETURN
PROCEDURE using
  PRINT "Usage: floppy [options] "
  PRINT "Options:"
  PRINT "  -h, --help               Display this information"
  PRINT "  -d <device>              Use this device (default: ";device$;")"
  PRINT "  --device <device>        Use this device (default: ";device$;")"
  PRINT "  --check                  check integrity of tracks (default)"
  PRINT "  --nocheck                do not check integrity of tracks"
  PRINT "  -o <path>                Place the output into <path>"
RETURN



' Do a command in the Firmware
PROCEDURE dc(a$)
  LOCAL a$,a
  PRINT "--> ";a$;" = ";
  FLUSH
  PRINT #1,a$;
  FLUSH #1
  a=INP(#1)-ASC("0")
  IF a=0
    PRINT "ERROR"
  ELSE
    PRINT "OK."
  ENDIF
  IF a$="?"
    a$=CHR$(INP(#1))+CHR$(INP(#1))+CHR$(INP(#1))+CHR$(INP(#1))
    PRINT a$
    IF a$<>"V1.5" AND a$<>"V1.4"
      PRINT "ERROR: Arduino Firmware is not correct."
      CLOSE
      QUIT
    ENDIF
  ELSE IF a$="M"
    PRINT CHR$(INP(#1));CHR$(INP(#1));CHR$(INP(#1));
    WHILE INP?(#1)
      a=INP(#1)
      PRINT CHR$(a);
      FLUSH
    WEND
  ELSE IF LEFT$(a$)="<"
    t=TIMER
    PRINT "wait for first byte"
    t$=CHR$(INP(#1))
    nochmal:
    WHILE INP?(#1)
      t$=t$+CHR$(INP(#1))
    WEND
    PAUSE 0.01
    IF INP?(#1)
      GOTO nochmal
    ENDIF
    PRINT LEN(t$);" Bytes."
  ELSE
    WHILE INP?(#1)
      a=INP(#1)
      PRINT CHR$(a);
      FLUSH
    WEND
  ENDIF
RETURN


FUNCTION check_track(tt$,trk,sid)
  LOCAL i,s$,a$,ok
  ok=1
  CLS
  PRINT CHR$(27);"[2K";"check track #";trk;"/";sid
  s$=""
  FOR i=0 TO LEN(tt$)-1
    s$=s$+BIN$(PEEK(VARPTR(tt$)+i) AND 255,8)
  NEXT i
  FOR i=0 TO 20-1
    a$=@get_sector$(i+1)
    IF present or i<9
      IF found
        IF sec_status
        ELSE
          ok=0
          BREAK
        ENDIF
      ELSE
        PRINT COLOR(33,1);"ERROR: Sektor ";i+1;"/";maxsector;" missig!";COLOR(1,0)
        ok=0
        BREAK
      ENDIF
      maxsector=MAX(maxsector,i+1)
    ENDIF
    EXIT IF LEN(a$)=0
  NEXT i
  RETURN ok
ENDFUNCTION


FUNCTION get_sector$(sec)
  LOCAL d1$,d2$,off,se$,track,side,sector,size,d$,c%
  d1$="1011101110011110111001111011100101010101010110"
  d2$="10111011100111101110011110111001010101011101"
  off=0
  se$=""
  sec_status=0
  present=0     ! If sector is present in track
  found=0       ! If sector data could be found
  DO
    off=INSTR(s$,d1$,off)+LEN(d1$)
    EXIT IF off=LEN(d1$)
    d$=@decode_mfm2$(off,6,0)
    c%=CRC16(d$)
    IF c%<>0x7212
      PRINT "CRC-ERROR in Field."
      sector=-1
    ELSE
      track=PEEK(VARPTR(d$))
      side=PEEK(VARPTR(d$)+1)
      sector=PEEK(VARPTR(d$)+2)
      size=PEEK(VARPTR(d$)+3)
      IF sector>20
        PRINT COLOR(31,1);"Illegal sector number: ";track;"/";side;"/";sector;"-";size;COLOR(1,0)
      ENDIF
      IF sector=sec
        present=1
      ENDIF
    ENDIF
    IF sector=sec
      PRINT CHR$(27);"[2K";STR$(track,2,2,1);"/";
      PRINT side;"-";sector;" ";
      PRINT "/";size*256,
      off=INSTR(s$,d2$,off)+LEN(d2$)
      EXIT IF off=LEN(d2$)
      found=1
      se$=@decode_mfm2$(off,256*size+2,1)
      ' MEMDUMP VARPTR(se$),LEN(se$)
      c%=CRC16(se$)
      IF c%<>0xa886
        PRINT COLOR(33,1);"CRC-ERROR.";COLOR(1,0)
      ELSE
        PRINT COLOR(32,1);"OK.";COLOR(1,0)
	sec_status=1
      ENDIF
    ENDIF
    EXIT IF present AND found AND sec_status
  LOOP
  RETURN LEFT$(se$,256*size)
ENDFUNCTION


FUNCTION decode_mfm2$(off,len,mfm)
  LOCAL u$,mfm,data$,mflong,i
  u$=""
  data$=""
  i=off
  DO
    ' This is a faster version of: a$=MID$(s$,i,2)
    a$=CHR$(PEEK(VARPTR(s$)+i-1) AND 255)+CHR$(PEEK(VARPTR(s$)+i+0) AND 255)
    IF a$="00"
     ' print "ERROR or END of sequence"
      BREAK
    ELSE IF a$="01"
      u$=u$+STR$(mfm)
    ELSE IF a$="10"
      IF mfm<>0
        u$=u$+"0"
	mfm=0
      ELSE
        u$=u$+"01"
	mfm=1
      ENDIF
    ELSE IF a$="11"
      IF mfm<>0
        u$=u$+"01"
	mfm=1
      ELSE
        u$=u$+"00"
	mfm=0
	PRINT COLOR(35,1);"s";COLOR(1,0);
      ENDIF
    ENDIF
    WHILE LEN(u$)>=8
      a$=LEFT$(u$,8)
      u$=RIGHT$(u$,LEN(u$)-8)
      data$=data$+CHR$(VAL("%"+a$))
    WEND
    EXIT IF LEN(data$)>=len
    ADD i,2
  LOOP
  RETURN data$
ENDFUNCTION
