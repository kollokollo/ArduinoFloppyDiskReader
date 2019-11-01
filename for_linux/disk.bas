#!/usr/bin/xbasic
' Interface to an Arduino Nano with FloppyDriveController Firmware
' Which then can read raw Track information with an old Floppy disk drive
'
' (c) by Markus Hoffmann 2019 written in X11-Basic
'
'
' Saves each track in a separate File. Checks integrety.
'
' To decode the data (sectors), and then produce a mountable image, 
' there is a separate program (decodetrack.bas)
'
' Note: The Firmware V.1.4 was modified to read HD disks (1.4 MB), 
' but the error rate is still too high.
'

maxtrack=82
maxsector=0

OPEN "UX:2000000:8:N:1:CTS:DTR",#1,"/dev/ttyUSB0"
PAUSE 3
@dc("?")   ! Say Hello
PAUSE 1
@dc("+")   ! Switch on Motor
PAUSE 0.5
@dc(".")   ! Seek to Track 0 (HOME)
PAUSE 1
@dc("M")   ! Measure and let firmware decide about HD/DD
PAUSE 6
' Todo: use the measuement results to decide if it is a HD Disk.
'@dc("D")   ! HD-Disk ("H" or "D")
'PAUSE 0.5

FOR track=0 TO maxtrack-1
  FOR side=0 TO 1
    name$="track"+STR$(track,3,3,1)+CHR$(ASC("a")+side)+".bin"
    IF NOT EXIST(name$)
      retry=0
      WHILE retry>=0 AND retry<10
        dec retry
        if retry>=0
          PRINT "retry=";retry
        endif
        IF track<10
          @dc("#0"+str$(track))
        ELSE
          @dc("#"+str$(track))
        ENDIF
        PAUSE 0.3
        IF side=1
          @dc("[")
        ELSE
          @dc("]")
        ENDIF
        PAUSE 0.3
        @dc("<")
        PAUSE 0.3
        IF LEN(t$)>9300
          if @check_track(t$,track,side)>0
            PRINT color(34,1);"--> ";name$;color(1,0)
            bsave name$,varptr(t$),len(t$)
	    exit if true
	  else
	    PRINT color(31,1);"###### retry / sector";color(1,0)
	    add retry,2
	  endif
        else
 	  PRINT color(31,1);"###### retry / track";color(1,0)
          add retry,2
        endif
      wend
    endif
  next side
next track

pause 4
print "closing"

s:
@dc(".")   ! Seek HOME
PAUSE 1
@dc("-")   ! Switch off motor

CLOSE
QUIT

' Do a command in the Firmware
procedure dc(a$)
  LOCAL a$,a
  print "--> ";a$;" = ";
  flush
  print #1,a$;
  FLUSH #1
  a=inp(#1)-asc("0")
  if a=0
    print "ERROR"
  else
    print "OK."
  endif
  if a$="?"
    print chr$(inp(#1));chr$(inp(#1));chr$(inp(#1));chr$(inp(#1))
  else if a$="M"
    print chr$(inp(#1));chr$(inp(#1));chr$(inp(#1));
    while inp?(#1)
      a=INP(#1)
      print CHR$(a);
      FLUSH
    WEND
  else if a$="<"
    t=timer
    do
      a$=INKEY$
      if timer-t>0.5+random(250)/100
        a$=chr$(10)
      endif
      if len(a$)
        print #1,a$;
        FLUSH #1
        exit if len(a$)
      endif
      PAUSE 0.1
      while inp?(#1)
        a=INP(#1)
      wend
    loop
    print "wait for first byte"
    t$=chr$(inp(#1))
    nochmal:
    while inp?(#1)
      t$=t$+CHR$(INP(#1))
    wend
    pause 0.01
    if inp?(#1)
      goto nochmal
    endif
    print len(t$);" Bytes."
  else
    while inp?(#1)
      a=INP(#1)
      print CHR$(a);
      FLUSH
    WEND
  ENDIF
RETURN


function check_track(tt$,trk,sid)
  LOCAL i,s$,a$,ok
  ok=1
  CLS
  PRINT "check track #";trk;"/";sid
  s$=""
  for i=0 to len(tt$)-1
    s$=s$+BIN$(PEEK(VARPTR(tt$)+i) AND 255,8)
  next i
  for i=1 to 20
    a$=@get_sector$(i)
    if len(a$)=0 and (i<=maxsector or maxsector<9)
      print color(33,1);"ERROR: Sektor ";i;"/";maxsector;" missig!";color(1,0)
      ok=0
    endif
    exit if len(a$)=0
  next i
  return ok
endfunction


function get_sector$(sec)
  LOCAL d1$,d2$,off,se$,track,side,sector,size
  d1$="1011101110011110111001111011100101010101010110"
  d2$="10111011100111101110011110111001010101011101"
  off=0
  se$=""
  while len(se$)=0
    off=instr(s$,d1$,off)+len(d1$)
    exit if off=len(d1$)
    d$=@decode_mfm2$(off,6,0)
    c%=CRC16(d$)
    if c%<>0x7212
      PRINT "CRC-ERROR in Field."
      sector=-1
    else
      track=peek(varptr(d$))
      side=peek(varptr(d$)+1)
      sector=peek(varptr(d$)+2)
      size=peek(varptr(d$)+3)
      if sector>20
        print "Unusual Sectornumber: ";track;"/";side;"/";sector;"-";size
      else
        maxsector=MAX(sector,maxsector)
      endif
    endif
    if sector=sec
      print "Track=";track,
      print "Side=";side,
      print "Sector=";sector,
      print "Size=";size,
      off=instr(s$,d2$,off)+len(d2$)
      exit if off=len(d2$)
      se$=@decode_mfm2$(off,512+2,1)
      c%=CRC16(se$)
      if c%<>0xa886
        PRINT color(33);"CRC-ERROR in sector.";color(1,0),
	se$=""
	sector=-1
      else
        print "OK.",
      endif
      print
    endif
  wend
  return left$(se$,512)
endfunction


function decode_mfm2$(off,len,mfm)
  local u$,mfm,data$,mflong,i
  u$=""
  data$=""
  i=off
'  print "Decoding from offset ";off
'  print mid$(s$,i,16)
  do
    a$=MID$(s$,i,2)
    if a$="00"
      print "ERROR or END of sequence"
      exit if true
    else if a$="01"
      u$=u$+STR$(mfm)
    else if a$="10"
      if mfm<>0
        u$=u$+"0"
	mfm=0
      else
        u$=u$+"01"
	mfm=1
      endif
    else if a$="11"
      if mfm<>0
        u$=u$+"01"
	mfm=1
      else
        u$=u$+"00"
	mfm=0
	PRINT "--sync--";chr$(13);
      endif
    endif
    while len(u$)>=8
      a$=left$(u$,8)
      u$=right$(u$,len(u$)-8)
      a=VAL("%"+a$)
      data$=data$+chr$(a)
    WEND
    EXIT IF LEN(data$)>=len
    ADD i,2
  LOOP
  RETURN data$
ENDFUNCTION
