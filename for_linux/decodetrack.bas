#!/usr/bin/xbasic
' Decode MFM coded Track information (from an Floppy Disk).
' This can be used with DD Disks (Atari ST or MS-DOS PC)
' (c) by Markus Hoffmann 2019
' 
' It tries to read up to 82 Tracks, 2 sides and tries to locate and decode 
' up to 20 Sectors on each track. (Sizes 512 Bytes)
' 
'
'
DIM bad_tracks(83*2)
ARRAYFILL bad_tracks(),0
maxsector=0

PRINT "decodetrack"
path$="Disk11"
i=1
WHILE LEN(PARAM$(i))
  path$=PARAM$(i)
  INC i
WEND

verbose=0

ofile$=path$+".img"
IF EXIST(ofile$)
  PRINT "existiert schon.  QUIT"
  QUIT
ENDIF
PRINT "--> "+ofile$
open "O",#2,ofile$
pause 3
for track=0 to 81
  for side=0 to 1
  name$="track"+str$(track,3,3,1)+chr$(ASC("a")+side)+".bin"
  name$=path$+"/"+name$
  PRINT "<-- ";name$,
  if exist(name$)
    OPEN "I",#1,name$
    t$=INPUT$(#1,LOF(#1))
    CLOSE #1
    print len(t$)
    s$=""
    for i=0 to len(t$)-1
      s$=s$+bin$(PEEK(VARPTR(t$)+i) AND 255,8)
    next i

' print left$(s$,79)

  for i=1 to 12
    a$=@get_sector$(i)
    if len(a$)=0 and i<=maxsector
      print color(33,1);"ERROR: Sektor ";i;"/";maxsector;" missig!";color(1,0)

      bad_tracks(2*track+side)=3
      a$=SPACE$(512)
    endif
    exit if len(a$)=0
    print #2,a$;
  next i
  else
    print "ERROR: Track ";track;"/";side;" missing"
    bad_tracks(2*track+side)=2
  endif
  next side
next track
close #2

for i=0 to 82
  for j=0 to 1
  if bad_tracks(2*i+j)
    print "Bad Track ",i,j,bad_tracks(2*i+j)
  endif
  next j
next i

quit


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
      ' memdump varptr(se$),len(se$)
      c%=CRC16(se$)
      if c%<>0xa886
        PRINT "CRC-ERROR in sektor.",
        'print hex$(CRC16(left$(se$,512+2)))
	bad_tracks(2*track+side)=1
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
    wend
    exit if len(data$)>=len
    add i,2
  loop
  return data$
endfunction
procedure decode_mfm(off,len)
  local u$,mfm,data$,mflong,i
  u$=""
  mfm=0
  data$=""
  mflong=0
  i=off+2
  print "Decoding from offset ";off
'  print mid$(s$,i,16)
  do
    a$=MID$(s$,i,2)
    if a$="00"
      print "ERROR or END of sequence"
      exit if true
    else if a$="01"
      u$=u$+STR$(ABS(mfm))
      mfmlong=0
    else if a$="10"
      u$=u$+STR$(ABS(mfm))
      if mfmlong<2
        if mfm=0
          u$=u$+"0"
        endif
        inc mfmlong
      else if mfmlong=2
        u$=u$+STR$(ABS(mfm))
        mfmlong=1  
      endif
      mfm=1-mfm
    else if a$="11"
      u$=u$+"10"
      mfmlong=0
    endif
    while len(u$)>=8
      a$=left$(u$,8)
      u$=right$(u$,len(u$)-8)
      a=VAL("%"+a$)
      data$=data$+chr$(a)
    wend
    exit if len(data$)>=len
    add i,2
  loop
  memdump varptr(data$),len(data$)
RETURN

