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

sectorstatus$=SPACE$(82*2*20)
DIM sectorinhalt$(82,2,20)

DIM sector$(20)

maxsector=9

dodelete=1


PRINT "decodetrack"
path$="Disk11"
ofile$="out.img"
i=1
WHILE LEN(PARAM$(i))
  IF LEFT$(PARAM$(i))="-"
    IF PARAM$(i)="-o"
      INC i
      IF LEN(PARAM$(i))
        ofile$=PARAM$(i)
      ENDIF
    ELSE  
      collect$=collect$+PARAM$(i)+" "
    ENDIF
  ELSE
    path$=PARAM$(i)
  ENDIF
  INC i
WEND

verbose=0

IF EXIST(ofile$)
  PRINT ofile$,"existiert schon.  QUIT"
  QUIT
ENDIF
PRINT "--> "+ofile$
open "O",#2,ofile$
for track=0 to 81
  POKE VARPTR(sectorstatus$)+track*2*20+side*20+19,10
  for side=0 to 1
    trackinhalt$=STRING$(20*512/8,"-empty- ")
    cc$="--------------------"
    name$="track"+str$(track,3,3,1)+chr$(ASC("a")+side)+".bin"
    name$=path$+"/"+name$
    if exist(name$)
      PRINT COLOR(35,1);"<-- ";name$,COLOR(1,0);
      OPEN "I",#1,name$
      t$=INPUT$(#1,LOF(#1))
      CLOSE #1
      print len(t$)
      cc$=@check_track$(t$,track,side)
    endif
    done=1
    for i=0 to maxsector-1
      IF PEEK(VARPTR(cc$)+i)=ASC(".")
        BMOVE VARPTR(trackinhalt$)+i*512,VARPTR(SECTOR$(i)),512
      else
        done=0
      endif
    next i
    print cc$
    global_cc$=cc$
    si=0
    while done=0
      bad_tracks(2*track+side)=2
      ' Now try to read some sectors from the bad track files. 
      badname$=name$+".bad."+STR$(si,3,3,1)
      if exist(badname$)
        PRINT COLOR(35,1);"<-- ";badname$,COLOR(1,0);
        OPEN "I",#1,badname$
        t$=INPUT$(#1,LOF(#1))
        CLOSE #1
        print len(t$)
        cc$=@check_track$(t$,track,side)
	print global_cc$,cc$
	inc si
        done=1
        for i=0 to maxsector-1
          IF PEEK(VARPTR(gloabl_cc$)+i)<>ASC(".")
	    IF PEEK(VARPTR(cc$)+i)=ASC(".")
	      BMOVE VARPTR(SECTOR$(i)),VARPTR(trackinhalt$)+i*512,512
	      POKE VARPTR(global_cc$)+i,PEEK(VARPTR(cc$)+i)
	      print "insert ";i,global_cc$
	    else 
              done=0
	    endif    
          endif
        next i
      else
	break
      endif
    wend
    if done=0
      print "ERROR: Track ";track;"/";side;" missing"
      PRINT "WARNING: Track data is still incomplete..."
      PRINT global_cc$
    endif
    ' MEMDUMP VARPTR(trackinhalt$),len(trackinhalt$)
    PRINT #2,LEFT$(trackinhalt$,512*maxsector);
  next side
next track
close #2

FOR i=0 to 82-1
  FOR j=0 to 1
    IF bad_tracks(2*i+j)
      PRINT "Bad Track ",i,j,bad_tracks(2*i+j)
      IF dodelete
        name$="track"+STR$(i,3,3,1)+CHR$(ASC("a")+j)+".bin"
        name$=path$+"/"+name$
        IF EXIST(name$)
          PRINT "REMOVE"
	  si=0
	  REPEAT
  	    badname$=name$+".bad."+STR$(si,3,3,1)
	    INC si
  	  UNTIL NOT EXIST(badname$)
          SYSTEM "mv "+name$+" "+badname$
        ENDIF
      ENDIF
    ENDIF
  NEXT j
NEXT i

BSAVE path$+"/sectorstatus",VARPTR(sectorstatus$),len(sectorstatus$)
OPEN "O",#1,path$+"sectors"
FOR i=0 to 82
  FOR j=0 to 1
    FOR k=0 to 20
      if sectorinhalt$(i,j,k)<>""
        seek #1,512*(k+82*j+82*2*i)
        print #1,sectorinhalt$(i,j,k);
      else

      endif
    NEXT k
  NEXT j
NEXT i
close #1
QUIT

FUNCTION check_track$(tt$,trk,sid)
  LOCAL i,s$,a$,ok$
  ok$=SPACE$(21)
  PRINT "check track #";trk;"/";sid
  s$=""
  FOR i=0 TO LEN(tt$)-1
    s$=s$+BIN$(PEEK(VARPTR(tt$)+i) AND 255,8)
  NEXT i
  FOR i=0 TO 20-1
    a$=@get_sector$(i+1)
    IF present or i<9
      IF found
        IF sec_status
          POKE VARPTR(ok$)+i,ASC(".")
	  sector$(i)=a$
	ELSE
          POKE VARPTR(ok$)+i,ASC("o")
          PRINT COLOR(33,1);"CRC-ERROR: Sektor ";i+1;COLOR(1,0)  
	  a$=STRING$(512/8,"-ERROR- ")
	ENDIF
      ELSE 
        PRINT COLOR(33,1);"ERROR: Sektor ";i+1;"/";maxsector;" missig!";COLOR(1,0)  
        POKE VARPTR(ok$)+i,ASC("-")
	bad_tracks(2*track+side)=3
	a$=STRING$(512/8,"-MISSING")
      ENDIF
      maxsector=MAX(maxsector,i+1)
    ENDIF
  NEXT i
  RETURN ok$
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
        PRINT "Unusual Sectornumber: ";track;"/";side;"/";sector;"-";size
      ENDIF
      if sector=sec
        present=1
      endif
    ENDIF
    IF sector=sec
      print "Track=";track,
      print "Side=";side;" ";
      print "Sector=";sector,
      print "Size=";size*256,
      off=instr(s$,d2$,off)+len(d2$)
      exit if off=len(d2$)
      found=1
      se$=@decode_mfm2$(off,256*size+2,1)
      ' memdump varptr(se$),len(se$)
      c%=CRC16(se$)
      if c%<>0xa886
        PRINT "CRC-ERROR in sektor.",
        'print hex$(CRC16(left$(se$,512+2)))
	bad_tracks(2*track+side)=1
	POKE VARPTR(sectorstatus$)+track*2*20+side*20+sector,ASC("-")
      else
        POKE VARPTR(sectorstatus$)+track*2*20+side*20+sector,ASC(".")
	sectorinhalt$(track,side,sector)=left$(se$,256*size)
        print "OK.",
	sec_status=1
      endif
      print
    endif
    EXIT IF present AND found AND sec_status
  LOOP
  return left$(se$,256*size)
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
