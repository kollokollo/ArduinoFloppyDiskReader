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

DIM sectorinhalt$(82,2,20)

DIM sector$(20)

maxsector=9
omaxsector=-1
dodelete=1


path$="Disk11"
ofile$="out.img"

logfile$="out.log"
sectormap$="secmap.dat"

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

if right$(path$)="/"
  path$=left$(path$,len(path$)-1)
endif
sectormap$=path$+"/sectormap.dat"
logfile$=path$+"/out.log"

verbose=0

IF EXIST(ofile$)
  PRINT ofile$,"existiert schon.  QUIT"
  QUIT
ENDIF
PRINT "--> "+logfile$
OPEN "O",#3,logfile$


PRINT "--> "+ofile$
OPEN "O",#2,ofile$
PRINT "--> "+sectormap$
OPEN "O",#4,sectormap$
for track=0 to 81
  for side=0 to 1
    trackinhalt$=STRING$(20*512/8,"-empty- ")
    cc$="-------------------- "
    name$="track"+str$(track,3,3,1)+chr$(ASC("a")+side)+".bin"
    name$=path$+"/"+name$
    PRINT AT(1,1);chr$(27);"[2K";
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
	BMOVE VARPTR(SECTOR$(i)),VARPTR(trackinhalt$)+i*512,512
      else
        done=0
      endif
    next i
    print cc$
    global_cc$=cc$
    si=0
    while done=0
      bad_tracks(2*track+side)=1

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
          IF PEEK(VARPTR(global_cc$)+i)<>ASC(".")
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
      bad_tracks(2*track+side)=-1
      print COLOR(41,1);"ERROR: Track ";track;"/";side;chr$(27);"[K"
      PRINT "WARNING: Track data is still incomplete...";chr$(27);"[K"
      PRINT global_cc$;COLOR(1,0);chr$(27);"[K"
      BEEP
  '    PAUSE 10
    ENDIF
    IF side=0
      PRINT #4,STR$(track,2,2,1);" ";global_cc$;" ";str$(bad_tracks(2*track+side),2,2);" ";
    ELSE
      PRINT #4,global_cc$;" ";str$(bad_tracks(2*track+side),2,2)
    ENDIF
    FLUSH #4
    ' MEMDUMP VARPTR(trackinhalt$),len(trackinhalt$)
    IF LEN(trackinhalt$)<512*maxsector
      PRINT COLOR(41,1);"ERROR trackinhalt too small.";COLOR(1,0)
      QUIT
    ENDIF
    IF omaxsector<>-1 AND omaxsector<>maxsector
      PRINT COLOR(41,1);"WARNING: maxsector has changed: ";omaxsector;" --> ";maxsector;COLOR(1,0)
' quit
' maxsector=10
     ' PAUSE 5
    ENDIF
    IF LOF(#2)/512/maxsector<>INT(LOF(#2)/512/maxsector)
      PRINT COLOR(41,1);"Something is wrong with file-len: ";LOF(#2);COLOR(1,0)
    ENDIF
    PRINT #2,LEFT$(trackinhalt$,512*maxsector);

    PRINT "--> ";chr$(27);"[J";lof(#2);" Bytes."
    omaxsector=maxsector
  NEXT side
NEXT track
CLOSE #2
PRINT chr$(27);"[J";
FOR i=0 to 82-1
  FOR j=0 to 1
    IF bad_tracks(2*i+j)=-1
      PRINT "Bad Track ",i,j,bad_tracks(2*i+j)
      PRINT #3,"Bad Track ",i,j,bad_tracks(2*i+j)
      IF dodelete
        name$="track"+STR$(i,3,3,1)+CHR$(ASC("a")+j)+".bin"
        name$=path$+"/"+name$
        IF EXIST(name$)
          PRINT "REMOVE"
          PRINT #3,"REMOVE"
	  si=0
	  REPEAT
  	    badname$=name$+".bad."+STR$(si,3,3,1)
	    INC si
  	  UNTIL NOT EXIST(badname$)
          SYSTEM "mv "+name$+" "+badname$
        ENDIF
      ENDIF
    ELSE if bad_tracks(2*i+j)=1
      PRINT "Bad Track ",i,j,bad_tracks(2*i+j)," was repaired."
      PRINT #3,"Bad Track ",i,j,bad_tracks(2*i+j)," was repaired."
    ENDIF
  NEXT j
NEXT i

OPEN "O",#1,path$+"sectors"
FOR i=0 to 82-1
  FOR j=0 to 1
    FOR k=0 to 20-1
      IF sectorinhalt$(i,j,k)<>""
        SEEK #1,512*(k+82*j+82*2*i)
        PRINT #1,sectorinhalt$(i,j,k);
      ENDIF
    NEXT k
  NEXT j
NEXT i
CLOSE #1
CLOSE #3,#4
QUIT

FUNCTION check_track$(tt$,trk,sid)
  LOCAL i,s$,a$,ok$
  ok$=SPACE$(21)
  PRINT chr$(27);"[2K";"check track #";trk;"/";sid
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
      ' PRINT "CRC-ERROR in Field."
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
      ' memdump varptr(se$),len(se$)
      c%=CRC16(se$)
      IF c%<>0xa886
        PRINT COLOR(33,1);"CRC-ERROR.";COLOR(1,0)
      ELSE
	sectorinhalt$(track,side,sector)=LEFT$(se$,256*size)
        print COLOR(32,1);"OK.";COLOR(1,0)
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
    add i,2
  LOOP
  RETURN data$
ENDFUNCTION
