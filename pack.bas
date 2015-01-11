'function declarations
DECLARE SUB progress (done!)
DECLARE SUB putBitsToFile (file%, bits$)
DECLARE FUNCTION lastIndexOf% (s$, item$)
DECLARE FUNCTION addFileExt$ (file$, ext$)
DECLARE SUB nibbleProb (file%, prob() AS SINGLE)
DECLARE SUB addBit (bits() AS STRING, map$, bit$)
DECLARE SUB packToFile (fsrc%, fdst%, bits() AS STRING)
DECLARE SUB selectSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)
DECLARE SUB insertSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)
DECLARE SUB huffmanCode (prob() AS SINGLE, bits() AS STRING)
DECLARE FUNCTION avgBitLength! (prob() AS SINGLE, bits() AS STRING)


'init
OPTION BASE 0
COMMON SHARED progressBegin!, progressEnd!
DIM prob(16) AS SINGLE
DIM bits(16) AS STRING
CLS

'info
COLOR 15
PRINT "File Pack"
COLOR 7
PRINT "---------"
PRINT

'get details
COLOR 14
INPUT "Source File"; fsrc$
PRINT
COLOR 12
PRINT "p : pack file"
PRINT "u : unpack file"
INPUT "Action (p/u)"; action$
action$ = LCASE$(LEFT$(action$, 1))
PRINT


'pack or unpack
SELECT CASE action$


'pack file
CASE "p"

'packed file deatils
COLOR 14
INPUT "Packed File"; fdst$
fdst$ = addFileExt$(fdst$, ".pk")
PRINT
COLOR 11

'open both files
OPEN "B", #1, fsrc$
OPEN "B", #2, fdst$

'get nibble probability
progressBegin! = 0
progressEnd! = .15
nibbleProb 1, prob()

'get huffman code bits
progressBegin! = .15
progressEnd! = .2
huffmanCode prob(), bits()

'get avg bit length
avgBits! = avgBitLength!(prob(), bits())
IF (avgBits! >= 4) THEN GOTO packerr

'save pk header to file
hdr$ = "pk"
PUT #2, , hdr$
slen& = LOF(1)
PUT #2, , slen&

'save compressed data
progressBegin! = .2
progressEnd! = 1
packToFile 1, 2, bits()

'close both files
CLOSE #1, #2

'unpack file
CASE ELSE


END SELECT
COLOR 14
PRINT
PRINT "done"
PRINT
COLOR 7
SYSTEM


OPEN "B", #1, file$
nibbleProb 1, prob()
CLOSE #1

PRINT "nibbleProb"
FOR i% = 0 TO 15
PRINT i%; " = "; prob(i%)
NEXT
PRINT
k$ = INPUT$(1)

PRINT "huffmanCode"
huffmanCode prob(), bits()
FOR i% = 0 TO 15
PRINT i%; " = "; prob(i%); " : "; bits(i%)
NEXT
PRINT
k$ = INPUT$(1)

PRINT "Avg Bit Length = "; avgBitLength!(prob(), bits())
k$ = INPUT$(1)



packerr:
COLOR 13
PRINT
PRINT "error: File cannot be packed"
PRINT
COLOR 7
SYSTEM



unpackerr:
COLOR 13
PRINT
PRINT "error: File cannot be unpacked"
PRINT
COLOR 7
SYSTEM

SUB addBit (bits() AS STRING, map$, bit$)

FOR i% = 1 TO LEN(map$)
imap% = ASC(MID$(map$, i%, 1))
bits(imap%) = bits(imap%) + bit$
NEXT

END SUB

FUNCTION addFileExt$ (file$, ext$)

fext$ = RIGHT$(file$, LEN(ext$))
IF fext$ = ext$ THEN fdst$ = file$ ELSE fdst$ = file$ + "." + ext$

addFileExt$ = fdst$
END FUNCTION

FUNCTION avgBitLength! (prob() AS SINGLE, bits() AS STRING)

avg! = 0
FOR i% = 0 TO 15
avg! = avg! + (prob(i%) * LEN(bits(i%)))
NEXT

avgBitLength! = avg!
END FUNCTION

SUB huffmanCode (prob() AS SINGLE, bits() AS STRING)

DIM map(16) AS STRING
DIM pr(16) AS SINGLE

'create default map
'copy probability
FOR i% = 0 TO 15
map(i%) = CHR$(i%)
pr(i%) = prob(i%)
NEXT
progress .1

'sort probability
selectSort pr(), map(), 0, 16
progress .3

FOR i% = 0 TO 14
addBit bits(), map(i%), "0"
addBit bits(), map(i% + 1), "1"
pr(i% + 1) = pr(i% + 1) + pr(i%)
map(i% + 1) = map(i% + 1) + map(i%)
insertSort pr(), map(), i% + 1, 16
NEXT
progress 1

END SUB

SUB insertSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)

'ascending order, 1st elem insert
ival! = value(ibegin%)
imap$ = map(ibegin%)
FOR i% = ibegin% + 1 TO iend% - 1
IF value(i%) >= ival! THEN EXIT FOR
value(i% - 1) = value(i%)
map(i% - 1) = map(i%)
NEXT
value(i% - 1) = ival!
map(i% - 1) = imap$

END SUB

FUNCTION lastIndexOf% (s$, item$)

'prepare
ls% = LEN(s$)
litem% = LEN(item$)

'find the item in reverse
FOR i% = ls% - litem% + 1 TO 1

'pick part of string
f$ = MID$(s$, i%, litem%)

'found?
IF f$ = item$ THEN
lastIndexOf% = i%
EXIT FUNCTION
END IF

NEXT

'nothing found
lastIndexOf% = 0

END FUNCTION

SUB nibbleProb (file%, prob() AS SINGLE)

'init
length& = LOF(file%)
left& = length&
ERASE prob
SEEK #file%, 1


WHILE left& > 0

'get data
IF left& >= 1024 THEN fetch& = 1024 ELSE fetch& = left&
left& = left& - fetch&
data$ = INPUT$(fetch&, #file%)
progress 1 - (left& / length&)

FOR i% = 1 TO LEN(data$)
byte% = ASC(MID$(data$, i%, 1))
prob(byte% AND 15) = prob(byte% AND 15) + 1
prob(byte% \ 16) = prob(byte% \ 16) + 1
NEXT

WEND

FOR i% = 0 TO 15
prob(i%) = prob(i%) / (length& * 2)
NEXT
progress 1

END SUB

SUB packToFile (fsrc%, fdst%, bits() AS STRING)

'get source details
SEEK #fsrc%, 1
length& = LOF(fsrc%)
left& = length&

WHILE left& > 0

'get data
IF left& >= 1024 THEN fetch& = 1024 ELSE fetch& = left&
left& = left& - fetch&
data$ = INPUT$(fetch&, #fsrc%)
progress 1 - (left& / length&)

'convert to bits
FOR i% = 1 TO LEN(data$)
byte% = ASC(MID$(data$, i%, 1))
bts$ = bts$ + bits(byte% AND 15)
bts$ = bts$ + bits(byte% \ 16)
NEXT

'save bits to file
putBitsToFile fdst%, bts$
bts$ = RIGHT$(bts$, LEN(bts$) MOD 8)

WEND

'save remaining bits
btadd% = LEN(bts$)
IF btadd% > 0 THEN
btadd% = 8 - btadd%
bts$ = bts$ + STRING$(btadd%, btadd%)
putBitsToFile fdst%, bts$
END IF
progress 1

END SUB

SUB progress (done!)
SHARED progressOn%, progressBegin!, progressEnd!

LOCATE CSRLIN, 1
prg! = progressBegin! + done! * (progressEnd! - progressBegin!)
prog% = INT(prg! * 100)
PRINT STR$(prog%); "% complete";

END SUB

SUB putBitsToFile (file%, bits$)

'get length of string that can be processed
length% = LEN(bits$)
length% = length% - (length% MOD 8)

FOR i% = 1 TO length% STEP 8
byte$ = MID$(bits$, i%, 8)
byte% = 0
FOR j% = 1 TO 8
byte% = byte% * 2 + VAL(MID$(byte$, j%, 1))
NEXT
data$ = data$ + CHR$(byte%)
IF LEN(data$) > 1024 THEN
PUT #file%, , data$
data$ = ""
END IF
NEXT
IF LEN(data$) > 0 THEN PUT #file%, , data$

END SUB

SUB selectSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)

'ascending sort
FOR i% = ibegin% TO iend% - 2
min! = value(i%)
mini% = i%
FOR j% = i% + 1 TO iend% - 1
IF value(j%) < min! THEN
min! = value(j%)
mini% = j%
END IF
NEXT
tval! = value(i%)
tmap$ = map(i%)
value(i%) = min!
map(i%) = map(mini%)
value(mini%) = tval!
map(mini%) = tmap$
NEXT

END SUB

