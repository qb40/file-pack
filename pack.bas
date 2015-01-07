'function declarations
DECLARE SUB addBit (bits() AS STRING, map$, bit$)
DECLARE SUB nibbleProb (file%, prob() AS SINGLE)
DECLARE SUB selectSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)
DECLARE SUB insertSort (value() AS SINGLE, map() AS STRING, ibegin%, iend%)
DECLARE SUB huffmanCode (prob() AS SINGLE, bits() AS STRING)
DECLARE FUNCTION avgBitLength! (prob() AS SINGLE, bits() AS STRING)


'init
OPTION BASE 0
DIM prob(16) AS SINGLE
DIM bits(16) AS STRING
CLS

'read file
INPUT "File name"; file$
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

SUB addBit (bits() AS STRING, map$, bit$)

FOR i% = 1 TO LEN(map$)
imap% = ASC(MID$(map$, i%, 1))
bits(imap%) = bits(imap%) + bit$
NEXT

END SUB

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

'sort probability
selectSort pr(), map(), 0, 16

FOR i% = 0 TO 14
addBit bits(), map(i%), "0"
addBit bits(), map(i% + 1), "1"
pr(i% + 1) = pr(i% + 1) + pr(i%)
map(i% + 1) = map(i% + 1) + map(i%)
insertSort pr(), map(), i% + 1, 16
NEXT

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

FOR i% = 1 TO LEN(data$)
byte% = ASC(MID$(data$, i%, 1))
prob(byte% AND 15) = prob(byte% AND 15) + 1
prob(byte% \ 16) = prob(byte% \ 16) + 1
NEXT

WEND

FOR i% = 0 TO 15
prob(i%) = prob(i%) / (length& * 2)
NEXT

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

