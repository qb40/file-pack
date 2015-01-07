'function declarations
DECLARE SUB nibbleProb (file%, prob() AS SINGLE)
DECLARE SUB nibbleSort (prob() AS SINGLE, map() AS INTEGER)
DECLARE SUB probBits (prob() AS SINGLE, bits() AS STRING, ibegin%, iend%)
DECLARE SUB nibbleBits (map() AS INTEGER, bits() AS STRING)


'init
OPTION BASE 0
DIM prob(16) AS SINGLE
DIM map(16) AS INTEGER
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

PRINT "nibbleSort"
nibbleSort prob(), map()
FOR i% = 0 TO 15
PRINT map(i%); " = "; prob(i%)
NEXT
PRINT
k$ = INPUT$(1)

PRINT "probBits"
probBits prob(), bits(), 0, 16
k$ = INPUT$(1)
FOR i% = 0 TO 15
PRINT map(i%); " = "; prob(i%); " : "; bits(i%)
NEXT
PRINT
k$ = INPUT$(1)

PRINT "nibbleBits"
nibbleBits map(), bits()
FOR i% = 0 TO 15
PRINT map(i%); " : "; bits(i%)
NEXT
PRINT
k$ = INPUT$(1)


SUB nibbleBits (map() AS INTEGER, bits() AS STRING)

'map bits to nibble
i% = 0
WHILE i% <= 15
nibble% = map(i%)
IF nibble% <> map(i%) THEN
tbits$ = bits(nibble%)
bits(nibble%) = bits(i%)
bits(i%) = tbits$
map(nibble%) = nibble%
map(i%) = map(nibble%)
ELSE
i% = i% + 1
END IF
WEND

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
prob(i%) = prob(i%) / length&
NEXT

END SUB

SUB nibbleSort (prob() AS SINGLE, map() AS INTEGER)

'prepare for sort
FOR i% = 0 TO 15
map(i%) = i%
NEXT

'descending sort
FOR i% = 0 TO 15
max! = prob(i%)
maxi% = i%
FOR j% = i% + 1 TO 15
IF prob(j%) > max! THEN
max! = prob(j%)
maxi% = j%
END IF
NEXT
tprob! = prob(i%)
tmap% = map(i%)
prob(i%) = max!
map(i%) = map(maxi%)
prob(maxi%) = tprob!
map(maxi%) = tmap%
NEXT

END SUB

SUB probBits (prob() AS SINGLE, bits() AS STRING, ibegin%, iend%)

'exit if too small
IF (iend% - ibegin% < 2) THEN EXIT SUB

'find total probability
total! = 0
FOR i% = ibegin% TO iend% - 1
total! = total! + prob(i%)
NEXT

'find half probability point
sum! = 0
half! = total! / 2
halfi% = ibegin%
FOR i% = ibegin% TO iend% - 1
sum! = sum! + prob(i%)
IF sum! <= half! THEN
bits(i%) = bits(i%) + "0"
halfi% = i%
ELSE
bits(i%) = bits(i%) + "1"
END IF
NEXT
halfi% = halfi% + 1

'do that same for each half part
probBits prob(), bits(), ibegin%, halfi%
probBits prob(), bits(), halfi%, iend%

END SUB

