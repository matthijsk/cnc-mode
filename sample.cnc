% sample.cnc

(------------------------------------------------------------------------------)
(                                                                              )
(                       Sample CNC file                                        )
(                                                                              )
(------------------------------------------------------------------------------)
G30   X-100.0000   Y100.0000   Z-100.0000
G31   X100.0000   Y-100.0000   Z0.0000

#100=10
#101=SIN[30]
#102=COS[30]
#103=TAN[30]
#104=ASIN[30]
#105=ACOS[30]
#106=ATAN[30]
#107=HSIN[30]
#108=HCOS[30]
#109=HTAN[30]                   ; this is a comment

#101=SQRT[30]
#102=ABS[30]
#103=EXP[30]
#104=LN[30]
#105=LOG[30]
#106=POW[30]
#107=HYPOT[30]

#110=ROUND[1.1]
#111=FIX[3.2]
#112=FUP[9.9]

#113=3.5 MOD 1

#200=PI

ERROR "This is an error"

IF [ #100 EQ 0 ] GOTO N100
IF [ #100 NE 0 ] GOTO N100
IF [ 10 GT 0 ] GOTO N100
IF [ 10 LT 0 ] GOTO N100
IF [ 10 GE 10 ] GOTO N100
IF [ 10 LE 10 ] GOTO N100
IF [ 1 GE 0 AND 1 LE 10 ] GOTO N100
IF [ 1 LT 0 OR 1 GT 10 ] GOTO N100
IF [ 1 GT 0 XOR 1 LE 10 ] GOTO N100

#120=DEGREES[PI]
#121=RADIANS[90]

G19   G90   G71   G40
G92   S6154
G97   S0
G00   Z45.0000
G00   X0.0000   Y0.0000   Z45.0000
G00   Z100.000 ; dlkfjsdf

; this is another comment

T04 (r3 tool)

G97   S2500 (this is a block comment)
M31
G00   Z45.0000
G00   X0.000   Y-10   Z-45   
M08   (coolant)

G94   F100.000
G01   Z-57
G02   X0.0000   Y-4.0000   Z-51   R6.0000
G02   x0.0000   Y34.5000   Z-83  R33 
G00   X0.0000   Y35.000   Z100 
G00   X0.0000   Y-10.000  Z100
G00   X0.0000   Y-10.000  Z-45
G01   Z-57.5
G02   X0.0000   Y-4.0000   Z-51.5 R6.0000
G02   X0.0000   Y34.5000   z-83.5 R33
G00   X0.0000   Y35.00   z100 
G00   X106.5  Y-10  Z100
G00   X106.5	  Y-10   Z-45
G01   Z-57
G02   X106.5  Y-4  Z-51  R6
G02   X106.5  Y34.5  Z-83  R33
G00   X106.5		  Y35  Z100
G00   X106.5  Y-10  Z100
G00   X106.5  Y-10 Z-45
G01   Z-57.5
G02   X106.5  Y-4  Z-51.5 R6
G02   X106.5  Y34.5 Z-83.5 R33
G00   X106.5  Y35 Z100
G00   X-106.5 Y-10 Z100
G00   X-106.5 Y-10 Z-45
G01   Z-57
G02   X-106.5 Y-4 Z-51 R6
G02   X-106.5 Y34.5 Z-83 R33
G00   X-106.5 Y35 Z100
G00   X-106.5 Y-10 Z100
G00   X-106.5 Y-10 Z-45
G01   Z-57.5
G02   X-106.5 Y-4 Z-51.5 R6
G02   X-106.5 Y34.5 Z-83.5 R33
G00   X-106.5 Y35 Z100 

M09   (coolant)


M30


