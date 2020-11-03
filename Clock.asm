;(c) Copyright 2003. Joe DiMeglio. All rights reserved.
;(c) Copyright 2005. Includes PAUSE button.

                include    p16f84a.inc

        list    p=16f84a
    __config     0x3ff1    ;cp=off, pwrt=on, wdt=off, osc=xt

;       RAM Definition
;    __MAXRAM     H'CF'
;    __BADRAM     H'07', H'50'-H'7F', H'87'

;Descriptions of the ports
;    PORTA                 bit 0       4bit address to drive 7 seg display encodes
;                          bit 1
;                          bit 2
;                          bit 3  
;    PORTB                 bit 0        IRQ - program push button (prg)             (input)  
;                          bit 1        seconds    push button (set/PAUSE)          (input)
;                          bit 2        na                                          (input)
;                          bit 3        select T1 or T2 to program                  (input)
;                          bit 4        output buzzer                               (output)  
;                          bit 5-7        used to select which 7 segment to encode      (outputs)

; 4.000 000 Mhz gives 500,000 tmr0 cycles per second
; 500,000 decimal = 0x07a120 hex
; for my oscillator = 0x07a0e4 hex (499,940 Hz)



;RAM starts at 0x0c -----------------------------------------

sec1                    equ     0x20        ;time variables that are displayed
sec10                  equ     0x21
min1                    equ     0x22

t0_sec1_save            equ     0x23              
t0_sec10_save           equ     0x24
t0_min1_save            equ     0x25  

t1_sec1_save             equ     0x26
t1_sec10_save           equ     0x27
t1_min1_save             equ     0x28

tmode                   equ     0x29        ;if 0 use t0 pre-set variables else use t1 (toogler)
digit                      equ     0x2A        ;which digit are programming
timerselect             equ     0x2b        ;which timer are we programming
onesec                  equ     0x2c

buzzertoggole          equ     0x2d        ;
buzzertimer             equ     0x2e

tconst1             equ            0x30        ;trm0 variables
tconst0             equ            0x31
tcount1             equ            0x32
tcount0             equ            0x33

tconsta1            equ            0x34        ;trm0 variables for 0.4
tconsta0            equ            0x35
tcounta1            equ            0x36
tcounta0            equ            0x37

tconstb1            equ            0x38        ;trm0 variables for 0.6
tconstb0            equ            0x39
tcountb1            equ            0x3a
tcountb0            equ            0x3b

e_adr                 equ             0x3c        ;EEPROM data adr
e_check             equ             0x3d        ;EEPROM end check cnt adr
e_size                equ            d'06'         ;EEPROM data size

;alarmcount       equ        0x3f        ;count down for alarm time..
temp                equ        0x40
LSD                  equ        0x41
MSD                 equ        0x42

;-------------------------------
;EEPROM Address (64bytes in total)

ee_sec1_t0            equ        0x00
ee_sec10_t0          equ        0x01
ee_min1_t0            equ        0x02  
ee_sec1_t1            equ        0x03
ee_sec10_t1          equ        0x04
ee_min1_t1            equ        0x05


;fraction        equ        0x34

                org h'2100'

                de        d'00'
                de        d'00'
                de        d'03'              
                de        d'00'
                de        d'03'
                de        d'00'


    ;-------------------------------------------------------------
    ;            goto start  
    ;            org    04
    ;
    ;Interupts----------------------------------------------------
    ;            bcf    INTCN,1                ;Clear Int flag  
    ;          
    ;
    ;  
    ;            RETFIE                                      
    ;-------------------------------------------------------------              
                  org        0
                call    INITPORTS
                call    INITRTCC              
                call    READEEPROM      
                          
                ;movlw    0x03                ;start with 3:00 minute for t0
                ;movwf    t0_min1_save          
                ;clrf    t0_sec1_save
                ;clrf    t0_sec10_save  

                ;movlw    0x02
                ;movwf    t1_sec10_save    ;start with 0:29 seonds for t1
                ;clrf    t1_min1_save
                ;movlw    0x09
                ;movwf    t1_sec1_save


                clrf    onesec                   ;clear toggle

RESTART    clrf    tmode                    ;start with t0 pre-saved
                ;call    LOAD_T0               ;load t0 constants

                movf    t0_min1_save,0
                movwf    min1
                movf    t0_sec10_save,0
                movwf    sec10
                movf    t0_sec1_save,0
                movwf    sec1
                clrf    tmode            ;start with t0  
                ;movlw    0x01
                ;xorwf    tmode,1
  
        ;-------MAIN LOOP HERE-----------------------------------
loop           clrw                            ;clear regiser W                              
                btfsc    onesec,1            ;see if one has elapsed
                goto    inc_tick               ;yes then dec counter..              
                call     WAIT                  ;else wait some more and

                btfss    PORTB,0             ;check to see if prg pressed                                       
                goto    CHGDIGIT            ;yes it was pressed else
      
                btfss    PORTB,1             ;check pause button
                goto    PAUSE                ;yes it was pressed else
                goto     loop                  ;goto to start

inc_tick      bcf        onesec,1        ;reset onesec flag to zero.
                call    PRINTALL              ;output to the screen              
                call    TICK                    ; - adjust time

            ;    movf    alarmcount,1                  
            ;    btfss    STATUS,Z
            ;    goto    alarmcountrol

                goto    loop

;alarmcountrol    clrw
;                decf    alarmcount
;                movfw    alarmcount  
;                sublw    01  
;                btfsc    STATUS,Z  
;                call    BUZZER_OFF
;                goto    loop


;--------------------------------------------------------------------------
debounce                                   ; Are any of PORTB's inputs active?
                movf    PORTB, W        ;
                andlw    0x3               ; Keep only the 3 switch values
DEB           movwf    temp            ;
                movlw    0x10                   ; This is the debounce delay
                movf    MSD, F                  ;
                clrf    LSD                         ;
KB_D_LP1    decfsz    LSD, F                ;
                goto    KB_D_LP1               ;
                decfsz    MSD, F                ;
                  goto    KB_D_LP1             ;
END_DELAY movf    PORTB, W           
                andlw    0x03                    ; Keep only the 3 switch values
                subwf    temp, F                ;
                btfss    STATUS, Z             ; Is the Zero bit set?
                                                      ;    (switches were the same on 2 reads)
                goto    DEB                       ; NO, Try another read
KEY_MATCH
                movwf    temp                  ; YES, need to see which is depressed.
                return
;--------------------------------------------------------------------------
;this routine will check to see if the timer IRQ has been set.
;must be called

WAITASEC  bcf    onesec,1                ; reset onesec flag
waitloop2    btfsc onesec,1                 ; check if set no then carry on
                return                  
                call    WAIT
                goto    waitloop2          
;--------------------------------------------------------------------------
WAITA4SEC bcf   onesec,1
waitloop3    btfsc onesec,1
                return
                call    WAIT4
                goto  waitloop3          
;--------------------------------------------------------------------------
WAITA3SEC bcf   onesec,1
waitloop4    btfsc onesec,1
                return
                call    WAIT3
                goto    waitloop4              
;-------------------------------------------------------------------------
WAIT         btfss    INTCON,T0IF             ;check to see if T0 IRQ occur
                return                                ;nope
                bcf        INTCON,T0IF           ;yep then clear the IRQ flag

                decfsz    tcount0,f                ;decrement tcount0, if zero
                return                  
                decfsz    tcount1,f                ;decrement tcount1,
                return          

                movf    tconst0,w                ;if zero reset tcount0
                movwf    tcount0          
                movf    tconst1,w                ;reset tcount1
                movwf    tcount1                 ;then add one to seconds          
                bsf        onesec,1                ;flag that a second has occured

            ;    movf    fraction,w        ;
            ;    subwf    TMR0,f            ; put tmr0 back a fraction of a cycle
            ;    btfss    STATUS,C        ; if c is clear tmr0 < fraction
            ;    incf    tcount0,f        ; so add 1 to tcount0 to ensure delay
              
                return

;----------------------------------------------------------------------------------------
WAIT4            btfss    INTCON,T0IF    ;check to see if T0 IRQ occur  (1/4 of a second)
                return                            ;nope
                bcf        INTCON,T0IF       ;yep then clear the IRQ flag

                decfsz    tcounta0,f          ;decrement tcount0, if zero
                return                  
                decfsz    tcounta1,f          ;decrement tcount1,
                return          

                movf    tconsta0,w           ;if zero reset tcount0
                movwf    tcounta0          
                movf    tconsta1,w           ;reset tcount1
                movwf    tcounta1            ;then add one to seconds              
                bsf        onesec,1            ;flag that a second has occured              
                return
;----------------------------------------------------------------------------------------
WAIT3       btfss    INTCON,T0IF        ;check to see if T0 IRQ occur (3/5 of a second)
                return                            ;nope
                bcf        INTCON,T0IF       ;yep then clear the IRQ flag

                decfsz    tcountb0,f         ;decrement tcount0, if zero
                return                  
                decfsz    tcountb1,f         ;decrement tcount1,
                return          

                movf    tconstb0,w         ;if zero reset tcount0
                movwf    tcountb0          
                movf    tconstb1,w         ;reset tcount1
                movwf    tcountb1         ;then add one to seconds              
                bsf        onesec,1         ;flag that a second has occured              
                return

;routine library ---------------------------------------------
;Increment seconds by one
INCSEC      incf    sec1,1                  
                movlw    0x0a
                xorwf    sec1,0
                btfsc    STATUS,Z        ;jump if not equal
                goto    subinc1  
                return

subinc1      clrf    sec1
                incf    sec10,1
                movlw    0x06
                xorwf    sec10,0            ;only sixty second
                btfsc    STATUS,Z
                goto    subinc2
                return

subinc2      clrf    sec10
                incf    min1,1
                movlw    0x0a
                xorwf    min1,0            ;only upto 9:59 minutes
                btfsc    STATUS,Z
                clrf    min1
subinc3      return                      

;------------------------------------------------------------
;Decrement seconds by one
DECSEC_t0  decf    sec1,1  
                btfsc    sec1,7            ;if number is negative                              
                goto    subdec1_t0
                return

subdec1_t0 movlw    0x09            ;return to 9
                movwf    sec1
                decf    sec10,1
                btfsc    sec10,7
                goto    subdec2_t0
                return

subdec2_t0 movlw    0x05
                movwf    sec10
                decf    min1,1
                btfsc    min1,7
                ;btfsc    STATUS,Z
                goto    LOAD_T1
                return  

;------------------------------------------------------------
;loads the preset variables for t0          
LOAD_T0    movf    t0_min1_save,W
                movwf    min1
                movf    t0_sec10_save,W
                movwf    sec10
                movf    t0_sec1_save,W
                movwf    sec1  
                movlw    0x01
                xorwf    tmode,1  
                call    BUZZER
                return  

;------------------------------------------------------------
;Decrement seconds by one
DECSEC_t1  decf    sec1,1  
                btfsc    sec1,7            ;if number is negative              
                goto    subdec1_t1        ;yes then jump
                return

subdec1_t1 movlw    0x09            ;then load 9 seconds
                movwf    sec1
                decf    sec10,1
                btfsc    sec10,7            ;is tens of seconds zero
                goto    subdec2_t1
                return

subdec2_t1 movlw    0x05            ;then load 5
                movwf    sec10
                decf    min1,1
                btfsc    min1,7            ;is minutes negative
                goto    LOAD_T0            ;yes then change timers
                return  

;------------------------------------------------------------
;loads the preset variables for t0          
LOAD_T1    movf    t1_min1_save,W
                movwf    min1
                movf    t1_sec10_save,W
                movwf    sec10
                movf    t1_sec1_save,W
                movwf    sec1
                movlw    0x01
                xorwf    tmode,1
                call    BUZZER  
                return  

;------------------------------------------------------------
;determines if t0 or t1 and then decrements the time
TICK          movf    tmode,1            ;increments the time
                btfsc    STATUS,Z
                goto    run_t0
                call    DECSEC_t1
                return

run_t0       call    DECSEC_t0
                return

;------------------------------------------------------------
;change the seconds digit only
PRINTSEC1
                movlw    B'10000000'        ;select the external 4511 chip that drives the seconds
                movwf    PORTB            ;the LE only latches when low.
                movf    sec1,0
                xorlw    B'11111111'
                movwf    PORTA
                clrw
                movwf    PORTB            ;store value
                return

;------------------------------------------------------------
;change the 10's of second digit only
PRINTSEC10       
                movlw    B'01000000'
                movwf    PORTB
                movf    sec10,0
                xorlw    B'11111111'
                movwf    PORTA
                clrw                ;latch the output
                movwf    PORTB          
                return

;------------------------------------------------------------
;change the minute only
PRINTMIN1       
                movlw    B'00100000'
                movwf    PORTB
                movf    min1,0
                xorlw    B'11111111'
                movwf    PORTA
                clrw                    ;latch the output
                movwf    PORTB          
                return

;------------------------------------------------------------
;SET_PORTB        movwf    temp            ;save value
;                movf    alarmcount,1    ;check to see if zero                  
;                btfss    STATUS,Z
;                goto    set_speaker
;                movfw    temp
;                return
;
;set_speaker        movfw    temp
;                iorlw    B'00010000'
;                return
          
;------------------------------------------------------------


PRINTALL    bcf        STATUS,RP0
                movlw    B'00100000'        ;select second
                movwf    PORTB
                movf    min1,0
                movwf    1
                btfsc    STATUS,Z        ;check if min is zero
                goto    leadingZero        ;if zero make invalid hence blank                  
                xorlw    B'11111111'
                movwf    PORTA
                movlw    B'00000000'
                movwf    PORTB            ;store value
                movlw    B'01000000'        ;select second
                movwf    PORTB
                movf    sec10,0
                xorlw    B'11111111'
                movwf    PORTA
                movlw    B'00000000'
                movwf    PORTB            ;store value
                goto    second

leadingZero clrw
                movwf    PORTA
                movlw    B'01000000'        ;select second
                movwf    PORTB
                movf    sec10,0
                movwf    1                ;test W register for zero
                btfsc    STATUS,Z        ;
                movlw    B'11111111'
                xorlw    B'11111111'
                movwf    PORTA
                movlw    B'00000000'
                movwf    PORTB            ;store value
second       movlw    B'10000000'        ;select second
                movwf    PORTB
                movf    sec1,0
                xorlw    B'11111111'
                movwf    PORTA
                movlw    B'00000000'
                movwf    PORTB            ;store value
                return


;------------------------------------------------------------
;sets the appropriate ports to input and outputs
INITPORTS        bsf        STATUS,RP0        ;select bank 1
                movlw    B'11110000'                  
                movwf    TRISA            ;Set port A to outputs
                movlw    B'00001111'      
                movwf    TRISB                  
                bcf        STATUS,RP0
                return

;------------------------------------------------------------
;timings for a 4MHz gives 500,000 cycles per second = 0x07a120 hex
;for my program (fine tuned it) 0x07a0ee
INITRTCC        movlw    0x07
                movwf    tconst1
                incf    tconst1,w
                movwf    tconst1
                movwf    tcount1
                movlw    0xa1
                movwf    tconst0
                movwf    tcount0
;                movlw    0x20
;                movwf    fraction

                movlw    0x03
                movwf    tconsta1
                incf    tconsta1,w
                movwf    tconsta1
                movwf    tcounta1
                movlw    0x0d
                movwf    tconsta0
                movwf    tcounta0

                movlw    0x04
                movwf    tconstb1
                incf    tconstb1,w
                movwf    tconstb1
                movwf    tcountb1
                movlw    0x93
                movwf    tconstb0
                movwf    tcountb0

                bsf        STATUS,RP0        ;select bank 1
                movlw    0x50            ;set option_reg to TOCS=0 (internal),PSA=0 (tmr0),
                movwf    OPTION_REG        ;tmr0 prescaler = 1 in 2, port b pullups on

                bcf        STATUS,RP0        ;the rule will be to always leave in bank0
                return  
          
;------------------------------------------------------------
;routine to play sound here  
BUZZER        ;    movlw    3  
            ;    movwf    alarmcount
                movlw    B'00010000'        ;turn on relay for sound
                movwf    PORTB
            ;    return

                call    WAITASEC
                call    WAITASEC
                call    WAITASEC
          
                movlw    B'00000000'        ;turn off relay for sound
                movwf    PORTB  
                return

;BUZZER_OFF        movlw    B'00000000'        ;turn off relay for sound
;                movwf    PORTB  
;                return  

;------------------------------------------------------------
CHGDIGIT        ;call    WAITASEC        ;debounce the switch
                ;call    WAITASEC
                call    debounce
                ;goto    RESTART
                ;call    LOAD_T0            ;start with Timer0

                movf    t0_min1_save,0
                movwf    min1
                movf    t0_sec10_save,0
                movwf    sec10
                movf    t0_sec1_save,0
                movwf    sec1  
      
                call    PRINTSEC1
                call    PRINTSEC10
                call    PRINTMIN1
                ;call    PRINTALL        ;display all the digits      
              
                movlw    01                ;start from first digit
                movwf    digit          
              
                clrf    tmode            ;also used to toggle flashing  
                clrf    timerselect        ;start with timer0

chkbutloop        btfss    PORTB,0            ;check to see if prg pressed
                goto    nextdigit
              
                btfsc    onesec,1
                goto    inc_time              
                call    WAIT4

                btfss    PORTB,1            ;inc button
                call    INC_DIGIT  

                goto    chkbutloop      

inc_time        bcf        onesec,1
                call    FLASHDIGIT
                goto    chkbutloop  
      
;--------------------------------------------------------------
;increment the specific digit by one

INC_DIGIT      
                ;call    WAITA4SEC
                bcf        onesec,1
                btfsc    digit,0            ;is seconds  
                goto    incsec
                btfsc    digit,1
                goto    incsec10

incmin1            incf    min1,1                  
                movlw    0x0a
                xorwf    min1,0
                btfsc    STATUS,Z        ;jump if not equal
                clrf    min1              
                goto    displaytme
      
incsec            incf    sec1,1                  
                movlw    0x0a
                xorwf    sec1,0
                btfsc    STATUS,Z        ;jump if not equal
                clrf    sec1  
                goto    displaytme

incsec10        incf    sec10,1                  
                movlw    0x06
                xorwf    sec10,0
                btfsc    STATUS,Z        ;jump if not equal
                clrf    sec10  
          
displaytme        call    PRINTSEC1
                call    PRINTSEC10
                call    PRINTMIN1
                call    WAITA3SEC
                ;call    WAITA4SEC
                return
;--------------------------------------------------------------

nextdigit        clrf    tmode            ;ensure back to the right toggle mode
                ;call    PRINTALL        ;display all digits
                call    PRINTSEC1
                call    PRINTSEC10
                call    PRINTMIN1
              
                call    debounce
              
                ;call    WAITA4SEC
                ;call    WAITA4SEC
                ;call    WAITASEC

                bcf        STATUS,C        ;ensure the carry flag is cleared
                rlf        digit,1            ;rotate left

                btfss    digit,3            ;all finished this timer
                goto    chkbutloop
              
            ;check which timer

                btfss    timerselect,0        ;is that the second timer
                goto    secondtimer  
                goto    progend
              
secondtimer        movlw    01                ;start from first digit
                movwf    digit              
                movwf    timerselect        ;second timer          
                clrf    tmode            ;also used to toggle flashing              
                movf    min1,0            ;store values for timer0
                movwf    t0_min1_save          
                movf    sec10,0
                movwf    t0_sec10_save              
                movf    sec1,0
                movwf    t0_sec1_save

                ;call    LOAD_T1
                movf    t1_min1_save,0
                movwf    min1
                movf    t1_sec10_save,0
                movwf    sec10
                movf    t1_sec1_save,0
                movwf    sec1
                movlw    0x01
                xorwf    tmode,1
                call    PRINTSEC1
                call    PRINTSEC10
                call    PRINTMIN1
                ;call    WAITASEC
                ;call    WAITASEC

                goto    chkbutloop        ;continue with second timer

progend            movf    min1,0            ;store values for timer0
                movwf    t1_min1_save          
                movf    sec10,0
                movwf    t1_sec10_save              
                movf    sec1,0
                movwf    t1_sec1_save
                call    SAVEEEPROM        ;
                goto    RESTART          
;------------------------------------------------------------------
FLASHALL        movlw    0x01            ;toggle flash
                xorwf    tmode,1  
                movf    tmode,1  
                movlw    01                ;start from first digit
                movwf    digit
                call    flash_start        ;also toggle counter
              
                bcf        STATUS,C        ;ensure the carry flag is cleared
                rlf        digit,1            ;rotate left
                call    flash_start
              
                bcf        STATUS,C        ;ensure the carry flag is cleared
                rlf        digit,1            ;rotate left
                call    flash_start
                return
;------------------------------------------------------------------
FLASHDIGIT        movlw    0x01
                xorwf    tmode,1  
                movf    tmode,1            ;increments the toogle
flash_start        btfsc    STATUS,Z
                goto     digiton            ;digits are off on this run                                      

digitoff        btfsc    digit,0            ;is it seconds?
                goto    sec1_off
                btfsc    digit,1            ;is it 10's of seconds
                goto    sec10_off
                goto    min1_off

digiton            btfsc    digit,0
                goto    PRINTSEC1
                btfsc    digit,1
                goto    PRINTSEC10
                goto    PRINTMIN1  

sec1_off        movlw    B'10000000'        ;select the external 4511 chip that drives the seconds
                movwf    PORTB            ;the LE only latches when low.
                ;movlw    B'11111111'        ;blank display
                clrw
                movwf    PORTA              
                movwf    PORTB            ;store value
                return

sec10_off        movlw    B'01000000'        ;select the external 4511 chip that drives the seconds
                movwf    PORTB            ;the LE only latches when low.
                ;movlw    B'11111111'
                clrw
                movwf    PORTA
                movwf    PORTB            ;store value
                return

min1_off        movlw    B'00100000'        ;select the external 4511 chip that drives the seconds
                movwf    PORTB            ;the LE only latches when low.
                ;movlw    B'11111111'
                clrw
                movwf    PORTA
                movwf    PORTB            ;store value
                return

;-------------------------------------------------------------------------------
READEEPROM      
                bcf        STATUS, RP0                ;bank 0
                movlw    00                        ;load eeprom address
                movwf    EEADR                    ;set eeprom address
                bsf        STATUS,RP0                ;bank 1
                bsf        EECON1,RD                ;start eeprom readings
                bcf        STATUS,RP0                ;bank 0
                movf    EEDATA,W                ;read the eeprom data
                andlw    B'00001111'
                movwf    t0_sec1_save
                ;return

                bcf        STATUS, RP0
                movlw    ee_sec10_t0
                movwf    EEADR  
                bsf        STATUS,RP0      
                bsf        EECON1,RD
                bcf        STATUS,RP0
                movf    EEDATA,W
                andlw    B'00001111'
                movwf    t0_sec10_save

                bcf        STATUS, RP0
                movlw    ee_min1_t0
                movwf    EEADR  
                bsf        STATUS,RP0      
                bsf        EECON1,RD
                bcf        STATUS,RP0
                movf    EEDATA,W
                andlw    B'00001111'
                movwf    t0_min1_save

                movlw    ee_sec1_t1
                movwf    EEADR  
                bsf        STATUS,RP0      
                bsf        EECON1,RD
                bcf        STATUS,RP0
                movf    EEDATA,W
                andlw    B'00001111'
                movwf    t1_sec1_save
              
                bcf        STATUS, RP0
                movlw    ee_sec10_t1
                movwf    EEADR  
                bsf        STATUS,RP0      
                bsf        EECON1,RD
                bcf        STATUS,RP0
                movf    EEDATA,W
                andlw    B'00001111'
                movwf    t1_sec10_save

                bcf        STATUS, RP0
                movlw    ee_min1_t1
                movwf    EEADR  
                bsf        STATUS,RP0      
                bsf        EECON1,RD
                bcf        STATUS,RP0
                movf    EEDATA,W
                andlw    B'00001111'
                movwf    t1_min1_save

                bcf        STATUS,RP0        ;the rule will be to always leave in bank0

  
                return

WRITEEEPROM        bsf        STATUS,RP0            ;bank 1
                bcf        INTCON,GIE            ;disable IRQs
                bsf        EECON1, WREN
                movlw    0x55
                movwf    EECON2
                movlw    0xaa
                movwf    EECON2
                bsf        EECON1, WR
                nop

wait_ee            btfss    EECON1,4
                goto    wait_ee
                bcf        EECON1,4
                return
;-------------------------------------------------------------------------------
SAVEEEPROM      
          

;seconds timer 0              
                bcf        STATUS,RP0            ;bank 0
                movlw    ee_sec1_t0            ;select saved sec1
                movwf    EEADR
                movf    t0_sec1_save,w
                movwf    EEDATA
                call    WRITEEEPROM
          
                      
;tens of seconds timer 0

                bsf        INTCON,GIE            ;enable irq                              
                bcf        STATUS,RP0            ;bank 0
            ;    bcf        INTCON,GIE            ;disable IRQs
                movlw    ee_sec10_t0            ;select saved sec1
                movwf    EEADR
                movf    t0_sec10_save,w
                movwf    EEDATA
                call    WRITEEEPROM
                                          
  
;minutes timer 0
                                              
                bcf        STATUS,RP0            ;bank 0
                movlw    ee_min1_t0            ;select saved sec1
                movwf    EEADR
                movf    t0_min1_save,w
                movwf    EEDATA
                call    WRITEEEPROM
              

;seconds timer 1              
                bcf        STATUS,RP0            ;bank 0
                movlw    ee_sec1_t1            ;select saved sec1
                movwf    EEADR
                movf    t1_sec1_save,w
                movwf    EEDATA
                call    WRITEEEPROM
              
                      
;tens of seconds timer 1
                                              
                bcf        STATUS,RP0            ;bank 0
                movlw    ee_sec10_t1            ;select saved sec1
                movwf    EEADR
                movf    t1_sec10_save,w
                movwf    EEDATA
                call    WRITEEEPROM
              

;minutes timer 1
                                              
                bcf        STATUS,RP0            ;bank 0
                movlw    ee_min1_t1            ;select saved sec1
                movwf    EEADR
                movf    t1_min1_save,w
                movwf    EEDATA
                call    WRITEEEPROM
              
                bcf        STATUS,RP0        ;the rule will be to always leave in bank0
                return
                  

;-------------------------------------------------------------------------------
PAUSE            bcf        STATUS,RP0        ;set bank zero
                clrf    tmode            ;also used to toggle flashing
                ;call    WAITASEC        ;debounce the switch
                ;call    WAITASEC
                call    debounce        ;debounce the switch
              
looppause        btfss    PORTB,0            ;check to see if prg pressed                                       
                goto    exit_pause        ;yes goto main loop  
      
                btfss    PORTB,1            ;else check pause button
                goto    exit_pause        ;yes goto main loop

;                btfsc    onesec,1
;                goto    debounce              
;                call    WAIT4            ;wait 1/4 second
                goto     looppause        ;keep waiting until keypressed.  

exit_pause        call    debounce
                call    INITRTCC
                goto    inc_tick

                ;-----
;inc_flash        bcf        onesec,1
;                ;CALL    FLASHALL      
;
;                goto    looppause
;
;debounce        call    WAITASEC        ;debounce the switch
;                call    WAITASEC
;                goto    inc_flash


                END
