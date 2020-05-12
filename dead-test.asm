; Original code created by Simon Rowe;
; https://eden.mose.org.uk/gitweb/?p=dead-test.git;a=summary

; CRC32 routine based on one copyright cloudgen 2006


Pass    = 122                   ; char for tick   
Fail    = 24                    ; char for cross
Green   = 5                     ; colour code for green
Red     = 2                     ; colour code for red
 
ADDR	= $10			        ; current address (word)
PTR	    = $12                   ; general purpose pointer (word)
CRCZ	= $14                   ; zero page address of CRCs
BSIZ	= $15                   ; block size to CRC (MSB)
BITMASK = $16                   ; bitmask
COLR	= $17                   ; colour for test state
MAXAD	= $18                   ; maximum address line
CURBIT	= $19                   ; current walking address pattern (word)
EADDR	= $21                   ; region end address (word)
POSPTR  = $23                   ; keep track of postion
CHARPOS = $25                   ; character position on screen (word)
COLPOS  = $27                   ; colour position on screen (word)
TMP	    = $29
SYMBOL  = $30                   ; store tick or x
CHIPPOS = $31                   ; chip position on screen
CHIPFLT = $32                   ; store chip ID to display
CHIPPTR = $33                   ; chip pointer
CRCBASE	= $C0                   ; base for current CRCs
CRCREF	= CRCBASE+$10           ; base for reference CRCs
RERRFL	= $E0                   ; error state for each ROM block (8 addresses)
ERRFLG	= $E8                   ; error state for each RAM block (odd offsets)
DUMMY	= $03FF
SCBASE	= $0200                 ; start of screen
LOWRAM  = $10A4                 ; screen position for LOW RAM
COLOUR  = $10BA                 ; screen position for COLOUR
CLBASE1  = $9400
CLBASE	= $9600                 ; start of colour attributes
VICCR0	= $9000                 ; Bits 0-6 set the left edge of the screen; bit 7 enables or disables interlace scan
                                ; $9001 Determine distance from top of the TV to the first line of characters.

VICCR2	= $9002                 ; video address and screen columns
                        		    ; bit	function
                        		    ; ---	--------
                        		    ; 7	video memory address va9
                        		    ; colour memory address va9
                        		    ; 6-0	number of columns
VICCR5	= $9005                 ; video and character memory addresses
                        		    ; bit	function
                        		    ; ---	--------
                        		    ; 7-4	video memory address va13-va10
                        		    ; 3-0	character memory address va13-va10
VICCRF	= $900F                 ; background and border colour
                        		    ; bit	function
                        		    ; ---	--------
			                        ; 7-4	background colour
            		                ; 3	reverse video
                    		        ; 2-0	border colour

VICPALNTSC = $EDE4		        ; Byte used to determine if PAL or NTSC
								    ; PAL =  $0C
								    ; NTSC = $05


*	=	$A000                   ; set for cartridge

.WORD	DEDCOLD
.WORD	$0000       		
.BYT	"A0",'C'+$80,'B'+$80,'M'+$80    ; Vic-20 looks for A0CBM on startup 

;=======================================================================================================
; Initialization
;=======================================================================================================

DEDCOLD
.(

    LDX	#SCRN-VICINIT           ; set byte count to copy. Load count into X
ILOOP                           ; Loops and copies setup to VIC Chip
    LDA	VICINIT-1,X             ; get byte from setup table.
    STA	VICCR0-1,X              ; save byte to VIC chip starting to $9000
    DEX                         ; decrement count/index. Dec X
    BNE	ILOOP                   ; loop if more to do. Branch is zero flg is clear
.)


    ; check if PAL or NTSC and adjust values in VICINIT
	LDA #$0C
    CMP VICPALNTSC              ; Check if PAL or NTSC and adjust setup accordingly
    BEQ PAL

NTSC
    LDA #$05
    STA VICCR0                  ; interlace and horizontal origin
    LDA #$19
    STA VICCR0+1                ; vertical origin
    JMP FILLSCREEN
PAL
    LDA #$0C
    STA VICCR0                  ; interlace and horizontal origin
    LDA #$26
    STA VICCR0+1                ; vertical origin    


    LDA #0
    TAY

FILLP                           ; Clear colour address map $9400 and $9600
    STA $0000,Y
    STA CLBASE,Y                ; $9600
    STA CLBASE+$100,Y           ; $9700
    STA CLBASE1,Y               ; $9400
    STA CLBASE1+$100,Y          ; $9500
    INY
    BNE FILLP

FILLSCREEN

    ; Video memory starts at $1000. This is set in VICINIT
	LDA SCRN,Y
    STA $1000,Y                         
    LDA SCRN+$100,Y
    STA $1000+$100,Y
    INY
    BNE FILLSCREEN



    LDA #$c2                    ; b1100 0010
                                ; Video memory address b1100 = $1000
                                ; Character memory address b0010 = $8000
    STA VICCR5                  ; $9005

;=======================================================================================================
; start testing routines
;=======================================================================================================
    ; test low memory region ($0000-$03ff), this contains zero
    ; page and the stack.
    ; Zeropage ($00xx)
    ; Stack ($01xx)

    ; OS workspace ($0200..$03FF)

    ; test bits 3-0 (UD2)

START
    LDY #$79        	        
    LDX #0          	        

LOWLP
    INY                         ; Store 0x79 at $0000,$0100, $0200, $0300. Inc value, store at $0000+x etc etc, until all values from 0000-03ff filles
    TYA                          
    STA $0000,X                     
    STA $0100,X                   
    STA $0200,X                   
    STA $0300,X                  
    INX                          
    BNE LOWLP                   ; Branch if zero flag is clear

LORLP
    INY             
    TYA             
    EOR $0000,X                 ; Check if memory location is the same value that was previously stores
    AND #$0F 		            ; test bits 3-0
   
    BEQ *+7 		            ; Branch if result = 0
    LDA #1                      ; Set to 1 to display lower chip faulty
    JMP LOWMEMFAULTY            ; Error. Update screen and halt
    TYA             
    EOR $0100,X     
    AND #$0F	                ; test bits 3-0	
    BEQ *+7
    LDA #1                      ; Set to 1 to display lower chip faulty
    JMP LOWMEMFAULTY
    TYA             
    EOR $0200,X		
    AND #$0F		            ; test bits 3-0
    BEQ *+7
    LDA #1                      ; Set to 1 to display lower chip faulty
    JMP LOWMEMFAULTY
    TYA             
    EOR $0300,X     
    AND #$0F                    ; test bits 3-0
    BEQ *+7
    LDA #1                      ; Set to 1 to display lower chip faulty        
    JMP LOWMEMFAULTY
    INX             
    BNE LORLP       

	; repeat with inverted pattern

    TYA                          
    EOR #$FF          
    TAY               
    BMI LOWLP         
                                ; test bits 7-4 (UE2)
    LDY #$79
    LDX #0

HIWLP	
    INY
    TYA                         ; Loop through $0000 to $03FF
    STA $0000,X
    STA $0100,X
    STA $0200,X
    STA $0300,X
    INX
    BNE HIWLP

HIRLP	
    INY
    TYA
    EOR $0000,X
    AND #$F0
    BEQ *+7
    LDA #2                      ; Set to 2 to display upper chip faulty
    JMP LOWMEMFAULTY
    TYA
    EOR $0100,X
    AND #$F0
    BEQ *+7
    LDA #$06                    ; Set to 2 to display upper chip faulty
    JMP LOWMEMFAULTY
    TYA
    EOR $0200,X
    AND #$F0
    BEQ *+7
    LDA #$07                    ; Set to 2 to display upper chip faulty
    JMP LOWMEMFAULTY
    TYA
    EOR $0300,X
    AND #$F0
    BEQ *+7
    LDA #$08                    ; Set to 2 to display upper chip faulty
    JMP LOWMEMFAULTY
    INX
    BNE HIRLP

    ; repeat with inverted pattern

    TYA
    EOR #$FF
    TAY
    BMI HIWLP

    ; If we get here them low memory passed
    LDA #Pass                   ; Set char to tick
    STA LOWRAM                  ; Location of LOWRAM on screen
    
    LDA #Green                  ; Set colour to green 
    STA LOWRAM+$8400            ; Colour add address starts at $9400   

    ; low memory OK, test colour attribute memory ($9400-$97ff)
    ; we need to save the current value as its being used
    ; test bits 3-0 (UE1)

    LDY #$79
    LDX #0

;=======================================================================================================
; Check colour RAM
; $9400 - 94FF is being used so we need to save and then restore value 
;=======================================================================================================

COLORAM_9400
    LDA CLBASE1,X               
    STA TMP                     
    INY
    TYA
    STA CLBASE1,X               ; Save test byte

    EOR CLBASE1,X               ; Check test byte
    AND #$0F                    ; check bits 0-3
    BEQ CONT_9400
    JMP COLOURFAULTY            ; Error
CONT_9400
    LDA TMP                     ; tested fine so restore value
    STA CLBASE1,x               ; Restore original value
    INX
    BNE COLORAM_9400

; $94500 - 95FF is being used so we need to restore value afterwards. 
COLORAM_9500
    LDA CLBASE1+$100,X          ; Save byte to A
    STA TMP                     ; Save A to temp varible
    INY
    TYA
    STA CLBASE1+$100,X          ; Save test byte
    nop
    nop
   nop
    EOR CLBASE1+$100,X          ; Check test byte
    AND #$0F                    ; check bits 0-3
    BEQ CONT_9500
    JMP COLOURFAULTY            ; Error
CONT_9500
    LDA TMP                     ; tested fine so restore value
    STA CLBASE1+$100,x          ; Restore original value
    INX
    BNE COLORAM_9500

;check rest of color RAM. Not used so no need to save/restore
    LDY #$79
    LDX #0
CLWLP	
    INY
	TYA
	STA CLBASE,X
	STA CLBASE+$100,X
	INX
	BNE CLWLP

CLRLP
	INY
    TYA
    EOR CLBASE,X
    AND #$0F                    ; check bits 0-3
    BEQ *+5
    JMP COLOURFAULTY
    TYA
    EOR CLBASE+$100,X
    AND #$0F                    ; check bits 0-3
    BEQ *+5
    JMP COLOURFAULTY
    INX
    BNE CLRLP                   ; Jump if result not 0

    ; repeat with inverted pattern
    TYA
    EOR #$FF
    TAY
    BMI CLWLP                   ; Jump on result minus

    ; low & colour memory OK
    LDA #Pass                   ; Set char to tick
    STA COLOUR                  ; Location of RAM1
    LDA #Green                  ; Set colour to green 
    STA COLOUR+$8400  

    ; we can now use zero page for indirection and call subroutines
    LDX #$FF
    TXS                         ; set stack pointer to $FF
    LDA #%0011110               ; set background to white, border to blue
    STA VICCRF

    ; clear zero page and fill colour attribute memory
    LDA #0
    TAY
FILLP1	
    STA $0000,Y
    STA CLBASE,Y                ; CLBASE=$9600
    STA CLBASE+$100,Y
    INY
    BNE FILLP1

; copy color attribute memory from $9400 to $9600
COPYCOL 
    LDA $9400,Y
    STA CLBASE,Y
    LDA $9400+$100,Y
    STA CLBASE+$100,Y
    INY
    BNE COPYCOL

    ; copy screen layout SCRN to $200
FILSC	
    LDA $1000,Y                 ; copy from current used screen address
    STA SCBASE,Y                ; $200 to Y                 
    LDA SCRN+$100,Y
    STA SCBASE+$100,Y
    INY
    BNE FILSC

    ; move video memory to $0200, character generator to $8800
    LDA VICCR2                  ; VICCR2 = $9002
    ORA #%10000000
    STA VICCR2

    LDA #$82                    ;b1000 0010
                         		    ; 7-4	video memory address va13-va10
                        		    ; 3-0	character memory address va13-va10
    STA VICCR5                  ;$9005

    ; identify and display ROMS
    JSR IDROMS                  ; Jump saving return address

    ; start test proper
    JSR TESTRAM  
                                ; test all internal and expansion RAM
    JSR TESTROM                 ; compare ROM checksums

    ;The END
    JMP *


;=======================================================================================================
; Colour RAM Faulty. Display faulty and halt. 
;=======================================================================================================

COLOURFAULTY            
    LDA #Fail                   ; Set char to tick
    STA COLOUR                  ; Location of RAM1
    LDA #Red                    ; Set colour to green 
    STA COLOUR+$8400            ; $9400 
    
    LDX #8                      ; char pointer
    LDY #0

COLOUR_CHARLOOP
    LDA CHIPS,X                 ; load character number x of the string
    BEQ COLOUR_CHARLOOP
    STA $11BA,Y                 ; save it at position x of the screen ram
    LDA #Red
    STA $95BA,Y                 ; Set text colour
    INX                  
    INY                    
    BNE COLOUR_CHARLOOP      

    JMP *                       ; Halt

;=======================================================================================================
; Lower RAM Faulty. Display faulty and halt. 
;=======================================================================================================

LOWMEMFAULTY           

; Check if A is 1 or 2 (1 = upper, 2  = lower mem faulty)
    CMP #1
    BEQ LOWER
    JMP UPPER
    
LOWER
    LDX #0                      ; char pointer
    JMP LOWER_DISPLAY
UPPER
    LDX #4                      ; char pointer
LOWER_DISPLAY
    LDY #0

LOWER_CHARLOOP

    LDA CHIPS,X                 ; load character number x of the string
    BEQ LOWEROUT
    STA $11BA,Y                 ; save it at position x of the screen ram
 
    STA $200
    LDA #Red
    STA $95BA,Y                 ; Set text colour
    INX                  
    INY                    
    BNE LOWER_CHARLOOP      

LOWEROUT
    LDA #Fail           
    STA $10A3                   ; Display fail
    LDA #Red                    ; Set colour to red
    STA $94A3                   ; ($9400 + location)
    JMP *                       ; Halt

;=======================================================================================================
; ID ROMS using CRC 
;=======================================================================================================

IDROMS
.(
    ; calculate ROM checksums
    LDA #CRCREF                 ; load memory location into A
    STA CRCZ                    ; address for CRC
    JSR CRCROMS                 ; jump to subroutine. generate crc

    LDX #CRCREF
    STX TMP+2
    LDY #0
    STY TMP+3
    
RLOOP	
    LDA ROMOFF,Y
    BEQ OUT

    STA CHARPOS

    JSR IDROM                   
    INC TMP+3
    LDY TMP+3
    LDA TMP+2
    CLC
    ADC #4
    STA TMP+2
    TAX
    BNE RLOOP

OUT	
    LDA #CRCBASE
    STA CRCZ
    RTS
.)

;=======================================================================================================
; Test RAM 
; $9400 - 94FF is being used so we need to save and then restore value 
;=======================================================================================================
TESTRAM
.(
    LDX #2                      ; stupid hack. Make sure CHARPOS is in memory 2xx
    STX CHARPOS+1               ; Used by screen. Little indian so $2xx

    LDA #>CLBASE                ; Load high byte of color attributes
    STA COLPOS+1          

    LDY #0              
    STY ADDR                    ; Set ADDR=0
    LDA #%00000100              ; A10
    STA MAXAD                   ; Store max address lines

    LDA #0
    STA CHIPPOS                 ; Make sure pointer is 0
    STA CHIPPTR

;=======================================================================================================
;RAM1
;=======================================================================================================

   ; test data lines first

    LDA #$04                    ; RAM1 1KB memory banks - page (MSB)
    STA ADDR+1

    INY

    LDA #3                      ; Set to #3 so we dont show chip ID
    STA CHIPFLT

    LDX #$54                    ; screen position
    STX CHARPOS
    STX COLPOS
    
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble
   
    JSR SETRESULT               ; set char/colour according to result
                                ; z=1 if passed test
    
    BEQ DHI_RAM1                ; branch if zero flag is set
    
    STA ERRFLG,Y                ; set error for this block

DHI_RAM1	
    LDA #3                      ; Set position for chip UD2 in case we need it
    STA CHIPFLT
    
    LDX #$53                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC
 
       ; Setup details to display result upper nibble
    JSR SETRESULT               ; set char/colour according to result
    BEQ CHKDER_RAM1
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAM1	

    LDX #$55                    ; screen position
    STX CHARPOS
    STX COLPOS

    LDA ERRFLG,Y
    BNE CONT_RAM2               ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT 


    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAM2

    ; check all cells
   
    LDA #$56                    ; screen position
    STA CHARPOS
    STA COLPOS
    
    LDA #$55                    ; b01010101
    
    JSR TSTCEL

    JSR SETRESULT               ; set colour according to result

    STA ERRFLG,Y                ; set error for this block
    
    BNE CONT_RAM2

    LDA #$AA                    ; b10101010

    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block

;=======================================================================================================
;RAM2
;=======================================================================================================
CONT_RAM2

    LDA #3                      ; Set to #3 so we dont show chip ID
    STA CHIPFLT

    LDA #$08                    ; RAM2 1KB memory banks - page (MSB)
    INY
    STA ADDR+1
    LDA ERRFLG,Y


	BNE CONT_RAM3		        ; errors on this block skip


    LDX #$6A                    ; screen position
    STX CHARPOS
    STX COLPOS                                               

    
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result
 


    BEQ DHI_RAM2                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAM2	
    LDX #$69                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC
   
    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAM2
    STA ERRFLG,Y                ; set error for this block

  


CHKDER_RAM2

    LDX #$6B                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y



    BNE CONT_RAM3               ; data errors detected skip to next block


                                ; test address lines next
    JSR TESTADDR                ; test address lines

    JSR SETRESULT   
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAM3

    ; check all cells


    LDA #$6C                    ; screen position
    STA CHARPOS
    STA COLPOS

    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAM3
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

;=======================================================================================================
;RAM3
;=======================================================================================================
CONT_RAM3


    LDA #3                      ; Set to #3 so we dont show chip ID
    STA CHIPFLT

    LDA #$0C                    ; RAM3 1KB memory banks - page (MSB)
    INY
   
    STA ADDR+1
    LDA ERRFLG,Y
    BNE CONT_RAMA		        ; errors on this block skip

    LDX #$80                    ; screen position
    STX CHARPOS
    STX COLPOS                                               
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble
  
    JSR SETRESULT               ; set char/colour according to result
 

    BEQ DHI_RAM3                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAM3	
    LDX #$7F                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAM3
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAM3

    LDX #$81                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE CONT_RAMA               ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMA

    ; check all cells

    LDA #$82                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMA
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

;=======================================================================================================
;RAMA UD3 (Low)  UE3 (Upper)
;=======================================================================================================
CONT_RAMA



    LDA #16                     ; Set position for chip UD3 in case we need it
    STA CHIPFLT

    
    LDA #$10                    ; RAMA 1KB memory banks - page (MSB)
    INY 

    STA ADDR+1
    LDA ERRFLG,Y

 
    BNE CONT_RAMB		        ; errors on this block skip

    LDX #$4A                    ; screen position
    STX CHARPOS
    STX COLPOS                                             
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK

    SEC                         ; set carry flag

    ; Setup details to display result lower nibble
    JSR SETRESULT               ; set char/colour according to result

    BEQ DHI_RAMA                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAMA	

    LDA #21                     ; Set position for chip UE3 in case we need it
    STA CHIPFLT

    LDX #$49                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC
    
    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAMA
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAMA

    LDX #$4B                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE CONT_RAMB               ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines

    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMB

    ; check all cells

    LDA #$4C                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMB
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

;=======================================================================================================
;RAMB UD4 (Low)  UE4 (Upper)
;=======================================================================================================
CONT_RAMB

    LDA #26                     ; Set position for chip UD4 in case we need it
    STA CHIPFLT

    LDA #$14                    ; RAMB 1KB memory banks - page (MSB)
    INY 
    
    STA ADDR+1
    LDA ERRFLG,Y

  
    BNE CONT_RAMC		        ; errors on this block skip

    LDX #$60                    ; screen position
    STX CHARPOS
    STX COLPOS                                              
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result
 

    BEQ DHI_RAMB                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAMB	
    LDA #31                     ; Set position for chip UE4 in case we need it
    STA CHIPFLT

    LDX #$5F                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAMB
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAMB

    LDX #$61                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE CONT_RAMC               ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMC

    ; check all cells

    LDA #$62                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMC
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y


;=======================================================================================================
;RAMC UD5 (Low)  UE5 (Upper)
;=======================================================================================================

CONT_RAMC
    LDA #36                     ; Set position for chip UD5 in case we need it
    STA CHIPFLT

    LDA #$18                    ; RAMC 1KB memory banks - page (MSB)
    INY
    
    STA ADDR+1
    LDA ERRFLG,Y

    BNE CONT_RAMD		        ; errors on this block skip

    LDX #$76                    ; screen position
    STX CHARPOS
    STX COLPOS                                         
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result

    BEQ DHI_RAMC                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAMC	
    LDA #41                     ; Set position for chip UE5 in case we need it
    STA CHIPFLT

    LDX #$75                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAMC
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAMC

    LDX #$77                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE CONT_RAMD               ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMD

    ; check all cells

    LDA #$78                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$78
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE CONT_RAMD
    LDA #$C7
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y
;=======================================================================================================
;RAMD UD6 (Low)  UE6 (Upper)
;=======================================================================================================

CONT_RAMD
    INY
    
    LDA #46                     ; Set position for chip UD6 in case we need it
    STA CHIPFLT

    LDA #$1C                    ; RAMD 1KB memory banks - page (MSB)
    STA ADDR+1
    LDX #$8C                    ; screen position
    STX CHARPOS
    STX COLPOS                                             
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result

    BEQ DHI_RAMD                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_RAMD	
    LDA #51                     ; Set position for chip UE6 in case we need it
    STA CHIPFLT

    LDX #$8B                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_RAMD
    STA ERRFLG,Y                ; set error for this block

CHKDER_RAMD

    LDX #$8D                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE RAM_BLK1                ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK1

    ; check all cells

    LDA #$8E                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK1
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

;=======================================================================================================
;BLK1
;=======================================================================================================
RAM_BLK1
	
    INY 
    LDA #%00100000		        ; A13
	STA MAXAD

    LDA #3                      ; Set to zero to not display any chip
    STA CHIPFLT
    
    LDA #$20                    ; RAMD 1KB memory banks - page (MSB)
    STA ADDR+1
    LDX #$96                    ; screen position
    STX CHARPOS
    STX COLPOS                                             
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result
 

    BEQ DHI_BLK1                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_BLK1	
    LDX #$95                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_BLK1
    STA ERRFLG,Y                ; set error for this block

CHKDER_BLK1

    LDX #$97                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE RAM_BLK2                ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK2

    ; check all cells
    LDA #$98                    ; screen position
    STA CHARPOS
    STA COLPOS  
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK2
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y


;=======================================================================================================
;BLK2
;=======================================================================================================
RAM_BLK2
    
    INY 
    LDA #$40                    ; RAMD 1KB memory banks - page (MSB)
    STA ADDR+1
    LDX #$AC                    ; screen position
    STX CHARPOS
    STX COLPOS                                             
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result
 
    BEQ DHI_BLK2                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_BLK2	
    LDX #$AB                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result

    BEQ CHKDER_BLK2
    STA ERRFLG,Y                ; set error for this block

CHKDER_BLK2
    LDX #$AD                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE RAM_BLK3                ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   
    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK3

    ; check all cells

    LDA #$AE                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE RAM_BLK3
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

;=======================================================================================================
;BLK3
;=======================================================================================================
RAM_BLK3
    INY 
    LDA #$60                    ; RAMD 1KB memory banks - page (MSB)
    STA ADDR+1
    LDX #$C2                    ; screen position
    STX CHARPOS
    STX COLPOS                                             
    JSR TESTDATALOW             ; test lower nibble. returns with Zb=1 of OK
    SEC                         ; set carry flag

    ; Setup details to display result lower nibble

    JSR SETRESULT               ; set char/colour according to result
 

    BEQ DHI_BLK3                ; branch if zero flag is set
    STA ERRFLG,Y                ; set error for this block

DHI_BLK3	
    LDX #$C1                    ; screen position
    STX CHARPOS
    STX COLPOS
    JSR TESTDATAHI              ; test upper nibble
    CLC

    JSR SETRESULT               ; set char/colour according to result


    BEQ CHKDER_BLK3
    STA ERRFLG,Y                ; set error for this block

CHKDER_BLK3
    LDX #$C3                    ; screen position
    STX CHARPOS
    STX COLPOS
    LDA ERRFLG,Y
    BNE RAM_COMPLETE            ; data errors detected skip to next block
                                ; test address lines next
    JSR TESTADDR                ; test address lines
    JSR SETRESULT   

    STA ERRFLG,Y                ; set error for this block
    BNE RAM_COMPLETE

    ; check all cells

    LDA #$C4                    ; screen position
    STA CHARPOS
    STA COLPOS
    LDA #$55
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y                ; set error for this block
    BNE RAM_COMPLETE
    LDA #$AA
    JSR TSTCEL
    JSR SETRESULT               ; set colour according to result
    STA ERRFLG,Y

                                ; RAM tests completed
RAM_COMPLETE

; Change colour of 2 chip ID rows to RED

    LDY #16                     ; Length of 16
    LDX #0

FILLCOL 
    LDA #Red
    STA $978E,X                 ; Line 1
    STA $978E+$16,X             ; Line 2
    INX
    DEY
    BNE FILLCOL

	RTS
.)

;=======================================================================================================
; Test ROM 
;=======================================================================================================
TESTROM
.(
    JSR CRCROMS
    LDY #0

TLOOP	
    LDA RERRFL,Y
    BNE NEXT

    LDA #>CLBASE                ; Load high byte of color attributes
    STA COLPOS+1    
    INC COLPOS+1                ; Need to increase as ROM test is stored in lower half of screen. 97xx

    LDA ROMOFF,Y
    BEQ OUT

    STA COLPOS
    TYA
    ASL
    ASL
    TAX
    LDA CRCREF,X
    EOR CRCBASE,X
    BNE FAIL
    LDA CRCREF+1,X
    EOR CRCBASE+1,X
    BNE FAIL
    LDA CRCREF+2,X
    EOR CRCBASE+2,X
    BNE FAIL
    LDA CRCREF+3,X
    EOR CRCBASE+3,X

FAIL	
    STA RERRFL,Y
    STY TMP
    JSR ROMST
    LDY TMP

NEXT	
    INY
    BNE TLOOP

OUT	RTS
.)

;=======================================================================================================
; Test data low addresses. test data lines b3-0 at ADDR. returns with Zb=1 of OK
;=======================================================================================================
TESTDATALOW
.(
	LDX #0

	; walking 1's

	LDA #%00000001
W1LOOP	
    STA (ADDR,X)		        ; store bit
	STA BITMASK
	STY DUMMY		            ; dummy write
	LDA (ADDR,X)		        ; get stored value
	AND #$0F		            ; mask off top bits
	CMP BITMASK		            ; compare with bitmask
	BNE OUT
	LDA BITMASK
	ASL			                ; shift 0 into b0
	CMP #%00010000
	BNE W1LOOP

	; walking 0's

	LDA #%11111110

W0LOOP	
    STA (ADDR,X)		        ; store bit
	STA BITMASK
	STY DUMMY		            ; dummy write
	LDA (ADDR,X)		        ; get stored value
	ORA #$F0		            ; mask on top bits
	CMP BITMASK		            ; compare with bitmask
	BNE OUT
	LDA BITMASK
	SEC
	ROL			                ; shift 1 into b0
	CMP #%11101111
	BNE W0LOOP

OUT	
    RTS
.)

;=======================================================================================================
; Colour RAM Faulty. Display faulty and halt. test data lines b7-4 at ADDR. returns with Zb=1 of OK 
;=======================================================================================================
TESTDATAHI
.(
    LDX #0

    ; walking 1's

    LDA #%00010000
W1LOOP	
    STA (ADDR,X)                ; store bit
    STA BITMASK
    STY DUMMY                   ; dummy write
    LDA (ADDR,X)                ; get stored value
    AND #$F0                    ; mask off bottom bits
    CMP BITMASK                 ; compare with bitmask
    BNE OUT
    LDA BITMASK
    ASL                         ; shift 0 into b0
    BNE W1LOOP

    ; walking 0's

    LDA #%11101111
W0LOOP	STA (ADDR,X)            ; store bit
    STA BITMASK
    STY DUMMY                   ; dummy write
    LDA (ADDR,X)                ; get stored value
    ORA #$0F                    ; mask on bottom bits
    CMP BITMASK                 ; compare with bitmask
    BNE OUT
    LDA BITMASK
    SEC
    ROL                         ; shift 1 into b0
    CMP #%11111111
    BNE W0LOOP

OUT	RTS
.)

;=======================================================================================================
; update the colour/symbol based on test
;=======================================================================================================
SETRESULT
.(
 BNE FAIL                       ; jump is Z=0, ie an error from test
      
    TYA                         ; save Y as it contains count
    PHA                         

    LDA #Pass                   ; Load pass tick
    STA SYMBOL                  ; Store in Symbol
    LDA #Green                  ; green
    JMP PASS                                                            
   
FAIL	
    TYA                         ; save Y as it contains count 
    PHA

  LDX CHIPFLT                   ; Display chip ID

   LDY CHIPPTR
   TYA
   CMP #16  ;check if we are near EOL. If so then change point to nest line #22
   BEQ EOL
   
   JMP RAMCHIP_CHARLOOP

EOL
    LDY #22

                                ; Display faulty chip ID
RAMCHIP_CHARLOOP
    LDA CHIPS,X                 ; load character number x of the string
    BEQ RAMOUT                  ; jump when end of line
    STA $038E,Y                 ; save to screen ram
    INX                  
    INY 
    BNE RAMCHIP_CHARLOOP      

RAMOUT

    TYA
    STA CHIPPTR

    LDA #Fail                   ; Load  fail x
    STA SYMBOL                  ; Store in SYMBOL
    LDA #Red                    ; red

PASS
    STA COLR                    ; store status colour
 
    PLA                         ; here would be a good point restore Y
    TAY 

    LDA SYMBOL
    LDX CHIPPOS
    STA (CHARPOS,X)
    LDA COLR
    LDX #0
    STA (COLPOS,X) 
    
    SEC        
	SBC #5                      ; Subtract from 5 (Green) to check if test passed  

    RTS
.)

;=======================================================================================================
; test MAXAD address lines for block at ADDR. returns with Zb=1 if OK
;=======================================================================================================

TESTADDR
.(
    ; store sentinel pattern at first address in region

    LDX #$00
    LDA #$55
    STA (ADDR,X)

    ; walking 1's up to MAXAD

    STX CURBIT+1
    LDA #$01
    STA CURBIT

SLOOP	
    LDA ADDR                    ; build address in PTR
    ORA CURBIT
    STA PTR
    LDA ADDR+1
    ORA CURBIT+1
    STA PTR+1

    LDA #$AA
    STA (PTR,X)                 ; store inverse sentinel pattern

    LDA (ADDR,X)                ; check sentinel value
    CMP #$55
    BNE OUT                     ; overwritten, test failed
                                ; shift current bitmask up
    ASL CURBIT
    ROL CURBIT+1
    LDA CURBIT+1
    CMP MAXAD                   ; compare with max
    BNE SLOOP

    ; walking 0's up to MAXAD

    LDX MAXAD
    DEX
    TXA
    CLC
    ADC ADDR+1
    STA EADDR+1                 ; end address MSB
    STX CURBIT+1
    LDX #$FE
    STX CURBIT
    INX
    STX EADDR                   ; end address LSB
    INX

    ; store sentinel pattern in last address in region

    LDA #$55
    STA (EADDR,X)

WLOOP	LDA ADDR                ; build address in PTR
    ORA CURBIT
    STA PTR
    LDA ADDR+1
    ORA PTR+1

    LDA #$AA
    STA (PTR,X)                 ; store inverse sentinel pattern

    LDA (EADDR,X)               ; check sentinel value
    CMP #$55
    BNE OUT                     ; overwritten, test failed

                                ; shift current bitmask up
    SEC
    ROL CURBIT
    LDA CURBIT+1
    ROL
    PHA                         ; save unmasked MSB
    AND EADDR+1
    STA CURBIT+1

    PLA                         ; restore unmasked MSB
    AND MAXAD                   ; AND with max
    BNE WLOOP                   ; final shift will have a 0

OUT	RTS
.)

;=======================================================================================================
; update the colour/symbol based on test
;=======================================================================================================
SETST
.(
    BNE FAIL
    LDA #Pass                                                              
    STA SYMBOL
    LDA #Green              
    JMP PASS
FAIL	
    LDA #Fail
    STA SYMBOL 
    LDA #Red                
 PASS
    STA COLR
    LDX POSPTR    
    STA CHARPOS                 ; store char pointer
    STA COLPOS                  ; store char attribute pointer
    LDA COLR
    LDX #0
    STA (COLPOS,X)               
    LDA SYMBOL
    LDX #0
    STA (CHARPOS,X)         
                                ; set colour pointer LSB
    SEC
    SBC #5
    RTS
.)

;=======================================================================================================
; test every location in the block between ADDR and EADDR by
; writing accending values starting at .A
; returns with Zb=1 if OK
;=======================================================================================================
TSTCEL
.(
    STA TMP
    LDA ADDR
    STA PTR
    LDA ADDR+1
    STA PTR+1
                                ; first fill the region
FLOOP
    LDA TMP
    STA (PTR,X)
    INC TMP
    INC PTR
    BNE FLOOP
    INC PTR+1
    LDA PTR+1
    CMP EADDR+1
    BCC FLOOP
    BEQ FLOOP
                                ; then check the correct value is present
    LDA ADDR
    STA PTR
    LDA ADDR+1
    STA PTR+1

CLOOP
    LDA TMP
    CMP (PTR,X)
    BNE OUT
    INC TMP
    INC PTR
    BNE CLOOP
    INC PTR+1
    LDA PTR+1
    CMP EADDR+1
    BCC CLOOP
    BEQ CLOOP
    TXA

OUT	
    RTS
.)

;=======================================================================================================
; generate the CRC32 for all ROMS, store in zero page locations
; indicated by CRCZ
;=======================================================================================================
CRCROMS
.(
    LDY #0

NEXTB	
    LDA #$20
    STA BSIZ
    LDA ROMPAG,Y                ; get block MSB
    BEQ OUT
    BIT ONE
    BEQ SZ8K
    LSR BSIZ                    ; 4K block, divide size by 2
    AND #$FE
SZ8K	
    STA ADDR+1
    CLC
    ADC BSIZ
    STA EADDR+1
    TYA                         ; copy index to .Y
    ASL                         ; multiply by ..
    ASL                         ; .. 4
    CLC
    ADC CRCZ                    ; add to zero page base
    TAX                         ; copy address to .X
    STY TMP+1                   ; save .Y
    JSR CRC32
    LDY TMP+1                   ; restore .Y

CONT	INY                     ; increment index to next block MSB
    BNE NEXTB                   ; branch always

OUT	
    RTS
.)

;=======================================================================================================
; match the ROM CRC at .X updating text starting at POS
;=======================================================================================================
IDROM
.(
    STX TMP+1
    LDY #0

ILOOP	LDA ROMSUMS,Y
    BEQ OUT                     ; branch if equal
    STY TMP                     ; Store Y

    CMP $00,X                   ; checks CRC values against 4 crc values from ROM
    BNE NEXT       
    INY
    LDA ROMSUMS,Y
    CMP $01,X       
    BNE NEXT
    INY
    LDA ROMSUMS,Y
    CMP $02,X
    BNE NEXT
    INY
    LDA ROMSUMS,Y
    CMP $03,X
    BNE NEXT

    ; matches. We found a matching rom

    LDX CHARPOS                 ; load X with value in POS (p=15d)
    
    INY                         ; contains first char of rom text to print to screen
    TYA
    CLC                         ; clear carry flag
    ADC #9                      ; add with carry. 9 is the length of string
    STA TMP     
    TYA
        
ULOOP
	LDA ROMSUMS,Y
    STA SCBASE+$100,X           ; Add $100 to display on bottom half of screen
    INX
    INY
    CPY TMP		                ; Compare memory and index Y
    BNE ULOOP
    BEQ OUT

NEXT	
    LDX TMP+1
    LDA TMP
    CLC
    ADC #13
    TAY
   
    BNE ILOOP

OUT	
    RTS
.)


;=======================================================================================================
; Test ROM
;=======================================================================================================
ROMST
.(
    BNE FAIL
    LDA #5                      ; green
    .BYT $2C                    ; make next instruction BIT $02A9
FAIL
	LDA #2                      ; red
    LDY #0

FLOOP	
    STA (COLPOS),Y
    INY
    CPY #9
    BNE FLOOP

    SEC         
    SBC #5     
    RTS
.)

;=======================================================================================================
; Calculate CRC32 of memory block
; ADDR+1 - start page
; EADDR+1 - end page
; .X - index to CRC32
; Returns CRC32 in $00,X
;=======================================================================================================
CRC32	
    LDY #$FF                    ; Initialise CRC
    STY $00,X
    STY $01,X
    STY $02,X
    STY $03,X
    LDA ADDR+1
    STA PTR+1
    INY
    STY PTR

CRLOOP	
    LDA (PTR),Y
    JSR CRC32U
    INY
    BNE CRLOOP

    INC PTR+1
    LDA PTR+1
    CMP EADDR+1
    BNE CRLOOP

    LDA $00,X                   ; Invert result
    EOR #$FF
    STA $00,X
    LDA $01,X
    EOR #$FF
    STA $01,X
    LDA $02,X
    EOR #$FF
    STA $02,X
    LDA $03,X
    EOR #$FF
    STA $03,X
    RTS

    ; Update CRC32 with value

    ; .A - 8 bit value
    ; .X - index to CRC32

CRC32U	
    STY TMP
    EOR $00,X
    TAY
    LDA $01,X
    EOR CRCT0,Y
    STA $00,X
    LDA $02,X
    EOR CRCT1,Y
    STA $01,X
    LDA $03,X
    EOR CRCT2,Y
    STA $02,X
    LDA CRCT3,Y
    STA $03,X
    LDY TMP
    RTS


; ROM banks - page (MSB), b0 = 1 = 4KB

ROMPAG	.BYT $81,$c0,$e0,0

; ROM banks - screen LSB

ROMOFF	.BYT $3F,$29,$55,0      ; rom offset for displaying. character, basic, kernal

ONE	.BYT 1

;=======================================================================================================
; CRC32 of known ROMs
;=======================================================================================================
ROMSUMS	
    .BYT $4b,$8a,$fd,$fc,"901460-02"                                    ; VIC-1001 char
    .BYT $a6,$32,$e0,$83,"901460-03"                                    ; standard char
    .BYT $c1,$43,$4c,$db,"901486-01"                                    ; BASIC
    .BYT $d7,$00,$69,$33,"901486-02"                                    ; VIC-1001 KERNAL
    .BYT $74,$c1,$e7,$e5,"901486-06"                                    ; NTSC KERNAL
    .BYT $b4,$7c,$e0,$4b,"901486-07"                                    ; PAL KERNAL
    .BYT $10,$78,$5e,$70,'J','i'-$60,'f'-$60,'f'-$60,'y'-$60,"PAL "     ; JiffyDOS PAL KERNAL
    .BYT $7f,$75,$3a,$68,'J','i'-$60,'f'-$60,'f'-$60,'y'-$60,"NTSC"     ; JiffyDOS NTSC KERNAL
    .BYT 0

    ; CRC tables

CRCT0	
    .BYT $00,$96,$2C,$BA,$19,$8F,$35,$A3,$32,$A4,$1E,$88,$2B,$BD,$07,$91
    .BYT $64,$F2,$48,$DE,$7D,$EB,$51,$C7,$56,$C0,$7A,$EC,$4F,$D9,$63,$F5
    .BYT $C8,$5E,$E4,$72,$D1,$47,$FD,$6B,$FA,$6C,$D6,$40,$E3,$75,$CF,$59
    .BYT $AC,$3A,$80,$16,$B5,$23,$99,$0F,$9E,$08,$B2,$24,$87,$11,$AB,$3D
    .BYT $90,$06,$BC,$2A,$89,$1F,$A5,$33,$A2,$34,$8E,$18,$BB,$2D,$97,$01
    .BYT $F4,$62,$D8,$4E,$ED,$7B,$C1,$57,$C6,$50,$EA,$7C,$DF,$49,$F3,$65
    .BYT $58,$CE,$74,$E2,$41,$D7,$6D,$FB,$6A,$FC,$46,$D0,$73,$E5,$5F,$C9
    .BYT $3C,$AA,$10,$86,$25,$B3,$09,$9F,$0E,$98,$22,$B4,$17,$81,$3B,$AD
    .BYT $20,$B6,$0C,$9A,$39,$AF,$15,$83,$12,$84,$3E,$A8,$0B,$9D,$27,$B1
    .BYT $44,$D2,$68,$FE,$5D,$CB,$71,$E7,$76,$E0,$5A,$CC,$6F,$F9,$43,$D5
    .BYT $E8,$7E,$C4,$52,$F1,$67,$DD,$4B,$DA,$4C,$F6,$60,$C3,$55,$EF,$79
    .BYT $8C,$1A,$A0,$36,$95,$03,$B9,$2F,$BE,$28,$92,$04,$A7,$31,$8B,$1D
    .BYT $B0,$26,$9C,$0A,$A9,$3F,$85,$13,$82,$14,$AE,$38,$9B,$0D,$B7,$21
    .BYT $D4,$42,$F8,$6E,$CD,$5B,$E1,$77,$E6,$70,$CA,$5C,$FF,$69,$D3,$45
    .BYT $78,$EE,$54,$C2,$61,$F7,$4D,$DB,$4A,$DC,$66,$F0,$53,$C5,$7F,$E9
    .BYT $1C,$8A,$30,$A6,$05,$93,$29,$BF,$2E,$B8,$02,$94,$37,$A1,$1B,$8D

CRCT1	
    .BYT $00,$30,$61,$51,$C4,$F4,$A5,$95,$88,$B8,$E9,$D9,$4C,$7C,$2D,$1D
    .BYT $10,$20,$71,$41,$D4,$E4,$B5,$85,$98,$A8,$F9,$C9,$5C,$6C,$3D,$0D
    .BYT $20,$10,$41,$71,$E4,$D4,$85,$B5,$A8,$98,$C9,$F9,$6C,$5C,$0D,$3D
    .BYT $30,$00,$51,$61,$F4,$C4,$95,$A5,$B8,$88,$D9,$E9,$7C,$4C,$1D,$2D
    .BYT $41,$71,$20,$10,$85,$B5,$E4,$D4,$C9,$F9,$A8,$98,$0D,$3D,$6C,$5C
    .BYT $51,$61,$30,$00,$95,$A5,$F4,$C4,$D9,$E9,$B8,$88,$1D,$2D,$7C,$4C
    .BYT $61,$51,$00,$30,$A5,$95,$C4,$F4,$E9,$D9,$88,$B8,$2D,$1D,$4C,$7C
    .BYT $71,$41,$10,$20,$B5,$85,$D4,$E4,$F9,$C9,$98,$A8,$3D,$0D,$5C,$6C
    .BYT $83,$B3,$E2,$D2,$47,$77,$26,$16,$0B,$3B,$6A,$5A,$CF,$FF,$AE,$9E
    .BYT $93,$A3,$F2,$C2,$57,$67,$36,$06,$1B,$2B,$7A,$4A,$DF,$EF,$BE,$8E
    .BYT $A3,$93,$C2,$F2,$67,$57,$06,$36,$2B,$1B,$4A,$7A,$EF,$DF,$8E,$BE
    .BYT $B3,$83,$D2,$E2,$77,$47,$16,$26,$3B,$0B,$5A,$6A,$FF,$CF,$9E,$AE
    .BYT $C2,$F2,$A3,$93,$06,$36,$67,$57,$4A,$7A,$2B,$1B,$8E,$BE,$EF,$DF
    .BYT $D2,$E2,$B3,$83,$16,$26,$77,$47,$5A,$6A,$3B,$0B,$9E,$AE,$FF,$CF
    .BYT $E2,$D2,$83,$B3,$26,$16,$47,$77,$6A,$5A,$0B,$3B,$AE,$9E,$CF,$FF
    .BYT $F2,$C2,$93,$A3,$36,$06,$57,$67,$7A,$4A,$1B,$2B,$BE,$8E,$DF,$EF

CRCT2	
    .BYT $00,$07,$0E,$09,$6D,$6A,$63,$64,$DB,$DC,$D5,$D2,$B6,$B1,$B8,$BF
    .BYT $B7,$B0,$B9,$BE,$DA,$DD,$D4,$D3,$6C,$6B,$62,$65,$01,$06,$0F,$08
    .BYT $6E,$69,$60,$67,$03,$04,$0D,$0A,$B5,$B2,$BB,$BC,$D8,$DF,$D6,$D1
    .BYT $D9,$DE,$D7,$D0,$B4,$B3,$BA,$BD,$02,$05,$0C,$0B,$6F,$68,$61,$66
    .BYT $DC,$DB,$D2,$D5,$B1,$B6,$BF,$B8,$07,$00,$09,$0E,$6A,$6D,$64,$63
    .BYT $6B,$6C,$65,$62,$06,$01,$08,$0F,$B0,$B7,$BE,$B9,$DD,$DA,$D3,$D4
    .BYT $B2,$B5,$BC,$BB,$DF,$D8,$D1,$D6,$69,$6E,$67,$60,$04,$03,$0A,$0D
    .BYT $05,$02,$0B,$0C,$68,$6F,$66,$61,$DE,$D9,$D0,$D7,$B3,$B4,$BD,$BA
    .BYT $B8,$BF,$B6,$B1,$D5,$D2,$DB,$DC,$63,$64,$6D,$6A,$0E,$09,$00,$07
    .BYT $0F,$08,$01,$06,$62,$65,$6C,$6B,$D4,$D3,$DA,$DD,$B9,$BE,$B7,$B0
    .BYT $D6,$D1,$D8,$DF,$BB,$BC,$B5,$B2,$0D,$0A,$03,$04,$60,$67,$6E,$69
    .BYT $61,$66,$6F,$68,$0C,$0B,$02,$05,$BA,$BD,$B4,$B3,$D7,$D0,$D9,$DE
    .BYT $64,$63,$6A,$6D,$09,$0E,$07,$00,$BF,$B8,$B1,$B6,$D2,$D5,$DC,$DB
    .BYT $D3,$D4,$DD,$DA,$BE,$B9,$B0,$B7,$08,$0F,$06,$01,$65,$62,$6B,$6C
    .BYT $0A,$0D,$04,$03,$67,$60,$69,$6E,$D1,$D6,$DF,$D8,$BC,$BB,$B2,$B5
    .BYT $BD,$BA,$B3,$B4,$D0,$D7,$DE,$D9,$66,$61,$68,$6F,$0B,$0C,$05,$02

CRCT3	
    .BYT $00,$77,$EE,$99,$07,$70,$E9,$9E,$0E,$79,$E0,$97,$09,$7E,$E7,$90
    .BYT $1D,$6A,$F3,$84,$1A,$6D,$F4,$83,$13,$64,$FD,$8A,$14,$63,$FA,$8D
    .BYT $3B,$4C,$D5,$A2,$3C,$4B,$D2,$A5,$35,$42,$DB,$AC,$32,$45,$DC,$AB
    .BYT $26,$51,$C8,$BF,$21,$56,$CF,$B8,$28,$5F,$C6,$B1,$2F,$58,$C1,$B6
    .BYT $76,$01,$98,$EF,$71,$06,$9F,$E8,$78,$0F,$96,$E1,$7F,$08,$91,$E6
    .BYT $6B,$1C,$85,$F2,$6C,$1B,$82,$F5,$65,$12,$8B,$FC,$62,$15,$8C,$FB
    .BYT $4D,$3A,$A3,$D4,$4A,$3D,$A4,$D3,$43,$34,$AD,$DA,$44,$33,$AA,$DD
    .BYT $50,$27,$BE,$C9,$57,$20,$B9,$CE,$5E,$29,$B0,$C7,$59,$2E,$B7,$C0
    .BYT $ED,$9A,$03,$74,$EA,$9D,$04,$73,$E3,$94,$0D,$7A,$E4,$93,$0A,$7D
    .BYT $F0,$87,$1E,$69,$F7,$80,$19,$6E,$FE,$89,$10,$67,$F9,$8E,$17,$60
    .BYT $D6,$A1,$38,$4F,$D1,$A6,$3F,$48,$D8,$AF,$36,$41,$DF,$A8,$31,$46
    .BYT $CB,$BC,$25,$52,$CC,$BB,$22,$55,$C5,$B2,$2B,$5C,$C2,$B5,$2C,$5B
    .BYT $9B,$EC,$75,$02,$9C,$EB,$72,$05,$95,$E2,$7B,$0C,$92,$E5,$7C,$0B
    .BYT $86,$F1,$68,$1F,$81,$F6,$6F,$18,$88,$FF,$66,$11,$8F,$F8,$61,$16
    .BYT $A0,$D7,$4E,$39,$A7,$D0,$49,$3E,$AE,$D9,$40,$37,$A9,$DE,$47,$30
    .BYT $BD,$CA,$53,$24,$BA,$CD,$54,$23,$B3,$C4,$5D,$2A,$B4,$C3,$5A,$2D

    ; initial values for VIC registers

VICINIT                 		; VIC Memory Map http://www.tinyvga.com/6561
    .byte	$0C         		; interlace and horizontal origin [PAL] Location $9000
    ; .byte	$05					; interlace and horizontal origin [NTSC]
                        		    ; bit	function
                        		    ; ---	--------
                        		    ; 7	interlace / non interlace
                        		    ; 6-0	horizontal origin
    .byte	$26         		; vertical origin [PAL].  Location $9001
    ; .byte	$19					; vertical origin [NTSC]
    .byte	$16         		; video address and columns, $9400 for colour RAM. Location $9002
                        		    ; bit	function
                                    ; ---	--------
                                    ; 7	video memory address va9
                                    ; 6-0	number of columns
    .byte	$2E                 ; screen rows and character height. Location $9003
                                    ; bit	function
                                    ; ---	--------
                                    ; 7	raster line b0
                                    ; 6-1	number of rows
                                    ; 0	character height (8/16 bits)
    .byte	$00                 ; b8-b1 raster line  Location $9004
    .byte	$C0                 ; video memory addresses, RAM $1000, ROM $8000. Location $9005
                                    ; bit	function
                                    ; ---	--------
                                    ; 7-4	video memory address va13-va10
                                    ; 3-0	character memory address va13-va10

                                ; 0000 ROM	$8000	set 1 - we use this
                                ; 0001	"	$8400
                                ; 0010	"	$8800	set 2
                                ; 0011	"	$8C00
                                ; 1100 RAM	$1000
                                ; 1101	"	$1400
                                ; 1110	"	$1800
                                ; 1111	"	$1C00

    .byte	$00                 ; light pen horizontal position. Location $9006
    .byte	$00                 ; light pen vertical position. Location $9007
    .byte	$00                 ; paddle X. Location $9008
    .byte	$00                 ; paddle Y. Location $9009
    .byte	$00                 ; oscillator 1 frequency. Location $900A
    .byte	$00                 ; oscillator 2 frequency. Location $900B
    .byte	$00                 ; oscillator 3 frequency. Location $900C
    .byte	$00                 ; white noise frequency. Location $900D
    .byte	$00                 ; auxiliary colour and volume. Location $900E
                                    ; bit	function
                                    ; ---	--------
                                    ; 7-4	auxiliary colour
                                    ; 3-0	volume
    .byte	$1B                 ; background and border colour. Location $900F
                                    ; bit	function
                                    ; ---	--------
                                    ; 7-4	background colour
                                    ; 3	reverse video
                                    ; 2-0	border colour


CHIPS   
        .BYT      "UE1",0,"UD2",0,"UE1",0,"UE2",0,"UD3 ",0,"UE3 ",0,"UD4 ",0,"UE4 ",0
        .BYT      "UD5 ",0,"UE5 ",0,"UD6 ",0,"UE6 ",0

SCRN
        .BYT "   VIC",$2d,"20 ",$44,$05,$01,$04,$20,$54,$05,$13,$14,"   "
        .BYT "   ",$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,$3d,"   "
        .BYT "                      "
        .BYT "  RAM",$1,"      RAM1      "
        .BYT "  RAM",$2,"      RAM2      " 
        .BYT "  RAM",$3,"      RAM3      " 
        .BYT "  RAM",$4,"      BLK1      "
        .BYT "  LOW       BLK2      " 
        .BYT "  COLOUR    BLK3      " 
        .BYT "                      "
        .BYT "                      " 
       
        .BYT $70,$40,$53,$14,$01,$14,$15,$13,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40
        .BYT $40,$6e,$5d,"                    ",$5d
        .BYT $5d," BASIC    ??????-?? ",$5d
        .BYT $5d," CHAR     ??????-?? ",$5d
        .BYT $5d," KERNAL   ??????-?? ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $5d,"                    ",$5d
        .BYT $6d,$40	;"             │└─"
        .BYT $40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,$40,"V1.0"
        .BYT $7d,";             "
