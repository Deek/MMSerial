********************************************************************
* s16550_large - 16550 serial driver
*
* $Id$
*
* Ed.    Comments                                       Who YY/MM/DD
* ------------------------------------------------------------------
* 28     Patched by Bob Brose to fix IRQ poll table     BOB ??/??/??
*        duplication bug

         nam   s16550
         ttl   os9 device driver    

* Disassembled 02/04/02 22:41:55 by Disasm v1.6 (C) 1988 by RML

         ifp1
         use   defsfile
	 use	my8250.d
         endc

tylg     set   Drivr+Objct   
atrv     set   ReEnt+rev
rev      set   $01

* OS-9 IT.PAR flow control bit settings
FCMASK	 equ	%00001111	Flow control mask
FCTXSW	 equ	%00001000	Rx data software (XON/XOFF) flow control
FCRXSW	 equ	%00000100	Tx data software (XON/XOFF) flow control
FCCTSRTS equ	%00000010	CTS/RTS hardware flow control
FCDSRDTR equ	%00000001	DSR/DTR hardware flow control
* these names are from xACIA
MdmKill	 equ	%00010000	When DCD drops, "kill" programs using port
ForceDTR equ	%10000000	Don't drop DTR in Term

* Our own TxFloCtl bit settings
* Free bits:	%10000000
TXF.Mask equ	%01110000
TXF.XOFF equ	%01000000	xon/xoff software control
TXF.RTS  equ	%00100000	cts/rts hardware control
TXF.DTR  equ	%00010000	dsr/dtr hybrid control
* Below are reasons we're _not_ sending
TXF.NBRK equ	%00001000	Sending break signal
* For various reasons, these must be identical to the bits in Wrk.Type
TXF.NXON equ	%00000100	currently waiting for XON
TXF.NCTS equ	%00000010	waiting for CTS to go back up
TXF.NDSR equ	%00000001	waiting for DSR to go back up

         mod   eom,name,tylg,atrv,start,size

u0000	rmb	V.SCF	($1d)
*	1	V.PAGE
*	2	V.PORT
*	1	V.LPRC
*	1	V.BUSY
*	1	V.WAKE
*	1	V.TYPE
*	1	V.LINE
*	1	V.PAUS
*	2	V,DEV2
*	1	V.INTR
*	1	V.QUIT
*	1	V.PCHR
*	1	V.ERR
*	1	V.XON
*	1	V.XOFF
*	1	V.KANJI
*	2	V.KBUF
*	2	V.MODADR
*	2	V.PDLHd
*	5	V.RSV

Wrk.Type    rmb   2	upper half parity code, lower half baud rate code
AltBauds    rmb   1
u0020    rmb   1
u0021    rmb   1
u0022    rmb   1
u0023    rmb   2
u0025    rmb   2
u0027    rmb   1
TxFloCtl    rmb   1
u0029    rmb   1
u002A    rmb   2
u002C    rmb   2
u002E    rmb   2
RxBufEnd    rmb   2	Receive Buffer Highest Address
RxBufSta    rmb   2	Receive Buffer Lowest Address
RxBufCnt    rmb   2	Number of chars in receive buffer
RxBufSiz    rmb   2	Size of receive buffer
OutNxt      rmb   2	Address used for next Write
TxBufPos    rmb   2	Transmit Buffer Current Position
TxBufEnd    rmb   2	Transmit Buffer Highest Address
TxBufSta    rmb   2	Transmit Buffer Lowest Address
TxBufCnt    rmb   1	Number of chars in transmit buffer
TxBufSiz    rmb   2
TxNow       rmb   1	if set non-negative, this will be sent before buffer
TxBuffer    rmb   256-.
size     equ   .

         fcb   $03 

name     fcs   /s16550/
         fcb   28

L0015    fcb   $05 

start    lbra  Init
         lbra  Read
         lbra  Write
         lbra  GetStat
         lbra  SetStat
         lbra  Term

* Init
*
* Entry:
*    Y  = address of device descriptor
*    U  = address of device memory area
*
* Exit:
*    CC = carry set on error
*    B  = error code
*
Init     clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         ldd   <V.PORT
         addd  #IStat
         pshs  y		save Y
         leax  >IRQPkt,pcr
         leay  >IRQRtn,pcr
         os9   F$IRQ    	install interrupt service routine
         puls  y		restore Y
         bcc   L004A		branch if ok
         puls  a,cc
         orcc  #Carry		set error flag
         puls  pc,dp		exit with error
L004A    lda   <M$Opt,y		get option count byte
         cmpa  #IT.XTYP-IT.DTP	Is there an xtyp byte in descriptor?
         bls   L005F		no, use one buffer page
         lda   <IT.XTYP,y	else grab driver specific byte
         anda  #$10		check for alt BR table
         sta   <AltBauds
         lda   <IT.XTYP,y
         anda  #$0F		mask out %00001111
         bne   L0061		if not zero, A holds number of 256 byte pages to allocate
L005F    lda   #$01		else allocate 1 256 byte page
L0061    clrb  
         pshs  u
         os9   F$SRqMem 	allocate memory
         tfr   u,x		transfer buffer start to X
         puls  u
         bcc   L0087
* Code here is in case of alloc error -- cleanup and return with error
         stb   1,s		no memory, error on stack
         ldx   #0		remove ourselves from polling table
         ldd   <V.PORT
         addd  #IStat
         pshs  y
         leay  >IRQRtn,pcr
         os9   F$IRQ    
         puls  y
         puls  dp,b,cc
         orcc  #Carry
         rts   

* D = size of allocated buffer in bytes
L0087    stx   <RxBufSta		store buffer start in several pointers
         stx   <u002C
         stx   <u002E
         std   <RxBufSiz
         leax  d,x		point at end of buffer
         stx   <RxBufEnd		store
         tfr   a,b		transfer size hi byte to B
         clra  			clear hi byte
         orb   #$02		OR original hi byte with 2
         andb  #$0E		clear bit 0 (b = %0000XXX0)
         lslb  
         lslb  
         lslb  
         lslb  
         tstb  
         bpl   L00A3
         ldb   #$80
L00A3    pshs  b,a
         ldd   <RxBufSiz
         subd  ,s++
         std   <u002A
         leax  <TxBuffer,u
         stx   <TxBufSta
         stx   <OutNxt
         stx   <TxBufPos
         leax  >size,u
         stx   <TxBufEnd
         ldd   #(size-TxBuffer)
         std   <TxBufSiz
         clr   <RxBufCnt
         clr   <RxBufCnt+1
         clr   <TxBufCnt
         ldd   <IT.PAR,y
         std   <Wrk.Type
         lbsr  L0318
         ldx   <V.PORT
         lda   LStat,x		read our ports to clear any crap out
         lda   UData,x
         lda   LStat,x
         lda   MStat,x
         anda  #RCTS!RDCD!RDSR	anything modem has active may be flow ctl
         sta   <u0020
         clrb  
floChk1  bita  #RCTS
         bne   floChk2
         orb   #CRTS
floChk2  bita  #RDSR
         bne   floChk3
         orb   #CDTR
floChk3  stb   <TxFloCtl		controlling DTR and RTS for flow control
         orcc  #IntMasks
         lda   >L0015,pcr
         bmi   L00F5
         sta   >MPI.Slct
L00F5    lda   >PIA1Base+3	fetch PIA1 CR B
         anda  #%11111100	disable *CART FIRQ
         sta   >PIA1Base+3	save it back
         lda   >PIA1Base+2	read any data out of PIA1
         lda   >D.IRQER 	read the GIME IRQ enable register copy
         ora   #$01		enable GIME *CART IRQ
         sta   >D.IRQER 	save it to the system...
         sta   >IrqEnR		...and the GIME itself.
         puls  pc,dp,b,cc

Write    clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         ldx   <OutNxt		Next spot to write a char
         sta   ,x+		store our char and increment
         cmpx  <TxBufEnd	did we just write the last char of buffer?
         bcs   L011D		no, we're good
         ldx   <TxBufSta	go back to start of buffer
L011D    orcc  #IntMasks	mask interrupts
         cmpx  <TxBufPos	caught up to the read position?
         bne   L0138		nope, still more room
         pshs  x		buffer full, save our ptr...
         lbsr  Sleeper		...and block the calling process.
         puls  x		get our pointer back
         ldu   >D.Proc
         ldb   <P$Signal,u	get pending signal, if any
         beq   L0136		branch if no signal
         cmpb  #S$Intrpt	interrupted?
         bls   L013E		yep, get TF outtahere
L0136    bra   L011D		nope, back to the top
L0138    stx   <OutNxt		update next output position
         inc   <TxBufCnt	increment output buffer count
         bsr   L0140
L013E    puls  pc,dp,b,cc

L0140    lda   #ELSI!EMSI!ERDA!ETEMT	enable all interrupts
         bra   L0146

L0144    lda   #ELSI!EMSI!ERDA		enable all BUT empty Tx (unreachable?)
L0146    ldx   <V.PORT
         sta   IrEn,x
         rts

Read     clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         orcc  #IntMasks	interrupts off
         ldd   <RxBufCnt		rx buffer count?
         beq   L0169		nothing waiting, go punt
         cmpd  #16		is buffer count 16?
         lbne  L018F		no, go receive
         andcc #^IntMasks
         bsr   L01BD
L0163    orcc  #IntMasks
         ldd   <RxBufCnt
         bne   L018F
L0169    lbsr  Sleeper		go to sleep
         ldx   >D.Proc		we're back! Check on the process
         ldb   P$Signal,x	did it get a signal?
         beq   L0178		signal 0 == S$Kill
         cmpb  #S$Intrpt	any signal <= 3, we care about
         bls   L018A
L0178    ldb   P$State,x
         andb  #Condem		process dying?
         bne   L018A		yes, he's a goner...exit with error
         ldb   <V.ERR		any errors?
         bne   L01A6		yep, go return them
         ldb   <V.WAKE
         beq   L0163
         orcc  #IntMasks
         bra   L0169

L018A    puls  dp,a,cc
         orcc  #Carry
         rts   

L018F    subd  #1
         std   <RxBufCnt
         ldx   <u002E
         lda   ,x+
         cmpx  <RxBufEnd
         bne   L019E
         ldx   <RxBufSta
L019E    stx   <u002E
         andcc #^IntMasks
         ldb   <V.ERR
         beq   L01BB		no error
L01A6    stb   <$3A,y		wtf is this doing?
         clr   <V.ERR		clear error accumulator
         puls  dp,a,cc
         bitb  #$20
         beq   L01B6
         ldb   #E$Read
         orcc  #Carry
         rts   

L01B6    ldb   #E$HangUp
         orcc  #Carry
         rts   

L01BB    puls  pc,dp,b,cc

L01BD    pshs  cc
         ldx   <V.PORT
         ldb   <TxFloCtl
         bitb  #TXF.Mask	%01110000	XOFF|RTS|DTR
         beq   L01D9
         bitb  #TXF.RTS	%00100000	RTS
         beq   L01DB
         orcc  #IntMasks
         ldb   <TxFloCtl
         andb  #^TXF.RTS	%11011111	^RTS
         stb   <TxFloCtl
         lda   MCtrl,x
         ora   #CRTS
         sta   MCtrl,x
L01D9    puls  pc,cc	restore ints and return

L01DB    bitb  #TXF.DTR	%00010000	DTR
         beq   L01EF
         orcc  #IntMasks
         ldb   <TxFloCtl
         andb  #^TXF.DTR	%11101111	^DTR
         stb   <TxFloCtl
         lda   MCtrl,x
         ora   #CDTR
         sta   MCtrl,x
         bra   L01D9
L01EF    bitb  #TXF.XOFF	%01000000	XOFF
         beq   L01D9
         ldb   <V.XON
         orcc  #IntMasks
         stb   <TxNow
         lbsr  L0140
         ldb   <TxFloCtl
         andb  #^TXF.XOFF	%10111111	^XOFF
         stb   <TxFloCtl
         bra   L01D9

********************
* GetStat
********************
*
* Entry:
*	A = Function code
*	Y = Address of path descriptor
*	U = Address of device memory area
*	Other regs depend on func code
*
* Exit:
*	CC = carry set on error, clear on none
*	B = error code if any
*	Other regs depend on func code
*
* Extra Info:
*	ANY codes not defined by IOMan or SCF are passed to the device driver.
*
*	The address of the registers at the time F$GetStt was called is in
*	PD.RGS, in the path descriptor (PD.RGS,Y)
*
*	From there, R$(CC|D|A|B|DP|X|Y|U|PC) get you the appropriate reg value.
GetStat  clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         cmpa  #SS.Ready
         bne   L0226
         ldd   <RxBufCnt
         beq   L021E
         tsta  
         beq   L0217
         ldb   #$FF
L0217    ldx   PD.RGS,y
         stb   R$B,x
         lbra  L0316
L021E    puls  b,cc
         orcc  #Carry
         ldb   #$F6
         puls  pc,dp
L0226    cmpa  #SS.ComSt
         bne   L024E
         ldd   <Wrk.Type
         tst   <AltBauds
         beq   L0236
         bitb  #$04
         bne   L0236
         andb  #$F7
L0236    ldx   PD.RGS,y
         std   R$Y,x
         clrb  
         lda   <u0020
         bita  #$80	%10000000 RDCD
         bne   L0243
         orb   #$10	%00010000
L0243    bita  #$20	%00100000 RDSR
         bne   L0249
         orb   #$40
L0249    stb   $02,x
         lbra  L0316
L024E    cmpa  #SS.EOF
         bne   L0256
         clrb  
         lbra  L0316
L0256    cmpa  #$C1
         bne   L026F
         ldx   PD.RGS,y
         ldd   #$00BC
         std   R$X,x
         clra  
         ldb   <TxBufCnt
         std   R$Y,x
         ldb   <TxFloCtl
         andb  #(TXF.NXON!TXF.NCTS!TXF.NDSR)	why aren't we sending?
         stb   $02,x
         lbra  L0316
L026F    cmpa  #$D0
         bne   L02DD
         ldb   <V.ERR
         lbne  L01A6
         orcc  #IntMasks
         ldd   <RxBufEnd
         subd  <u002E
         cmpd  <RxBufCnt
         bcs   L0288
         ldd   <RxBufCnt
         beq   L021E
L0288    andcc #^IntMasks
         ldu   PD.RGS,y
         cmpd  R$Y,u
         bls   L0293
         ldd   R$Y,u
L0293    std   R$Y,u
         beq   L02DB
         pshs  b,a
         pshs  u,y,x
         std   $02,s
         ldd   <u002E
         std   ,s
         ldd   R$X,u
         std   $04,s
         ldx   >$0050
         ldb   $06,x
         lda   >$00D0
         puls  u,y,x
         orcc  #IntMasks
         os9   F$Move   
         ldd   <RxBufCnt
         subd  ,s
         std   <RxBufCnt
         andcc #^IntMasks
         cmpd  #$0010
         bhi   L02CD
         addd  ,s
         cmpd  #$0010
         bls   L02CD
         lbsr  L01BD
L02CD    puls  b,a
         ldx   <u002E
         leax  d,x
         cmpx  <RxBufEnd
         bne   L02D9
         ldx   <RxBufSta
L02D9    stx   <u002E
L02DB    bra   L0316
L02DD    cmpa  #$D2
         bne   L02F5
         ldd   #$0B04
         ldy   $06,y
         std   $01,y
         ldd   #$0077
         std   $06,y
         ldd   #$0001
         std   $08,y
         bra   L0316
L02F5    cmpa  #SS.ScSiz
         bne   L030E
         ldx   $06,y
         ldy   $03,y
         ldy   $04,y
         clra  
         ldb   <$2C,y
         std   $06,x
         ldb   <$2D,y
         std   $08,x
         bra   L0316
L030E    puls  b,cc
         orcc  #Carry
         ldb   #$D0
         puls  pc,dp
L0316    puls  pc,dp,b,cc
L0318    pshs  u
         tfr   b,a
         leau  >L07E3,pcr
         ldx   <V.PORT
         andb  #$0F
         lslb  
         lslb  
         leau  b,u
         lsra  
         lsra  
         lsra  
         lsra  
         lsra  
         eora  #$03
         anda  #$03
         pshs  a,cc
         lda   <Wrk.Type
         lsra  
         lsra  
         anda  #$38
         ora   $01,s
         sta   $01,s
         ora   #$80
         orcc  #IntMasks
         sta   $03,x
         ldd   ,u++
         exg   a,b
         std   ,x
         lda   $01,s
         sta   $03,x
         ldd   ,u
         sta   <u0021
         ora   #$06
         sta   $02,x
         stb   <u0029
         puls  pc,u,a,cc

SetStat  clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         cmpa  #$D1
         lbne  L03F5
         ldu   $06,y
         ldx   V.TYPE,u
         ldd   V.PAUS,u
         pshs  x,b,a
         beq   L03D3
L036F    ldd   <TxBufPos
         cmpd  <TxBufSta
         bne   L037D
         ldd   <TxBufEnd
         subd  #$0001
         bra   L0387
L037D    subd  #$0001
         cmpd  <OutNxt
         bcc   L0387
         ldd   <TxBufEnd
L0387    subd  <OutNxt
         beq   L03D8
         cmpd  ,s
         bls   L0392
         ldd   ,s
L0392    pshs  b,a
         ldx   >$0050
         lda   $06,x
         ldb   >$00D0
         ldu   <OutNxt
         ldx   $04,s
         ldy   ,s
         orcc  #IntMasks
         os9   F$Move   
         ldd   ,s
         ldu   <OutNxt
         leau  d,u
         cmpu  <TxBufEnd
         bcs   L03B5
         ldu   <TxBufSta
L03B5    stu   <OutNxt
         clra  
         ldb   <TxBufCnt
         addd  ,s
         stb   <TxBufCnt
         andcc #^IntMasks
         ldd   ,s
         ldx   $04,s
         leax  d,x
         stx   $04,s
         ldd   $02,s
         subd  ,s++
         std   ,s
         bne   L036F
         lbsr  L0140
L03D3    leas  $04,s
         lbra  L0543
L03D8    orcc  #IntMasks
         lbsr  L0140
         lbsr  Sleeper
         ldx   >$0050
         ldb   <$19,x
         beq   L03EC
         cmpb  #$03
         bls   L03D3
L03EC    ldb   $0C,x
         andb  #$02
         bne   L03D3
         lbra  L036F
L03F5    cmpa  #SS.ComSt
         bne   L0426
         ldy   $06,y
         ldd   $08,y
         tst   <AltBauds
         beq   L0408
         bitb  #$04
         bne   L0408
         orb   #$08
L0408    std   <Wrk.Type
         lbsr  L0318
         clr   <u0022
         tst   <V.QUIT
         bne   L0423
         tst   <V.INTR
         bne   L0423
         tst   <V.PCHR
         bne   L0423
         ldb   <Wrk.Type
         bitb  #$04
         bne   L0423
         inc   <u0022
L0423    lbra  L0543
L0426    cmpa  #SS.HngUp
         bne   L0441
         ldx   <V.PORT
         lda   MCtrl,x		get current control byte
         pshs  x,a		save port, byte
         anda  #^CDTR		drop DTR for...
         sta   MCtrl,x
         ldx   #30		...30 ticks, half a second
         os9   F$Sleep
         puls  x,a		restore modem control
         sta   MCtrl,x
         lbra  L0543
L0441    cmpa  #SS.Break
         bne   L0491
         orcc  #IntMasks
         ldx   <V.PORT
         lda   <TxFloCtl
         ora   #TXF.NBRK	stop transmit, sending break
         sta   <TxFloCtl
         lda   #(ELSI!EMSI!ERDA) disable Transmitter Empty interrupt
         sta   IrEn,x
         clr   <TxBufCnt
         ldd   <TxBufSta
         std   <TxBufPos
         std   <OutNxt
         lda   <u0021
         ora   #$04
         sta   $02,x
         clra  
         sta   ,x
L0464    lda   $05,x
         anda  #$40
         bne   L0476
         andcc #^IntMasks
         ldx   #$0001
         os9   F$Sleep  
         ldx   <V.PORT
         bra   L0464

L0476    lda   LCtrl,x
         ora   #LSBRK		add Send Break bit to line control
         sta   LCtrl,x
         ldx   #30		...30 ticks, half a second
         os9   F$Sleep  
         ldx   <V.PORT
         anda  #^LSBRK		Stop sending Break
         sta   LCtrl,x
         lda   <TxFloCtl
         anda  #^TXF.NBRK	no longer sending break, we can transmit again
         sta   <TxFloCtl
         lbra  L0543
L0491    cmpa  #$C2
         bne   L04A7
         ldb   <TxFloCtl
         andb  #$F8
         stb   <TxFloCtl
         tst   <TxBufCnt
         lbeq  L0543
         lbsr  L0140
         lbra  L0543
L04A7    cmpa  #$1A
         bne   L04C4
         lda   $05,y
         ldy   $06,y
         ldb   $07,y
         orcc  #IntMasks
         ldx   <RxBufCnt
         bne   L04BD
         std   <u0025
         lbra  L0543
L04BD    puls  cc
         os9   F$Send   
         puls  pc,dp,b
L04C4    cmpa  #SS.Relea
         bne   L04D5
         lda   $05,y
         cmpa  <u0025
         bne   L04D2
         clra  
         clrb  
         std   <u0025
L04D2    lbra  L0543
L04D5    cmpa  #SS.CDSig
         bne   L04E4
         lda   $05,y
         ldy   $06,y
         ldb   $07,y
         std   <u0023
         bra   L0543
L04E4    cmpa  #SS.CDRel
         bne   L04F6
         orcc  #IntMasks
         lda   $05,y
         cmpa  <u0023
         bne   L04F4
         clra  
         clrb  
         std   <u0023
L04F4    bra   L0543
L04F6    cmpa  #SS.Close
         lbne  L0511
         orcc  #IntMasks
         lda   $05,y
         ldx   #$0000
         cmpa  <u0025
         bne   L0509
         stx   <u0025
L0509    cmpa  <u0023
         bne   L050F
         stx   <u0023
L050F    bra   L0543
L0511    cmpa  #$C3
         bne   L052B
         orcc  #IntMasks
         ldb   #$0D
         stb   $01,x
         ldd   <TxBufSta
         std   <OutNxt
         std   <TxBufPos
         clr   <TxBufCnt
         ldb   <u0021
         orb   #$04
         stb   $02,x
         bra   L0543
L052B    cmpa  #SS.Open
         bne   L053B
         ldx   <V.PORT
         lda   #$03
         sta   $04,x
         ldb   #$0F
         stb   $01,x
         bra   L0543
L053B    puls  b,cc
         orcc  #Carry
         ldb   #$D0
         puls  pc,dp
L0543    puls  pc,dp,b,cc

Term     clrb  
         pshs  dp,b,cc
         lbsr  GetDP
         orcc  #IntMasks
         clra  
         clrb  
         std   <RxBufCnt
         ldx   <RxBufSta
         stx   <u002C
         stx   <u002E
         pshs  x,b,a
         ldb   $04,s
         tfr   b,cc
         ldx   >$0050
         lda   ,x
         sta   <V.BUSY
         sta   <V.LPRC
L0566    orcc  #IntMasks
         tst   <TxBufCnt
         bne   L0576
         ldx   <V.PORT
         ldb   LStat,x
         eorb  #$20
         andb  #$20
         beq   L0585
L0576    orcc  #IntMasks
         lbsr  Sleeper
         ldd   $02,s
         std   <u002C
         ldd   ,s
         std   <RxBufCnt
         bra   L0566
L0585    leas  $04,s
         clr   $01,x
         clr   $04,x
         andcc #^IntMasks
         ldd   <RxBufSiz
         pshs  u
         ldu   <RxBufSta
         os9   F$SRtMem 
         puls  u
         ldx   #$0000
         ldd   <V.PORT
         addd  #$0002
         pshs  y
         leay  >IRQRtn,pcr
         os9   F$IRQ    
         puls  y
         puls  pc,dp,b,cc

Sleeper  ldd   >D.Proc		current (calling) process
         sta   <V.WAKE		wake it up when the I/O completes
         tfr   d,x
         lda   P$State,x
         ora   #Suspend
         sta   P$State,x
         andcc #^IntMasks	turn the IRQs back on if they're off, and...
         ldx   #$0001
         os9   F$Sleep		...sleep 'til the slice is over.
         rts

* Transfer hi-byte of U to Direct Page
GetDP    pshs  u
         puls  dp
         leas  $01,s
         rts   

*		MStt  TEMT  RxAv  rxav  ????  ????  RxTO
*jtbl    fdb   $0160,$0115,$001b,$01bb,$0004,$0004,$002a
*		0000  0010  0100  0110  1000  1010  1100
jtbl	fdb	mstt-jump	Modem Status Change
	fdb	temt-jump	Transmitter Empty
	fdb	rxav-jump	Receive Data Available
	fdb	lstt-jump	Line Status Change
	fdb	L0601-jump	invalid, ignore & try again
	fdb	L0601-jump	invalid, ignore & try again
	fdb	rxto-jump	Receive Data Timeout

* IRQ Service Routine
IRQRtn   clrb
L05D8    pshs  dp,b,cc
         bsr   GetDP
         clr   <u0027
         ldy   <V.PORT
         ldb   IStat,y
         bitb  #IrPend
         beq   L05F4	0 = pending interrupt
         tfr   a,b
         andb  #IrId
         bne   L05F4	bits are set
         puls  cc	else return error
         orcc  #Carry
         puls  pc,dp
L05F4    leax  >jtbl,pcr
         andb  #IrId
         abx   
         tfr   pc,d
jump     addd  ,x
         tfr   d,pc
L0601    ldb   IStat,y
         bitb  #IrPend
         beq   L05F4
         lda   <V.WAKE
         beq   L0616
         clrb  
         stb   <V.WAKE
         tfr   d,x
         lda   $0C,x
         anda  #$F7
         sta   $0C,x
L0616    puls  pc,dp,b,cc

rxav     ldx   <u002C
         lda   LStat,y
         bmi   L062B
         ldb   <u0029
L0620    bsr   L0651
         decb  
         bne   L0620
         bra   L0629

rxto     ldx   <u002C
L0629    lda   LStat,y
L062B    bita  #OE!PE!FE!BI
         beq   L0634
         lbsr  L07BF
         bra   L0629
L0634    bita  #DR
         beq   L063C
L0638    bsr   L0651
         bra   L0629
L063C    tst   <u0027
         bne   L064D
         ldd   <u0025
         beq   L064D
         stb   <u0027
         os9   F$Send   
         clra  
         clrb  
         std   <u0025
L064D    stx   <u002C
         bra   L0601
L0651    lda   UData,y
         beq   L0679
         tst   <u0022
         bne   L0679
         cmpa  <V.QUIT
         bne   L0662
         lda   #$02
         lbra  L06FD
L0662    cmpa  <V.INTR
         bne   L066B
         lda   #$03
         lbra  L06FD
L066B    cmpa  <V.XON
         beq   L06E3
         cmpa  <V.XOFF
         beq   L06F2
         cmpa  <V.PCHR
         lbeq  L070A
L0679    pshs  b
         sta   ,x+
         cmpx  <RxBufEnd	reached end of buffer?
         bne   L0683		no, keep going
         ldx   <RxBufSta	yes, go back to start
L0683    cmpx  <u002E		reached read pointer?
         bne   L0697		no, keep going
* 6309:  oim #%00000010;<V.ERR
         ldb   #$02		set bit 1 in error accumulator
         orb   <V.ERR
         stb   <V.ERR
         cmpx  <RxBufSta
         bne   L0693
         ldx   <RxBufEnd
L0693    leax  -1,x
         bra   L06A5
L0697    stx   <u002C
         ldd   <RxBufCnt
         addd  #1
         std   <RxBufCnt
         cmpd  <u002A
         beq   L06A7
L06A5    puls  pc,b
L06A7    ldb   <TxFloCtl
         bitb  #TXF.Mask	TX flow control mask
         bne   L06A5
         lda   <Wrk.Type
         bita  #FCCTSRTS	CTS/RTS flow ctl
         beq   L06BF
         orb   #TXF.RTS 	enable RTS control
         stb   <TxFloCtl
         lda   MCtrl,y
         anda  #^CRTS		stop asserting RTS signal
         sta   MCtrl,y
         bra   L06A5
L06BF    bita  #FCDSRDTR	DSR/DTR flow ctl
         beq   L06CF
         orb   #TXF.DTR 	enable DTR for flow control
         stb   <TxFloCtl
         lda   MCtrl,y
         anda  #^CDTR		drop DTR
         sta   MCtrl,y
         bra   L06A5
L06CF    bita  #FCTXSW		XON/XOFF TX flow ctl
         beq   L06A5
         orb   #TXF.XOFF	enable sending XOFF for flow control
         stb   <TxFloCtl
         lda   <V.XOFF		Get the character to use for XOFF
         beq   L06A5		there isn't one, skip it
         sta   <TxNow		put it at the head of the queue
         ldb   #IrPend!IrId
         stb   IrEn,y
         bra   L06A5

L06E3    lda   <TxFloCtl
         anda  #^TXF.XON	No longer waiting for XON
         sta   <TxFloCtl
         tst   <TxBufCnt
         beq   L06F1
         lda   #ELSI!EMSI!ERDA!ETEMT "Normality restored." (enable ETEMT)
         sta   IrEn,y
L06F1    rts   

L06F2    lda   <TxFloCtl
         ora   #TXF.NXON	Start waiting for XON
         sta   <TxFloCtl
         lda   #ELSI!EMSI!ERDA	no ETEMT
         sta   IrEn,y
         rts   

L06FD    pshs  b
         tfr   a,b
         lda   <V.LPRC
         stb   <u0027
         os9   F$Send   
         puls  pc,b
L070A    ldu   <V.DEV2
         beq   L0711
         sta   <V.PAUS,u
L0711    rts   

temt     ldx   <TxBufPos
         lda   <TxNow
         ble   L071E		is TxNow <= 0?
         sta   UData,y		write the byte
         anda  #$80		clear it
         sta   <TxNow
L071E    tst   <TxBufCnt
         beq   L0757
         ldb   <TxFloCtl
         bitb  #TXF.NBRK	sending break?
         bne   L0757		yes, don't send
         andb  #(TXF.NXON!TXF.NCTS!TXF.NDSR)	are we waiting for flow control?
         andb  <Wrk.Type	Do we care?
         bne   L0757		yes, don't send
         ldb   <TxBufPos+1	otherwise, start transmitting
         negb  
         cmpb  #$0F
         bls   L0737
         ldb   #$0F
L0737    cmpb  <TxBufCnt
         bls   L073D
         ldb   <TxBufCnt
L073D    pshs  b
L073F    lda   ,x+
         sta   ,y
         decb  
         bne   L073F
         cmpx  <TxBufEnd
         bcs   L074C
         ldx   <TxBufSta
L074C    stx   <TxBufPos
         ldb   <TxBufCnt
         subb  ,s+
         stb   <TxBufCnt
L0754    lbra  L0601
L0757    lda   #ELSI!EMSI!ERDA not ETEMT
         sta   IrEn,y
         bra   L0754

mstt     lda   MStat,y
         tfr   a,b
         andb  #RDCD!RDSR!RCTS	signals we're interested in
         stb   <u0020		save new mstatus
         ldb   <TxFloCtl
         andb  #^(TXF.NCTS!TXF.NDSR)	the bits we might change
         bita  #RCTS		CTS up?
         bne   L076F		yes, continue
         orb   #TXF.NCTS	no, start waiting for it
L076F    bita  #RDSR		DSR up?
         bne   L0775		yes, continue
         orb   #TXF.NDSR	no, wait for it
L0775    bita  #DDCD		DCD changed?
         beq   L07AF		no, continue
         bita  #RDCD		it changed -- is it up now?
         bne   L0799		branch if it was down, but up now
         lda   <Wrk.Type
         bita  #MdmKill 	DCD dropped, report errors?
         beq   L0791		branch if we don't kill
         ldx   <V.PDLHd 	Get path descriptor link head
         beq   L0791		nil, skip
         lda   #PST.DCD 	path status "DCD lost"
L0789    sta   <PD.PST,x	set the path status
         ldx   <PD.PLP,x	link to the next one
         bne   L0789		if another, loop again

L0791    lda   #$20
         ora   <V.ERR
         sta   <V.ERR
         andb  #^TXF.NXON	clear "XOFF" status
L0799    tst   <u0027
         bne   L07AF
         stb   <TxFloCtl
         ldd   <u0023
         tstb  
         beq   L07B1
         os9   F$Send   
         stb   <u0027
         clra  
         clrb  
         std   <u0023
         bra   L07B1
L07AF    stb   <TxFloCtl
L07B1    lda   #ELSI!EMSI!ERDA!ETEMT
         sta   IrEn,y
         lbra  L0601

lstt     lda   LStat,y
         bsr   L07BF
         lbra  L0601
L07BF    pshs  b
         clrb  
         bita  #$02
         beq   L07C8
         orb   #$04
L07C8    bita  #$04
         beq   L07CE
         orb   #$01
L07CE    bita  #$08
         beq   L07D4
         orb   #$02
L07D4    bita  #$10
         bne   L07DE
         orb   #$08
         orb   <V.ERR
         stb   <V.ERR
L07DE    puls  pc,b

* IRQ Flip/Mask/Priority Bytes
IRQPkt   fcb   $01,$01,10

L07E3    fdb	$4174,$0101,$1800,$0101,$0C00,$4104,$0600,$8108
	 fdb	$0300,$C10E,$0180,$C10E,$00C0,$8108,$0060,$8108
	 fdb	$0030,$8108,$0020,$8108,$0010,$8108,$0008,$8108
	 fdb	$0004,$8108,$0002,$8108,$0001,$8108,$003B,$8108

         emod
eom      equ   *
         end

