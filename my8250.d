********************
* Register offset definitions.
*
LDiv	equ	0	Low byte of divisor Latch (DLAB=1)
HDiv	equ	1	High byte of divisor Latch (DLAB=1)

UData	equ	0	Data register (RW) (DLAB=0)
IrEn	equ	1	Interrupt Enable register (RW) (DLAB=0)
IStat	equ	2	Interrupt status register (R)
FCtrl	equ	2	FIFO control register (W)
LCtrl	equ	3	Line control register (RW)
MCtrl	equ	4	Modem control register (RW)
LStat	equ	5	Line status register (R)
MStat	equ	6	Modem status register (R)
SPR	equ	7	Scratchpad register (RW)

* IrEn Interrupt Enable Register bit positions
ERDA	equ	%00000001	Enable Receive Data Avail Intr.
ETEMT	equ	%00000010	Enable Transmitter Empty Intr.
ELSI	equ	%00000100	Enable Receiver Line Status Intr.
EMSI	equ	%00001000	Enable Modem Status Intr.

* IStat Interrupt Status Register bit positions
IrPend	equ	%00000001	Interrupt pending if 0
IrId	equ	%00001110	Interrupt Identification bits
*	The above three bits can have the following values
IrLStat	equ	%00000110	Receiver Line Status change
IrRcvAv	equ	%00000100	Receive Data Available
IrRcvTO	equ	%00001100	Receive Data Timeout
IrTEMT	equ	%00000010	Transmitter Empty

IrMStat	equ	%00000000	Modem Status change
IrFEnab	equ	%11000000	FIFO Enabled

* FCtrl FIFO Control Register bit positions
FCEn	equ	%00000001	FIFO Enable (1=enable, req'd to set any bits)
FCRR	equ	%00000010	RX FIFO reset (1=clear RX fifo)
FCTR	equ	%00000100	TX FIFO reset (1=clear TX fifo)
FDMA	equ	%00001000	DMA Mode Select
FRT	equ	%11000000	RX trigger levels
*	Values for above FIFO RX Trigger bits
FRT1	equ	%00000000	RX Trigger @ 1 byte
FRT4	equ	%01000000	RX Trigger @ 4 bytes
FRT8	equ	%10000000	RX Trigger @ 8 bytes
FRT14	equ	%11000000	RX Trigger @ 14 bytes

* LCtrl Line Control Register bit positions
LWLS	equ	%00000011	Word Length Select bits
*	Values for above Word Length Select bits
LWLS5	equ	%00000000	5 bit Word Length
LWLS6	equ	%00000001	6 bit Word Length
LWLS7	equ	%00000010	7 bit Word Length
LWLS8	equ	%00000011	8 bit Word Length
LSTBT	equ	%00000100	# of Stop bits (0=1 bit, 1=1.5 or 2 bits)
LPAR	equ	%00111000	Parity Select bits
*	Values for above Parity Select bits
LPARN	equ	%00000000	No Parity
LPARO	equ	%00001000	Odd Parity
LPARE	equ	%00011000	Even Parity
LPARM	equ	%00101000	Mark Parity
LPARS	equ	%00111000	Space Parity

LSBRK	equ	%01000000	Set Break Bit (1=Break level)
DLAB	equ	%10000000	Divisor Latch Access Bit (DLAB)

* MCtrl Modem Control Register bit positions
CDTR	equ	%00000001	Data Terminal Ready bit
CRTS	equ	%00000010	Request to Send bit
COUT1	equ	%00000100	Output 1
COUT2	equ	%00001000	Output 2
*	OUT2 is commonly used in PC Modems to enable interrupts
CLOOP	equ	%00010000	Loopback Mode Bit
CACTS	equ	%00100000	Auto CTS flow control enable
CACTSRTS equ	%00100010	Automatic CTS/RTS

* LStat Line Status Register bit positions
DR	equ	%00000001	Data Ready
OE	equ	%00000010	Overrun Error
PE	equ	%00000100	Parity Error
FE	equ	%00001000	Framing Error
BI	equ	%00010000	Break Interrupt
THRE	equ	%00100000	Transmitter Holding Register Empty
TEMT	equ	%01000000	Transmitter Empty
FDE	equ	%10000000	FIFO data error

OE_B	equ	1		Overrun Error bit position
PE_B	equ	2		Parity Error bit position
FE_B	equ	3		Framing Error bit position
FDE_B	equ	7		FIFO Data Error bit position

* MStat Modem Status Register bit positions
DCTS	equ	%00000001	Delta CTS
DDSR	equ	%00000010	Delta DSR
TERI	equ	%00000100	Trailing Edge Ring Indicator
DDCD	equ	%00001000	Delta Data Carrier Detect (DCD)
RCTS	equ	%00010000	Clear To Send (CTS)
RDSR	equ	%00100000	Data Set Ready (DSR)
RRI	equ	%01000000	Ring Indicator (RI)
RDCD	equ	%10000000	Data Carrier Detect (DCD)
