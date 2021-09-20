MYADDR	equ	$FF40
MYNAME	MACRO
name	fcs	/T2/
	ENDM

	use tx_s16550.a
