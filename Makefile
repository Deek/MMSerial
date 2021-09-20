AFLAGS= --6309 --list-nofiles --format=os9

all: mmmpi.serial

s16550: s16550_large.asm defsfile
	lwasm $(AFLAGS) -l$@.list -o$@ $<

t2_s16550.dd: t2.asm tx_s16550.a
	lwasm $(AFLAGS) -o$@ $<

t3_s16550.dd: t3.asm tx_s16550.a
	lwasm $(AFLAGS) -o$@ $<

mmmpi.serial: s16550 t2_s16550.dd
	cat $^ > $@

install: mmmpi.serial
	mount /sdc
	os9 copy -r $< /sdc/hdd.dsk,
	os9 attr /sdc/hdd.dsk,mmmpi.serial -e
	umount /sdc
