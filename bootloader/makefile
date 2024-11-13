mntpt = /mnt/HobbyOS
OS_img = HobbyOS.img
purge_files = yes
stage2 = KRNLDR.SYS

all: emu clean

emu: bootcpy cpy 
	qemu-system-x86_64 -enable-kvm -drive format=raw,file=$(OS_img) -serial stdio 

assemble:
	nasm -f bin boot.asm -o boot.bin
	nasm -f bin stage2.asm -o $(stage2)
	
mkdisk: assemble
	qemu-img create -f raw $(OS_img) 1.44M
	# dd if=/dev/zero of=$(OS_img) bs=512 count=2880
	mkfs.fat -F 12 $(OS_img)

bootcpy: mkdisk 
	# dd if=boot.bin of=$(OS_img) bs=512 count=1 conv=notrunc
	dd if=boot.bin of=$(OS_img) bs=1 count=3 conv=notrunc
	dd if=boot.bin of=$(OS_img) bs=1 skip=62 count=450 seek=62 conv=notrunc

cpy: mntdsk bootcpy 
	sudo cp $(stage2) $(mntpt)

mntdsk: 
	sudo mount $(OS_img) $(mntpt)

umnt:
	sudo umount $(mntpt)

clean: umnt
ifeq ($(purge_files), yes)
	rm boot.bin
	rm $(stage2)
	rm HobbyOS.img
endif