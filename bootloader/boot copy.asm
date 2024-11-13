;*************************************************
;
; bootloader.asm
; The file consists the code for the first stage bootloader
;
;*************************************************

org 0x7C00; BIOS sets the CS:IP to 0:0x7C00 and we need our bootloader code to begin from here.

bits 16; We are in 16 bit real mode and we need to indicate that to nasm

start: jmp loader;
;*************************************************;
;	OEM Parameter block
;*************************************************;

TIMES 0Bh-$+start     db 0

bpbBytesPerSector:  	dw 512
bpbSectorsPerCluster: db 1
bpbReservedSectors: 	dw 1
bpbNumberOfFATs: 	    db 2
bpbRootEntries: 	    dw 224
bpbTotalSectors: 	    dw 2880
bpbMedia: 	          db 0xF0
bpbSectorsPerFAT: 	  dw 9
bpbSectorsPerTrack: 	dw 18
bpbHeadsPerCylinder: 	dw 2
bpbHiddenSectors:     dd 0
bpbTotalSectorsBig:   dd 0
bsDriveNumber: 	      db 0
bsUnused: 	          db 0
bsExtBootSignature: 	db 0x29
bsSerialNumber:	      dd 0xa0a1a2a3
bsVolumeLabel: 	      db "MOS FLOPPY "
bsFileSystem: 	      db "FAT12   "

msg	db	`Bootloader Loaded...\n\r`, 0

BOOT_DRIVE: db 0; Make space for storing Boot drive

;*************************************************;
; Print function
;*************************************************;
Print:
  pusha;
  mov AH, 0x0E;
  print_loop:
    lodsb; Loads the next byte from SI into AL
    or AL, AL; Check if null terminator
    jz print_end;
    int 0x10;
    jmp print_loop;
print_end:
  popa;
  ret;

LOAD_ROOT:
     
; compute size of root directory and store in "cx"

  xor     cx, cx
  xor     dx, dx
  mov     ax, 0x0020                      ; 32 byte directory entry
  mul     WORD [bpbRootEntries]           ; total size of directory
  div     WORD [bpbBytesPerSector]        ; sectors used by directory
  xchg    ax, cx
    
; compute location of root directory and store in "ax"

  mov     al, BYTE [bpbNumberOfFATs]       ; number of FATs
  mul     WORD [bpbSectorsPerFAT]          ; sectors used by FATs
  add     ax, WORD [bpbReservedSectors]    ; adjust for bootsector
  mov     WORD [datasector], ax            ; base of root directory
  add     WORD [datasector], cx
    
; read root directory into memory (7C00:0200)

  mov     bx, 0x0200                        ; copy root dir above bootcode
  call    ReadSectors

; browse root directory for binary image
  mov     cx, [bpbRootEntries]        ; the number of entrys. If we reach 0, file doesnt exist
  mov     di, 0x0200        ; Root directory was loaded here
.LOOP:
  push    cx
  mov     cx, 11            ; eleven character name
  mov     si, ImageName     ; compare the 11 bytes with the name of our file
  push    di
rep  cmpsb                     ; test for entry match
  pop     di
  je      LOAD_FAT          ; they match, so begin loading FAT
  pop     cx
  add     di, 32            ; they dont match, so go to next entry (32 bytes)
  loop    .LOOP
  jmp     FAILURE           ; no more entrys left, file doesnt exist :(

LOAD_FAT:
     
; save starting cluster of boot image

  mov     si, msgCRLF
  call    Print
  mov     dx, WORD [di + 0x001A]
  mov     WORD [cluster], dx                  ; file's first cluster
    
; compute size of FAT and store in "cx"

  xor     ax, ax
  mov     al, BYTE [bpbNumberOfFATs]                ; number of FATs
  mul     WORD [bpbSectorsPerFAT]                ; sectors used by FATs
  mov     cx, ax

; compute location of FAT and store in "ax"

  mov     ax, WORD [bpbReservedSectors]          ; adjust for bootsector
    
; read FAT into memory (7C00:0200)

  mov     bx, 0x0200                          ; copy FAT above bootcode
  call    ReadSectors
;*************************************************;
; Bootloader Entry Point
;*************************************************;

loader:
  mov byte[BOOT_DRIVE], dl;
  xor AX, AX; Reset AX to 0, faster than mov.
  mov DS, AX;
  mov ES, AX;

  mov SI, msg;
  call Print;

_Read:
  mov AH, 0x2;
  mov AL, 1; Number of sectors to read
  push AX;
  mov CH, 0; 
  mov CL, 2; Specify which sector to read
  mov DH, 0; Head number
  mov DL, byte[BOOT_DRIVE]; Which drive to read from
  mov BX, 0x7E00;
  int 0x13;
  jc _Read; Read failed, retry
  pop CX;
  xor AL, CL;
  jne _Read; The number of sectors read don't match the requested number of sectors
  jmp 0x0:0x7E00; Jump and execute the newly loaded sectors

  times 510 - ($ - $$) db 0;
  dw 0xAA55; Boot signature

; Second Sector begins here
mov SI, sector_loaded;
  call Print;
  cli; Clear interupt flag, CPU ignores any interupts
  hlt; Set CPU to low power mode and do nothing.
sector_loaded: db `Sector successfully loaded...\r\n`, 0