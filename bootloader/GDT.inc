;*************************************************
;	Gdt.inc
;		-GDT Routines
;
;	OS Development Series
;*************************************************
 
%ifndef __GDT_INC_67343546FDCC56AAB872_INCLUDED__
%define __GDT_INC_67343546FDCC56AAB872_INCLUDED__
 
bits	16
 
;*******************************************
; InstallGDT()
;	- Install our GDT
;*******************************************
 
InstallGDT:
 
	cli				; clear interrupts
	pusha				; save registers
	lgdt 	[toc]			; load GDT into GDTR
	popa				; restore registers
	sti				; enable interrupts
	ret				; All done!
 
;*******************************************
; Global Descriptor Table (GDT)
;*******************************************
 
; Bits 56-63: Bits 24-32 of the base address

; Granularity byte
; Bit 55: Granularity
; 0: None
; 1: Limit gets multiplied by 4K
; Bit 54: Segment type
; 0: 16 bit
; 1: 32 bit
; Bit 53: Reserved-Should be zero
; Bits 52: Reserved for OS use
; Bits 48-51: Bits 16-19 of the segment limit

; Acess Byte 
; Bit 47 Segment is in memory (Used with Virtual Memory)
; Bits 45-46: Descriptor Privilege Level
  ; 0: (Ring 0) Highest
  ; 3: (Ring 3) Lowest
; Bit 44: Descriptor Bit
  ; 0: System Descriptor
  ; 1: Code or Data Descriptor
; Bits 41-43: Descriptor Type
  ; Bit 43: Executable segment 0: Data Segment 1: Code Segment
    ; 0: Data Segment
    ; 1: Code Segment
  ; Bit 42: Expansion direction (Data segments), conforming (Code Segments)
  ; Bit 41: Readable and Writable
    ; 0: Read only (Data Segments); Execute only (Code Segments)
    ; 1: Read and write (Data Segments); Read and Execute (Code Segments)
; Bit 40: Access bit (Used with Virtual Memory)

; Bits 16-39: Bits 0-23 of the Base Address
; Bits 0-15: Bits 0-15 of the Segment Limit

gdt_data: 
	dd 0 				; null descriptor
	dd 0 
 
; gdt code:				; code descriptor
	dw 0FFFFh 			; limit low
	dw 0 				; base low
	db 0 				; base middle
	db 10011010b 			; access
	db 11001111b 			; granularity
	db 0 				; base high
 
; gdt data:				; data descriptor
	dw 0FFFFh 			; limit low (Same as code)
	dw 0 				; base low
	db 0 				; base middle
	db 10010010b 			; access
	db 11001111b 			; granularity
	db 0				; base high
 
end_of_gdt:
toc: 
	dw end_of_gdt - gdt_data - 1 	; limit (Size of GDT)
	dd gdt_data 			; base of GDT
 
 
%endif ;__GDT_INC_67343546FDCC56AAB872_INCLUDED__