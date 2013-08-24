 ;
 ; Selfer (extended).
 ;
 ; Shikhin Sethi, the author of selfer, has dedicated the work to the public domain
 ; by waiving all of his or her rights to the work worldwide under copyright law, 
 ; including all related and neighboring rights, to the extent allowed by law.
 ;
 ; You can copy, modify, distribute and perform the work, even for commercial purposes, 
 ; all without asking permission.
 ;
 ; https://creativecommons.org/publicdomain/zero/1.0/
 ;

; 16 bits, starting at 0x7C00.
BITS 16
ORG 0x7C00

; DEFINES.
%define SECTOR_SIZE 512

%define READ_SECTOR         0x00
%define WRITE_SECTOR        0x01

%define LEFT_SCANCODE       75
%define RIGHT_SCANCODE      77

%define UP_SCANCODE         72
%define DOWN_SCANCODE       80

%define FREE_SPACE          0x500

%define MEANING_OF_LIFE     0x42
%define NOP_OPCODE          0x90

%define F_OPCODE(no)        0x3A + no

%define LINE_LENGTH         40

 ; Output an entire sector.
 ;     EBP -> contains the base address of the sector to output.
 ;     ESI -> current index.
%macro OUTPUT_SECTOR 0 
    pushad

    ; Set ES for screen output.
    mov ax, 0xB800
    mov es, ax

    ; Memset the entire display memory (till the point we display) to 0x0F300B30, 
    ; such that colors are: white & light cyan.
    xor di, di
    mov cx, 512
    mov eax, 0x0F300B30
    rep stosd

    ; Store BP in BX, and get exact index in AX.
    mov bx, bp
    add bp, si
    xchg bp, ax

    ; At si * 4 (since every si represents two bytes, i.e. two characters, 
    ; and there are two bytes for each character in display memory) get red color.
    shl si, 2
    mov dword [es:si], 0x0C300C30

    ; Display the address.
    ; We start outputting from 0xF06 into display memory, and move down.
    mov si, 0xF06

    ; Two bytes (a word address).
    mov cx, 2

.LoopAddress:
    ; Get the ASCII values and store it.
    call HexToASCII
    mov [es:si], dl
    mov [es:si - 2], dh

    ; Move 4 down (two bytes displayed).
    sub si, 4

    ; Get the next byte in address.
    shr ax, 8
    loop .LoopAddress

    ; Start from index 0, and display the entire sector.
    xor si, si

.LoopIndex:
    ; Get the byte in DL, and call HexToASCII.      
    mov al, [bx]
    call HexToASCII

    ; Store DH & DL in display memory.
    mov [es:si], dh
    mov [es:si + 2], dl

    ; Increment BX, to get to the next byte.
    inc bx

    ; Add 4 to si (for reasons described above), and if we've done 2048,
    ; that is, one sector, then we're done.
    add si, 4

    ; If below 2048, then loop.
    cmp si, 2048
    jb .LoopIndex

    popad
%endmacro

CPU 386

 ; Main entry point where BIOS leaves us.
 ;     DL    -> Expects the drive number to be present in dl.
 ;     CS:IP -> Expects CS:IP to point to the linear address 0x7C00.
Main:
    jmp 0x0000:.FlushCS                    ; Some BIOS' may load us at 0x0000:0x7C00, while others at 0x07C0:0x0000. Let's just make this uniform.

; If the BIOS doesn't support int 0x13 extensions, display '$'.
.ErrorInt13Ext:
    mov al, '$'

; General error function.
.Error:
    ; Print whatever is in AL via the BIOS function.
    xor bx, bx
    mov ah, 0x0E
    int 0x10

; Halt!
    jmp $

; Flush segments.
.FlushCS:
    ; Check for int 0x13 extensions.
    mov ah, 0x41
    mov bx, 0x55AA
    int 0x13

    ; Error if carry set.
    jc .ErrorInt13Ext

    ; Set up segments.
    xor ax, ax

    ; Stack.
    mov ss, ax
    mov sp, Main

    ; Clear direction flag.
    cld

    ; Get LBA into ECX.
    mov ecx, [si + 8]

    ; DS segment.
    mov ds, ax

    ; Store the boot drive number.
    mov [FREE_SPACE], dl

    ; Store the LBA.
    mov [FREE_SPACE + 3], ecx

    ; Set to mode 0x03, or 80x25 text mode.
    ; AH should be zero as used above.
    mov al, 0x03
    int 0x10

    ; Hide the hardware cursor.               
    mov ch, 0x26
    mov ah, 0x1
    int 0x10

    ; Index 0.
    xor si, si

    ; Read everything from 0x7C00 to 0x10000.

    ; Start from the end, i.e. 0x10000 - 0x200, and
    ; LBA 0x41.
    mov bp, 0x10000 - 0x200
    xor di, di
    mov bx, MEANING_OF_LIFE - 1

.NextSector:
    call RWSector
    sub bp, SECTOR_SIZE

    ; Read till all not read.
    dec bx
    jnz .NextSector

    ; If there is a NOP, we'd need the address to jump to in BX.
    mov bx, 0x7E00

    ; If the value at 0x7E00 is a NOP, then simply jump to it.
    cmp byte [0x7E00], NOP_OPCODE
    je EventLoop.CallCode

; Events.
EventLoop:
    ; Display the current sector.
    OUTPUT_SECTOR

    xor ah, ah
    ; Get input.
    int 0x16

    ; BX points to the exact index.
    mov bx, bp
    add bx, si

; Left key (previous byte)
.Left:
    cmp ah, LEFT_SCANCODE
    jne .Right

    ; Go to previous byte.
    dec si
    jmp .Next

; Right key (next byte)
.Right:
    cmp ah, RIGHT_SCANCODE
    jne .NextLine

    ; Go the next byte.
    inc si
    jmp .Next

; Go to the next line.
.NextLine:
    cmp ah, DOWN_SCANCODE
    jne .PreviousLine

    add si, LINE_LENGTH
    jmp .Next

; Go to the previous line.
.PreviousLine:
    cmp ah, UP_SCANCODE
    jne .Retain

    sub si, LINE_LENGTH
    jmp .Next

; Retain.
.Retain:
    cmp al, 'K'
    jne .Move

    ; Save the current byte pointing too.
    mov [FREE_SPACE + 1], bx

    jmp .Next

; Sort-of like memmove.
.Move:
    cmp al, 'M' 
    jne .Paste

    jmp .CommonPaste

; Paste.
.Paste:
    cmp al, 'P'
    jne .Run

.CommonPaste:
    ; Get the address in DI.
    mov di, [FREE_SPACE + 1]

    ; Get the byte into CL, and then into current index.
    mov cl, [di]
    mov [bx], cl

    ; If it was memmove, then we decrement pointer.
    cmp al, 'M'
    je .DecrementPointer

; Or increment pointer, for paste.
.IncrementPointer:
    inc si
    inc word [FREE_SPACE + 1]

    jmp .Next

.DecrementPointer:
    ; Move one point below, and decrement the byte pointing to at.
    dec si
    dec word [FREE_SPACE + 1]

    jmp .Next

; Run from the current cell.
.Run:
    cmp al, 'R'
    jne .Save

.CallCode:
    ; Save everything important.
    push bp
    push si
    push ds
    push es

    call bx

.RetRun:
    pop es
    pop ds
    pop si
    pop bp

    jmp .Next

; Save the current sector.
.Save:
    cmp al, 'S'
    jne .Write

    ; Write.
    xor di, di
    inc di

    ; Get the LBA into BX.
    shr bx, 9
    sub bx, 0x3E
    call RWSector

    jmp .Next

; Write's all the sectors.
.Write:
    cmp al, 'W'
    jne .NextSector

    push bp

    ; Write everything from 0x7C00 to 0x10000.

    ; Again, we do it in reverse order here.
    mov bp, 0x10000 - 0x200
    xor di, di
    inc di
    mov bx, MEANING_OF_LIFE - 1

.LoopSector:
    call RWSector
    sub bp, SECTOR_SIZE

    ; Loop till all sectors not done.
    dec bx
    jnz .LoopSector

    pop bp
    jmp .Next 

; Next sector.
.NextSector:
    cmp al, 'X' 
    jne .PreviousSector

    ; Are we going beyond our region?
    cmp bp, 0x10000 - SECTOR_SIZE
    je .Next

    ; If not, move one sector above.
    add bp, SECTOR_SIZE
    jmp .Next

; Previous sector.
.PreviousSector:
    cmp al, 'Z' 
    jne .Input
    nop
    nop

    ; Are we going beyond our region?
    cmp bp, 0x7C00
    je .Next

    ; If not, move one sector below.
    sub bp, SECTOR_SIZE

.Next:
    ; Mask of SI.
    and si, 0x1FF

    jmp EventLoop

.Input:
    ; Get another keystroke.
    shl eax, 16
    int 0x16

    ; Arrange both into AX.
    xchg ah, al
    shr eax, 8

    ; If second key was ESC, then throw away input.
    cmp al, 0x1B
    je .Next

    mov cx, 2

.GetHexLoop:
    ; Get '0' to '9'.
    sub al, 48

    ; If larger, subtract 7 to get hex.
    cmp al, 9
    jbe .NextChar

    sub al, 7

.NextChar:
    ; Do next character, now.
    xchg al, ah
    loop .GetHexLoop

; Display whatever was outputted.
.Display:
    shl al, 4
    shr ax, 4

    mov [bx], al

    ; Go to next byte.
    inc si

    jmp .Next

 ; Read/write a sector.
 ;     EBX -> logical block address.
 ;     EBP -> where to read/write to/from.
 ;     EDI -> 0x00 for read; 0x01 for write.
RWSector:
    pushad

    ; DS:SI now points to the buffer.
    mov si, FREE_SPACE + 7

    xor ecx, ecx

    ; Zero it up.
    mov [si + 4], ecx
    mov [si + 12], ecx

    ; 0x0001 -> one block.
    ; 0x00   -> reserved.
    ; 0x10   -> size of packet.
    mov dword [si], 0x00010010

    add ebx, [si - 4]
    
    ; Get the LBA into EBX.
    mov [si + 4], bp
    mov [si + 8], ebx

    ; 0x42 for read; 0x43 for write.
    shl di, 8
    mov ah, 0x42
    add ax, di

    ; Boot drive number into DL.
    mov dl, [FREE_SPACE]

    int 0x13
    jc .Error

.Return:
    popad 
    ret

.Error:
    ; Get in the character.
    mov al, '@'
    jmp Main.Error

 ; Converts a byte to a ASCII hexadecimal value.
 ;     AL -> the byte to convert.
 ;
 ; Returns:
 ;     DX -> the output.
HexToASCII:
    movzx dx, al
    shl dx, 4
    shr dl, 4

    mov al, 2

.Loop:
    cmp dl, 9
    jg .Char

    add dl, 48
    jmp .Next

.Char:
    add dl, 55

.Next:
    xchg dh, dl
    dec al
    jnz .Loop

    ret

; Padding.
times 510 - ($ - $$) db 0

BIOSSignature:
    dw 0xAA55