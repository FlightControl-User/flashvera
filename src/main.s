.import spi_get_jedec
.import spi_deselect
.import spi_buffer
.import spi_get_uniq
.import spi_block_erase
.import spi_write_page_begin
.import spi_write
.import spi_wait_non_busy
.import spi_read
.import spi_read_flash

LASTBANK = 33

.segment "LOADADDR"
    .word $0801
.segment "BASICSTUB"
    .word start-2
    .byte $00,$00,$9e
    .byte "2061"
    .byte $00,$00,$00
.segment "STARTUP"
start:
    jmp main
.segment "BSS"
buffer:
    .res 64
bitstream_pages:
    .res 2
programmed_pages:
    .res 2

.segment "ZEROPAGE"
ptr1:
    .res 2
flashaddr:
    .res 3
.segment "CODE"

.include "x16.inc"

main:
    ; Set ISO mode
    lda #$0f
    jsr X16::Kernal::CHROUT

    jsr load_bitstream_from_disk
    bcs fail

    jsr verify_bitstream_size
    bcs fail

    jsr verify_bitstream
    bcs fail

    jsr close_j1
    bcs fail

    jsr check_flash_meta
    bcs fail

    jsr confirm_flash
    bcs fail

    jsr erase_flash
    bcs fail

    jsr program_flash
    bcs fail

    jsr verify_flash
    bcs fail

    jsr open_j1
    bcs fail

    jsr printstring
    .byte $0D,"DONE.",$0D,0

fail:
    jsr spi_deselect
    rts

.proc load_bitstream_from_disk
    jsr printstring
    .byte $0D,"LOADING VERA.BIN INTO RAM",$0D,0

    lda #1
    sta X16::Reg::RAMBank
    
    lda #bitstream_filename_len
    ldx #<bitstream_filename
    ldy #>bitstream_filename
    jsr X16::Kernal::SETNAM

    lda #2
    ldx #8
    ldy #2
    jsr X16::Kernal::SETLFS

    ldx #0
    ldy #$A0
    lda #0
    jsr X16::Kernal::LOAD
    bcc loadok

    jsr printstring
    .byte $0D,"WASN'T ABLE TO LOAD VERA.BIN FROM DISK",$0D,0
    jsr waitforkey
    sec
    rts
loadok:
    stz ptr1
    sty ptr1+1
    txa
    tay
    lda #$FF
page: ; fill the rest of the page in memory after load with $FF
    sta (ptr1),y
    iny
    bne page

    ; count the number of pages in the bitstream
    stz bitstream_pages+1

    lda X16::Reg::RAMBank
    dec

    ; each used bank is 32 pages
.repeat 5
    asl
    rol bitstream_pages+1
.endrepeat
    sta bitstream_pages

    lda ptr1+1
    sec
    sbc #$A0 ; subtract the RAM window
    clc
    adc #1 ; the last bank used counts
    
    adc bitstream_pages
    sta bitstream_pages
    lda bitstream_pages+1
    adc #0
    sta bitstream_pages+1

    jsr printstring
    .byte "BITSTREAM IS $",0

    lda bitstream_pages+1
    jsr print_hex

    lda bitstream_pages
    jsr print_hex

    jsr printstring
    .byte " PAGES LONG",$0D,0

    clc
    rts
bitstream_filename:
    .byte "VERA.BIN"
bitstream_filename_len = *-bitstream_filename
.endproc

.proc verify_bitstream_size
    lda bitstream_pages+1
    cmp #$02
    bcs success

    cmp #$01
    bcc fail

    lda bitstream_pages
    cmp #$97
    bcc fail
success:
    jsr printstring
    .byte $0D,"BITSTREAM SIZE OK",$0D,0
    clc
    rts
fail:
    jsr printstring
    .byte $0D,"BITSTREAM SIZE NOT OK (<$197 pages) ",$0D,0
    jsr waitforkey
    sec
    rts
.endproc

.proc verify_bitstream
    lda #1
    sta X16::Reg::RAMBank

    stz ptr1
    lda #$A0
    sta ptr1+1

start:
    lda (ptr1)
    cmp #$ff
    bne check_initial_preamble
    ldy #1
    lda (ptr1),y
    cmp #$00
    beq :+
    jmp fail
:
    lda ptr1
    clc
    adc #2
    sta ptr1
    lda ptr1+1
    adc #0
    sta ptr1+1
strings_find_preamble:
    lda (ptr1)
    cmp #$7e
    bne string1
    ldy #1
    lda (ptr1),y
    cmp #$aa
    bne string1
    ldy #2
    lda (ptr1),y
    cmp #$99
    bne string1
    ldy #3
    lda (ptr1),y
    cmp #$7e
    bne string1
    bra success
string1:
    lda (ptr1)
    beq newline
    cmp #$20
    bcc advance
    cmp #$7F
    bcs advance
    jsr X16::Kernal::CHROUT
advance:
    inc ptr1
    bne :+
    inc ptr1+1
    lda ptr1+1
    cmp #$C0
    bcs fail
:
    bra strings_find_preamble
newline:
    lda #$0d
    jsr X16::Kernal::CHROUT
    bra advance
check_initial_preamble:
    lda (ptr1)
    cmp #$7e
    bne fail
    ldy #1
    lda (ptr1),y
    cmp #$aa
    bne fail
    ldy #2
    lda (ptr1),y
    cmp #$99
    bne fail
    ldy #3
    lda (ptr1),y
    cmp #$7e
    bne fail
success:
    jsr printstring
    .byte $0D,"BITSTREAM SIGNATURE FOUND",$0D,0
    jsr waitforkey
    clc
    rts
fail:
    jsr printstring
    .byte $0D,"NO BITSTREAM SIGNATURE FOUND",$0D,0
    jsr waitforkey
    sec
    rts
.endproc

.proc printstring
    pla
    sta ptr1
    pla
    sta ptr1+1

    ldy #1
loop:
    lda (ptr1),y
    beq end
    jsr X16::Kernal::CHROUT
    iny
    bra loop
end:
    tya
    clc
    adc ptr1
    sta ptr1
    lda ptr1+1
    adc #0
    pha
    lda ptr1
    pha

    rts
.endproc

.proc waitforkey
    jsr printstring
    .byte "PRESS ANY KEY",$0D,0
:
    wai
    jsr X16::Kernal::STOP
    beq stopped
    jsr X16::Kernal::GETIN
    beq :-

    clc
    rts
stopped:
    sec
    rts
.endproc


.proc print_hex
    jsr byte_to_hex
    jsr X16::Kernal::CHROUT
    txa
    jsr X16::Kernal::CHROUT
    rts
.endproc

.proc byte_to_hex ; converts a number to two ASCII/PETSCII hex digits: input A = number to convert, output A = most sig nybble, X = least sig nybble, affects A,X
    pha

    and #$0f
    tax
    pla
    lsr
    lsr
    lsr
    lsr
    pha
    txa
    jsr xf_hexify
    tax
    pla
xf_hexify:
    cmp #10
    bcc @nothex
    adc #$66
@nothex:
    eor #%00110000
    rts
.endproc

.proc erase_flash
    jsr printstring
    .byte $0d,"ERASING FLASH IN 64K BLOCKS",$0d,0

    ; ptr1 temporarily contains the number of blocks to erase,
    ; which is the high byte of the number of pages in the bitstream
    ; incremented by 1
    lda bitstream_pages+1
    inc
    sta ptr1

    stz flashaddr
    stz flashaddr+1
    stz flashaddr+2
eraseloop:
    jsr spi_wait_non_busy
    bcs fail
    ldx flashaddr
    ldy flashaddr+1
    lda flashaddr+2
    jsr spi_block_erase

    lda #'.'
    jsr X16::Kernal::CHROUT

    inc flashaddr+2
    lda flashaddr+2
    cmp ptr1
    bcc eraseloop

    clc
    rts
fail:
    jsr printstring
    .byte $0d,"ERASING FAILED, DO NOT RESET OR POWER DOWN THE SYSTEM",$0d,0
    jsr printstring
    .byte "UNTIL YOU SUCCESSFULLY FLASH UNLESS YOU HAVE A WAY TO UNBRICK",$0d,0
    jsr printstring
    .byte "VERA WITH AN EXTERNAL PROGRAMMER!",$0d,0
    jsr open_j1
    sec
    rts
.endproc

.proc program_flash
    jsr printstring
    .byte $0d,"PROGRAMMING FLASH STARTING FROM 0K",$0d,0

    lda #1
    sta X16::Reg::RAMBank

    stz ptr1
    lda #$A0
    sta ptr1+1

    stz flashaddr
    stz flashaddr+1
    stz flashaddr+2

    stz programmed_pages
    stz programmed_pages+1

programloop:
    jsr spi_wait_non_busy
    bcs fail

    ldx flashaddr
    ldy flashaddr+1
    lda flashaddr+2

    jsr spi_write_page_begin

    ldy #0
pploop:
    lda (ptr1),y
    jsr spi_write
    iny
    bne pploop

    inc programmed_pages
    bne :+
    inc programmed_pages+1
:

    inc ptr1+1
    lda ptr1+1
    cmp #$C0
    bcc :+
    lda #$A0
    sta ptr1+1
    inc X16::Reg::RAMBank
    lda #'.'
    jsr X16::Kernal::CHROUT
:
    inc flashaddr+1
    bne :+
    inc flashaddr+2
:
    lda programmed_pages
    cmp bitstream_pages
    bne programloop

    lda programmed_pages+1
    cmp bitstream_pages+1
    bcc programloop

    clc
    rts
fail:
    jsr printstring
    .byte $0d,"PROGRAMMING FAILED, DO NOT RESET OR POWER DOWN THE SYSTEM",$0d,0
    jsr printstring
    .byte "UNTIL YOU SUCCESSFULLY FLASH UNLESS YOU HAVE A WAY TO UNBRICK",$0d,0
    jsr printstring
    .byte "VERA WITH AN EXTERNAL PROGRAMMER!",$0d,0
    jsr open_j1
    sec
    rts
.endproc

.proc check_flash_meta
    jsr printstring
    .byte "READING JEDEC ID",$0d,0

    jsr spi_get_jedec
    stx flashaddr
    sty flashaddr+1
    sta flashaddr+2

    txa
    jsr print_hex
    lda flashaddr+1
    jsr print_hex
    lda flashaddr+2
    jsr print_hex

continue:
    jsr spi_deselect

    jsr printstring
    .byte $0d,"READING SERIAL NUMBER",$0d,0

    jsr spi_get_uniq
    ldy #0
:
    lda spi_buffer, y
    jsr print_hex
    iny
    cpy #8
    bcc :-

    lda flashaddr
    cmp #$EF
    bne fail
    lda flashaddr+1
    cmp #$40
    bne fail
    lda flashaddr+2
    cmp #$15
    bne fail

    clc
    rts
fail:
    jsr printstring
    .byte $0d,"JEDEC ID MISMATCH: EXPECTED EF4015",$0d,0
    jsr open_j1
    sec
    rts
.endproc

.proc verify_flash
    jsr printstring
    .byte $0D,"VERIFYING FLASH",$0D,0
    
    lda #1
    sta X16::Reg::RAMBank
    
    stz ptr1
    lda #$A0
    sta ptr1+1

    stz programmed_pages
    stz programmed_pages+1

    jsr spi_wait_non_busy

    lda #0
    ldx #0
    ldy #0
    jsr spi_read_flash

    ldy #0
pageloop:
    jsr spi_read
    cmp (ptr1),y    
    bne fail

    iny
    bne pageloop

    inc programmed_pages
    bne :+
    inc programmed_pages+1
:

    inc ptr1+1
    lda ptr1+1
    cmp #$C0
    bcc :+
    lda #$A0
    sta ptr1+1
    inc X16::Reg::RAMBank
    lda #'.'
    jsr X16::Kernal::CHROUT
:
    ldy #0
    
    lda programmed_pages
    cmp bitstream_pages
    bne pageloop

    lda programmed_pages+1
    cmp bitstream_pages+1
    bcc pageloop 

    jsr printstring
    .byte $0D,"SUCCESS",$0D,0

    clc
    rts
fail:
    jsr printstring
    .byte $0d,"VERIFY FAILED, DO NOT RESET OR POWER DOWN THE SYSTEM",$0d,0
    jsr printstring
    .byte "UNTIL YOU SUCCESSFULLY FLASH UNLESS YOU HAVE A WAY TO UNBRICK",$0d,0
    jsr printstring
    .byte "VERA WITH AN EXTERNAL PROGRAMMER!",$0d,0

    jsr open_j1
    sec
    rts
.endproc


.proc close_j1
    jsr printstring
    .byte $0D,"CLOSE VERA JP1 JUMPER HEADER THEN ",0
    jmp waitforkey
.endproc

.proc open_j1
    jsr printstring
    .byte $0D,"OPEN VERA JP1 THEN ",0
    jmp waitforkey
.endproc

.proc confirm_flash
    jsr printstring
    .byte $0D,"PRESS CTRL+C TO ABORT, OR TO START FLASHING ",0
    jmp waitforkey
.endproc
