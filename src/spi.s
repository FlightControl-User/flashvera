.export spi_get_jedec
.export spi_deselect
.export spi_buffer
.export spi_get_uniq
.export spi_read_flash_to_bank
.export spi_read_flash_to_bank_continue
.export spi_sector_erase
.export spi_write_page_begin
.export spi_write
.export spi_block_erase
.export spi_wait_non_busy
.export spi_read
.export spi_read_flash

.segment "ZEROPAGE"
ptr:
    .res 2
.segment "BSS"
spi_buffer:
    .res 256
.segment "CODE"

.include "x16.inc"

; Returns
; .X = Vendor ID
; .Y = Memory Type
; .A = Memory Capacity
.proc spi_get_jedec
    jsr spi_fast

    jsr spi_select
    lda #$9F
    jsr spi_write
    jsr spi_read
    tax
    jsr spi_read
    tay
    jsr spi_read
    rts
.endproc

.proc spi_get_uniq
    jsr spi_select

    lda #$4B
    jsr spi_write
    jsr spi_read
    jsr spi_read
    jsr spi_read
    jsr spi_read

    ldx #0
:
    jsr spi_read
    sta spi_buffer, x
    inx
    cpx #8
    bcc :-

    rts
.endproc

; .X [7:0]
; .Y [15:8]
; .A [23:16]
.proc spi_read_flash
    pha

    jsr spi_select
    lda #$03
    jsr spi_write
    pla
    jsr spi_write
    tya
    jsr spi_write
    txa
    jsr spi_write

    rts
.endproc


; .X [7:0]
; .Y [15:8]
; .A [23:16]
.proc spi_read_flash_to_bank
    jsr spi_read_flash
.endproc

.proc spi_read_flash_to_bank_continue
    stz ptr
    lda #$A0
    sta ptr+1

    ldy #0
pageloop:
    jsr spi_read
    sta (ptr),y
    iny
    bne pageloop

    inc ptr+1
    lda ptr+1
    cmp #$C0
    bcc pageloop
    inc X16::Reg::RAMBank

    rts
.endproc

; .X [7:0]
; .Y [15:8]
; .A [23:16]
.proc spi_sector_erase
    pha

    ; write enable
    jsr spi_select
    lda #$06
    jsr spi_write

    jsr spi_select
    lda #$20
    jsr spi_write

    pla
    jsr spi_write
    tya
    jsr spi_write
    txa
    jsr spi_write

    jsr spi_deselect

    rts
.endproc


; .X [7:0]
; .Y [15:8]
; .A [23:16]
.proc spi_block_erase ; 64k
    pha

    ; write enable
    jsr spi_select
    lda #$06
    jsr spi_write

    jsr spi_select
    lda #$d8
    jsr spi_write

    pla
    jsr spi_write
    tya
    jsr spi_write
    txa
    jsr spi_write

    jsr spi_deselect

    rts
.endproc


; .X [7:0]
; .Y [15:8]
; .A [23:16]
.proc spi_write_page_begin
    pha

    ; write enable
    jsr spi_select
    lda #$06
    jsr spi_write

    jsr spi_select
    lda #$02
    jsr spi_write
    pla
    jsr spi_write
    tya
    jsr spi_write
    txa
    jsr spi_write

    rts
.endproc

.proc spi_wait_non_busy
    ldy #0
top:
    jsr spi_select
    lda #$05
    jsr spi_write

    jsr spi_read
    and #1
    bne wait_restart
    clc
    rts
fail:
    sec
    rts
wait_restart:
    iny
    beq fail
    wai    
    bra top
.endproc

.proc spi_write
	sta Vera::Reg::SPIData
@1:	bit Vera::Reg::SPICtrl
	bmi @1
	rts
.endproc

.proc spi_read
	stz Vera::Reg::SPIData
@1:	bit Vera::Reg::SPICtrl
	bmi @1
    lda Vera::Reg::SPIData
	rts
.endproc

.proc spi_select
    jsr spi_deselect

    lda Vera::Reg::SPICtrl
    ora #$01
    sta Vera::Reg::SPICtrl
	rts
.endproc

.proc spi_deselect
    lda Vera::Reg::SPICtrl
    and #$fe
    sta Vera::Reg::SPICtrl
    jsr spi_read
	rts
.endproc

.proc spi_slow
    lda Vera::Reg::SPICtrl
    ora #%00000010
    sta Vera::Reg::SPICtrl
	rts
.endproc

.proc spi_fast
    lda Vera::Reg::SPICtrl
    and #%11111101
    sta Vera::Reg::SPICtrl
	rts
.endproc
