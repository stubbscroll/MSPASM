        .processor 65816

        *=$2000

        fjols equ 222

        jmp [start]
start   lda #$10
        sta $d020
        lda 23432,x
        lda @1
        lda !1
        lda #<irq
        sta $0314
        lda #>irq
        sta $0315
        stz fjols
        cmp #fjols
        cmp #soppe-irq
        cmp #"A"
        cli
        jmp *

irq:    ldx #%00000000
        sta start*5
        lda 5*($d020/2)
        lda#$00
        lda#2
        lda#76
        rep #%00110000
        lda#2343
        ldx#$1111
        ldy#$6666
        sep#%00110000
        lda#24
        cmp#44
        rti
        lda (23,s),y
        jmp (12312,x)
        lda [$11]
        lda[56],y
        lda $aaaaaa
        sta $566666,x
        bne irq
        bne soppe
        brl soppe
        lda (12,s),y
        sta 44,s
        jmp($1111)

        lda #"a"
soppe   .text "Dette er en test og du er en hest.",13
        .word soppe,soppe-3,irq
