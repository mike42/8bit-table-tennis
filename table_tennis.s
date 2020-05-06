;
; table_tennis.s
;
; 6502 assembly for 8bit table tennis
; Based on https://github.com/bbbradsmith/NES-ca65-example

; iNES header
;

.segment "HEADER"

INES_MAPPER = 0 ; 0 = NROM
INES_MIRROR = 1 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG chunk count
.byte $01 ; 8k CHR chunk count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

;
; CHR ROM
;

.segment "TILES"
.incbin "build/background.chr"
.incbin "build/sprite.chr"

;
; vectors placed at top 6 bytes of memory area
;

.segment "VECTORS"
.word nmi
.word reset
.word irq

;
; reset routine
;

.segment "CODE"
reset:
  sei       ; mask interrupts
  lda #0
  sta $2000 ; disable NMI
  sta $2001 ; disable rendering
  sta $4015 ; disable APU sound
  sta $4010 ; disable DMC IRQ
  lda #$40
  sta $4017 ; disable APU IRQ
  cld       ; disable decimal mode
  ldx #$FF
  txs       ; initialize stack
  ; wait for first vblank
  bit $2002
  :
    bit $2002
    bpl :-
  ; clear all RAM to 0
  lda #0
  ldx #0
  :
    sta $0000, X
    sta $0100, X
    sta $0200, X
    sta $0300, X
    sta $0400, X
    sta $0500, X
    sta $0600, X
    sta $0700, X
    inx
    bne :-
  ; place all sprites offscreen at Y=255
  lda #255
  ldx #0
  :
    sta oam, X
    inx
    inx
    inx
    inx
    bne :-
  ; wait for second vblank
  :
    bit $2002
    bpl :-
  ; NES is initialized, ready to begin!
  ; enable the NMI for graphical updates, and jump to our main program
  lda #%10001000
  sta $2000
  jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "CODE"
nmi:
  ; save registers
  pha
  txa
  pha
  tya
  pha
  ; prevent NMI re-entry
  lda nmi_lock
  beq :+
    jmp @nmi_end
  :
  lda #1
  sta nmi_lock
  ; increment frame counter
  inc nmi_count
  ;
  lda nmi_ready
  bne :+ ; nmi_ready == 0 not ready to update PPU
    jmp @ppu_update_end
  :
  cmp #2 ; nmi_ready == 2 turns rendering off
  bne :+
    lda #%00000000
    sta $2001
    ldx #0
    stx nmi_ready
    jmp @ppu_update_end
  :
  ; sprite OAM DMA
  ldx #0
  stx $2003
  lda #>oam
  sta $4014
  ; palettes
  lda #%10001000
  sta $2000 ; set horizontal nametable increment
  lda $2002
  lda #$3F
  sta $2006
  stx $2006 ; set PPU address to $3F00
  ldx #0
  :
    lda palette, X
    sta $2007
    inx
    cpx #32
    bcc :-
  ; nametable update
  ldx #0
  cpx nmt_update_len
  bcs @scroll
  @nmt_update_loop:
    lda nmt_update, X
    sta $2006
    inx
    lda nmt_update, X
    sta $2006
    inx
    lda nmt_update, X
    sta $2007
    inx
    cpx nmt_update_len
    bcc @nmt_update_loop
  lda #0
  sta nmt_update_len
@scroll:
  lda scroll_nmt
  and #%00000011 ; keep only lowest 2 bits to prevent error
  ora #%10001000
  sta $2000
  lda scroll_x
  sta $2005
  lda scroll_y
  sta $2005
  ; enable rendering
  lda #%00011110
  sta $2001
  ; flag PPU update complete
  ldx #0
  stx nmi_ready
@ppu_update_end:
  ; if this engine had music/sound, this would be a good place to play it
  ; unlock re-entry flag
  lda #0
  sta nmi_lock
@nmi_end:
  ; restore registers and return
  pla
  tay
  pla
  tax
  pla
  rti

;
; irq
;

.segment "CODE"
irq:
  rti

;
; drawing utilities
;

.segment "CODE"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
  lda #1
  sta nmi_ready
  :
    lda nmi_ready
    bne :-
  rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
  lda nmi_count
  :
    cmp nmi_count
    beq :-
  rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
  lda #2
  sta nmi_ready
  :
    lda nmi_ready
    bne :-
  rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
  lda $2002 ; reset latch
  tya
  lsr
  lsr
  lsr
  ora #$20 ; high bits of Y + $20
  sta $2006
  tya
  asl
  asl
  asl
  asl
  asl
  sta temp
  txa
  ora temp
  sta $2006 ; low bits of Y + X
  rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
  pha ; temporarily store A on stack
  txa
  pha ; temporarily store X on stack
  ldx nmt_update_len
  tya
  lsr
  lsr
  lsr
  ora #$20 ; high bits of Y + $20
  sta nmt_update, X
  inx
  tya
  asl
  asl
  asl
  asl
  asl
  sta temp
  pla ; recover X value (but put in A)
  ora temp
  sta nmt_update, X
  inx
  pla ; recover A value (tile)
  sta nmt_update, X
  inx
  stx nmt_update_len
  rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
  pha ; temporarily store A on stack
  tya
  pha ; temporarily store Y on stack
  ldy nmt_update_len
  txa
  sta nmt_update, Y
  iny
  pla ; recover Y value (but put in Y)
  sta nmt_update, Y
  iny
  pla ; recover A value (byte)
  sta nmt_update, Y
  iny
  sty nmt_update_len
  rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad_player_1: .res 1
gamepad_player_2: .res 1

.segment "CODE"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   If DPCM samples are played they can conflict with gamepad reading, which
;   may give incorrect results.
gamepad_poll_player1:
  ; strobe the gamepad to latch current button state
  lda #1
  sta $4016
  lda #0
  sta $4016
  ; read 8 bytes from the interface at $4016
  ldx #8
  :
    pha
    lda $4016
    ; combine low two bits and store in carry bit
    and #%00000011
    cmp #%00000001
    pla
    ; rotate carry into gamepad variable
    ror
    dex
    bne :-
  sta gamepad_player_1
  rts

gamepad_poll_player2:
  ; strobe the gamepad to latch current button state
  lda #1
  sta $4017
  lda #0
  sta $4017
  ; read 8 bytes from the interface at $4016
  ldx #8
  :
    pha
    lda $4017
    ; combine low two bits and store in carry bit
    and #%00000011
    cmp #%00000001
    pla
    ; rotate carry into gamepad variable
    ror
    dex
    bne :-
  sta gamepad_player_2
  rts

;
; main
;

.segment "RODATA"
example_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$18,$28,$38 ; bg3 yellow
.byte $0F,$12,$22,$32 ; sp0 marine
.byte $0F,$00,$10,$30 ; sp1 greyscale
.byte $0F,$14,$24,$34 ; sp2 purple
.byte $0F,$1B,$2B,$3B ; sp3 teal


.segment "ZEROPAGE"
player1_x: .res 1
player1_y: .res 1
player1_size: .res 1 ; not yet used
player2_x: .res 1
player2_y: .res 1
player2_size: .res 1 ; not yet used
ball_x: .res 1
ball_y: .res 1
ball_x_speed: .res 1
ball_y_speed: .res 1
ball_state: .res 1
player1_score: .res 1
player2_score: .res 1

BALL_LEFT      = $01
BALL_UP        = $02
BALL_SERVE     = $04

.segment "CODE"
main:
  ; setup 
  ldx #0
  :
    lda example_palette, X
    sta palette, X
    inx
    cpx #32
    bcc :-
  jsr setup_background
  ; place player sprites
  lda #0
  sta player1_x
  lda #248
  sta player2_x
  lda #110
  sta player1_y
  sta player2_y
  lda #4 ; not yet used
  sta player1_size
  sta player2_size
  lda #((256/2) - 4)
  sta ball_x
  lda #((240/2) - 4)
  sta ball_y
  lda #BALL_SERVE
  sta ball_state
  lda #2
  sta ball_x_speed
  lda #0
  sta player1_score
  sta player2_score
  lda #1
  sta ball_y_speed
  ; show the screen
  jsr draw_game_sprites
  jsr ppu_update
  ; main loop
@loop:
  ; read gamepad
  jsr gamepad_poll_player1
  jsr gamepad_poll_player2
  ; respond to gamepad state
  lda gamepad_player_1
  and #PAD_U
  beq :+
    jsr player_1_up
  :
  lda gamepad_player_1
  and #PAD_D
  beq :+
    jsr player_1_down
  :
  lda gamepad_player_1
  and #PAD_A
  beq :+
    jsr player_1_a
  :
  lda gamepad_player_2
  and #PAD_U
  beq :+
    jsr player_2_up
  :
  lda gamepad_player_2
  and #PAD_D
  beq :+
    jsr player_2_down
  :
  lda gamepad_player_2
  and #PAD_A
  beq :+
    jsr player_2_a
  :
  jsr apply_physics
@draw:
  ; draw everything and finish the frame
  jsr draw_game_sprites
  jsr ppu_update
  ; keep doing this forever!
  jmp @loop

apply_physics: ; yeah 'physics'
  clc
  lda ball_state
  and #BALL_SERVE
    beq :+
    rts
  :
  clc
  lda ball_state
  and #BALL_UP
    beq :+
    ; Move up
    lda ball_y
    sec
    sbc ball_y_speed
    sta ball_y
    ; Test if bouncing (up)
    cmp #(4*8)
    bcs :+
      lda #(4*8)
      sta ball_y
      lda #BALL_UP
      eor ball_state
      sta ball_state
    :
  :
  lda ball_state
  and #BALL_UP
    bne :+
    ; Move down
    lda ball_y_speed
    adc ball_y
    sta ball_y
    ; Test if bouncing (bottom)
    cmp #(27*8)
    bcc :+
      lda #(27*8)
      sta ball_y
      lda #BALL_UP
      eor ball_state
      sta ball_state
    :
  :
  clc
  lda ball_state
  and #BALL_LEFT
    ; Move left
    beq :+
    lda ball_x
    sec
    sbc ball_x_speed
    sta ball_x
    ; Test if bouncing (left)
    cmp #(8)
    bcs :+
      ; TODO test if player2 missed the ball
      lda #(8)
      sta ball_x
      lda #BALL_LEFT
      eor ball_state
      sta ball_state
    :
  :
  lda ball_state
  and #BALL_LEFT
    ; Move right
    bne :+
    lda ball_x
    adc ball_x_speed
    sta ball_x
    ; Test if bouncing (right)
    cmp #(30*8)
    bcc :+
      ; TODO test if player1 missed the ball
      lda #(30*8)
      sta ball_x
      lda #BALL_LEFT
      eor ball_state
      sta ball_state
    :
  :
  rts

player_1_up:
  dec player1_y
  dec player1_y
  dec player1_y
  ; max y-value
  lda player1_y
  cmp #(4*8-1)
  bcs :+
    lda #(4*8-1)
    sta player1_y
  :
  rts

player_1_down:
  ; TODO account for paddle height
  inc player1_y
  inc player1_y
  inc player1_y
  lda player1_y
  cmp #(24*8-1)
  bcc :+
    lda #(24*8-1)
    sta player1_y
  :
  rts
  

player_2_up:
  dec player2_y
  dec player2_y
  dec player2_y
  ; max y-value
  lda player2_y
  cmp #(4*8-1)
  bcs :+
    lda #(4*8-1)
    sta player2_y
  :
  rts

player_2_down:
  ; TODO account for paddle height
  inc player2_y
  inc player2_y
  inc player2_y
  lda player2_y
  cmp #(24*8-1)
  bcc :+
    lda #(24*8-1)
    sta player2_y
  :
  rts

player_1_a:
  ; Might need to serve
  clc
  lda ball_state
  and #BALL_SERVE
  beq :+
    lda #BALL_SERVE
    eor ball_state
    sta ball_state
  :
  rts

player_2_a:
  ; Might need to serve
  clc
  lda ball_state
  and #BALL_SERVE
  beq :+
    lda #BALL_SERVE
    eor ball_state
    sta ball_state
  :
  rts

draw_game_sprites:
  ; player 1 paddle bricks y-pos
  lda player1_y
  clc
  sta oam+(0*4)+0
  adc #8
  sta oam+(1*4)+0
  adc #8
  sta oam+(2*4)+0
  adc #8
  sta oam+(3*4)+0
  ; player 2 paddle bricks y-pos
  lda player2_y
  clc
  sta oam+(4*4)+0
  adc #8
  sta oam+(5*4)+0
  adc #8
  sta oam+(6*4)+0
  adc #8
  sta oam+(7*4)+0
  lda ball_y
  sta oam+(8*4)+0
  lda #0 ; paddle brick tile
  sta oam+(0*4)+1
  sta oam+(1*4)+1
  sta oam+(2*4)+1
  sta oam+(3*4)+1
  sta oam+(4*4)+1
  sta oam+(5*4)+1
  sta oam+(6*4)+1
  sta oam+(7*4)+1
  lda #2 ; ball tile
  sta oam+(8*4)+1
  ; attributes
  lda #%00000000 ; no flip
  sta oam+(0*4)+2
  sta oam+(1*4)+2
  sta oam+(2*4)+2
  sta oam+(3*4)+2
  sta oam+(4*4)+2
  sta oam+(5*4)+2
  sta oam+(6*4)+2
  sta oam+(7*4)+2
  lda #%00000001 ; greyscale
  sta oam+(8*4)+2
  ; x position
  lda player1_x
  sta oam+(0*4)+3
  sta oam+(1*4)+3
  sta oam+(2*4)+3
  sta oam+(3*4)+3
  lda player2_x
  sta oam+(4*4)+3
  sta oam+(5*4)+3
  sta oam+(6*4)+3
  sta oam+(7*4)+3
  lda ball_x
  sta oam+(8*4)+3
  rts

setup_background:
  ; first nametable, start by clearing to empty
  lda $2002 ; reset latch
  lda #$20
  sta $2006
  lda #$00
  sta $2006
  ; empty nametable
  lda #0
  ldy #30 ; 30 rows
  :
    ldx #32 ; 32 columns
    :
      sta $2007
      dex
      bne :-
    dey
    bne :--
  ; set all attributes to 0
  ldx #64 ; 64 bytes
  :
    sta $2007
    dex
    bne :-
  ; draw top border
  ldy #3 ; start at row 3 
  ldx #0 ; start at column 0
  jsr ppu_address_tile
  lda #1 ; geometric brick
  ldx #32 ; columns to write
  :
    sta $2007
    dex
    bne :-
  ; draw lower border
  ldy #28 ; start at row 30
  ldx #0 ; start at column 0
  jsr ppu_address_tile
  lda #1 ; geometric brick
  ldx #32 ; columns to write
  :
    sta $2007
    dex
    bne :-
  ; TODO draw a net
  rts

;
; end of file
;
