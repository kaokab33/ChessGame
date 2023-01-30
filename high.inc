.286
.model SMALL
.stack 64
public TabyaHighligth
public KingHighlight
public HorseHighlight
public SoliderHighlight
public FielHighlight
public highlightSelect
public FREEZEHIGH
.DATA
  CHECKSOL    DW  0
    colTH       dw ?
    rowTH       dw ?
    colKH       dw ?
    rowKH       dw ?
    currColor db ?
    REVColor db 0FH
Kstr         dw ?
stco         dw ?
nancya dw ?; to store the initial value of cx(represent the x axis)
nancyb dw ?;to store the initial value of dx(represent the y axis)
nancyc dw ?;to temporary store the cx value to draw
nancyd dw ?;
WAZER DW ?
.CODE
FREEZEHIGH PROC FAR
    PUSH DX
    PUSH CX
                   SUB DX,22
      ADD CX,24
      MOV AL,TCOLRIZE
      MOV AH,0CH
      INT 10H
      INC CX
      INT 10H
      INC CX
      INT 10H
      INC DX
      INT 10H
      INC DX
      INT 10H
      DEC CX
      INT 10H
      DEC DX
      INT 10H
      DEC CX
      INT 10H
      INC DX
      INT 10H
      
      POP CX
      POP DX
    
    RET
    FREEZEHIGH ENDP
cah PROC FAR 
   push cx
                 PUSH DX
      ;PASS COLUMN TO CL
      ;PASS ROW TO DL
                 MOV  AX,CX
                 MOV  CL,28
                 DIV  CL
                 MOV  AH,0
                 MOV  DI,AX
                 MOV  AX,DX
                 MOV  CL,23
                 DIV  CL
                 MOV  AH,0
                 DEC  AX
                 MOV  BX,AX
                 MOV  CL,COLSIZE
                 MUL  CL
                 MOV  AH,0
                 ADD  AX,DI
                 MOV  SI,AX
                 MOV  AL,Array1[SI]
                 POP  DX
                 POP  CX
                 RET
                 cah ENDP

highlight PROC FAR
         push dx
                       push cx
      ;TAKES COLUMN IN CX
                       SUB  DX,23
                       ;inc cx
                       MOV  AL,COLORI
                       MOV  AH,0CH
                       MOV  DI,CX
                       MOV  BH,22
                       MOV  BL,27
      DRAWRECT1:       INT  10H
                       INC  CX
                       DEC  BL
                       JNZ  DRAWRECT1                  ;COLUMN DRAWING
      COLN:            INT  10H
                       INC  DX
                       DEC  BH
                       JNZ  COLN
                       MOV  BL,27
                       MOV  BH,22
      LROW:            INT  10H
                       DEC  CX
                       DEC  BL
                       JNZ  LROW
      COLNU:           INT  10H
                       DEC  DX
                       DEC  BH
                       JNZ  COLNU
                       pop cx
                       pop dx
                       RET
                    
                    
highlight ENDP

highlightSelect PROC FAR
         push dx
                       push cx
                       inc cx
      ;TAKES COLUMN IN CX
                       SUB  DX,22
                       ;inc cx
                       MOV  AL,COLORI
                       MOV  AH,0CH
                       MOV  DI,CX
                       MOV  BH,20
                       MOV  BL,25
      DRAWRECT2:       INT  10H
                       INC  CX
                       DEC  BL
                       JNZ  DRAWRECT2                  ;COLUMN DRAWING
      COLN1:            INT  10H
                       INC  DX
                       DEC  BH
                       JNZ  COLN1
                       MOV  BH,20
                       MOV  BL,25
      LROW1:            INT  10H
                       DEC  CX
                       DEC  BL
                       JNZ  LROW1
      COLNU1:           INT  10H
                       DEC  DX
                       DEC  BH
                       JNZ  COLNU1
                       pop cx
                       pop dx
                   pusha

cmp COLORI,7 ;are we clearing??
jz goaway2
cmp COLORI,8 ;are we clearing??
jz goaway2

call cah
SHL AX,4

CMP Al,40h ;IS A KING HIGHLIGHTED?
jnz goaway2

;SETTING CURSOR
MOV AX,0
mov ah,2
mov dx,0FE08h
int 10h 
;;;CLEARING PAST MESSAGES
MOV AX,0
mov AH,9 ;INT 21/9
MOV DX,OFFSET MESCLR
INT 21H 
;;;;;;;;;;;;
;SETTING CURSOR
MOV AX,0
mov ah,2
mov dx,0FE08h
int 10h 
mov AH, 9
MOV DX,OFFSET MESCHECH
int 21h 
mov ISCHECKED,1

             goaway2:   popa
             RET
                    
                    
highlightSelect ENDP




TabyaHighligth PROC FAR

pusha
                     PUSH SI
                     CMP  ayhaga,0
                     JZ   STB
                     MOV  SI,OFFSET HIGHARRAYW
                     JMP  YAGJ
      STB:           MOV  SI,OFFSET HIGHARRAYB
      YAGJ:          PUSH SI
                     MOV  BX,0
                     PUSH CX
                     PUSH DX
                     CALL cah
                     SHL  AX,4
                     MOV  currColor,AH
      ;XOR REVColor,AH
                     POP  DX
                     POP  CX
                     POP  SI
                     mov  colTH,cx
                     mov  rowTH,dx
      up:            cmp  dx,23
                     jz   down
                     sub  dx,23
                     PUSH SI
                     PUSH CX
                     PUSH DX
                     call cah
      ;AH HAVE THE COLOR
                     MOV  REVColor,AL
                     SHL  AX,4
            
                     POP  DX
                     POP  CX
                     POP  SI
                     CMP  AX,0
                     JZ   PO
                     CMP  AH,currColor
                     JZ   down1
      PO:            mov  bh,CL
                     mov  bl,dl
                     MOV  [SI],bx
                     ADD  SI,2
                     call highlightSelect
                     CMP  REVColor,0
                     JNZ  down1
                     cmp  dx,23
                     jnz  up
                     
      DOWN1:         
                     mov  dx,rowTH
      ;RESET
      down:          cmp  dx,184
                     jz   left
                     add  dx,23
                     PUSH SI
                     PUSH CX
                     PUSH DX
                     call cah
      ;AH HAVE THE COLOR
                     MOV  REVColor,AL
                     SHL  AX,4
            
                     POP  DX
                     POP  CX
                     POP  SI
                     CMP  AX,0
                     JZ   PO1
                     CMP  AH,currColor
                     JZ   LEFT1
      PO1:           mov  bh,CL
                     mov  bl,dl
                     MOV  [SI],bx
                     ADD  SI,2
                     call highlightSelect
                     CMP  REVColor,0
                     JNZ  LEFT1
                     call highlightSelect
                     cmp  dx,184
                     jnz  down
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      left1:         mov  dx,rowTH                   ;RESET
      left:          cmp  cx,0
                     jz   right
                     sub  cx,28
                     PUSH SI
                     PUSH CX
                     PUSH DX
                     call cah
      ;AH HAVE THE COLOR
                     MOV  REVColor,AL
                     SHL  AX,4
            
                     POP  DX
                     POP  CX
                     POP  SI
                     CMP  AX,0
                     JZ   PO2
                     CMP  AH,currColor
                     JZ   RIGTH1
      PO2:           mov  bh,CL
                     mov  bl,dl
                     MOV  [SI],bx
                     ADD  SI,2
                     call highlightSelect
                     CMP  REVColor,0
                     JNZ  RIGTH1
                     call highlightSelect
                     cmp  cx,0
                     jnz  left
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      RIGTH1:        mov  cx,colTH                   ;RESET
      right:         cmp  cx,196
                     jz   bye
                     add  cx,28
                     PUSH SI
                     PUSH CX
                     PUSH DX
                     call cah
      ;AH HAVE THE COLOR
                     MOV  REVColor,AL
                     SHL  AX,4
            
                     POP  DX
                     POP  CX
                     POP  SI
                     CMP  AX,0
                     JZ   PO3
                     CMP  AH,currColor
                     JZ   BYE
      PO3:           mov  bh,CL
                     mov  bl,dl
                     MOV  [SI],bx
                     ADD  SI,2
                     call highlightSelect
                     CMP  REVColor,0
                     JNZ  BYE
                     call highlightSelect
                     cmp  cx,196
                     jnz  right
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      bye:           mov  cx,colTH
                     mov  dx,rowTH
                     MOV  WAZER,SI
                     POP  SI
                     popa
                     ret
TabyaHighligth ENDP


KingHighlight PROC FAR
                     CMP ayhaga,0
JZ STB1
MOV SI,OFFSET HIGHARRAYW
JMP YAGJ1
STB1:MOV SI,OFFSET HIGHARRAYB
YAGJ1:PUSH SI         
                      MOV BX,0
                      PUSH SI
                      PUSH CX
                      PUSH DX
                      CALL cah
                 SHL AX,4
                 MOV currColor,AH
POP DX 
POP CX
POP SI
                      mov  colKH,cx
                      mov  rowKH,dx
                      
        n1:           cmp  cx,0
                      JZ   n3
                      sub  cx,28
                      PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI
            CMP AH,currColor
            JZ n2
       PI:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                       call highlightSelect
        n2:           cmp  dx,184
                      jz   n51
                      add  dx,23
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI2
            CMP AH,currColor
            JZ n3
       PI2:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n3:           mov  dx,rowKH                     ;RESET
                      mov  cx,colKH                     ;RESET
                      cmp  dx,184
                     N51: jz   n5
                      add  dx,23
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI3
            CMP AH,currColor
            JZ n4
       PI3:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n4:           cmp  cx,196
                      jz   n71
                      add  cx,28
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI4
            CMP AH,currColor
            JZ n5
       PI4:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n5:           mov  dx,rowKH                     ;RESET
                      mov  cx,colKH                     ;RESET
                      cmp  cx,196
                    N71:  jz   n7
                      add  cx ,28
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI5
            CMP AH,currColor
            JZ n6
       PI5:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n6:           cmp  dx,23
                      jz   BYE1
                      sub  dx,23
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI6
            CMP AH,currColor
            JZ n7
       PI6:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n7:           mov  dx,rowKH                     ;RESET
                      mov  cx,colKH                     ;RESET
                      sub  dx,23
                    BYE1:  jz   byee
                              PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI7
            CMP AH,currColor
            JZ n8
       PI7:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        n8:           cmp  cx,0
                      jz   byee
                      sub  cx,28
                                PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PI8
            CMP AH,currColor
            JZ byee
       PI8:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
        byee:  
        mov  dx,rowKH                     ;RESET
      mov  cx,colKH       
        POP SI
        ret
KingHighlight ENDP


HorseHighlight proc FAR
CMP ayhaga,0
JZ STB2
MOV SI,OFFSET HIGHARRAYW
JMP YAGJ2
STB2:MOV SI,OFFSET HIGHARRAYB
YAGJ2:PUSH SI         
PUSH SI
MOV BX,0
PUSH CX
PUSH DX
CALL cah
SHL AX,4
MOV currColor,AH
POP DX 
POP CX
POP SI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        mov Kstr,dx
        mov stco,cx
        ;;;;;;;;;;;;;;;;;;;;;;;
        mov dx,Kstr
        mov cx,stco
        sub dx,23
        sub cx,56
        cmp dx,23
        jl sk1
        cmp cx,0
        jl sk1
        cmp dx,184
        jg sk1
        cmp cx,196
        jg sk1
        mov al,4
                  PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY
            CMP AH,currColor
            JZ SK1
       PY:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
       call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk1:
        mov dx,Kstr
        mov cx,stco
        sub dx,23
        add cx,56
        cmp dx,23
        jl sk2
        cmp cx,0
        jl sk2
        cmp dx,184
        jg sk2
        cmp cx,196
        jg sk2
                          PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY1
            CMP AH,currColor
            JZ SK2
       PY1:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
        call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk2:
        mov dx,Kstr
        mov cx,stco
        sub dx,46
        add cx,28
        cmp dx,23
        jl sk3
        cmp cx,0
        jl sk3
        cmp dx,184
        jg sk3
        cmp cx,196
        jg sk3 
                          PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY2
            CMP AH,currColor
            JZ SK3
       PY2:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
        call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk3:
        mov dx,Kstr
        mov cx,stco
        add dx,46
        add cx,28
        cmp dx,23
        jl sk4
        cmp cx,0
        jl sk4
        cmp dx,184
        jg sk4
        cmp cx,196
        jg sk4 
                         PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY3
            CMP AH,currColor
            JZ SK4
       PY3:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk4:
        mov dx,Kstr
        mov cx,stco
        add dx,23
        add cx,56
        cmp dx,23
        jl sk5
        cmp cx,0
        jl sk5
        cmp dx,184
        jg sk5
        cmp cx,196
        jg sk5 
                          PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY4
            CMP AH,currColor
            JZ SK5
       PY4:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
        call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk5:
        mov dx,Kstr
        mov cx,stco
        add dx,23
        sub cx,56
        cmp dx,23
        jl sk6
        cmp cx,0
        jl sk6
        cmp dx,184
        jg sk6
        PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY5
            CMP AH,currColor
            JZ SK6
       PY5:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
        call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk6:
        mov dx,Kstr
        mov cx,stco
        add dx,46
        sub cx,28
        cmp dx,23
        jl sk7
        cmp cx,0
        jl sk7
        cmp dx,184
        jg sk7
        cmp cx,196
        jg sk7 
                          PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY6
            CMP AH,currColor
            JZ SK7
       PY6:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
        call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk7:
        mov dx,Kstr
        mov cx,stco
        sub dx,46
        sub cx,28
        cmp dx,23
        jl sk8
        cmp cx,0
        jl sk8
        cmp dx,184
        jg sk8
        cmp cx,196
        jg sk8 
                          PUSH SI                      
PUSH CX
PUSH DX
call cah
;AH HAVE THE COLOR
MOV REVColor,AL
SHL AX,4
            
POP DX
            POP CX
            POP SI
            CMP AX,0
            JZ PY7
            CMP AH,currColor
            JZ SK8
       PY7:               mov bh,CL
                      mov bl,dl
            MOV [SI],bx
          ADD SI,2
                      call highlightSelect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sk8:
        mov dx,Kstr
        mov cx,stco
        pop SI
      ret
HorseHighlight endp

blackSoldier PROC
                  PUSHA
                  MOV   CHECKSOL,0
                   ADD   DX,23
                   CMP DX,207
                   JNZ CONTIB
                   RET
                   CONTIB:CALL  cah

                   CMP   AX,0
                   JNZ   CONTISOLDB1             ;THE SQUARE IS EMPTY
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYB[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI

  CONTISOLDB1:      ADD   CX,28
                   CMP   CX,224
                   JZ    CONTISOLDB2
                   CALL  cah
                   CMP   AX,0                   ;IF EMPTY SQUARE
                   JZ    CONTISOLDB2
                   SHR   AX,4
                   CMP   AX,0FH
                   JNZ   CONTISOLDB2
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYB[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI
  CONTISOLDB2:     SUB   CX,56
                   CMP   CX,0FFE4H
                   JZ    CONTISOLDB3
                   CALL  cah
                   CMP   AX,0                   ;IF EMPTY SQUARE
                   JZ    CONTISOLDB3
                   SHR   AX,4
                   CMP   AX,0FH
                   JNZ   CONTISOLDB3             ;IF WHITE SKIP
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYB[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI
  CONTISOLDB3:  
                   MOV CHECKSOL,0
                   POPA
                   RET
blackSoldier ENDP

  ;description
whiteSoldier PROC
  ;take cx for column and dx for row
                   PUSHA
                   MOV   CHECKSOL,0
                   SUB   DX,23
                   CMP DX,0
                   jNz CONTIW
RET
                   CONTIW:CALL  cah

                   CMP   AX,0
                   JNZ   CONTISOLD1             ;THE SQUARE IS EMPTY
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYW[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI

  CONTISOLD1:      
                   ADD   CX,28
                   CMP   CX,224
                   JZ    CONTISOLD2
                   CALL  cah
                   CMP   AX,0                   ;IF EMPTY SQUARE
                   JZ    CONTISOLD2
                   SHR   AX,4
                   CMP   AX,0
                   JNZ   CONTISOLD2
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYW[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI
  CONTISOLD2:      
                   SUB   CX,56
                   CMP   CX,0FFE4H
                   JZ    CONTISOLD3
                   CALL  cah
                   CMP   AX,0                   ;IF EMPTY SQUARE
                   JZ    CONTISOLD3
                   SHR   AX,4
                   CMP   AX,0
                   JNZ   CONTISOLD3             ;IF WHITE SKIP
                   CALL  highlightSelect
                   PUSH  DI
                   PUSH  CX
                   PUSH  DX
                   SHL   CX,8
                   MOV   DH,CH
                   MOV   DI,CHECKSOL
                   MOV   HIGHARRAYW[DI],DX
                   ADD   CHECKSOL,2
                   POP   DX
                   POP   CX
                   POP   DI
  CONTISOLD3:     

                   return:                   MOV   CHECKSOL,0
                   POPA
                   ret
whiteSoldier ENDP

SoliderHighlight PROC FAR
  ;PUT COLOR OF SOLDIER IN BL
  ;PUT CURRENT COLUMN IN CX AND CURRENT ROW IN DX
                   MOV   BH,0
                   CMP   BL,BH
                   JNZ   DRAWWHITE
                   CALL  blackSoldier
                   RET
  DRAWWHITE:       
                   CALL  whiteSoldier
                   RET
SoliderHighlight ENDP



FielHighlight proc FAR
;MOV SI,0
mov nancya,CX
mov nancyb,dx
PUSH SI
CALL cah
POP SI
MOV AH,0
 SHL AX,4
MOV currColor,AH
;;;;;;; left upper available moves 
MOV CX,nancya
MOV DX,nancyb
lu: sub cx,28
sub dx,23 
cmp dx,0
jz k
cmp cx,0ffe4h
jz k
mov nancyc,cx
mov nancyd,dx
PUSH SI 
CALL cah
POP SI
MOV REVColor,AL
MOV AH,0
SHL AX,4
CMP AX,00
JZ OVER
CMP AH,currColor
JZ K
OVER:mov cx,nancyc
mov dx,nancyd
call highlightSelect 
mov cx,nancyc
mov dx,nancyd
mov bh,CL
mov bl,dl
MOV [SI],BX
ADD SI,2
CMP REVColor,0
JNZ K
mov cx,nancyc
mov dx,nancyd

jmp lu 
  
k: 
mov cx,nancya
mov dx,nancyb 
;;;;;;; right upper available moves
ru:
add cx,28
sub dx,23
cmp dx,0
jz de
cmp cx,224d
jz de
mov nancyc,cx
mov nancyd,dx
PUSH SI 
CALL cah
POP SI
MOV REVColor,AL
MOV AH,0
SHL AX,4
CMP AX,00
JZ OVER1
CMP AH,currColor
JZ DE
OVER1:mov cx,nancyc
mov dx,nancyd
call highlightSelect 
mov cx,nancyc
mov dx,nancyd
mov bh,CL
mov bl,dl
MOV [SI],BX
ADD SI,2
CMP REVColor,0
JNZ DE
mov cx,nancyc
mov dx,nancyd
jmp ru 

de:
mov cx,nancya
mov dx,nancyb       
;;;;;;;;;;;; left down available moves
ld:
sub cx,28
add dx,23
cmp dx,207d
jz f
cmp cx,0ffe4h
jz f
mov nancyc,cx
mov nancyd,dx
PUSH SI 
CALL cah
POP SI
MOV REVColor,AL
MOV AH,0
SHL AX,4
CMP AX,00
JZ OVER2
CMP AH,currColor
JZ F
OVER2:mov cx,nancyc
mov dx,nancyd
call highlightSelect 
mov cx,nancyc
mov dx,nancyd
mov bh,CL
mov bl,dl
MOV [SI],BX
ADD SI,2
CMP REVColor,0
JNZ F
mov cx,nancyc
mov dx,nancyd

jmp ld 
f:

;;;;;;;;;;
mov cx,nancya
mov dx,nancyb

;;draw the right down available moves  
rd:
add cx,28
add dx,23
cmp dx,207d
jz p
cmp cx,224d
jz p
mov nancyc,cx
mov nancyd,dx
PUSH SI 
CALL cah
POP SI
MOV REVColor,AL
MOV AH,0
SHL AX,4
CMP AX,00
JZ OVER5
CMP AH,currColor
JZ P
OVER5:mov cx,nancyc
mov dx,nancyd
call highlightSelect 
mov cx,nancyc
mov dx,nancyd
mov bh,CL
mov bl,dl
MOV [SI],BX
ADD SI,2
CMP REVColor,0
JNZ P
mov cx,nancyc
mov dx,nancyd
jmp rd 
p:

;;to exit the run
ret
FielHighlight endp
