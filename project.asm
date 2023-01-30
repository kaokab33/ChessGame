                 include draw.inc
                 include high.inc
                 .286
        .MODEL HUGE
        .STACK 256
.DATA
      TCOLRIZE      DB  ?
      CURRTIME      DW  ?
      username      db  30 dup('$')
      username2     db  30 dup('$')
      keep          db  ?
      keep2         db  ?
      store         db  '$'
      welcome       db  'Please enter your name:','$'
      keywait       db  'Please enter a key to continue','$'
      gamestart     db  'To start the game press F2$'
      chatstart     db  'To start chatting press F1$'
      endall        db  'To end the program press ESC$'
      chatinvitemsg db  ' send you a chat invitation, to accept press F1','$'
      gameinvitemsg db  ' send you a game invitation, to accept press F2','$'
      sendgamemsg   db  'you send a game invitation, to ','$'
      sendchatmsg   db  'you send a chat invitation, to ','$'
      finally       db  0
      KINGKILL      DB  ?
      movingblack   dw  '$$'
      movingwhite   dw  '$$'
      HIGHARRAYW    DW  29 DUP('$')
      HIGHARRAYB    DW  29 DUP('$')
      COLUMNBoard   DW  ?
      ROWBoard      DW  ?
      COLORI        DB  ?
      pressblack    db  0
      presswhite    db  0
      colhigh1      db  56
      colhigh2      db  56
      rowhigh1      db  184
      rowhigh2      db  23
      TESTING       DB  0
      ROWTEST       DB  184
      COLTEST       DB  56

      ROWFirst      DB  184
      COLFirst      DB  56
      
      ROWSec        DB  23
      COLSec        DB  56

      ayhaga        db  ?

      ROWHIGH       DB  ?
      COLHIGH       DB  ?
      freeze        db  0
      COLSIZE       EQU 8

      line          db  80 dup('-'),'$'
      value         db  '$'
      valueREc      db  '$'
      cR            dw  1800h
      csend         dw  0a00h

      gameinvite    db  0
      chatinvite    db  0
      
      gamesend      db  0
      chatsend      db  0

      who           db  1
      counterY      db  0
      
      GRIDCOLOR     DB  8,7,8,7,8,7,8,7
                    DB  7,8,7,8,7,8,7,8
                    DB  8,7,8,7,8,7,8,7
                    DB  7,8,7,8,7,8,7,8
                    DB  8,7,8,7,8,7,8,7
                    DB  7,8,7,8,7,8,7,8
                    DB  8,7,8,7,8,7,8,7
                    DB  7,8,7,8,7,8,7,8

      Array1        db  1H,2H,3H,5H,4H,3H,2H,1H
                    db  6H,6H,6H,6h,6H,6H,6H,6H                                    ;BLACK
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    db  0H,0H,0H,0H,0H,0H,0H,0H
                    db  0H,0H,0H,0H,0H,0H,0H,0H                                    ;1->Tabya
                    db  0H,0H,0H,0H,0H,0H,0H,0H                                    ;6->pawn
                    db  0H,0H,0H,0H,0H,0H,0H,0H
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                    db  0F6H,0F6H,0F6H,0F6H,0F6H,0F6H,0F6H,0F6H                    ;WHITE
                    db  0F1H,0F2H,0F3H,0F5H,0F4H,0F3H,0F2H,0F1H
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      TIMERARRAY    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
                    dw  0H,0H,0H,0H,0H,0H,0H,0H
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      MESEND        DW  'HW','TI',' E','IW','SN','!$'
      MESCHECH      DB  'CHECK!','$'
      MESCOLOR      DW  'HW','TI',' E','  '                                        ;WHITE
      MESPIECE      DW  'IK','GN','  ',' '                                         ;KING
      MESDIED       DB  'DIED','$'
      MESCLR        DB  '                          ','$'
      MESCLRChat    DB  ' ','$'
      ISCHECKED     DB  0
      GAMETIMERMIN  DB  0,0
      time          DB  ':'
      GAMETIMERSEC  DB  0,0,'$'

      InitialMIN    DB  ?
      InitialSEC    DB  ?
      beprinted     db  ?

.CODE
timerehigh PROC FAR
                     PUSHA
                     mov   AX,0
                     MOV   AH,2ch
                     INT   21H
                     MOV   AH,CL
                     MOV   AL,DH
                     MOV   CURRTIME,AX
                     MOV   SI,0
                     MOV   DI,0
                     MOV   BX,0
                     MOV   CX,0
                     MOV   DX,23
      LETSLOOP:      
      ; call calcArr
                     MOV   AX,TIMERARRAY[SI]
                     CMP   AX,0
                     JZ    HIGHGRIDCOLOR
                     MOV   DI,CURRTIME
                     SUB   DI,AX
                     cmp   DI,3
                     JA    HIGHGRIDCOLOR
                     MOV   TCOLRIZE,4
                     CALL  FREEZEHIGH
                     JMP   CONTINUEFUNC
      HIGHGRIDCOLOR: 
                     MOV   TIMERARRAY[SI],0
                     MOV   AL,GRIDCOLOR[BX]
                     MOV   TCOLRIZE,AL
                     CALL  FREEZEHIGH
      CONTINUEFUNC:  
                     ADD   CX,28
                     CMP   CX,224
                     JNZ   LETS
                     MOV   CX,0
                     ADD   DX,23

      LETS:          add   si,2
                     
                     INC   bx
                     CMP   BX,64
                     JB    LETSLOOP
                     POPA
                     RET
timerehigh ENDP
      ;description
GRIDI PROC FAR
                  
                  
                     MOV   [DI],0
                     MOV   [SI],0
                     MOV   BL,28
                     MOV   BH,23
                     MOV   CX,[DI]
                     MOV   DX,[SI]
                     MOV   AL,8
                     MOV   AH,0CH


      DRAW:          INT   10H
                     INC   CX
                     DEC   BL
                     JNZ   DRAW                         ;COLUMN DRAWING
                     INC   DX
                     MOV   CX,[DI]
                     MOV   BL,28
                     DEC   BH
                     JNZ   DRAW                         ; ONE  LINE DRAW


                     MOV   DX,[SI]                      ; ROW INTIALIZATION PER ITERATION
        
        
                     CMP   AL,7                         ;COLOR SWAP
                     JZ    GREY
                     MOV   AL,7
        
                     JMP   CONT
      GREY:          MOV   AL,8


      CONT:          MOV   BL,28
                     MOV   BH,23
                     ADD   [DI],28
                     MOV   CX,[DI]
                     CMP   [DI],224
                     JL    DRAW                         ; DRAW ANOTHER SQUARE OF SIDE 20
        
        
                     CMP   AL,8                         ;COLOR SWAP FOR NEW ROW SQUARE
                     JNZ   GREY1
                     MOV   AL,7
                     JMP   CONT1
      GREY1:         MOV   AL,8

      CONT1:         MOV   [DI],0

      ;CONSTRUCTING NEW ROW OF SQUARES TILL FORMING 8 ROWS
                     MOV   CX,0
                     ADD   [SI],23
                     MOV   DX,[SI]
                     CMP   [SI],184
                     JL    DRAW
                     RET
        
GRIDI ENDP

      ;description
calcArr PROC FAR
                     push  cx
                     PUSH  DX
      ;mov dh,0
      ;PASS COLUMN TO CL
      ;PASS ROW TO DL
                     MOV   AX,CX
                     MOV   CL,28
                     DIV   CL
                     MOV   AH,0
                     MOV   DI,AX
                     MOV   AX,DX
                     MOV   CL,23
                     DIV   CL
                     MOV   AH,0
                     DEC   AX
                     MOV   BX,AX
                     MOV   CL,COLSIZE
                     MUL   CL
                     MOV   AH,0
                     ADD   AX,DI
                     MOV   SI,AX
                     MOV   AL,Array1[SI]
                     POP   DX
                     POP   CX

                     RET
      
calcArr ENDP

calc PROC FAR
                     MOV   CL,COLTEST
                     MOV   DL,ROWTEST
                     MOV   AX,CX
                     MOV   CL,28
                     DIV   CL
                     MOV   AH,0
                     MOV   DI,AX
                     MOV   AX,DX
                     MOV   CL,23
                     DIV   CL
                     MOV   AH,0
                     DEC   AX
                     MOV   BX,AX
                     MOV   CL,COLSIZE
                     MUL   CL
                     MOV   AH,0
                     ADD   AX,DI
                     MOV   SI,AX
                     MOV   AL,GRIDCOLOR[SI]
                     PUSH  SI
                     MOV   COLORI,AL
                     MOV   CL,COLTEST
                     MOV   DL,ROWTEST
              
                     CALL  highlight
                     POP   SI
                     ret
calc ENDP

Cleardata PROC
                     mov   GAMETIMERMIN,0
                     mov   GAMETIMERMIN[1],0
                     mov   GAMETIMERSEC,0
                     mov   GAMETIMERSEC[1],0
                     mov   cx,64
                     MOV   SI,0
      awaw:          mov   TIMERARRAY[si],0
                     add   si,2
                     dec   cx
                     cmp   cx,0
                     jnz   awaw
                     mov   freeze,0
                     mov   pressblack , 0
                     mov   presswhite , 0
                     MOV   SI,0
                     cmp   HIGHARRAYW[si],'$'
                     jz    cleartheblack
                     mov   HIGHARRAYW[si],'$'
                     add   si,2
      cleartheblack: 
                     MOV   SI,0
                     cmp   HIGHARRAYW[si],'$'
                     jz    setrest
                     mov   HIGHARRAYW[si],'$'
                     add   si,2
      setrest:       
                     mov   ROWFirst, 184
                     mov   COLFirst,  56
                     mov   ROWSec,23
                     mov   COLSec ,56

mov value,'$'
mov valueREc,'$'

mov store,'$'
;mov ayhaga,'$'
mov who,1



                     ret
      
Cleardata ENDP
      ;description
CLEARHIGH PROC FAR
                     pusha
                     MOV   SI,0
                     cmp   ayhaga,0
                     jz    LOPB
      LOP:           
                     CMP   HIGHARRAYW[SI],'$'
                     JZ    EX
                     MOV   DX,HIGHARRAYW[SI]
                     MOV   CL,DH
                     MOV   DH,0
                     mov   ch,0                         ;;;;;;;;;;YAAARAAAA
                     PUSH  SI
                     CALL  calcArr
                     MOV   AL,GRIDCOLOR[SI]
                     MOV   COLORI,AL
                     POP   SI
                     MOV   DX,HIGHARRAYW[SI]
                     MOV   CL,DH
                     CALL  highlightSelect
      ;MOV DX,'$'
                     MOV   HIGHARRAYW[SI],'$'
                     ADD   SI,2
                     JMP   LOP

      LOPB:          
                     CMP   HIGHARRAYB[SI],'$'
                     JZ    EX
                     MOV   DX,HIGHARRAYB[SI]
                     MOV   CL,DH
                     MOV   DH,0
                     PUSH  SI
                     CALL  calcArr
                     MOV   AL,GRIDCOLOR[SI]
                     MOV   COLORI,AL
                     POP   SI
                     MOV   DX,HIGHARRAYB[SI]
                     MOV   CL,DH
                     CALL  highlightSelect
                     
      ;MOV DX,'$'
                     MOV   HIGHARRAYB[SI],'$'
                     ADD   SI,2
                     JMP   LOPB


      EX:            cmp   ISCHECKED,0
                     jz    bb
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0FE08h
                     int   10h

                     mov   AH, 9
                     MOV   DX,OFFSET MESCLR
                     int   21h
                     mov   ISCHECKED,0
      bb:            popa
                     RET
CLEARHIGH ENDP

VIEWMES PROC FAR
                     PUSHA
                     PUSH  AX
      ;SETTING CURSOR
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0FE08h
                     int   10h
      ;;;CLEARING PAST MESSAGES
                     MOV   AX,0
                     mov   AH,9                         ;INT 21/9
                     MOV   DX,OFFSET MESCLR
                     INT   21H
      ;;;;;;;;;;;;
      ;SETTING CURSOR
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0FE08h
                     int   10h

                     POP   AX
      ;CHOOSING MESSAGE TO DISPLAY
                     CMP   AH,00h
                     JZ    BLACKCOL                     ;IF THE COLOR IS BLACK

      ;IF COLOR IS WHITE
                     MOV   MESCOLOR,'HW'
                     MOV   MESCOLOR[2],'TI'
                     MOV   MESCOLOR[4],' E'
                     JMP   PIECES
      BLACKCOL:      MOV   MESCOLOR,'LB'
                     MOV   MESCOLOR[2],'CA'
                     MOV   MESCOLOR[4],' K'

      ;;;;WHICH PIECE DIED??;;;;
      PIECES:        CMP   AL, 10h
                     JZ    TD                           ;TABYA
                     CMP   AL, 20h
                     JZ    HD                           ;HORSE
                     CMP   AL, 30h
                     JZ    FD                           ;FIEL
                     CMP   AL, 40h
                     JZ    EMP                          ;KING
                     CMP   AL, 50h
                     JZ    WD                           ;WAZEER
                     CMP   AL, 60h
                     JZ    SD                           ;SOLIDER
                     JMP   EMP

      TD:            MOV   MESPIECE,'OR'
                     MOV   MESPIECE[2],'KO'
                     JMP   HMMM

      HD:            MOV   MESPIECE,'NK'
                     MOV   MESPIECE[2],'GI'
                     MOV   MESPIECE[4],'TH'
                     JMP   HMMM

      FD:            MOV   MESPIECE,'IB'
                     MOV   MESPIECE[2],'HS'
                     MOV   MESPIECE[4],'PO'
                     JMP   HMMM

      ; KD: MOV MESPIECE,'IK'
      ;     MOV MESPIECE[2],'GN'
      ;     JMP HMMM

      WD:            MOV   MESPIECE,'UQ'
                     MOV   MESPIECE[2],'EE'
                     MOV   MESPIECE[4],' N'
                     JMP   HMMM

      SD:            MOV   MESPIECE,'AP'
                     MOV   MESPIECE[2],'NW'
    
      HMMM:          
                     mov   AH,9                         ;INT 21/9
                     MOV   DX,OFFSET MESCOLOR
                     INT   21H
      EMP:           MOV   MESPIECE,'  '
                     MOV   MESPIECE[2],'  '
                     MOV   MESPIECE[4],'  '
                     mov   ISCHECKED,0
                     POPA
                     RET
VIEWMES ENDP

      ;description
      ; clearchat PROC
      ;                      pusha
      ;                      cmp   who ,0
      ;                      jnz   clrsSend
      ;       ;cr
      ;       clrsSend:
      ;                      mov   bx,0
      ;                      mov   dx,0
      ;                      mov   cx,0
      ;                      MOV   AX,0
      ;                      mov   ah,2
      ;                      mov   dx,0de4h
      ;                      mov   csend,0de4h
      ;                      int   10h

      ;                      mov   bx,csend
      ;                      mov bl,0
      ; mov ax,0

      ;       myloop:        mov   dh,bh
      ;                      mov   dl,0e4h
      ;                      mov   ah,2
      ;                      int   10h
      ;                      mov   AH,9                         ;INT 21/9
      ;                      MOV   DX,OFFSET MESCLRChat
      ;                      INT   21H
      ;                      inc   bh
      ;                      inc   counterY
      ;                      cmp   counterY,9
      ;                      jnz   myloop

      ;                      popa
      ;                      ret
      ; clearchat ENDP
      ;description
inlinechat PROC
                     pusha
                     mov   cx,0
                     mov   bx,0
                     MOV   DX,0
                     mov   beprinted,al
                     cmp   who,0
                     jnz   kj

                     mov   bx,cr
                     inc   cR
                     inc   bx
                     push  bx
                     mov   bx,0
                     mov   dx,0
                     mov   cx,0
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,cr
                     int   10h
                     pop   bx
                     cmp   bl,0efh
                     jnz   zy
                     
                     mov   dh,bh
                     inc   dh

                     cmp   dh,0bh
                     jz    scroooll2

                     mov   cx,0
                     mov   ah,2
                     mov   bh,0
                     mov   dl,0e4h
                     int   10h
                     mov   cr,dx
      zy:            jmp   zxx
      ;;;;;;;;;;;;;;;;scroll up te upper half
      scroooll2:     mov   bl,0
                     mov   ax,0600h
                     mov   bh,0
                     mov   cx,041ch
                     mov   dx,0b4fh
                     int   10h
      ; call  clearchat
                     
                     
                     mov   bx,0
                     mov   dx,0
                     mov   cx,0
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,04e4h
                     mov   cr,04e4h
                     int   10h
                     jmp zxx


      kj:            
                     mov   bx,csend
                     inc   csend
                     inc   bx
                     push  bx
                     mov   bx,0
                     mov   dx,0
                     mov   cx,0
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,csend
                     int   10h
                     pop   bx
                     cmp   bl,0efh
                     jnz   zxx
                     
                     mov   dh,bh
                     inc   dh

                     cmp   dh,15h
                     jz    scroooll

                     mov   cx,0
                     mov   ah,2
                     mov   bh,0
                     mov   dl,0e4h
                     int   10h
                     mov   csend,dx
                     jmp   zxx
      ;;;;;;;;;;;;;;;;scroll up te upper half
      scroooll:      mov   bl,0
                     mov   ax,0600h
                     mov   bh,0
                     mov   cx,0e1ch
                     mov   dx,184fh
                     int   10h
      ; call  clearchat
                     
                     
                     mov   bx,0
                     mov   dx,0
                     mov   cx,0
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0ee4h
                     mov   csend,0ee4h
                     int   10h

      zxx:           
                     mov   bx,0
                     mov   dx,0
                     mov   cx,0
                     mov   ah,2
                     mov   dl,beprinted
                     int   21h

                     popa
                     ret
inlinechat ENDP

HIGHL PROC FAR
      ;INITIAL STATE
                     pusha
                     cmp   ayhaga,00h
                     jz    play2

      play1:         mov   DL,ROWFirst
                     MOV   CL,COLFirst
                     MOV   ROWTEST,DL
                     mov   COLTEST,cl
      ;mov   ayhaga,0fh
                     JMP   y21

      play2:         mov   DL,ROWSec
                     MOV   CL,COLSec
                     MOV   ROWTEST,DL
                     mov   COLTEST,cl
      ;mov   ayhaga,0h
      y21:           PUSH  AX
                     call  calc
                     MOV   AL,Array1[SI]
                     POP   AX



                     MOV   BH,75D                       ;LEFT BUTTON
                     CMP   BH,AH
                     JZ    MOVLEFT
       
                     MOV   BH,77D                       ;RIGHT BUTTON
                     CMP   BH,AH
                     JZ    MOVRIGHT

                     MOV   BH,80D                       ;DOWN BUTTON
                     CMP   BH,AH
                     JZ    MOVDOWN1
       
                     MOV   BH,72D                       ;UP BUTTON
                     CMP   BH,AH
                     JZ    MOVUP
                     MOV   AH,AL
                  
                     call  inlinechat
                     jmp   FIN


      MOVDOWN1:      JMP   MOVDOWN
               
      MOVLEFT:       
      ;CHECK IF LEFTMOST
                     MOV   CX,00
                     MOV   DX,00h
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST

      ; MOV  ROWHIGH,DL
      ;MOV  COLHIGH,CL
                     SUB   CX,28
                     CMP   CX,0FFE4H
                     JNZ   CONTI
                     MOV   CX,0
                     JMP   CONTI
         
      MOVRIGHT:      
      ;CHECK IF RIGHTMOST
                     MOV   CX,00
                     MOV   DX,00h
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     ADD   CX,28
                     CMP   CX,224
                     JNZ   CONTI
                     SUB   CX,28
                     JMP   CONTI
         
      MOVUP:         
      ;CHECK IF UPPERMOST
                     MOV   CX,00
                     MOV   DX,00h
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     SUB   DX,23
                     CMP   DX,0
                     JNZ   CONTI
                     ADD   DX,23
                     JMP   CONTI
         
      MOVDOWN:       
      ;CHECK IF DOWNMOST
                     MOV   CX,00
                     MOV   DX,00h
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     ADD   DX,23
                     CMP   DX,207
                     JNZ   CONTI
                     SUB   DX,23
 
      CONTI:         

                     MOV   ROWTEST,DL
                     MOV   COLTEST,CL
                     MOV   BL,2
                     MOV   AL,2
                     MOV   COLORI,4
                     CALL  highlight
                     cmp   ayhaga,00
                     jnz   play11

      play22:        
                     mov   DL,ROWTEST
                     MOV   CL,COLTEST
                     MOV   ROWSec,DL
                     mov   COLSec,cl
                   
                     JMP   FIN

      play11:        mov   DL,ROWTEST
                     MOV   CL,COLTEST
                     MOV   ROWFirst,DL
                     mov   COLFirst,cl

      FIN:           popa
                     RET
HIGHL ENDP


DRAWCOMP PROC FAR
      ;PUT THE VAL IN AL

                     MOV   AH,0
                     SHL   AX,4
                     SHR   AL,4

                     CMP   AL,0
                     JZ    EXIT
                     CMP   AL,1
                     JZ    SETTABYA
                     CMP   AL,2
                     JZ    SETHORSE
                     CMP   AL,3
                     JZ    SETFEIL
                     CMP   AL,4
                     JZ    SETKING
                     CMP   AL,5
                     JZ    SETWAZEER
                     CMP   AL,6
                     JZ    SETPAWN
                
                     JMP   EXIT


      SETTABYA:      MOV   CH,CL
                     MOV   CL,DL
                     MOV   AL,AH
                     CALL  Tabya
                     JMP   EXIT


      SETHORSE:      MOV   DI,CX
                     MOV   BX,DX
                     SUB   BX,23
                     MOV   AL,AH
                     CALL  DrawHorse
                     JMP   EXIT
      SETFEIL:       
                     MOV   AL,AH
                     CALL  DrawFiel
                     JMP   EXIT
      SETKING:       
                     MOV   AL,AH
                     CALL  KING
                     JMP   EXIT
      SETWAZEER:     
                     MOV   AL,AH
                     CALL  dr
                     JMP   EXIT

      SETPAWN:       MOV   AL,AH
                     CALL  DrawPawn

      EXIT:          RET
DRAWCOMP ENDP


HIGHCOMP PROC FAR
      ;PUT THE VAL IN AL
     
                     MOV   CL,COLTEST
                     MOV   DL,ROWTEST

                     MOV   AX,CX
                     MOV   CL,28
                     mov   ch,0                         ;;;;;;;;;;YAAARAAAA
                     DIV   CL
                     MOV   AH,0
                     MOV   DI,AX
                     MOV   AX,DX
                     MOV   CL,23
                     mov   ch,0                         ;;;;;;;;;;YAAARAAAA
                     DIV   CL
                     MOV   AH,0
                     DEC   AX
                     MOV   BX,AX
                     MOV   CL,COLSIZE
                     MUL   CL
                     MOV   AH,0
                     ADD   AX,DI
                     MOV   SI,AX
                     MOV   AL,Array1[SI]

                     MOV   AH,0
                     SHL   AX,4
                     SHR   AL,4
                     MOV   CH,00h
                     MOV   CL,COLTEST
                     MOV   DH,0
                     MOV   DL,ROWTEST

                     CMP   AL,0
                     JZ    EXIIT
                     push  ax
                     call  calc
                     pop   ax

                     cmp   ah,00h
                     jnz   whiteeeeee
                     mov   COLORI,0dh
                     jmp   gooooonnn
      whiteeeeee:    mov   COLORI,10
      gooooonnn:     CMP   AL,1
                     JZ    SETTABYA1
                     CMP   AL,2
                     JZ    SETHORSE1
                     CMP   AL,3
                     JZ    SETFEIL1
                     CMP   AL,4
                     JZ    SETKING1
                     CMP   AL,5
                     JZ    SETWAZEER1
                     CMP   AL,6
                     JZ    SETPAWN1
                
      EXIIT:         JMP   EXIT1



      SETTABYA1:     
                     MOV   AL,AH
                     CALL  TabyaHighligth
                     JMP   EXIT


      SETHORSE1:     
                     MOV   AL,AH
                     CALL  HorseHighlight
                     JMP   EXIT
      SETFEIL1:      
                   
                     MOV   AL,AH
                     PUSH  SI
                     CMP   ayhaga,0
                     JZ    STB6
                     MOV   SI,OFFSET HIGHARRAYW
                     JMP   YAGJ6
      STB6:          MOV   SI,OFFSET HIGHARRAYB
      YAGJ6:         
                     CALL  FielHighlight
                     POP   SI
                     JMP   EXIT
      SETKING1:      

                    
                     MOV   AL,AH
                     CALL  KingHighlight
                     JMP   EXIT
      SETWAZEER1:    
                    
                     MOV   AL,AH
                     call  TabyaHighligth
                     MOV   SI,WAZER
                     CALL  FielHighlight
                     JMP   EXIT

      SETPAWN1:      
                     mov   al,ah
                     MOV   bl,al
                     CALL  SoliderHighlight
    

      EXIT1:         RET
HIGHCOMP ENDP
     

      ;description
MOVCOMP PROC FAR
      ;                cmp   ayhaga,0
      ;                jnz   cvcv
      ;                mov   movingblack,'$$'
      ;                jmp   dsdsds
      ; cvcv:          mov   movingwhite,'$$'
      dsdsds:        PUSH  DX
                     PUSH  CX
                     PUSH  BX
                     MOV   CX,00h
                     MOV   DX,00h
                     MOV   BX,0
                     MOV   DL,ROWHIGH
                     MOV   CL,COLHIGH
                     CALL  calcArr
                     MOV   BL,AL
                     MOV   Array1[SI],00
                     PUSH  AX
                     MOV   AH,0
                     SHL   AX,4

      ;SWAP THE COLOR TO THE GRID COLOR
                     MOV   AH,GRIDCOLOR[SI]
                     SHR   AX,4
      ;AL MUST HAVE THE 2-HEX FORMULA
                     MOV   DL,ROWHIGH
                     MOV   CL,COLHIGH
                     CALL  DRAWCOMP

      ;PREVIOUS COMPONENT MUST BE REMOVED

      ;      MOV Array1[SI],al
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     CALL  calcArr                      ;who is killed?
                     MOV   BL,AL
                     MOV   KINGKILL,BL
                     MOV   AH,0
                     SHL   AX,4
                     call  VIEWMES
                     cmp   ax,0
                     jz    empty
                     push  BX
                     

                     mov   bl,ayhaga
                     mov   ayhaga,ah
                     call  CLEARHIGH                    ;;?
                     mov   ayhaga,bl
                     pop   bx
      ;SWAP THE COLOR TO THE GRID COLOR
      empty:         MOV   AH,GRIDCOLOR[SI]
                     push  cx
                     push  dx
                     push  ax
                     mov   ah,2ch
                     INT   21H
                     mov   ch,CL
                     mov   cl,DH

                     PUSH  SI
                     MOV   AX,SI
                     MOV   SI,2
                     MUL   SI
                     MOV   SI,AX
                     mov   TIMERARRAY[si],cx
                     POP   SI
                     pop   ax
                     pop   dx
                     pop   cx
                     SHR   AX,4
      ;    CMP  AL,40
      ;    JZ   ENDT
      ;AL MUST HAVE THE 2-HEX FORMULA
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
 
                     CALL  DRAWCOMP
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     CALL  calcArr
                     POP   ax

                     MOV   Array1[SI],AL
                     MOV   DL,ROWTEST
                     MOV   CL,COLTEST
                     CALL  DRAWCOMP
                     CALL  CLEARHIGH

                     MOV   BL,KINGKILL
                     SHL   BX,4
                     CMP   BL,40H
                     JZ    ENDT

                     cmp   al,00h                       ;black
                     jz    whiteRef

                     cmp   HIGHARRAYB[0],'$'
                     jz    gooo
                     pusha
                     mov   al,ROWTEST
                     mov   bl,COLTEST
                     mov   ayhaga,0
                     mov   cl,colhigh2
                     mov   COLTEST,cl
                     mov   dl,rowhigh2
                     mov   ROWTEST,dl

                     call  CLEARHIGH
                     call  HIGHCOMP

                     mov   ROWTEST,al
                     mov   COLTEST,bl
                     mov   ayhaga,0fh
                     popa
                     jmp   gooo

      whiteRef:      cmp   HIGHARRAYw[0],'$'
                     jz    gooo
                     pusha
                     mov   bl,ROWTEST
                     mov   al,COLTEST
                     mov   ayhaga,0fh

                     mov   cl,colhigh1
                     mov   COLTEST,cl
                     mov   dl,rowhigh1
                     mov   ROWTEST,dl

                     call  CLEARHIGH
                     call  HIGHCOMP

                     mov   ROWTEST,bl
                     mov   COLTEST,al
                     mov   ayhaga,0
                     popa

      gooo:          POP   BX
                     POP   CX
                     POP   DX
                     

                     JMP   OVERJUMP
      ENDT:          
      ;SETTING CURSOR
                     POP   BX
                     POP   CX
                     POP   DX
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0FE08h
                     int   10h
      ;;;CLEARING PAST MESSAGES
                     MOV   AX,0
                     mov   AH,9                         ;INT 21/9
                     MOV   DX,OFFSET MESCLR
                     INT   21H
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,0FE08h
                     int   10h
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     MOV   BL,KINGKILL
                     SHL   BX,4
                     CMP   Bh,0fH
                     JZ    BLACKCOLWINS                 ;IF THE COLOR IS BLACK

      ;IF COLOR IS WHITE
                     MOV   MESEND,'HW'
                     MOV   MESEND[2],'TI'
                     MOV   MESEND[4],' E'
                     JMP   DISPLAYEND
      BLACKCOLWINS:  MOV   MESEND,'LB'
                     MOV   MESEND[2],'CA'
                     MOV   MESEND[4],' K'
      ;;;;;;;;;;;;
      ;SETTING CURSOR
      DISPLAYEND:    
                     mov   AH, 9
                     MOV   DX,OFFSET MESEND
                     int   21h
                     mov   finally,1
      ;    MOV   AH, 4CH
      ;    INT   21H

      OVERJUMP:      RET
MOVCOMP ENDP

TIMEGAME PROC FAR
                     PUSHA

                     mov   ax,00h
                     mov   bx,00h
                     mov   dx,00h
                     mov   cx,0

                     MOV   AH,2ch                       ;DH->SEC CL->MIN
                     INT   21H
                     mov   ah,00h

                     MOV   DL,DH
                     SUB   DH,InitialSEC
                     test  DH, DH
                     jNS   is_POSITIVE

                     CMP   CL,00h
                     JZ    MINRESET
                     DEC   CL
                     JMP   GOGOGO

      MINRESET:      MOV   CL,59D

      GOGOGO:        MOV   DH,60d
                     SUB   DH,InitialSEC                ;60 - INITIALSEC + VALUE FROM INT
                     ADD   DH,DL
                     JMP   is_POSITIVE



      is_POSITIVE:   
                     MOV   DL,0
                     cmp   DH,9
                     JA    twonumbersSEC
                     MOV   AL,'0'
                     ADD   DH,'0'
                     MOV   AH,DH
                     SUB   DH,'0'
                     jmp   FINDMIN



      twonumbersSEC: mov   al,DH
                     mov   Bl,10
                     div   Bl
                     ADD   AL,'0'
                     ADD   AH,'0'


      FINDMIN:       
                     mov   GAMETIMERSEC,al              ;SETTING SEC
                     mov   GAMETIMERSEC[1],ah           ;SETTING SEC

                     MOV   AX,0

                     MOV   CH,CL
                     SUB   CL,InitialMIN
                     TEST  CL,CL
                     JNS   IS_POSITIVE2
                     MOV   CL,60d
                     SUB   CL,InitialMIN                ;60 - INITIALMIN + VALUE FROM INT
                     ADD   CL,CH

      IS_POSITIVE2:  mov   ch,00h
                     cmp   CL,9
                     JA    twonumbersmin
                     MOV   AL,'0'
                     ADD   CL,'0'
                     MOV   AH,CL
                     jmp   print

      twonumbersmin: mov   al,CL
                     mov   Bl,10
                     div   Bl
                     ADD   AL,'0'
                     ADD   AH,'0'
      PRINT:         
                     mov   GAMETIMERMIN,al
                     mov   GAMETIMERMIN[1],ah


                     MOV   AX,0
                     mov   ah,2
                     mov   dx,00E5h
                     int   10h
                     mov   AH, 9
                     MOV   DX,OFFSET GAMETIMERMIN
                     int   21h
  
                     POPA
                     RET
TIMEGAME ENDP

      ;description
startSerial PROC

                     mov   dx,3fbh                      ; Line Control Register
                     mov   al,10000000b                 ;Set Divisor Latch Access Bit
                     out   dx,al                        ;Out it
      ;;;;;;;;;;;;
                     mov   dx,3f8h
                     mov   al,0ch
                     out   dx,al
      ;;;;;;;;;;;;;
                     mov   dx,3f9h
                     mov   al,00h
                     out   dx,al
      ;;;;;;;;;;;;
                     mov   dx,3fbh
                     mov   al,00011011b
                     out   dx,al
                     ret
startSerial ENDP
      ;description
chatting PROC
                     pusha
      ;call  startSerial
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     mov   cR, 1800h
                     mov   csend  ,0a00h
                     mov   value,'$'
                     mov   valueREc,'$'

                     mov   ah,0
                     mov   al,3h
                     int   10h
      ;;;;;;;;;;;; text mode
                     mov   ah,2
                     mov   bh,0
                     mov   dx,0b00h
                     int   10h
      ;;;;set cursor to draw
                     mov   ah, 9
                     mov   dx, offset line
                     int   21h
                     ;;;;;;;;;;;;;;;;;;;;
                       mov   ah,2
                     mov   bh,0
                     mov   dx,000h
                     int   10h
      ;;;;set cursor to draw
                     mov   ah, 9
                     mov   dx, offset username
                     int   21h
                     ;;;;;;;;;;;;;
                       mov   ah,2
                     mov   bh,0
                     mov   dx,0c00h
                     int   10h
      ;;;;set cursor to draw
                     mov   ah, 9
                     mov   dx, offset username2
                     int   21h
      ;;;;;;;;;;;;;;;;;;; draw the middle line
                     mov   ah,2
                     mov   bh,0
                     mov   dx,0a00h
                     int   10h
      ;;set cursor
      l:             
  

                     MOV   AL,0
                     MOV   BX,0
                     MOV   AH,1
                     INT   16H
                     CMP   AX,256
                     jz    other
      ;;;;check the buffer
                     mov   ah,0
                     int   16h
                     cmp   al,0
                     jz    gotoAH
                     mov   value,al
                     jmp   storeplz
      gotoAH:        mov   value,ah
      ;;;;;;;;;;;; clear the buffer
      storeplz:      cmp   al,13                        ;cmp with enter key
                     jz    scroll
                     cmp al,8
                     jz backed
      ;;;print te character
                     mov   ah,2
                     mov   bh,0
                     mov   dx,csend
                     int   10h
      ;;;;;;;;
                     mov   cx,1
                     mov   ah,2
                     mov   dl,al
                     int   21h

      ;;;;
      ;;get the cursor position
              
                     mov   dx,csend
                     inc   csend
                     inc   dl
                     cmp   dl,79d                       ;;end of the line
                     jnz   other
                     jz    scroll

                 jmp   l
      backed:
                     dec csend
                     mov   ah,2
                     mov   bh,0
                     mov   dx,csend
                     int   10h
                     mov   cx,1
                     mov   ah,2
                     mov   dl,' '
                     int   21h
                     jmp l15
      scroll:        
      ;;;;set the cursor position for the begining of the line
                     mov   ah,2
                     mov   bh,0
                     mov   dx,0a00h
                     int   10h
                     mov   csend,0a00h
      ;;;;;;;;;;;;;;;;scroll up te upper half
                     mov   ax,0601h
                     mov   bh,07
                     mov   cx,0100h
                     mov   dx,0a4fh
                     int   10h
      other:         
   
      ;;;;;;;;;;;;;                      ;;;;;;;;;;;;;;;;;;;;try serial
                     mov   dx , 3FDH                    ; Line Status Register
                     In    al , dx                      ;Read Line Status
                     test  al , 00100000b
                     jz    con
                     cmp   value,'$'
                     jz    con
                     mov   dx , 3F8H                    ; Transmit data register
                     mov   al,VALUE
                     out   dx , al
                     cmp   al,3Dh                       ;;compare with esc
                     jz    endt1
                     mov   value,'$'
      con:           
                     mov   dx , 3FDH                    ; Line Status Register
                     in    al , dx
                     test  al , 1
                     jz    cn2
                     mov   dx , 03F8H
                     in    al , dx
                     mov   VALUErec , al
      ;;;;;;;;;;;;
                     cmp   al,13                        ;cmp with enter key
                     jz    scrolldown
                     cmp   al,3Dh                       ;;compare with esc
      endt1:         jz    endt22
      cmp al,8
      jz bebacked
      ;;;;;;;;;;print
                     mov   ah,2
                     mov   bh,0
                     mov   dx,cR
                     int   10h
                     mov   cx,1
                     mov   ah,2
                     mov   dl,al
                     int   21h

      ;;;;
      ;;get the cursor position
                    
                     mov dx,cr
                     inc   cr
                     inc   dl
                     cmp   dl,79d                      ;;end of the line
                     jnz   l15
                   ;  cmp   dh,18h                       ;;;;cant remember
                     jz    scrolldown
      ;;;inc the x to pring the next value

        l15:             jmp   l
        bebacked:      dec cr
                     mov   ah,2
                     mov   bh,0
                     mov   dx,cr
                     int   10h
                     mov   cx,1
                     mov   ah,2
                     mov   dl,' '
                     int   21h
                     jmp l15
      scrolldown:    
      ;;;;set the cursor position for the begining of the line
                     mov   ah,2
                     mov   bh,0
                     mov   dx,1800h
                     mov   cr,1800h
                     int   10h
      ;;;;;;;;;;;;;;;;scroll up te upper half
                     mov   ax,0601h
                     mov   bh,07
                     mov   cx,0d00h
                     mov   dx,184fh
                     int   10h

      cn2:           
                     jmp   l

      endt22:        
                     popa
                     ret
chatting ENDP
      ;description
cll PROC
                     cmp   pressblack,2
                     jnz   ll4
                     call  CLEARHIGH
                     mov   pressblack,0
      ll4:           cmp   presswhite,2
                     jnz   ff2
                     call  CLEARHIGH
                     mov   presswhite,0
      ff2:           ret
cll ENDP

GAME PROC FAR
                     pusha
                     MOV   DI,OFFSET COLUMNBoard
                     MOV   SI,OFFSET ROWBoard
     
                     MOV   AH,0
                     MOV   AL,13H
                     INT   10H

                     MOV   AH,2ch                       ;DH->SEC CL->MIN
                     INT   21H
                     MOV   InitialMIN,CL
                     MOV   InitialSEC,DH
                     CALL  TIMEGAME
                     CALL  GRIDI
                     CALL  Cleardata
                     call  Initial
                     pusha
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,02e5h
                     int   10h
                     mov   ah, 9
                     mov   dx, offset username2
                     int   21h
                     MOV   AX,0

                     mov   csend,0ee3h
                     mov   cr,04e3h
                     mov   ah,2
                     mov   dx,0ce5h
                     int   10h
                     mov   ah, 9
                     mov   dx, offset username
                     int   21h
                     mov   ah,9                         ;Display
                     mov   ah,2
                     mov   dx,0be4h
                     int   10h
                     mov   ah, 9
                     mov   al,'-'                       ;Letter D
                     mov   cx,12                        ;5 times
                     mov   bl,0Ah                       ;Green (A) on white(F) background
                     int   10h

                     popa
      ;call  startSerial

      DRAWGRIDI:     
                     call  timerehigh
                     CALL  TIMEGAME
                     jmp   yuy
      ppff:          popa
                     jmp   chk22
      yuy:           
                     pusha
                     mov   dx , 3FDH                    ; Line Status Register
                     in    al , dx
                     AND   al , 1
                     JZ    ppff
      ;If Ready read the VALUE in Receive data register
                     mov   dx , 03F8H
                     in    al , dx
                     mov   valueREc , al
                     mov   who,0
                     xor   ayhaga,0fh
                     popa
                     jmp   chk22
      DRAWGRIDI2220: jmp   DRAWGRIDI

      chk22:         cmp   pressblack,2
                     jnz   ll
                     call  CLEARHIGH
                     mov   pressblack,0
      ll:            cmp   presswhite,2
                     jnz   ff
                     call  CLEARHIGH
                     mov   presswhite,0
      ff:            mov   freeze,0
                     MOV   DH,0
                     MOV   DL,ROWFirst
                     MOV   CL,COLFirst
                     MOV   COLORI,1
                     CALL  highlight
                     MOV   COLORI,4
                     MOV   DL,ROWSec
                     MOV   CL,COLSec
                     CALL  highlight

                     cmp   who,0
                     jz    ggg
                     MOV   AL,0
                     MOV   BX,0
                     MOV   AH,1
                     INT   16H
                     CMP   AX,256
                     JZ    DRAWGRIDI2220

                     MOV   AH,0
                     INT   16H
                     cmp   al,0
                     jnz   sendal
                     mov   value,ah
                     jmp   ggg22

      sendal:        mov   value,al

      ggg22:         pusha
      ;Checking buffer
                     mov   dx , 3FDH                    ; Line Status Register
                     In    al , dx                      ;Read Line Status
                     test  al , 00100000b
                     jz    kkkk
                     cmp   value,'$'
                     jz    kkkk                         ;
                     mov   dx , 3F8H                    ; Transmit data register
                     mov   al,value
                     out   dx , al
                     mov   value,'$'
      kkkk:          popa
                     jmp   ggg


      ;Q LETTER
      ggg:           cmp   who,0
                     jnz   gfgf
                     mov   al,valueREc
      gfgf:          CMP   AL,'0'
                     JZ    CHOOSE3
                     cmp   who,0
                     jnz   aa
                     mov   ah,valueREc
      aa:            CMP   Ah, 62D
                     JZ    stopgame


      ;1 white
                     CALL  HIGHL
                     call  cll
                     cmp   who,0
                     jnz   DRAWGRIDI22
                     xor   ayhaga,0fH
                     mov   who,1
      DRAWGRIDI22:   JMP   DRAWGRIDI

      choose3:       
                     cmp   ayhaga,0H
                     jnz   choose2
                  
                     push  ax
                     mov   al, COLsec
                     mov   ah,ROWsec
                     mov   COLTEST,AL
                     mov   ROWTEST,ah
                     mov   al,colhigh2
                     mov   ah,rowhigh2
                     MOV   COLHIGH,al
                     MOV   ROWHIGH,ah
                     pop   ax
                     jmp   choose

      stopgame:      jmp   MAINBACK
      choose2:       push  ax
                     mov   al,colhigh1
                     mov   ah,rowhigh1
                     MOV   COLHIGH,al
                     MOV   ROWHIGH,ah
                     mov   al, COLFirst
                     mov   ah,ROWFirst
                     mov   COLTEST,AL
                     mov   ROWTEST,ah
                     pop   ax

                     cmp   HIGHARRAYW[0],'$'
                     jnz   CHOOSE92

                     pusha
                     mov   cl,COLTEST
                     mov   dl,ROWTEST
                     call  calcArr
                     shl   ax,4
                     cmp   ah,0fh
                     jnz   notinc
                     inc   presswhite
      notinc:        popa
                     jmp   sas
    

      CHOOSE:        
                     CMP   HIGHARRAYB[0],'$'
              
      CHOOSE92:      JNZ   CHOOSE93
                     pusha
                     mov   cl,COLTEST
                     mov   dl,ROWTEST
                     call  calcArr
                     cmp   ax,0H
                     jz    notinc2
                     shl   ax,4
                     cmp   ah,00h
                     jnz   notinc2
                     inc   pressblack
      notinc2:       popa

      sas:           call  calc
                     push  ax
                     mov   ah,0
                     mov   al,Array1[si]
                     shl   ax,4
                     cmp   ah,ayhaga
                     jnz   notcolor
                     pop   ax
                     jmp   qqqqqqqqqqqqqq
      notcolor:      pop   ax
      
      breakjump:     call  cll
                     cmp   who,0
                     jnz   hgh23
                     xor   ayhaga,0fh
                     mov   who,1
      hgh23:         jmp   DRAWGRIDI
  
      CHOOSE93:      jmp   CHOOSE9
      qqqqqqqqqqqqqq:
      ;;;;;;;;;;;;;;;;;;;;;;;freeze
                     push  cx
                     push  dx
                     push  ax
                     
                     PUSH  SI
                     MOV   AX,SI
                     MOV   SI,2
                     MUL   SI
                     MOV   SI,AX
                     mov   dx,TIMERARRAY[si]
                     POP   SI
                     cmp   dx,00h
                     JNE   above
      back:          pop   ax
                     pop   dx
                     pop   cx
                     cmp   freeze,1
                     je    breakjump
      ;;;;;;;;;;;;;;freeze
                     cmp   ayhaga,0
                     jnz   wew
                     push  ax
                     mov   al,COLTEST
                     mov   ah,ROWTEST
                     mov   colhigh2,al
                     mov   rowhigh2,ah
                     pop   ax
                     jmp   asd
                     
      wew:           
                     push  ax                           ;;white
                     mov   al,COLTEST
                     mov   ah,ROWTEST
                     mov   colhigh1,al
                     mov   rowhigh1,ah
                     pop   ax
      asd:           CALL  HIGHCOMP

                     jmp   breakjump

      ;  MOV  SI,0
      CHOOSE9:       MOV   DI,0
       
                     cmp   ayhaga,0
                     jz    FINDB1
                     inc   presswhite
               
      FIND:          CMP   HIGHARRAYw[DI],'$'
                     jZ    rotate


                     PUSH  DI
    
      
                     MOV   DH,COLTEST
                     MOV   DL,ROWTEST

                     CMP   HIGHARRAYW[DI],DX
                     JZ    MOVE
     
                     POP   DI
                     ADD   DI,2
                     ADD   SI,2
      ;CALL MOVE FUNCTION
                     JMP   FIND
      above:         
                     cmp   ayhaga,0
                     jnz   cvcv22
                     dec   pressblack
      cvcv22:        dec   presswhite
                     mov   freeze,1
                     jmp   back

      FINDB1:        inc   pressblack

      FINDB:         
                     CMP   HIGHARRAYB[DI],'$'
                

                     jZ    rotate


                     PUSH  DI

      
                     MOV   DH,COLTEST
                     MOV   DL,ROWTEST

                     CMP   HIGHARRAYB[DI],DX
                     JZ    MOVE
                    

                     POP   DI
                     ADD   DI,2
                     ADD   SI,2
                     jmp   FINDB
      ;CALL MOVE FUNCTION
                 
      MOVE:          
                     CALL  MOVCOMP
                     pop   di
                     cmp   finally,1
                     jz    MAINBACK1
                    
      rotate:        
                     call  cll
      ;call CLEARHIGH
                     cmp   who,0
                     jnz   hgh
                     xor   ayhaga,0fh
                     mov   who,1
      hgh:           JMP   DRAWGRIDI
      MAINBACK1:     
                     mov   cx,0ffffH
      oopoo:         dec   cx
                     cmp   cx,00h
                     jnz   oopoo
                     mov   cx,0ffffH
      oopoo1:        dec   cx
                     cmp   cx,00h
                     jnz   oopoo5
                     mov   cx,0ffffH
      oopoo5:        dec   cx
                     cmp   cx,00h
                     jnz   oopoo1
                     mov   finally,0

       locked:        ;MOV   AH,0
      ;                INT   16H
      ;                CMP   Ah, 62D
      ;                JZ    MAINBACK
      ;                jmp   locked

      MAINBACK:      popa
                     ret

GAME ENDP

VIEWNOTF PROC
                     PUSHA
                     cmp   chatsend,1
                     jnz   nextnotf
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,1601h
                     int   10h
                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET sendchatmsg
                     int   21h

                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET username2
                     int   21h

      nextnotf:      cmp   chatinvite,1
                     jnz   nextnotf2
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,1601h
                     int   10h

                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET username2
                     int   21h

                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET chatinvitemsg
                     int   21h

      nextnotf2:     cmp   gamesend,1
                     jnz   nextnotf3
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,1701h
                     int   10h
                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET sendgamemsg
                     int   21h

                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET username2
                     int   21h
      nextnotf3:     
                     cmp   gameinvite,1
                     jnz   nextnotf4
                     MOV   AX,0
                     mov   ah,2
                     mov   dx,1701h
                     int   10h
                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET username2
                     int   21h

                     mov   ax,0
                     mov   ah,9
                     mov   dx,OFFSET gameinvitemsg
                     int   21h
      nextnotf4:     POPA
                     RET
VIEWNOTF ENDP

main proc far
                     mov   ax,@data
                     mov   ds,ax

                     mov   ah,0
                     mov   al,3h
                     int   10h
      ;;;;;;;;;;;
                     mov   ah,2
                     mov   dx,0206h
                     int   10h

                     call  startSerial
      ;                mov   ayhaga,0fh
          ;           call  chatting
      ; ;;;;;;;;
                     mov   ah, 9
                     mov   dx, offset welcome
                     int   21h
      ;;;;;;;;;;;
                     mov   ah,2
                     mov   dx,040ah
                     int   10h
                     mov   si,0
                     mov   di,0
                     mov   bx,offset username2
      l999:          
                     mov   ax,0
                     mov   ah,1
                     int   16h
                     cmp   keep,13
                     jz    other99
                     cmp   al,0
                     jz    other99
                     mov   ah,0
                     int   16h
                     mov   username[si],al
                     mov   keep,al
                     cmp   keep,13
                     jz    other99
                
                     inc   si

                     mov   cx,1
                     mov   ah,2
                     mov   dl,al
                     int   21h
      other99:       
   
      ;;;;;;;;;;;;;
                     cmp   username[di],'$'
                     jz    con22                        ;;;;;;;;;;;;;;;;;;;;try serial
                     mov   dx , 3FDH                    ; Line Status Register
                     In    al , dx                      ;Read Line Status
                     test  al , 00100000b
                     jz    con22
                     mov   dx , 3F8H                    ; Transmit data register
                     mov   al,username[di]
                     cmp   username[di],13
                     jnz   ppppp
                     mov   username[di],'$'
      ppppp:         out   dx , al
                     inc   di
      con22:         cmp   keep2,13
                     jz    dc
                     mov   dx , 3FDH                    ; Line Status Register
                     in    al , dx
                     test  al , 1
                     jz    l999
                     mov   dx , 03F8H
                     in    al , dx
                     cmp   al,13
                     jz    dc
                     mov   [bx],al
                     inc   bx
                     jmp   l999
      dc:            mov   keep2,13
                     cmp   keep,13
                     jz    cccccppp
                     jmp   l999

      cccccppp:      mov   ah,2
                     mov   dx,0C06h
                     int   10h
                     mov   ah, 9
                     mov   dx, offset keywait
                     int   21h
      ;;;;;;;;;;;
                     mov   ah,0
                     int   16h
      ;;;;;;;;;;;;;;;
                     mov   ax,0600h
                     mov   bh,07
                     mov   cx,0
                     mov   dx,184FH
                     int   10h

      mainloop:      

                     mov   ah,0
                     mov   al,3h
                     int   10h

      ;;;;;;;;;;;;;;;
                     mov   ah,2
                     mov   dx,1500h
                     mov   bx,0
                     int   10h
      ;;;;;;;
                     mov   ah,9
                     mov   bh,0
                     mov   al,'-'
                     mov   cx,80
                     mov   bl,0Fh
                     int   10h
      ; ;;;;;;;;;;;;;;;
                     mov   ah,2
                     mov   dx,40ch
                     int   10h
      ;;;;;;;;;;;;
                     mov   ah, 9
                     mov   dx, offset chatstart
                     int   21h
      ;;;;;;;;;;;
                     mov   ah,2
                     mov   dx,60ch
                     int   10h
      ;;;;;;;;;;;;
                     mov   ah, 9
                     mov   dx, offset gamestart
                     int   21h
      ;;;;;;;;;
                     mov   ah,2
                     mov   dx,80ch
                     int   10h
      ;;;;;;;;;;;;
                     mov   ah, 9
                     mov   dx, offset endall
                     int   21h
      ;;;;;;;;;;;;

                     Call  VIEWNOTF
    
      nomsg2:        MOV   AL,0
                     MOV   BX,0
                     MOV   AH,1
                     INT   16H
                     CMP   AX,256
                     jz    rece2
                     mov   ah,0
                     int   16h
                     mov   store,ah
                     mov   dx , 3FDH                    ; Line Status Register
                     In    al , dx                      ;Read Line Status
                     AND   al , 00100000b
                     JZ    ccccc
      ;If empty put the VALUE in Transmit data register
                     cmp   store,'$'
                     jz    ccccc
                     mov   dx , 3F8H                    ; Transmit data register
                     mov   al,store
                     out   dx , al
                     mov   store,'$'

      ;;;;;;;;;;;;;;
      ccccc:         cmp   ah,60d
                     jnz   notgame
                     mov   gamesend,1
                     cmp   gameinvite,1
                     jz    gotoGames
      ;    MOV   AX,0
      ;    mov   ah,2
      ;    mov   dx,1701h
      ;    int   10h
      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET sendgamemsg
      ;    int   21h
      ;    mov   al,0
      ;    mov   dx,OFFSET username2
      ;    int   21h
                     call  VIEWNOTF

      rece2:         jmp   rece

      gotoGames:     mov   ayhaga,00h
                     mov   gamesend,0
                     mov   gameinvite,0
                     call  game
                     jmp   mainloop
      notgame:       
                     cmp   ah,3Bh
                     jnz   notchat
                     mov   chatsend,1

                     cmp   chatinvite,1
                     jz    gotoChats

      ;    MOV   AX,0
      ;    mov   ah,2
      ;    mov   dx,1601h
      ;    int   10h
      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET sendchatmsg
      ;    int   21h

      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET username2
      ;    int   21h
                     call  VIEWNOTF
 
                     jmp   rece
      nomsg222:      jmp   nomsg2

      gotoChats:     mov   chatinvite,0
                     mov   chatsend,0
                     call  chatting
                     jmp   mainloop
      notchat:       
                     cmp   al,27d
                     jz    escape23
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      rece:          mov   dx , 3FDH                    ; Line Status Register
                     in    al , dx
                     test  al , 1
                     jz    nomsg222
                     mov   dx , 03F8H
                     in    al , dx
                     cmp   al,60d                       ;F2
                     jnz   notgame2
                     mov   gameinvite,1
                    
      ;    MOV   AX,0
      ;    mov   ah,2
      ;    mov   dx,1701h
      ;    int   10h
      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET username2
      ;    int   21h

      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET gameinvitemsg
      ;    int   21h

                     call  VIEWNOTF
 
                     cmp   gamesend,1
                     jz    gotoGame
                     jmp   nomsg2

      gotoGame:      mov   ayhaga,0fh
                     mov   gameinvite,0
                     mov   gamesend,0
                     call  game
                     jmp   mainloop
      escape23:      jmp   escape
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      notgame2:      cmp   al,3Bh                       ;F1
                     jnz   notchat2
                     mov   chatinvite,1

                     call  VIEWNOTF
      ;    MOV   AX,0
      ;    mov   ah,2
      ;    mov   dx,1601h
      ;    int   10h

      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET username2
      ;    int   21h

      ;    mov   ax,0
      ;    mov   ah,9
      ;    mov   dx,OFFSET chatinvitemsg
      ;    int   21h

                    
 
                     cmp   chatsend,1
                     jz    gotoChat

                     jmp   notchat2
                     

      gotoChat:      mov   chatinvite,0
                     mov   chatsend,0
                     call  chatting
                     jmp   mainloop

      notchat2:      cmp   al,27d
                     jz    escape
                     jmp   nomsg2
                 

      escape:        MOV   AH, 4CH
                     INT   21H

main endp
      ;description



END MAIN