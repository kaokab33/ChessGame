.286
.model SMALL
.stack 64
public DrawFiel
        public DrawPawn
        public dr
        public DrawHorse
        public Tabya
        public KING
        public Initial
.DATA
   ycounter  db 0
   counter  dw 0
   counter2 dw 0
   rowNo    dw ?
   colNo    dw ?
   color    db ?
        a           dw ?
        z           dw ?
        col         dw ?
        row         dw ?
.CODE


DrawPawn proc FAR
            mov  ycounter,0
            mov counter2,0
            add  cx,5          ;new dim of board
            sub  dx,5          ;new dim of board
            mov  rowNo, dx
            mov  colNo, cx
            mov  color,al
            mov  ah,0ch        ;Draw Pixel Command
            mov  bl,18
   base:    int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,18
            jnz  base
            mov  cx,colNo      ;Column
            dec  dx
            mov  ycounter,0
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   base2:   int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,bl
            jnz  base2
            inc  counter2
            dec  dx
            mov  cx,colNo
            add  cx,counter2
            mov  ycounter,0
            sub  bl,2
            cmp  bl,8
            jnz  base2
            mov  ycounter,0
            dec  cx
            add  bl,2
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   base3:   int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,bl
            jnz  base3
            dec  dx
            mov  ycounter,0
            mov  counter2,0
            mov  cx,colNo
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            add  cx,3
   row11:   int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,12
            jnz  row11
            inc  counter2
            dec  dx
            mov  ycounter,0
            mov  cx,colNo
            add  cx,3
            cmp  counter2,3
            jnz  row11
            mov  ycounter,0
            mov  counter2,0
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            inc  cx
            mov  bx,cx
            mov  bl,10
   d:       int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,bl
            jnz  d
            inc  counter2
            dec  dx
            mov  cx,colNo
            add  cx,counter2
            add  cx,4
            mov  ycounter,0
            sub  bl,2
            cmp  bl,4
            jnz  d
            mov  ycounter,0
            dec  cx
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   c:       int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,6
            jnz  c
            sub  cx,5
            dec  dx
            mov  ycounter,0
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   y:       int  10h
            inc  cx
            inc  ycounter
            cmp  ycounter,4
            jnz  y
            dec  dx
            sub  cx,2
            int  10h
            dec  cx
            int  10h
            ret
DrawPawn endp

liner proc
        line2:    int  10h
                  add  cx,di
                  int  10h
                  sub  cx,di
                  sub  cx,di
                  int  10h
                  add  cx,di
                  dec  di
                  cmp  di,0
                  jnz  line2
                  dec  dx
                  mov  di,a                         ;restart the row in the upper column
                  cmp  dx,si
                  jnz  line2
                  ret
liner endp
lr proc
        ;the number of lines note we add dx as we used line macro
        ;before it in the same row
        l3:       int  10h
                  add  cx,di
                  int  10h
                  sub  cx,di
                  sub  cx,di
                  int  10h
                  add  cx,di
                  dec  di
                  cmp  di,z                         ;the end of the line
                  jnz  l3
                  inc  bx
                  mov  di,a                         ;number of the repetition
                  dec  dx
                  cmp  dx,si
                  jnz  l3
                  ret
    
lr endp
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;s

dr proc FAR
                  add  cx,23
                  sub  dx,3
                  mov  ah,0ch                       ;draw command
        ;given the most right and down corner of the cell
       
                  mov  si,dx                        ;to compare with it
                  sub  cx,10d                       ;get the middle of it 20(the cell dimension)/2
         
         
                  mov  di,5                         ;half the lenght of the line
                  sub  si,3
                  mov  a,5
                  call liner
                  mov  di,6                         ;half the lenght of the line
                  sub  si,2
                  mov  a,6
                  call liner
                  mov  di,7                         ;half the lenght of the line
                  sub  si,2
                  mov  a,7
                  call liner
                  mov  di,3                         ;half the lenght of the line
                  sub  si,1
                  mov  a,3
                  call liner
       
                  mov  di,8                         ; start of the line
                  add  dx,1
                  mov  a,8
                  mov  z,5
        
                  call lr
  
                  mov  di,3                         ;half the lenght of the line
                  sub  si,2
                  mov  a,3
                  call liner
      
                  mov  di,9                         ; start of the line
                  add  dx,2
                  mov  a,9
                  mov  z,6
                  call lr
 
   
       
                  mov  di,2                         ;half the lenght of the line
                  sub  si,2
                  mov  a,2
                  call liner
       
                  mov  di,10                        ; start of the line
                  add  dx,2
                  mov  a,10
                  mov  z,7
                  call lr
   
  
      
                  mov  di,1                         ;half the lenght of the line
                  sub  si,1
                  mov  a,1
                  call liner
   
                  int  10h
    
                  mov  di,11                        ; start of the line
                  add  dx,2
                  mov  a,11
                  mov  z,8
                  call lr
        
  
     
                  mov  di,2                         ;half the lenght of the line
                  sub  si,2
                  mov  a,2
                  call liner
       
                  mov  di,3                         ;half the lenght of the line
                  sub  si,1
                  mov  a,3
                  call liner
       
                  mov  di,2                         ;half the lenght of the line
                  sub  si,1
                  mov  a,2
                  call liner
                  ret
dr endp


DrawHorse proc FAR

                  add  bx,2
                  add  di,3

                  mov  row ,bx
                  mov  col ,di


                  add  bx,19
                  add  di,2

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,18
        l1:       int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l1
        ;;;;;;;;;;;;;;;;;;;;;;;;

                  mov  bx,row
                  mov  di,col
                  add  bx,18
                  add  di,2
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,18
        l111:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l111
        ;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,17
                  add  di,2
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,18
        l112:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l112
        ;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,16
                  add  di,2
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,18
        l4:       int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l4
        ;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,15
                  add  di,14

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  bx,10
        l6:       int  10h
                  dec  dx
                  cmp  dx,bx
                  jnz  l6
        ;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,15
                  add  di,7
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,8
        l5:       int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l5
        ;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,13
                  add  di,7
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,8
        l55:      int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l55
        ;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,14
                  add  di,7
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,8
        l556:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l556
        ;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,12
                  add  di,7
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,8
        l558:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l558
        ;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,11
                  add  di,7
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,8
        l559:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  l559

        ;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,9
                  add  di,15
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  bx,5
        l7:       int  10h
                  dec  dx
                  cmp  dx,bx
                  jnz  l7
        ;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,10
                  add  di,15
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,14
        l77:      int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l77
        ;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,9
                  add  di,14
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,13
        l777:     int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l777
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,8
                  add  di,14
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,13
        l7777:    int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l7777
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,7
                  add  di,14
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,11
        l77777:   int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l77777
        ;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,6
                  add  di,14
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,10
        l77776:   int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l77776
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,5
                  add  di,15
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,10
        lp1:      int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  lp1
        ;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,4
                  add  di,14
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,10
        lp:       int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  lp
        ;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,3
                  add  di,13
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,12
        lp2:      int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  lp2
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,2
                  add  di,12
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,11
        l11:      int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  l11
        ;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,1
                  add  di,5
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  di,3
        lao:      int  10h
                  dec  cx
                  cmp  cx,di
                  jnz  lao
        ;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,2
                  add  di,2
                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  bx ,2
        lo:       int  10h
                  inc  dx
                  cmp  dx,bx
                  jnz  lo

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,11
                  add  di,1

                 
                  mov  dx,bx
                  mov  cx,di
                  mov  ah,0ch
                  int  10h

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,12

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  bx,2
        l23:      int  10h
                  inc  dx
                  cmp  dx,bx
                  jnz  l23

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,13
                  add  di,2

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  sub  bx ,4
        la:       int  10h
                  dec  dx
                  inc  cx
                  cmp  dx,bx
                  jnz  la
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,11
                  add  di,1

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,4
        laa:      int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  laa

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,12
                  add  di,1

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,3
        laa8:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  laa8
        ;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  bx,row
                  mov  di,col
                  add  bx,13
                  add  di,1

                  mov  ah,0ch
                 
                  mov  dx,bx
                  mov  cx,di
                  add  di,2
        laa7:     int  10h
                  inc  cx
                  cmp  cx,di
                  jnz  laa7
        ;;;;;;;;;;;;;;;;;;;;;;
 CMP AL,7
                  JZ SALAAAM
                  CMP AL,8
                  JZ SALAAAM
                  mov  bx,row
                  mov  di,col
                  add  bx,6
                  add  di,9
                  xor al,0fh
                  mov  ah,0ch 
                  mov  dx,bx
                  mov  cx,di
                  int  10h
                 SALAAAM: ret
DrawHorse endp

Tabya proc FAR

      
                  ;MOV  AL,BL
                  MOV  DX,0
                  MOV  DL,CL
                  MOV  CL,0
                  SHR  CX,8
                  SUB  DX,23
                  ADD  CX,5
                  
                  MOV  AH,0CH
        ;COLUMN IN CX
        ;ROW IN DX
                  INC  CX
                  ADD  DX,2
        
        
        
                  MOV  BL,4
        ;GATES/HEAD DRAWING
        
        HEAD:     INT  10H
                  INC  CX
                  INT  10H
                  INC  CX
        
                  ADD  CX,4
                  INT  10H
                  INC  CX
                  INT  10H
                  INC  CX
                  INT  10H
                  INC  CX
                  INT  10H
                  INC  CX
                  ADD  CX,4
                  INT  10H
                  INC  CX
                  INT  10H
        
                  INC  DX
                  SUB  CX,15
                  DEC  BL
                  CMP  BL,0
                  JNZ  HEAD
        
                  MOV  BL,16
                  MOV  BH,3
        ;CODE FOR RECTANGLE AFTER GATES
        UPBODY:   INT  10H
                  INC  CX
                  DEC  BL
                  CMP  BL,0
                  JNZ  UPBODY
                  MOV  BL,16
                  SUB  CX,16
                  INC  DX
                  DEC  BH
                  CMP  BH,0
                  JNZ  UPBODY
        
                  ADD  CX,2
        
                  MOV  BL,12
                  MOV  BH,9
        
        ;CODE FOR MIDDLE RECTANGLE
        MIDBODY:  INT  10H
                  INC  CX
                  DEC  BL
                  CMP  BL,0
                  JNZ  MIDBODY
                  MOV  BL,12
                  SUB  CX,12
                  INC  DX
                  DEC  BH
                  CMP  BH,0
                  JNZ  MIDBODY
                  SUB  CX,2
        
                  MOV  BL,16
                  MOV  BH,4
        ;CODE FOR DOWN RECTANGLE
        DOWNBOD:  INT  10H
                  INC  CX
                  DEC  BL
                  CMP  BL,0
                  JNZ  DOWNBOD
                  MOV  BL,16
                  SUB  CX,16
                  INC  DX
                  DEC  BH
                  CMP  BH,0
                  JNZ  DOWNBOD
                  ret
tabya endp

KING PROC FAR
       add  cx,23
                  sub  dx,3
                  mov  ah,0ch
    
         mov si,dx ;to compare with it 
         sub cx,10d ;get the middle of it 20(the cell dimension)/2
         
         mov di,4 ;half the lenght of the line
        sub si,1
        mov a,4     
      call liner 
    mov di,5 ;half the lenght of the line
        sub si,1
        mov a,5     
      call liner 
         mov di,6 ;half the lenght of the line
        sub si,1
        mov a,6     
       call liner
           mov di,7 ;half the lenght of the line
        sub si,1
        mov a,7     
       call liner
          mov di,8 ;half the lenght of the line
        sub si,1
        mov a,8     
       call liner 
        mov di,9 ;half the lenght of the line
        sub si,1
        mov a,9     
       call liner
        mov di,10 ;half the lenght of the line
        sub si,1
        mov a,10     
       call liner
         
           mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,3
        
        call lr  
                   mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,4
        
        call lr
                   mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10; start of the line 
        add dx,1
        mov a,10
        mov z,5
        call lr
        mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,6
        
        call lr
        
                   mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,7
        
        call lr
             mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,8
        call lr
             mov di,1 ;half the lenght of the line
        sub si,1
        mov a,1     
       call liner 
       
       mov di,10 ; start of the line 
        add dx,1
        mov a,10
        mov z,9
        
        call lr

          add  dx,3
          mov si, cx
          add si,4
          sub cx,3
          mov di,1
        qw:     int  10h
                  add  dx,di
                  int  10h
                  sub  dx,di
                  sub  dx,di
                  int  10h
                  add  dx,di
                  dec  di
                  cmp  di,0
                  jnz  qw
                  inc  cx
                  mov  di,1                       ;restart the row in the upper column
                  cmp  cx,si
                  jnz  qw
                  
        
                  ret     
KING ENDP

DrawFiel proc FAR
mov counter,0
                  add  cx,4                         ;new dim of board
                  sub  dx,3                         ;new dim of board
                  mov  rowNo, dx
                  mov  colNo, cx
                  mov  color,al
        ;LOCAL label1, label2, label3, label4 ,label5 , labelPlus
        ;pusha
                  mov  al,color                     ;Pixel color
                  mov  ah,0ch                       ;Draw Pixel Command
                  mov  cx,colNo                     ;Column
                  mov  dx,rowNo                     ;Row

                  inc  cx
                  int  10h
                  dec  dx
                  mov  bx,0
        label1:   
                  inc  cx
                  int  10h
                  inc  counter
                  cmp  counter,6
                  jnz  label1
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  counter,0
                  add  cx,4
        label2:   
                  inc  cx
                  int  10h
                  inc  counter
                  cmp  counter,6
                  jnz  label2
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  counter,0
                  inc  cx
                  inc  dx
                  int  10h
                  sub  dx,2
                  mov  cx,colNo
                  add  cx,8
                  int  10h
                  add  cx,3
                  int  10h
                  dec  dx
                  dec  cx
                  int  10h
                  dec  cx
                  int  10h
                  sub  cx,3
                  dec  dx
                  mov  bx,8
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        label3:   
                  int  10h
                  inc  cx
                  inc  counter
                  cmp  counter,bx
                  jnz  label3

                  sub  cx,counter
                  dec  cx
                  mov  counter,0
                  dec  dx
                  add  bx,2
                  cmp  bx,16
                  jnz  label3
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  mov  counter , 0
                  mov  bx,0
        label4:   
                  inc  cx
                  inc  bx
                  int  10h
                  cmp  bx,14
                  jnz  label4
                  sub  cx,bx
                  mov  bx,0
                  inc  counter
                  cmp  counter,2
                  jnz  label4
                  add  cx,2
                  mov  counter , 0
                  mov  bx,12
                  dec  dx
        ;;;;;;;;;;;;;;;;;;;;;
        label5:   
                  int  10h
                  inc  cx
                  inc  counter
                  cmp  counter,bx
                  jnz  label5
              
                  sub  cx,counter
                  inc  cx
                  mov  counter,0
                  dec  dx
                  sub  bx,2
                  cmp  bx,2
                  jnz  label5
                   CMP AL,7
                  JZ BYEEEEEEE
                  CMP AL,8
                  JZ BYEEEEEEE
                  add  dx ,5
                  ;mov  al,color                         ;Pixel color
xor al,0fh
                  int  10h
                  inc  cx
                  int  10h
                  inc  dx
                  sub  cx,2
                  mov  counter ,0
                  mov  bx,0
                 
        labelPlus:
                  int  10h
                  inc  cx
                  inc  counter
                  cmp  counter,4
                  jnz  labelPlus
                  sub  cx,2
                  inc  dx
                  int  10h
                  dec  cx
                  int  10h
        ;popa
                 BYEEEEEEE: ret
DrawFiel endp

Initial proc FAR
PUSH AX
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       mov Array1[0],1H
       mov Array1[1],2H
       mov array1[2],3H
       mov array1[3],5H
       mov Array1[4],4H
       mov Array1[5],3H
       mov Array1[6],2H
       mov Array1[7],1H

push BX
mov bx,8D
backtobasic: MOV Array1[BX],6H ;BLACK SOLIDERS
INC BX
CMP BX,16D
JNZ backtobasic
;BX=16D NOW

backtobasic2: MOV Array1[BX],0H 
INC BX
CMP BX,48D
JNZ backtobasic2
;BX = 48D NOW

backtobasic3: MOV Array1[BX],0F6H ;BLACK SOLIDERS
INC BX
CMP BX,56D
JNZ backtobasic3
POP BX

      mov Array1[56],0F1H
       mov Array1[57],0F2H
       mov array1[58],0F3H
       mov array1[59],0F5H
       mov Array1[60],0F4H
       mov Array1[61],0F3H
       mov Array1[62],0F2H
       mov Array1[63],0F1H
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            MOV  CH,0
            MOV  CL,184
            MOV  AL,0fh                     ;white color
            call Tabya
            MOV  CH,196
            MOV  CL,184
            MOV  AL,0fh                     ;white color
            call Tabya
            mov  bx,161
            mov  di,28
            mov  al,0fh
            call DrawHorse
            mov  bx,161
            mov  di,168
            mov  al,0fh
            call DrawHorse
            mov  cx,56
            mov  dx,184
            mov  al,0fh
            call DrawFiel
            mov  cx,140
            mov  dx,184
            mov  al,0fh
            call DrawFiel
            mov  cx,84
            mov  dx,184
            mov  al,0fh
            call dr
            mov  cx,112
            mov  dx,184
            mov  al,0fh
            call KING
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            MOV  CX,0
            mov  dx,161
            MOV  al,0fh                     ;white color
            call DrawPawn
            mov  dx,161
            MOV  cx,196
            MOV  al,0fh                     ;white color
            call DrawPawn
            mov  dx,161
            mov  cx,28
            call DrawPawn
            mov  dx,161
            mov  cx,168
            call DrawPawn
            mov  cx,56
            mov  dx,161
            mov  al,0fh
            call DrawPawn
            mov  cx,140
            mov  dx,161
            mov  al,0fh
            call DrawPawn
            mov  cx,84
            mov  dx,161
            mov  al,0fh
            call DrawPawn
            mov  cx,112
            mov  dx,161
            mov  al,0fh
            call DrawPawn
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            MOV  CH,0
            MOV  CL,23
            MOV  AL,00h                     ;black color
            mov Array1[0],1H
            call Tabya
            MOV  CH,196
            MOV  CL,23
            MOV  AL,00h                     ;black color
            call Tabya
            mov  bx,0
            mov  di,28
            mov  al,00h
            mov Array1[1],2H
            call DrawHorse
            mov  bx,0
            mov  di,168
            mov  al,00h
            call DrawHorse
            mov  cx,56
            mov  dx,23
            mov  al,00h
            call DrawFiel
            mov  cx,140
            mov  dx,23
            mov  al,00h
            call DrawFiel
            mov  cx,84
            mov  dx,23
            mov  al,00h
            call dr
            mov  cx,112
            mov  dx,23
            mov  al,00h
            call KING
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            MOV  CX,0
            mov  dx,46
            MOV  al,00h                     ;black color
            call DrawPawn
            mov  dx,46
            MOV  Cx,196
            MOV  al,00h                     ;black color
            call DrawPawn
            mov  dx,46
            mov  cx,28
            mov  al,00h
            call DrawPawn
            mov  dx,46
            mov  cx,168
            mov  al,00h
            call DrawPawn
            mov  cx,56
            mov  dx,46
            mov  al,00h
            call DrawPawn
            mov  cx,140
            mov  dx,46
            mov  al,00h
            call DrawPawn
            mov  cx,84
            mov  dx,46
            mov  al,00h
            call DrawPawn
            mov  cx,112
            mov  dx,46
            mov  al,00h
            call DrawPawn
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      POP AX

ret
Initial endp