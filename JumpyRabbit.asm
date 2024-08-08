; M Hamid Javed (22L-6704)
; Salman Mehmood (22L-6586)

[org 0x0100]

jmp start

seconds: dw 0
timerflag: dw 0
jump_flag: db 0
resume_flag: db 0
timerBrickFlag: db 0
brikCurr: dw 20
brikStartPos: db 0
brikRange: dw 20, 40
brikfinalPos:dw 40
brikCol: dw 50
brikrow: dw 41
brik2Col: dw 50
brik2row: dw 34
flag: db 0
row : dw 43
col : dw 132
score: dw 0


oldkb: dd 0
oldTimer: dd 0

rabitPos: dw 66
carrot: dw 70,76,58,65
index: dw 2
brikpos: dw 50


colorsRange: dw 0x1107, 0x2207, 0x4407, 0x5507, 0x6607, 0x0007
colorIndex1: dw 0
colorIndex2: dw 0



;...............................................


delay: push cx
       mov cx, 0xFFFF
loop1: loop loop1
       pop cx
       ret  
 
DesiredLocation:
           push bp
           mov bp,sp    
           mov al, [col] ; load al with columns per row
           mul byte [bp+6] ; 132 x r
           add ax, [bp+4] ; word number (132xr) + c
           shl ax, 1 ; byte no (((132xr) + c)x2)
           mov [bp+8],ax
           pop bp
           ret 4
   
Division: push bp    
          mov bp,sp
    push es
    push ax
    push di
    push si
   
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
    mov ah,0x77
    mov al,' '
    mov si,di
    add si,264
NextLoc: mov word[es:di],ax
          add di,2
          cmp di,si
          jne NextLoc
           
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2

Bricks: 
    push bp    
    mov bp,sp
    push es
    push ax
    push di
    push si
   
    mov ax,0xb800
    mov es,ax
    mov di,[bp+6]
    mov ax,[bp+4]
    mov si,di
    add si,60
NextLocation:  
          mov word[es:di],ax
          add di,2
          cmp di,si
          jne NextLocation
           
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 4
		  
Carrot: 
    push bp    
    mov bp,sp
    push es
    push ax
    push di
    push si
   
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
	mov ah,0x22
	mov al,' '	  
    mov word[es:di],ax
	add di,2
    mov word[es:di],ax
	add di,262
	mov ah,0x44
    mov word[es:di],ax
	add di,2	  
    mov word[es:di],ax	
           
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2	  

sky: push bp    
     mov bp,sp
    push es
    push ax
    push di
    push si
   
    mov ax,0xb800
    mov es,ax
    mov si,[bp+4]
    mov ah,0x30
    mov al,' '
    mov di,0
sky_loop: mov word[es:di],ax
          add di,2
          cmp di,si
          jne sky_loop
           
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2

Bird: push bp
      mov bp,sp
   push es
   push ax
   push di
   
   mov ax,0xb800
   mov es,ax
     mov di,[bp+4]
   mov ah,0x30
   mov al,'\'  
      mov word[es:di],ax
      add di,2
      mov al,'/'  
      mov word[es:di],ax

   pop di
   pop ax
   pop es
   pop bp
   ret 2
   
sun: push bp
     mov bp,sp
     push es
     push ax
     push di
     mov ax, 1003h
     mov bx, 0 ; disable blinking.
     int 10h
     mov ax, 0xb800
     mov es, ax ; point es to video base
     mov di, [bp+4]  
     mov ah, 0x3e

     mov al, '\'
     mov word [es:di], ax
     add di, 2
     mov al,'|'
     mov word [es:di], ax
     add di, 2
     mov al,'/'
     mov word [es:di], ax
     add di,264
     sub di,6
     mov al, '-'
     mov word [es:di], ax
     add di, 2
     mov al, '-'
     mov word [es:di], ax
     add di, 2
     mov al,'@'
     mov word [es:di], 0xec40
   
     add di, 2
     mov al, '-'
     mov word [es:di], ax
     add di, 2
     mov al,'-'
     mov word [es:di], ax
 
     add di,264
     sub di,6
     mov al, '/'
     mov word [es:di], ax
     add di, 2
     mov al,'|'
     mov word [es:di], ax
     add di, 2
     mov al,'\'
     mov word [es:di], ax
 
     pop di
     pop ax
     pop es
     pop bp
     ret 2

mountain: push bp
          mov bp,sp
          push es
          push ax
          push di
          push cx
          mov ax, [row]
          sub ax,1
          mov bx,2
          mul bx
          mul word[col]
          mov bx,3
          div bx
          mov cx,ax
          mov ax, 0xb800
          mov es, ax ; point es to video base
          mov di, [bp+4] ; point di to top left column
          mov ah, 0x68
          mov al, '-'  
          add di,264
          add di,264
          add di,264
          mov si,di
          add si,cx
          mov cx, 0
loop3:
      add cx, 4 ; move to next screen location
      add di, 264      
      cmp di,si
      ja el
      jne left

left: sub di, cx
      shl cx,1
      push si
      mov si,2
   
l4:  
   mov word [es:di], ax  
   add di,2
   add si,2
   cmp si, cx
   jna l4
   shr cx,1
   sub di,cx
   pop si
   jmp loop3    
               
el:pop cx  
   pop di
   pop ax
   pop es
   pop bp
   ret 2
   
road: push bp
      mov bp,sp
      push es
      push ax
      push di
	  push cx

      mov ax, 1003h
      mov bx, 0 ; disable blinking.
      int 10h
      mov ax, 0xb800
      mov es, ax ; point es to video base
      mov si, [bp+4] ; point di to top left column
      mov ah, 0x80
      mov al, ' '
      mov di,si
      shr di,1  
      add si,264
      mov cx,0
road_lp:
         mov word [es:di], ax ; clear next char on screen
         add di, 2
         cmp di,si
         jne road_lp
         mov ax, 21
         mov bx, 132
         mul bx
         shl ax,1
         mov si, ax ; point di to top left column
         mov ah, 0x77
         mov al, ' '
         mov di,si
         add si,264
line_lp:
          mov word [es:di], ax ; clear next char on screen
          inc cx
		  cmp cx,7
		  jae skip
          add di, 2
          cmp di,si
          jne line_lp
	
skip: mov cx,0
      add di,14
	  cmp di,si
	  jae end
      jmp line_lp

end:      pop cx	  
          pop di
          pop ax
          pop es
          pop bp
          ret 2
		  
Rabbit: 
    push bp    
    mov bp,sp
    push es
    push ax
    push di
    push si
	push cx
   
    mov ax,0xb800
    mov es,ax
    mov di,[bp+4]
	push di
   
    mov ax,0x7720
    mov si,di
    add si,16
	mov cx,3
	
NextLocation1:  
          cmp cx,0
		  je end1
          mov word[es:di],ax
          add di,2
          cmp di,si
          jne NextLocation1			  
          dec cx
		  add di,264
		  sub di,16
		  add si,264
		  jmp NextLocation1
		  
end1:	  pop di
		  sub di,268
		  mov word[es:di],ax
		  add di,2
		  mov word[es:di],ax
		  add di,18
		  mov word[es:di],ax
		  add di,2
		  mov word[es:di],ax
		  mov ah,0x07
		  add di,514
		  mov word[es:di],ax
		  add di,6
		  mov word[es:di],ax
     		  
		  pop cx
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2	  
   
car: push bp
   mov bp,sp
   push es
   push ax
   push di
   push cx
   push bx
   mov cx,0
   mov ax,[row]
   sub ax,1
   mov bx, [col]
   mul bx
   shl ax,1
   mov bx,3
   div bx
   mov cx, ax
   mov ax, 0xb800
   mov es, ax ; point es to video base
   mov di, [bp+4]
   add di, cx
   shl cx,1
   mov ah, 0x13
   mov al, "_"
   mov cx,di
   add cx, 264
   add cx, 264
 
   mov bx,0
   mov dx,2
l3:
   
   add dx,8
   mov si,di
   sub si,dx
   mov bx, si
   add bx,4
   add bx,dx
   add bx,dx
l2:
   mov word[es:si],ax
   add si,2
   cmp si, bx
   jna l2  
   add di,264
   cmp di,cx
   jnae l3
   sub di,8
   mov ah, 0x8b
   mov al,"O"
   mov word[es:di],ax
   add di,20
   mov word[es:di],ax
   
  pop bx
  pop cx  
  pop di
  pop ax
  pop es
  pop bp
  ret 2

grass: push bp
       mov bp,sp
       push es
       push ax
       push di

       mov ax, 0xb800
       mov es, ax ; point es to video base
       mov si, [bp+4] ; point di to top left column
       mov ah, 0x82
       mov di,si
       shr di,1
       add si,264
grass_lp:
          mov word [es:di], ax ; clear next char on screen
          add di, 2
          jne grass_lp ; if no clear next position
          pop di
          pop ax
          pop es
          pop bp
          ret 2
 
   
fun_Division: mov ax,[row]
              sub ax,1
              mov bx, [col]
              mul bx
              shl ax,1
              mov bx,3
              div bx
              push ax
              call Division
              add ax,ax
              push ax
              call Division
              ret
     
Print_sky:    mov ax,14
              mov bx, [col]
              mul bx
              shl ax,1
              push ax
              call sky
              ret

Print_Bird:
             mov ax,0xCCCC
             push ax
             mov ax,6
             push ax
             mov ax,10
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Bird
   
             mov ax,0xCCCC
             push ax
             mov ax,9
             push ax
             mov ax,125
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Bird
   
             mov ax,0xCCCC
             push ax
             mov ax,4
             push ax
             mov ax,40
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Bird
   
             mov ax,0xCCCC
             push ax
             mov ax,5
             push ax
             mov ax,62
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Bird
   
             mov ax,0xCCCC
             push ax
             mov ax,4
             push ax
             mov ax,90
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Bird    
   
             ret
			 
Print_Brick:
			push si
				mov ax,0xCCCC
             push ax
             mov ax,[brikrow]
             push ax
             mov ax,[brikCol]
             push ax
             call DesiredLocation
			 pop ax
             push ax
			 mov si,word[colorIndex1]
			
			 mov ax,[colorsRange+si]
            
			 push ax
			 call Bricks
			
			 
			 mov ax,0xCCCC
             push ax
             mov ax,[brik2row]
             push ax
             mov ax,[brik2Col]
             push ax
             call DesiredLocation
			 pop ax
             push ax   
			 mov si,[colorIndex2]
             mov ax, [colorsRange+si]
     
			 push ax
			 call Bricks
			 pop si
			 ret
             			 

Print_Sun:   mov ax,0xCCCC
             push ax
             mov ax,0

             push ax
             mov ax,115
             push ax
             call DesiredLocation
             pop ax
             push ax
             call sun
             ret
   
Print_Mountain: mov ax, 40
                 push ax
                 call mountain
                 mov ax, 100
                 push ax
                 call mountain
                 mov ax, 160
                 push ax
                 call mountain
                 mov ax, 208
                 push ax
                 call mountain
                 ret  

Print_Road:   mov ax,[row]
              sub ax,1
              mov bx, [col]
              mul bx
              shl ax,1
              mov bx,3
              div bx
              add ax,ax
              push ax
              call road
              ret


carCol: dw 30
Print_Carrot:
			push ax
			push si
			mov si,[index]
			
             mov ax,0xCCCC
             push ax
             mov ax,[carCol]
             push ax
             mov ax,[carrot+si]
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Carrot
			 
			
			 pop si
			 pop ax
			 ret
     
Print_Rabbit:
			 push ax
             mov ax,0xCCCC
             push ax
             mov ax,37
             push ax
             mov ax, word[rabitPos]
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			 pop ax
			 ret
	 

Print_Car:  push ax
			mov ax,0xCCCC
             push ax
             mov ax,9
             push ax
             mov ax,30
             push ax
             call DesiredLocation
             pop ax
             push ax
             call car
   
             mov ax,0xCCCC
             push ax
             mov ax,4
             push ax
             mov ax,40
             push ax
             call DesiredLocation
             pop ax
             push ax
             call car
			 
			 mov ax,0xCCCC
             push ax
             mov ax,4
             push ax
             mov ax,100
             push ax
             call DesiredLocation
             pop ax
             push ax
             call car
   
             mov ax,0xCCCC
             push ax
             mov ax,10
             push ax
             mov ax,100
             push ax
             call DesiredLocation
             pop ax
             push ax
             call car
			 pop ax
             ret

Print_Grass:  mov ax,[row]
              sub ax,1
              mov bx, [col]
              mul bx
              shl ax,1
              mov bx,3
              div bx
              add ax,ax
              add ax,ax
              push ax
              call grass
              ret  

MoveScreen:
   push bp
   mov bp,sp
   push bx
   push ax
   push es
   push ds
   push di
   push si
   push cx
   
   
   mov ax,[bp + 8]
   mov bx,2
   mul bx
   mov bx,132
   mul bx
   mov di,ax
   mov si,ax
   add si,2
   mov ax,0xb800
   mov ds,ax
   mov es,ax
   mov bx,[bp+6]
   cmp word[bp+4],1
   jne move_loop2
   
   
    add di,262
     add si,258
   move_loop7:
	
	mov cx,131
     mov dx,[es:di]
     std
     rep movsw
     mov [es:di],dx
     add si,264
     add di,264
     add di,262
     add si,262
     
	; call Print_Car
     dec bx
	 cmp bx,0
     jne move_loop7
	  jmp end5
   
move_loop2: mov cx,131
     mov dx,[es:di]
     cld
     rep movsw
     mov [es:di],dx
     add si,2
     add di,2
     dec bx
	 cmp bx,0
     jne move_loop2
	
	 

	 
end5:
   pop cx
   pop si
   pop di
   pop ds
   pop es
   pop ax
   pop bx
   pop bp
   
   ret 6
   
;...................................board



MoveBoard:
   push bp
   mov bp,sp
   push ax
   push es
   push ds
   push di
   push si
   push cx
   push bx

   mov ax,[bp + 4]
   mov bx,2
   mul bx
   mov bx,132
   mul bx
 
   mov di,ax
   mov si,ax
  
   mov ax,0xb800
   mov ds,ax
   mov es,ax
   
   cmp byte[bp+6],1
   jne leftm
	add di,262
    add si,260
	  ;sub si,2
	  std
	  jmp skip_mov
	leftm:  
	    add si,2
		cld
	 
  skip_mov:
	mov cx,131
     mov dx,[es:di]
	 
	 
    ; std
     rep movsw
     mov [es:di],dx
  
	pop bx
   pop cx
   pop si
   pop di
   pop ds
   pop es
   pop ax
   pop bp
   ret 4      
   
   
Move_Board1: 
	
;loop2: 
	 ;mov cx,5
	 cmp cx,20
	 ja l5
     push bx
     call MoveBoard
	 call delay
	
    
	inc cx
	jmp e1
	 
	
l5: 
	push bx
     call MoveBoard 
    call delay		 
	 inc cx
	 cmp cx,40
	 jna e1
	; mov cx,0
e1:	 
	 ;jmp loop2
	 ret 
	 
Move_Board2: 
	
;loop2: 
	 ;mov cx,5
	
	
	 mov cx,word[brikCurr]
	 cmp cx,word[brikfinalPos]
	 jne Check1
		cmp byte[brikStartPos],1
		je Check1_pre
			mov byte[brikStartPos],1
			mov ax,[brikRange]
			sub ax,30
			jmp check1_assign
		Check1_pre:
			mov ax,[brikRange+2]
			mov byte[brikStartPos],0
			
			
		check1_assign:
			mov word[brikfinalPos],ax
			
		
	
	Check1: 
	
	 cmp byte[brikStartPos],0
	 jne Leftmove
	 
	 add word[brikCurr],1
	 push 1
     push bx
     call MoveBoard
	 add word[brikCol],1
	 add word[brik2Col],1
	sub bx,1
	push 1
	 push bx
     call MoveBoard
	sub bx,1
	push 1
	 push bx
     call MoveBoard
	 sub bx,1
	
	push 1
	push bx
     call MoveBoard
	 sub bx,1
	 push 1
	 push bx
     call MoveBoard
	 add word[rabitPos],1
	 call delay
		
    
	
	jmp e2
	 
	
Leftmove: 
	sub word[brikCurr],1
	push 0
	push bx
     call MoveBoard
	 sub word[brikCol],1
	 sub word[brik2Col],2
	sub bx,1
	push 0
	push bx
     call MoveBoard
	 sub bx,1
	 push 0
	push bx
     call MoveBoard
	sub bx,1
	push 0
	push bx
     call MoveBoard
	 sub bx,1
	 push 0
	push bx
     call MoveBoard
	 sub word[rabitPos],1
    call delay		 
	 
	 
	 ;mov cx,0
e2:	  
	 ;jmp loop2
	 ret 	 



PlayAnimation:
	
	push cx
	push ax
	push dx
l1:	
	cmp byte[flag],0
			jne retu
			
	cmp byte[jump_flag],1
		jne jump_skip
			mov byte[jump_flag],0
			call movement
		jump_skip:
		
		cmp byte[resume_flag],1
		jne resume_skip
		again_ask:
			push ax
			mov ah,0
			int 0x16
			cmp al,'t'
			jne again_ask 
			mov byte[resume_flag],0
			pop ax
		resume_skip:
	
     push 4
	 push 17
	 push 0
     call MoveScreen
	 
     push 23
	 push 5
	 push 1
     call MoveScreen 
	; call Print_Car
		call delay
		call delay
		mov cx,0
		; mov bx, 33
		; call Move_Board1
		mov byte[timerBrickFlag],0
		cmp word[colorIndex2],0
		jne skip_movement
		cmp word[brik2row],41
		jne skip_movement
			mov cx,1
			jmp skip_movement2
		skip_movement:
		cmp word[colorIndex1],0
		jne skip_movement2
		cmp word[brikrow],41
		jne skip_movement2
			mov cx,1
			
		skip_movement2:
			cmp cx,1
			je skip_move
				mov bx, 40
			call Move_Board2
			
			jmp l1
		skip_move:
		mov byte[timerBrickFlag],1;
		;mov word[seconds],0
		
			
		jmp l1
	retu:	
		
		pop dx
		pop ax
		pop cx
		ret
	

	
   
PrintMainScreen:
   
     call Print_sky
     call Print_Bird
     call Print_Sun
     call Print_Mountain
     call Print_Road
     call Print_Car
     call Print_Grass
	 call SaveScreenInBuffer
	 
	 
	
     call fun_Division
	 call Print_Brick
	 mov cx,6
			mov ax,3
			tree_loop3:
			push 12
			push ax
			call tree
			push 12
			mov bx,131
			sub bx,ax
			push bx
			call tree
			add ax,10
			loop tree_loop3
			
	 mov ax,0xCCCC
             push ax
             mov ax,70
             push ax
             mov ax,38
             push ax
             call DesiredLocation
			 pop ax
             push ax   
			 mov si,[colorIndex2]
             mov ax, [colorsRange+si]
     
			 push ax
			 call Bricks
	 
	 ;call Print_Carrot
	 call Print_Rabbit
	 
     ret

SaveScreenInBuffer:  
			push ax
			push cx
			push si
			push di
			push es
			push ds
			push bx
				
			
			mov ax,29
			mov bx,132
			
			mul bx
			shl ax,1
			mov si, ax
			mov di, 0
			

			mov ax, 0xb800
			mov ds, ax
			mov ax, screen
			mov es, ax


			mov cx, 1892
			cld
			rep movsw
			pop bx
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			ret 
	 
 scrollup:	push bp
			mov bp,sp
			push ax
			push cx
			push si
			push di
			push es
			push ds
			push bx	
				
			mov ax,13
			mov bx,132
			
			mul bx
			shl ax,1
			mov si, ax
			mov di, 0

			mov ax, screen;0xb800
			mov ds, ax
			mov ax, buffer
			mov es, ax


			mov cx, 132
			cld
			rep movsw
			
			mov bl,[bp+4]
			sub bl,29
			mov ax, 132 ; load chars per row in ax
			mul bl ; calculate source position
			mov di, ax 
			shl di, 1 ; convert to byte offset

			mov cx, 1892 ; number of screen locations
			sub cx, 132 ; count of words to move

			mov ax, screen;0xb800;[buffer]
			mov es, ax ; point es to video base
			mov ds, ax ; point ds to video base
		
			mov si,di
			sub si,264
			add si,262
			add di,262; point di to top left column
			std ; set auto increment mode
			rep movsw ; scroll up
			;[es:di] = [ds:si]
			mov ax,0
			mov bx,132
			mul bx
			shl ax,1

			mov di, ax
			mov si, 0


			mov ax, screen;0xb800
			mov es, ax
			mov ax, buffer
			mov ds, ax

			mov cx, 132
			cld
			rep movsw 
			
			mov ax,29
			mov bx,132
			
			mul bx
			shl ax,1
			mov di, ax
			mov si, 0
			

			mov ax, 0xb800
			mov es, ax
			mov ax, screen
			mov ds, ax


			mov cx, 1892
			cld
			rep movsw
			
			pop bx
			pop ds
			pop es
			pop di
			pop si
			pop cx
			pop ax
			pop bp
			ret 2
			
			
			
			
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, [bp+6] ; point di to 70th column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x87 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax 
pop es
pop bp
ret 4


movement:  
		
		push ax
		push bx
		push cx
		push si
		push dx
		mov word[brikCol],50
		mov word[brik2Col],50
		mov cx,7
		
		mov si,[index]
	    mov ax,word[carrot+si]
		cmp ax,word[rabitPos]
		jnae next_index
		mov dx,word[rabitPos]
		add dx,8
		cmp ax,dx
		ja next_index
		add word[score],1
next_index:
		push si
		push 4
		call GenRandNum
		pop si
							
			
		shl si,1
			
		mov word[index],si
		xor si,si
		push si
		push 6
		call GenRandNum
		pop si
		mov di,si
				 
				
			
loop5:	
			mov bx,42
			push bx
			
            call scrollup
			call Print_Brick
			call Print_Carrot
			
			 cmp word[brik2row],42
			 jne skip_brik2
				mov word[brik2row],28
				
				shl di,1
				mov [colorIndex2],di
			 
			skip_brik2:
			 cmp word[brikrow],42
			 jne skip_reset
				mov word[brikrow],28
				
				shl di,1
				mov [colorIndex1],di
				
				
			 
			skip_reset:
			 add word[brikrow],1
			 add word[brik2row],1
			
			call Print_Rabbit
			
			push 29*132*2+130*2
			push word[score]
			 call printnum;
			call delay
		
			
				
			cmp cx,2
			jna ig
			push 4
			push 17
	        push 0
			call MoveScreen
			push 23
			push 5
	        push 1
			call MoveScreen
			
ig:         loop loop5
			mov ax,word[rabitPos]
			
			 add ax,10
			 cmp ax,word[brikCol]
			 ja end6
			  mov byte[flag],1
			  jmp end7
end6:        mov dx,word[brikCol]
			 add dx,30
			 sub ax,10
			 cmp ax,dx
			 jnae end7
			mov byte[flag],1
			
end7:
			pop dx
			pop si
			pop cx
			pop bx
			pop ax
			ret	
	
printstr:	push bp
			mov bp, sp
			push es
			push ax
			push cx
			push si
			push di

			mov ax, 0xb800
			mov es, ax				; point es to video base

			mov al, 132				; load al with columns per row
			mul byte [bp+12]		; 80 x r
			add ax, [bp+10]			; word number (80xr) + c
			shl ax, 1				; byte no (((80xr) + c)x2)

			mov di, ax				; point di to required location
			mov si, [bp+6]			; point si to string
			mov cx, [bp+4]			; load length of string in cx
			mov ah, [bp+8]			; load attribute in ah

nextchar:	mov al, [si]			; load next char of string
			mov [es:di], ax			; show this char on screen
			add di, 2				; move to next screen location
			add si, 1				; move to next char in string
			call delay
			loop nextchar			; repeat the operation cx times
			

			pop di
			pop si
			pop cx
			pop ax
			pop es
			pop bp
			ret 10	
		
		
;...........timer

randNum: dw 0
GenRandNum:
push bp
mov bp,sp
push cx
push ax
push dx
push si

; mov ah,00
; int 0x1b
rdtsc
;mov ax,dx
;xor dx,dx
mov cx,[bp+4]
div cx

;add dx,'0'

mov word[bp+6],dx

pop si
pop dx
pop ax
pop cx
pop bp

ret 2 




timer:		push ax

			cmp byte[cs:timerBrickFlag], 1 ; is the printing flag set
			jne skipall ; no, leave the ISR
			
			inc word [cs:seconds] ; increment tick count
			cmp word[cs:seconds],100
			jnae skip_time
			mov byte[flag],1		
			mov byte[cs:timerBrickFlag], 0
			jmp skipall
			skip_time:
			
skipall:	mov al, 0x20
			out 0x20, al ; send EOI to PIC
			
			pop ax
			iret ; return from interrupt
		
			
kbisr:
			push ax
			in al, 0x60 ; read char from keyboard port

			cmp al, 16 
			jne nextcmp ; no, try next comparison
			;jmp ll

			mov byte[flag],1
			
			;jmp exit
			jmp nomatch
			
			
nextcmp:	cmp al, 0x48 
			 jne nextcmp2
			 mov word[seconds],0
			mov byte[jump_flag],1
			;call movement
			jmp exit
nextcmp2:
		cmp al, 0x15 
			 jne nomatch 
			mov byte[resume_flag],1
			;call movement
			jmp exit
			
nomatch:	
			pop ax
		
			jmp far [cs:oldkb] ; call the original ISR

exit:		mov al, 0x20
			out 0x20, al ; send EOI to PIC
			pop ax
		
			iret ; return from interrupt       
			
Print_Welcome:
        pusha
      
        mov ax, 18
		push ax				; push r position............[bp+12]
		mov ax, 20
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Welcome_Message
		push ax				; push address of message............[bp+6]
		push word [welcome_msg_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret
		
Print_Names:
        pusha
      
        mov ax, 42
		push ax				; push r position............[bp+12]
		mov ax, 95
		push ax				; push c position............[bp+10]
		mov ax, 0x60			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Names
		push ax				; push address of message............[bp+6]
		push word [Names_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret	

Print_Play:
        pusha
        mov ax, 20
		push ax				; push r position............[bp+12]
		mov ax, 60
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Play_Message
		push ax				; push address of message............[bp+6]
		push word [play_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret
		
Print_Quit:
        pusha
       
        mov ax, 22
		push ax				; push r position............[bp+12]
		mov ax, 59
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Quit
		push ax				; push address of message............[bp+6]
		push word [quit_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret	
	
Print_Instructions:
        pusha
       
        mov ax, 29
		push ax				; push r position............[bp+12]
		mov ax, 0
		push ax				; push c position............[bp+10]
		mov ax, 0x67			
		push ax				; push attribute............[bp+8]
		mov ax, Instruction
		push ax				; push address of message............[bp+6]
		push word [Ins_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret
		
Print_Instruction1:
        pusha
       
        mov ax, 31
		push ax				; push r position............[bp+12]
		mov ax, 0
		push ax				; push c position............[bp+10]
		mov ax, 0x67			
		push ax				; push attribute............[bp+8]
		mov ax, Ins1
		push ax				; push address of message............[bp+6]
		push word [Ins1_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret		

Print_Instruction2:
        pusha
       
        mov ax, 33
		push ax				; push r position............[bp+12]
		mov ax, 0
		push ax				; push c position............[bp+10]
		mov ax, 0x67			
		push ax				; push attribute............[bp+8]
		mov ax, Ins2
		push ax				; push address of message............[bp+6]
		push word [Ins2_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret		

Print_Instruction3:
        pusha
       
        mov ax, 35
		push ax				; push r position............[bp+12]
		mov ax, 0
		push ax				; push c position............[bp+10]
		mov ax, 0x67			
		push ax				; push attribute............[bp+8]
		mov ax, Ins3
		push ax				; push address of message............[bp+6]
		push word [Ins3_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret	

Print_Instruction4:
        pusha
       
        mov ax, 37
		push ax				; push r position............[bp+12]
		mov ax, 0
		push ax				; push c position............[bp+10]
		mov ax, 0x67			
		push ax				; push attribute............[bp+8]
		mov ax, Ins4
		push ax				; push address of message............[bp+6]
		push word [Ins4_size]	; push message length ....[bp+4]
		
		call printstr
		popa
        ret		
	
clrscr:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax                   ; point es to video base
	xor di, di                   ; point di to top left column
	mov al, " "                  ; storing ASCII value of ' '(space) to dl
	mov ah, 10001000b            ;storing blue color for bg
	mov cx, 5676                 ; number of screen locations
	cld                          ; auto increment mode
	rep stosw                    ; clear the whole screen
	pop di
	pop cx
	pop ax
	pop es
	ret
Print_JumpingRabbit:
push es
push ax
push bx
push cx
push dx
push si
push di
push bp
   mov ax, 0xb800  ;video memory
   mov es, ax
   mov al, " "                   ; storing ASCII value of '' to dl
   mov ah, 0x70            ; storing attribute value of '_' to dh
   
;-----------J----------  
   mov di, 1066                  
l1_1:
  ; call Large_delay
   mov [es:di], ax
   add di,2
   cmp di,1080
   jne l1_1
   
   mov di, 1338
l2_1:    
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3182
   jnae l2_1

   mov di,2920
l3_1:
  ; call Large_delay
   mov [es:di], ax
   sub di,2
   cmp di,2912
   jne l3_1
   
   mov di,2650
   mov [es:di], ax
;------------- U --------------
   mov di,1082
l4_1:
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3194
   jne l4_1
;call Large_delay
   mov di,2932
   mov [es:di], ax
   mov di,2934
   mov [es:di], ax
   mov di,2936
   mov [es:di], ax
   mov di,2938
   mov [es:di], ax
   mov di,2940
l5_1:
 ; call Large_delay
   mov [es:di], ax
   sub di,264
   cmp di,828
   jne l5_1
   
; --------M-------
  mov di,1096
l6:
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3208
   jne l6
;call Large_delay
   mov di,1098
   mov [es:di], ax
   mov di,1100
   mov [es:di], ax
   mov di,2950
l7:
  ;call Large_delay
   mov [es:di], ax
   sub di,264
   cmp di,838
   jne l7
   call Large_delay
   mov di,1104
   mov [es:di], ax
   mov di,1106
   mov [es:di], ax
    mov di,1108
l8:
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3220
   jne l8
   
;-------- P ----------------
   mov di,1112
l9:
;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3224
   jne l9
   call Large_delay
   mov di,1114
   mov [es:di], ax
   mov di,1116
   mov [es:di], ax
   mov di,1118
   mov [es:di], ax
   mov di,1120
   mov [es:di], ax
   mov di,1122
   mov [es:di], ax
   mov di,1124
l10:
 ;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,2444
   jne l10
   ;call Large_delay
mov di,2178
mov [es:di], ax
mov di,2176
mov [es:di], ax
mov di,2174
mov [es:di], ax
mov di,2172
mov [es:di], ax
mov di,2170
mov [es:di], ax

; -------I-------

mov di,1128
l11:
 ;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3240
   jne l11
   ;call Large_delay
   
; -------N------
  mov di,1132
l12:
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3244
   jne l12
;call Large_delay
   mov di,1134
   mov [es:di], ax
   mov di,1136
   mov [es:di], ax
   mov di,1138
   mov [es:di], ax
   mov di,1140
   mov [es:di], ax
   mov di,2990
l13:
  ;call Large_delay
   mov [es:di], ax
   sub di,264
   cmp di,878
   jne l13
   call Large_delay 

;--------G---------
 mov di,1146
l14:
;call Large_delay
   mov [es:di], ax
   add di,2
   cmp di,1158
   jne l14
   ;call Large_delay
   
   mov di,1410
l15:
 ;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3258
   jne l15
   mov di,2994
l16:
   ;call Large_delay
   mov [es:di], ax
   add di,2
   cmp di,3006
   jne l16
   
   mov di,2740
   mov [es:di], ax
   
   mov di,2476
   mov [es:di], ax
   mov di,2212
   mov [es:di], ax
   mov di,2210
   mov [es:di], ax
   mov di,2208
   mov [es:di], ax
  
;------- R ------------
mov di,1180
l17:
;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3292
   jne l17
   ;call Large_delay
mov di,1182
mov [es:di], ax
mov di,1184
mov [es:di], ax
mov di,1186
mov [es:di], ax
mov di,1188
mov [es:di], ax
mov di,1190
mov [es:di], ax
mov di,1192
mov [es:di], ax
mov di,1456
mov [es:di], ax
mov di,1720
mov [es:di], ax
mov di,1984
l18:
;call Large_delay
   mov [es:di], ax
   sub di,2
   cmp di,1972
   jne l18 
mov di,1982
l19:
;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3302
   jne l19 
   ;call Large_delay
mov di,3040
mov [es:di], ax

;------- A ------------------
mov di,1196
l20:
;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3308
  jne l20
  ;call Large_delay
  mov di, 1198
l21:
  ;call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,1210
  jne l21
l22:
  ;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3322
  jne l22
 ; call Large_delay
  mov di,1990
l23:
 ; call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,2002
  jne l23

;---------B------------  
 
 mov di,1214
l24:
;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3326
  jne l24
  ;call Large_delay
  mov di, 1216
l25:
  call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,1228
  jne l25
l26:
  ;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3340
  jne l26
 ; call Large_delay
  mov di,2006
l27:
  ;call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,2020
  jne l27
  
  mov di,3076
l28:
;  call Large_delay
  mov [es:di], ax
  sub di,2
  cmp di,3062
  jne l28
  
;---------B------------  
 
 mov di,1232
l29:
;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3344
  jne l29
  ;call Large_delay
  mov di, 1234
l30:
 ; call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,1246
  jne l30
l31:
  ;call Large_delay
  mov [es:di], ax
  add di,264
  cmp di,3358
  jne l31
  ;call Large_delay
  mov di,2024
l32:
;  call Large_delay
  mov [es:di], ax
  add di,2
  cmp di,2038
  jne l32  
  mov di,3092
l33:
  ;call Large_delay
  mov [es:di], ax
  sub di,2
  cmp di,3080
  jne l33

;-----------I-------------
mov di,1250
l34:
 ;call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3362
   jne l34
   mov di,1252
l38:
   mov [es:di], ax
   add di,264
   cmp di,3364
   jne l38
   ;call Large_delay

;--------T----------

 mov di, 1256                 
l35:
   ;call Large_delay
   mov [es:di], ax
   add di,2
   cmp di,1274
   jne l35
   
   mov di, 1264
l36:    
  ; call Large_delay
   mov [es:di], ax
   add di,264
   cmp di,3376
   jnae l36

pop bp
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
ret

UserNameprint:
	push es
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	mov ax, 1003h
     mov bx, 0 ; disable blinking.
     int 10h
	mov ah, 0x13                 ;moves service number 13 to ah
	mov cx, [name_size]          ; cx takes size of string
	mov al, 0                    ;does not update cursor
	mov bh, 0                    ;display msg at page number 0
	mov dl, 22                   ;column number
	mov dh, 15                   ;row number
	mov bl, 0x87            ;attribute of msg
	push cs
	pop es
	mov bp, username                 ;moves msg in bp
	int 0x10                     ;call to interrupt 10 to print msg
	mov ah, 0x02                 ;setting cursor position
	mov dh,15
	mov dl,48
	mov bh,0	   ; DOS' service A   buffered input
	int 0x10
	mov dx, buffer1			; input buffer (ds:dx pointing to input buffer)
    mov ah,0x0A
	int 0x21
	mov ah, 0x02                              ;setting cursor position
	mov dh,24
	mov dl,0
	mov bh,0	   ; DOS' service A   buffered input
	int 0x10
pop bp
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
ret

Print_Boxes:
    push ax
	push bx
	push cx
	push dx
	push si
	push di
	push bp
; use this code for compatibility with dos / cmd prompt full screen mode:
	mov ax, 1003h
	mov bx, 0                    ; disable blinking.
	int 10h
	
	mov ah, 06h                  ;pointing ah to the function 06 of int 10h
	mov al, 0                    ;keeping al==0 to disable scrolling
	mov bh, 0x60                 ;attribute
	mov ch, 28                    ;ch == upper row number
	mov dh, 42                    ;dh == lower row number
	mov cl, 90                   ;cl == left column number
	mov dl, 131                   ;dl == right column number
	int 0x10                     ;calling int 10h service routine

	mov ah, 06h                  ;pointing ah to the function 06 of int 10h
	mov al, 0                    ;keeping al==0 to disable scrolling
	mov bh, 0x60                  ;attribute
	mov ch, 28                    ;ch == upper row number
	mov dh, 42                    ;dh == lower row number
	mov cl, 0                   ;cl == left column number
	mov dl, 40                   ;dl == right column number
	int 0x10                     ;calling int 10h service routine
pop bp
pop di
pop si
pop dx
pop cx
pop bx
pop ax
ret

sand: push bp    
     mov bp,sp
    push es
    push ax
    push di
    push si
   
    
    mov ax,0xb800
    mov es,ax
    mov si,[bp+4]
    mov ah,0x80
    mov al,' '
    mov di,3696
sand_loop: mov word[es:di],ax
          add di,2
          cmp di,si
          jne sand_loop
           
          pop si
          pop di
          pop ax
          pop es
          pop bp
          ret 2
		  
tree:  
            push bp         ;pushing bp on stack
            mov bp,sp	    ;pointing bp to sp
            push es         ;pushing extra segment on stack
			push ax         ;pushing ax register on stack
			push di         ;pushing di index on stack
           

			mov ax, 0xb800           ; load video base in ax
			mov es, ax					; point es to video base
			mov al,132           ;storing total columns
			mul byte [bp+6]           ;multply with row position
			add ax,[bp+4]           ;add in column position
			shl ax,1                ;multiply by two the position bcz one cell is of 2 bytes
			mov di, ax					; point di to starting location
			
	;printTree:  		
	         ;printing tree...........

			 
      		mov word [es:di], 0x2700	
			sub di, 264
            mov word [es:di], 0x2700	
			add di, 2
            mov word [es:di], 0x2700		
			add di, 2	
            mov word [es:di], 0x2700		
			add di, 264
            mov word [es:di], 0x2700		
			sub di, 2
			mov word [es:di], 0x4f7c
			add di, 264
			mov word [es:di], 0x4f7c
			 

			pop di           ;poping di index from stack
			pop ax           ;poping ax register from stack
			pop es          ;poping extra segment from stack
			pop bp           ;poping bp index from stack
			ret 4            ;return back
			

Large_delay: push cx
			 mov cx, 0xFFFF
loop6:		loop loop6
		    mov cx, 0xFFFF
loop2:	    loop loop2

			pop cx
			ret

Print_Sand:   mov ax,[row]
              sub ax,1
              mov bx, [col]
              mul bx
              shl ax,1
              mov bx,3
              div bx
              add ax,ax
              push ax
              call sand
              ret			  		

Print_LoginPage: 
            pusha
			
			  mov ax, 1003h
        mov bx, 0 ; disable blinking.
        int 10h
            call clrscr
			call Print_Grass
			call Print_Sand
			call Print_JumpingRabbit
			mov cx,4
			mov ax,3
			tree_loop:
			push 26
			push ax
			call tree
			push 26
			mov bx,131
			sub bx,ax
			push bx
			call tree
			add ax,10
			loop tree_loop
			call Print_Boxes

			  mov ax,0xCCCC
             push ax
             mov ax,40
             push ax
             mov ax, 50
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			  mov ax,0xCCCC
             push ax
             mov ax,40
             push ax
             mov ax, 73
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			   mov ax,0xCCCC
             push ax
             mov ax,30
             push ax
             mov ax, 56
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			 call Print_Rabbit
			 call Print_Welcome
			 call Print_Names
			 call Print_Play
			 call Print_Quit
			 call Print_Instructions
			 call Print_Instruction1
			 call Print_Instruction2
			 call Print_Instruction3
			 call Print_Instruction4
		     call UserNameprint
			popa
			ret
			
Inter_Page: 

			call PrintMainScreen
			mov cx,0
			call PlayAnimation
			
            ret

terminate: push bx
		push cx
		push ax
		push es
		push ds
		push di
		push si
		
		
		call Print_sky
		call Print_Mountain
		call Print_Sand
		call Print_Bird
		;call Print_Rabbit
		call Print_Grass
		  mov ax,0xCCCC
             push ax
             mov ax,40
             push ax
             mov ax, 50
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			  mov ax,0xCCCC
             push ax
             mov ax,40
             push ax
             mov ax, 73
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
			   mov ax,0xCCCC
             push ax
             mov ax,30
             push ax
             mov ax, 56
             push ax
             call DesiredLocation
             pop ax
             push ax
             call Rabbit
		call Print_Boxes
        call Print_Names
		mov cx,7
			mov ax,3
			tree_loop2:
			push 12
			push ax
			call tree
			push 12
			mov bx,131
			sub bx,ax
			push bx
			call tree
			add ax,10
			loop tree_loop2
        				
		mov ax, 20
		push ax				; push r position............[bp+12]
		mov ax, 60
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, message
		push ax				; push address of message............[bp+6]
		push word [length]	; push message length ....[bp+4]
		
		call printstr
		
		mov ax, 18
		push ax				; push r position............[bp+12]
		mov ax, 54
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, ending_Message
		push ax				; push address of message............[bp+6]
		push word [end_size]	; push message length ....[bp+4]
		
		call printstr
		
		mov ax, 24
		push ax				; push r position............[bp+12]
		mov ax, 58
		push ax				; push c position............[bp+10]
		mov ax, 0x80			
		push ax				; push attribute............[bp+8]
		mov ax, Play_Message
		push ax				; push address of message............[bp+6]
		push word [play_size]	; push message length ....[bp+4]
		
		call printstr
		
		 mov ax, 22
		push ax				; push r position............[bp+12]
		mov ax, 59
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Quit
		push ax				; push address of message............[bp+6]
		push word [quit_size]	; push message length ....[bp+4]
		
		call printstr
		
		 mov ax, 26
		push ax				; push r position............[bp+12]
		mov ax, 55
		push ax				; push c position............[bp+10]
		mov ax, 0x80			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, Ins5
		push ax				; push address of message............[bp+6]
		push word [Ins5_size]	; push message length ....[bp+4]
		
		call printstr
		
		push 26*132*2+70*2
			push word[score]
			 call printnum;
		
		pop si
		pop di
		pop ds
		pop es
		pop ax
		pop cx
		pop bx
		ret

terminate2: push bx
		push cx
		push ax
		push es
		push ds
		push di
		push si
		
		
		call Print_sky
		call Print_Mountain
		call Print_Sand
		call Print_Bird
		;call Print_Rabbit
		call Print_Boxes
        call Print_Names
        				
		
		mov ax, 18
		push ax				; push r position............[bp+12]
		mov ax, 54
		push ax				; push c position............[bp+10]
		mov ax, 0x70			; blue on black attribute
		push ax				; push attribute............[bp+8]
		mov ax, ending_Message
		push ax				; push address of message............[bp+6]
		push word [end_size]	; push message length ....[bp+4]
		
		call printstr
		
		mov ax, 20
		push ax				; push r position............[bp+12]
		mov ax, 56
		push ax				; push c position............[bp+10]
		mov ax, 0x70			
		push ax				; push attribute............[bp+8]
		mov ax, Quit_Message
		push ax				; push address of message............[bp+6]
		push word [quit_messagesize]	; push message length ....[bp+4]
		
		call printstr
		
		pop si
		pop di
		pop ds
		pop es
		pop ax
		pop cx
		pop bx
		ret


SetGlobal:

mov word[seconds],0
mov word[timerflag], 0
mov byte[jump_flag], 0
mov byte[resume_flag], 0
mov byte[timerBrickFlag], 0
mov word[brikCurr],20
mov byte[brikStartPos], 0

mov word[brikfinalPos], 40
mov word[brikCol] ,50
mov word[brikrow], 41
mov word[brik2Col], 50
mov word[brik2row], 34
mov byte[flag], 0
mov word[row], 43
mov word[col], 132
mov word[score], 0
mov word[rabitPos], 66
mov word[index], 2
mov word[brikpos],50

mov word[colorIndex1], 2
mov word [colorIndex2], 0



ret

		
		    
			
start:

	xor ax, ax
			mov es, ax ; point es to IVT base
			
			mov ax, [es:9*4]
			mov [oldkb], ax ; save offset of old routine
			mov ax, [es:9*4+2]
			mov [oldkb+2], ax ; save segment of old routine
			
			mov ax,[es:8*4]
			mov [oldTimer],ax
			mov ax,[es:8*4+2]
			mov [oldTimer+2],ax	
				
			cli ; disable interrupts
			mov word [es:9*4], kbisr ; store offset at n*4
			mov [es:9*4+2], cs ; store segment at n*4+2
			mov word [es:8*4], timer ; store offset at n*4
			mov [es:8*4+2], cs ; store segment at n*4+
			sti ; enable interrupts
			

			mov ah,0x00
     mov al, 0x54
     mov bx,0100; VESA mode 0x101 (132x43, 8-bit color)
     int 0x10
		
		lp_again1:
		call SetGlobal
		call Print_LoginPage
		lp_again:mov ah, 0                    ; service 0 Ã¢â‚¬â€œ get keystroke
	    int 0x16                     ; call BIOS keyboard service
	    cmp al,"p"
	    jne nextCheck
		
		call Inter_Page	
		call terminate
		checkKeyAgain:
			xor ax,ax
    		mov ah, 0                    ; service 0 Ã¢â‚¬â€œ get keystroke
			int 0x16                     ; call BIOS keyboard service
			cmp al,"p"	
			je lp_again1
			cmp al,27
			jne checkKeyAgain
			jmp end_game 
	    nextCheck:cmp al,27
	    jne lp_again
		
		;jmp restart
		jmp end_game
		


end_game:
			call terminate2
			
			xor ax,ax
			mov ax,[oldkb] ; save offset of old routine
			mov bx,[oldkb+2]; save segment of old routine
			cli ; disable interrupts
			mov word [es:9*4], ax ; store offset at n*4
			mov word[es:9*4+2], bx ; store segment at n*4+2
			sti
			
			xor ax,ax
			xor bx,bx
			mov ax,[oldTimer]
			
			mov bx,[oldTimer+2]

			cli ; disable interrupts
			
			mov word [es:8*4], ax ; store offset at n*4
			mov word[es:8*4+2], bx ; store segment at n*4+
			sti ; enable interrupts
					
			mov ax, 0x4c00
			int 0x21			



message: db 'Game Over!!' ; string to be printed
length: dw 11 ; length of the string
username: db 'Enter Your Good Name xD:', 0
name_size: dw 24
Welcome_Message: db '-----------------------------Welcome to jumping Rabbit--------------------------', 0
welcome_msg_size: dw 80
Names: db 'By Hamid Javed & Salman Mehmood!'
Names_size: dw 32
Play_Message: db 'press p to play'
play_size: dw 15
Quit: db 'press esc to quit'
quit_size: dw 17
Quit_Message: db 'You Quit the Game :)'
quit_messagesize: dw 20
ending_Message: db 'Hope you enjoyed the game'
end_size: dw 25
Instruction: db'Instructions:'
Ins_size: dw 13
Ins1: db'1) Press keyboard up key to play'
Ins1_size: dw 32
Ins2: db'2) Press y to pause'
Ins2_size: dw 19
Ins3: db'3) Press t to resume'
Ins3_size: dw 20

Ins4: db'4) Press q to quit during game'
Ins4_size: dw 30

Ins5: db'Your Score is: '
Ins5_size: dw 13



buffer1: dw 800
screen: times 1892 dw 0
buffer: times 132 dw 0
