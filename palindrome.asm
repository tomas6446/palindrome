.model small
.stack 100H

MAX_BUFF = 50
MAX_LINE_LENGTH = 800

JUMPS ; auto generate inverted condition jmp on far jumps

.data

about 				db 'Programa iesko palindromus', 13, 10, 9, 'p.exe [ file1 [file2] [...] ]', 13, 10, 13, 10, 9, ' / ? - pagalba', 13, 10, '$'
ask_input			db 'Iveskite failo pavadinimus: ', 13, 10, '$'
err_op 				db ' - nepavyko atidaryti skaitymui', 13, 10, '$'
err_dest			db ' - nepavyko atidaryti rasymui', 13, 10, '$'
err_read 			db 'nepavyko skaityti is failo', 13, 10, '$'

delimeter			db '.$'
endline 			db 13, 10, '$'
string 				db 10 dup(0)
buffer 				dw MAX_LINE_LENGTH dup(0)

purified_sentence	dw MAX_LINE_LENGTH dup(0)
char				db 0, '$'

purified_length		dw 0
purified_div_length dw 0
sentence_length		dw 0
buffer_length		dw 0
buffer_counter		dw 0


is_palindrome		db 9, 9, ' - PALINDROME$'
not_palindrome		db 9, 9, ' - IS NOT A PALINDROME$'
skip				db 9, 9, ' - SKIPPED$'

file     			dw MAX_BUFF, ?, MAX_BUFF dup(0)
fileHandle 			dw ?

dest     			db 'res.txt', 0
destHandle			dw ?

.code
	
START:
	mov ax, @data
	mov es, ax                   ; es kad galetume naudot stosb funkcija: Store AL at address ES:(E)DI
	
	mov si, 81h                  ; programos paleidimo parametrai rasomi segmente es pradedant 129 (arba 81h) baitu
	
	call skip_spaces

	;; ar yra parametru
	mov al, byte ptr ds:[si]      ; nuskaityti pirma parametro simboli
	cmp al, 13                   ; jei nera parametru
	je help                      ; tai isvesti pagalba
	;; ar reikia isvesti pagalba
	mov ax, word ptr ds:[si]
	cmp ax, 3F2Fh                ; jei nuskaityta " / ?" - 3F = '?'; 2F = ' / '
	je help                      ; rastas " / ?", vadinasi reikia isvesti pagalba

	lea di, file
	call read_filename
	cmp byte ptr ds:[file], '$'   ; jei nieko nenuskaite
	je help

	push ds si
	mov ax, @data
	mov ds, ax

	;; rasymui
	mov	dx, offset dest		; ikelti i dx dest - failo pavadinima
	mov	ah, 3ch				; isvalo/sukuria faila - komandos kodas
	mov	cx, 0				; normal - no attributes
	int	21h					; INT 21h / AH= 3Ch - create or truncate file.
							;   Jei nebus isvalytas - tai perrasines senaji,
							;   t.y. jei pries tai buves failas ilgesnis - like simboliai isliks.
	jc	err_destination
	mov	ah, 3dh				; atidaro faila - komandos kodas
	mov	al, 1				; rasymui
	int	21h					; INT 21h / AH= 3Dh - open existing file.
	jc	err_destination
	mov	destHandle, ax		; issaugom handle

	jmp start_collecting_data

read_more:
	pop si ds
	lea di, file
	call read_filename           ; perkelti is parametro i eilute

	push ds si
	mov ax, @data
	mov ds, ax
	
	cmp byte ptr ds:[file], '$'
	jne start_collecting_data    ; jei buvo kazkas nuskaityta
	jmp _end

start_collecting_data:
	cmp byte ptr ds:[file], '$'
	jne from_source_file         ; jei buvo kazkas nuskaityta
	
	mov fileHandle, 0
	jmp read_more        		; jei ne tai readinam, ar yra dar vienas failas komando eiluteje

from_source_file:
	mov ah, 3Dh                  ; atidaro faila - komandos kodas
	mov dx, offset file          ; failo pavadinimas
	mov al, 0                    ; 0 - reading, 1 - writing, 2 - abu
	int 21h                      ; INT 21h / AH= 3Dh - open existing file
	jc err_open                  ; CF set on error AX = error code.

	mov fileHandle, ax           ; issaugojam filehandle

	; jei failas egzistuoja
	call file_read            	

	mov bx, fileHandle           ; pabaiga skaitomo failo
	mov ah, 3eh                  ; uzdaryti
	int 21h
	
	jmp read_more

help:
	mov ax, @data
	mov ds, ax
	
	mov dx, offset about
	mov ah, 09h
	int 21h
	
	jmp _end

err_open:
	mov ah, 09h
	mov dx, offset file
	int 21h
	
	mov dx, offset err_op
	int 21h
	
	jmp read_more        ; atidaryti kita skaitoma faila, jei yra
err_destination:
	mov ah, 09h
	mov dx, offset dest
	int 21h

	mov dx, offset err_dest
	int 21h	
_end:
	; uzdarom destination file
	mov bx, destHandle           ; pabaiga skaitomo failo
	mov ah, 3eh                  ; uzdaryti
	int 21h

	
	mov ax, 4c00h
	int 21h

strlen proc
	push di
	;; get string length
	mov di, dx 			; in the di
	mov al, '$' 		; search for dollar
	mov cx, MAX_LINE_LENGTH 	; maximum buffer size
	repnz scasb 		; search
	sub di, dx 			; substract to get the distance 
	sub di, 1 			; substract 1 to remove dollar sign
	;; cx is the string length without dollar sign
	mov cx, di
	pop di
	ret
strlen ENDP

skip_spaces PROC near
skip_spaces_loop:
	cmp byte ptr ds:[si], ' '
	jne skip_spaces_end
	inc si
	jmp skip_spaces_loop

skip_spaces_end:
	ret
	skip_spaces ENDP
	
read_filename PROC near
	push ax
	call skip_spaces

read_filename_start:
	cmp byte ptr ds:[si], 13      ; jei nera parametru
	je read_filename_end         ; tai taip, tai baigtas failo vedimas
	cmp byte ptr ds:[si], ' '     ; jei tarpas
	jne read_filename_next       ; tai praleisti visus tarpus, ir sokti prie kito parametro

read_filename_end:
	mov al, '$'                  ; irasyti gale dolleri
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	pop ax
	ret

read_filename_next:
	lodsb                        ; uzkrauna kita simboli
	stosb                        ; Store AL at address ES:(E)DI, di = di + 1
	jmp read_filename_start
read_filename ENDP

file_read PROC near
	mov sentence_length, 0
	mov purified_length, 0
	mov di, offset purified_sentence
new_buffer:
	mov ah, 3Fh                  ;read file
	mov bx, fileHandle
	mov cx, MAX_LINE_LENGTH
	mov dx, offset buffer
	int 21h
	jc read_error

	add buffer_length, ax
	
	mov cx, ax
	cmp ax, 0                    ; jei nenuskaite
	je ending                     ; tai ending

	mov si, offset buffer
_rep:
	lodsb

	cmp al, 13
	je sleep

	cmp al, 10
	je sleep
	
	call print_sentence

sleep:

	inc sentence_length
	inc buffer_counter

	cmp al, 13
	jne purify_sentence		; until al is not '.', remove unnecessary chars like './;'[]{} '
							; as well as make all letters lowercase
	call find_palindrome	; else find palindrome

purify_sentence:
	call purify				
	loop _rep

	jmp new_buffer			; jmp to the new buffer
read_error:
	mov ah, 9h
	mov dx, offset err_read
	int 21h

ending:
	ret
file_read ENDP


;; prints one char in al
print_sentence proc
	push ax dx bx si di
	mov byte ptr[char], al
	mov dx, offset char
	call write
	pop di si bx dx ax
	; ; prints purified sentence
	; push di

	; inc di
	; mov byte ptr[di], '$'	; add dollar at the end
	; sub di, purified_length	; get to the start of the sentence
	; mov dx, di				
	; call write

	; pop di
return:
	ret
print_sentence endp


write proc
	push cx

	; find dollar at the end of the string
	call strlen	; length of the string is in the cx

	mov ah, 40h
	mov bx, destHandle
	int  21h	; print string stored in dx

	pop cx
	ret
write ENDP


find_palindrome proc
	push si di cx 			; save all registers in a stack for future
	;call print_sentence

	cmp purified_length, 5 	; the length of the sentence is below 5 chars
	jb	skip_sentence		; skip


	;; find palindrome distance to middle
	;  by diving the length of the sentence by 2
	mov ax, purified_length
	xor dx, dx
	mov bx, 2	
	div bx
	mov purified_div_length, ax

	
	mov si, offset di			; points to the end
	dec purified_length
	sub di, purified_length 	; points to the start
	
	xor cx, cx
 _loop:
	; mov ah, 2
	; mov dl, [di]
	; int 21h

	; mov dl, '='
	; int 21h

	; mov dl, [si]
	; int 21h

	; mov ah, 9
	; mov dx, offset endline
	; int 21h

	;; compare
	mov al, [di] 					; compare char at the start
	mov ah, [si] 					; with the char at the end
 	cmp  al, ah						; if they are not equal
 	jne	false						; its not a palindrome

	inc di 							; increment di pointer
	dec si							; decremnt si pointer
	inc cx							; increment counter
	cmp cx, purified_div_length    ; compare cx to the limit
	jb _loop
true:
	mov dx, offset is_palindrome
	call write

	mov dx, offset endline
	call write

	jmp continue
false:
	mov dx, offset not_palindrome
	call write

	mov dx, offset endline
	call write

	jmp continue
skip_sentence:
	mov dx, offset skip
	call write

	mov dx, offset endline
	call write

continue:
 	pop cx di si

	mov sentence_length, 0
	mov purified_length, 0
	mov di, offset purified_sentence	; new sentence

	ret
find_palindrome endp



;; remove spaces and make letters lowercase
purify proc near
	;; skip all characters from 0 to 64
	cmp al, 64
	jbe	_ending

	;; skip all characters from 91 to 96
	cmp al, 91
	jb	con
	cmp al, 96
	jbe	_ending

con:	
	;; skip all characters from 123 to 127
	cmp al, 123 
	jae	_ending

	;; everything else make lower case
	cmp al, 'a'
	jb makelower
	cmp al, 'z'
	ja makelower

	;; add in buff
	jmp add_buf

makelower:
	;; make lower case & add in buff
	xor al, 32
	jmp add_buf

add_buf:
	;; increment length, pointer 'di'
	;; put character stored in al in buffer
	inc purified_length
	inc di
	mov byte ptr [di], al
_ending:
	ret
purify ENDP



print_number proc
	push si
	mov si, offset string + 9    ; nurodyti i paskutini simboli
	mov byte ptr [si], '$'       ; kelti pabaigos simboli
	
	mov bx, 10                   ; dalinti reiksmes is 10

asc2:
	xor dx, dx                   ; isvalyti dx
	div bx                       ; dx bus liekana is ax / bx
	add dx, '0'                  ; prideti 48, kad paruosti simboli isvedimui
	dec si                       ; imam kita simboli (decrementinam pointeri)
	mov [si], dl                 ; padedam skaitmeni
	
	cmp ax, 0                    ; jei skaicius isdalintas
	jz _print                    ; tai einam i pabaiga
	jmp asc2                     ; kitu atveju imam kita skaitmeni
	

_print:
	mov ah, 9h                   ; atspausdinti skaitmenis
	mov dx, si
	int 21h
	pop si
	ret
print_number ENDP

	
end START
