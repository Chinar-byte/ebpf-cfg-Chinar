;; Compute the sum of the first five integers
; sum = 0
; i = 5
; while i > 0:
;    sum = sum + i
;    i = i - 1
    mov r0, 0
    mov r1, 5
    ja +2
    add r0, r1
    sub r1, 1
    jgt r1, 0, +-3
    exit
