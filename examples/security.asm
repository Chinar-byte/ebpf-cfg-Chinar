mov r0, 1
mov r2, 2
mov r3, 3
mov r4, 4
jeq r0, 1, +1
    add r2, r0
jeq r3, 3, +1
    add r3, r4
exit
;; 
