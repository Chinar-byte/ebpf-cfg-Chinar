mov32 r0, 21
mov r1, 21
jeq r0, 0, +2
  add r0, r1
exit
;; r0 == 42
