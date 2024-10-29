mov r0, 0
mov r3, 0
ja +5
mov r4, r1
add r4, r3
add r3, 1
; here we insert the r5 := dGuard(r5) which does the following - Binary B64 And (Reg 5) (Imm 0x0000FFFF) ; Binary B64 Or (Reg 5) (Imm 0x12340000)
ldxb r5, [r4]
add r0, r5
jlt r3, r2, +-6
exit