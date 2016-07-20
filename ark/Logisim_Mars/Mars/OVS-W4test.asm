start:
addi		$t1, $a0, 3
addi		$t2, $a1, 5

condition:
divu		$t3, $t1, $t2
beq		$t2, $zero, end
beq		$t3, $zero, end
nop

ifthen:
slt		$t0, $t2, $t1
bne		$t0, $zero, else
sub		$t1, $t1, $t2
j		condition
nop

else:
sub		$t2, $t2, $t1
j		condition
nop

end:
add		$a0, $t1, $zero
add		$a1, $t2, $zero