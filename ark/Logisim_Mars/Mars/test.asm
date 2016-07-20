addiu	$t1, $zero, 42
addiu   $t1, $t1, 1
addiu	$t2, $t1, 1
addu	$t3, $t2, $t1
subu	$t4, $t2, $t1
and	$t5, $t4, $t4
andi 	$t6, $t5, 1
or	$t7, $zero, $t4
ori	$t8, $t6, 0
sw	$t3, 0($sp)
jal 	jump
lw	$t9, 0($sp)
jal	quit
slti	$s2, $zero, 11

jump:
slt	$t0, $t1, $t6
beq	$t0, $zero, jump
addiu	$t6, $t6, 20
jr	$ra
addiu	$s1, $t8, 17

quit:
