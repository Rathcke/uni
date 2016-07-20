.data
	.align 2
	array: .word 4, 5, 3, 2, 1, 8, 9, 7, 6
.text

main:
la	$a0, array
add	$a1, $zero, $zero
addi	$a2, $zero, 8
jal 	qsort
j 	exit


qsort:

addi	$sp, $sp, -12		#Decremenerer stackpointeren med 12
sw	$ra, 0($sp)
sw	$s0, 4($sp)
sw	$s1, 8($sp)
sll	$s0, $a2, 2		#Lægger right*4 i saved variable
add	$s0, $s0, $a0		#lægger a0 til
slt 	$t0, $a1, $a2		#Left < Right
beq	$t0, $zero, return	#Hvis ikke sandt, goto return
sll	$t1, $a1, 2		#Ganger Left med 4
add	$t1, $t1, $a0		#Gemmer (left*4)+a0 i t1
add	$t2, $a1, $a2		#Lægger left og right sammen
srl	$t2, $t2, 1		#Divider med 2
sll	$t2, $t2, 2
add	$t2, $t2, $a0		
lw	$t3, 0($t1)		#Henter v[left]
lw 	$t4, 0($t2)		#Henter v[left+right]/2
sw	$t3, 0($t2)
sw	$t4, 0($t1)
add	$t5, $t1, $zero		#Gemmer last = left
addi	$t6, $t1, 4		#Initialiserer i

floop:

lw	$t7, 0($t6)
lw	$t8, 0($t1)
slt	$t0, $t7, $t8		#v[i] < v[left]
beq	$t0, 0, endif		#Hvis ikke sandt, goto endif
addi	$t5, $t5, 4		#Lægger 4 til last
lw 	$t3, 0($t5)		
lw	$t4, 0($t6)
sw 	$t3, 0($t6)
sw	$t4, 0($t5)

endif:
addi	$t6, $t6, 4		#Inkrementer i med 4
slt	$t0, $s0, $t6		#checker i > right
beq	$t0, $zero, floop	#Hvis ikke sand, goto floop
lw	$t3, 0($t1)
lw	$t4, 0($t5)
sw	$t3, 0($t5)
sw	$t4, 0($t1)
sub	$s1, $t5, $a0		#Trækker a0 fra last
srl	$s1, $s1, 2		#Dividerer last med 4
addi	$a2, $s1, -1		#Sætter right til last-1
jal	qsort
addi	$a1, $s1, 1		#Sætter left til last+1
sub	$s0, $s0, $a0		#trækker 0 fra ikke raw right
srl	$s0, $s0, 2		#Dividerer den med 4
add	$a2, $s0, $zero		#Sætter right tilbage
jal	qsort

return:
lw	$ra, 0($sp)
lw	$s0, 4($sp)
lw	$s1, 8($sp)
addi	$sp, $sp, 12		#Decremenerer stackpointeren med 12
jr	$ra

exit:

	
