	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
_stop_:
	ori	$2, $0, 10
	syscall
# Function main
main:
	sw	$31, -4($29)
	addi	$29, $29, -16
	sw	$18, 8($29)
	sw	$17, 4($29)
	sw	$16, 0($29)
	ori	$4, $28, 0
# was:	ori	_assign__7_, 28, 0
	addi	$28, $28, 40
# was:	addi	28, 28, 40
	ori	$3, $4, 0
# was:	ori	_tmp__8_, _assign__7_, 0
	ori	$2, $0, 2
# was:	ori	_tmp__9_, 0, 2
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 3
# was:	ori	_tmp__9_, 0, 3
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 3
# was:	ori	_tmp__9_, 0, 3
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	sw	$3, -4($3)
# was:	sw	_tmp__8_, -4(_tmp__8_)
	ori	$2, $0, 2
# was:	ori	_tmp__9_, 0, 2
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 3
# was:	ori	_tmp__9_, 0, 3
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 4
# was:	ori	_tmp__9_, 0, 4
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 5
# was:	ori	_tmp__9_, 0, 5
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 6
# was:	ori	_tmp__9_, 0, 6
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
	ori	$2, $0, 7
# was:	ori	_tmp__9_, 0, 7
	sw	$2, 0($3)
# was:	sw	_tmp__9_, 0(_tmp__8_)
	addi	$3, $3, 4
# was:	addi	_tmp__8_, _tmp__8_, 4
# 	ori	a_local__5_,_assign__7_,0
	ori	$2, $4, 0
# was:	ori	_t1__11_, a_local__5_, 0
	addi	$17, $0, 2
# was:	addi	_pointer__20_, 0, 2
	sll	$17, $17, 2
# was:	sll	_pointer__20_, _pointer__20_, 2
	addi	$16, $0, 1
# was:	addi	_t4__14_, 0, 1
_getDim__15_:
	lw	$3, 0($2)
# was:	lw	_t3__13_, 0(_t1__11_)
	mul	$16, $16, $3
# was:	mul	_t4__14_, _t4__14_, _t3__13_
	addi	$2, $2, 4
# was:	addi	_t1__11_, _t1__11_, 4
	addi	$17, $17, -4
# was:	addi	_pointer__20_, _pointer__20_, -4
	bne	$17, $0, _getDim__15_
# was:	bne	_pointer__20_, 0, _getDim__15_
	addi	$18, $0, 4
# was:	addi	_nextElm__19_, 0, 4
	addi	$17, $0, 2
# was:	addi	_pointer__20_, 0, 2
	sll	$17, $17, 2
# was:	sll	_pointer__20_, _pointer__20_, 2
	add	$2, $2, $17
# was:	add	_t1__11_, _t1__11_, _pointer__20_
	add	$17, $2, $18
# was:	add	_pointer__20_, _t1__11_, _nextElm__19_
	addi	$3, $16, -2
# was:	addi	_t2__12_, _t4__14_, -2
	beq	$3, $0, _endRed__17_
# was:	beq	_t2__12_, 0, _endRed__17_
	lw	$3, 4($2)
# was:	lw	_t3__13_, 4(_t1__11_)
	lw	$2, 0($2)
# was:	lw	_t1__11_, 0(_t1__11_)
	addi	$16, $16, -2
# was:	addi	_t4__14_, _t4__14_, -2
_reduce__16_:
# 	ori	_funarg__23_,_t1__11_,0
# 	ori	_funarg__24_,_t3__13_,0
# 	ori	3,_funarg__24_,0
# 	ori	2,_funarg__23_,0
	jal	plus
# was:	jal	plus, 23
# 	ori	_t1__11_,2,0
	beq	$16, $0, _endRed__17_
# was:	beq	_t4__14_, 0, _endRed__17_
	lw	$3, 4($17)
# was:	lw	_t3__13_, 4(_pointer__20_)
	add	$17, $17, $18
# was:	add	_pointer__20_, _pointer__20_, _nextElm__19_
	addi	$16, $16, -1
# was:	addi	_t4__14_, _t4__14_, -1
	j	_reduce__16_
_oneElm__18_:
	lw	$2, 0($2)
# was:	lw	_t1__11_, 0(_t1__11_)
	addi	$16, $16, -2
# was:	addi	_t4__14_, _t4__14_, -2
_endRed__17_:
	xor	$3, $16, $1
# was:	xor	_t2__12_, _t4__14_, 1
	beq	$3, $0, _oneElm__18_
# was:	beq	_t2__12_, 0, _oneElm__18_
# 	ori	_assign__10_,_t1__11_,0
# 	ori	x_local__6_,_assign__10_,0
# 	ori	_dat__25_,x_local__6_,0
# 	ori	2,_dat__25_,0
	jal	writeInt
# was:	jal	writeInt, 2
	ori	$4, $28, 0
# was:	ori	_assign__26_, 28, 0
	addi	$28, $28, 24
# was:	addi	28, 28, 24
	ori	$3, $4, 0
# was:	ori	_tmp__27_, _assign__26_, 0
	ori	$2, $0, 2
# was:	ori	_tmp__28_, 0, 2
	sw	$2, 0($3)
# was:	sw	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 4
# was:	addi	_tmp__27_, _tmp__27_, 4
	ori	$2, $0, 3
# was:	ori	_tmp__28_, 0, 3
	sw	$2, 0($3)
# was:	sw	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 4
# was:	addi	_tmp__27_, _tmp__27_, 4
	ori	$2, $0, 3
# was:	ori	_tmp__28_, 0, 3
	sw	$2, 0($3)
# was:	sw	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 4
# was:	addi	_tmp__27_, _tmp__27_, 4
	addi	$3, $3, 4
# was:	addi	_tmp__27_, _tmp__27_, 4
	sw	$3, -4($3)
# was:	sw	_tmp__27_, -4(_tmp__27_)
	ori	$2, $0, 0
# was:	ori	_tmp__28_, 0, 0
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
	ori	$2, $0, 1
# was:	ori	_tmp__28_, 0, 1
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
	ori	$2, $0, 1
# was:	ori	_tmp__28_, 0, 1
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
	ori	$2, $0, 0
# was:	ori	_tmp__28_, 0, 0
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
	ori	$2, $0, 1
# was:	ori	_tmp__28_, 0, 1
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
	ori	$2, $0, 0
# was:	ori	_tmp__28_, 0, 0
	sb	$2, 0($3)
# was:	sb	_tmp__28_, 0(_tmp__27_)
	addi	$3, $3, 1
# was:	addi	_tmp__27_, _tmp__27_, 1
# 	ori	b_local__3_,_assign__26_,0
	ori	$2, $4, 0
# was:	ori	_t1__30_, b_local__3_, 0
	addi	$18, $0, 2
# was:	addi	_pointer__39_, 0, 2
	sll	$18, $18, 2
# was:	sll	_pointer__39_, _pointer__39_, 2
	addi	$17, $0, 1
# was:	addi	_t4__33_, 0, 1
_getDim__34_:
	lw	$3, 0($2)
# was:	lw	_t3__32_, 0(_t1__30_)
	mul	$17, $17, $3
# was:	mul	_t4__33_, _t4__33_, _t3__32_
	addi	$2, $2, 4
# was:	addi	_t1__30_, _t1__30_, 4
	addi	$18, $18, -4
# was:	addi	_pointer__39_, _pointer__39_, -4
	bne	$18, $0, _getDim__34_
# was:	bne	_pointer__39_, 0, _getDim__34_
	addi	$16, $0, 1
# was:	addi	_nextElm__38_, 0, 1
	addi	$18, $0, 2
# was:	addi	_pointer__39_, 0, 2
	sll	$18, $18, 2
# was:	sll	_pointer__39_, _pointer__39_, 2
	add	$2, $2, $18
# was:	add	_t1__30_, _t1__30_, _pointer__39_
	add	$18, $2, $16
# was:	add	_pointer__39_, _t1__30_, _nextElm__38_
	addi	$3, $17, -2
# was:	addi	_t2__31_, _t4__33_, -2
	beq	$3, $0, _endRed__36_
# was:	beq	_t2__31_, 0, _endRed__36_
	lb	$3, 1($2)
# was:	lb	_t3__32_, 1(_t1__30_)
	lb	$2, 0($2)
# was:	lb	_t1__30_, 0(_t1__30_)
	addi	$17, $17, -2
# was:	addi	_t4__33_, _t4__33_, -2
_reduce__35_:
# 	ori	_funarg__42_,_t1__30_,0
# 	ori	_funarg__43_,_t3__32_,0
# 	ori	3,_funarg__43_,0
# 	ori	2,_funarg__42_,0
	jal	xor
# was:	jal	xor, 23
# 	ori	_t1__30_,2,0
	beq	$17, $0, _endRed__36_
# was:	beq	_t4__33_, 0, _endRed__36_
	lb	$3, 1($18)
# was:	lb	_t3__32_, 1(_pointer__39_)
	add	$18, $18, $16
# was:	add	_pointer__39_, _pointer__39_, _nextElm__38_
	addi	$17, $17, -1
# was:	addi	_t4__33_, _t4__33_, -1
	j	_reduce__35_
_oneElm__37_:
	lw	$2, 0($2)
# was:	lw	_t1__30_, 0(_t1__30_)
	addi	$17, $17, -2
# was:	addi	_t4__33_, _t4__33_, -2
_endRed__36_:
	xor	$3, $17, $1
# was:	xor	_t2__31_, _t4__33_, 1
	beq	$3, $0, _oneElm__37_
# was:	beq	_t2__31_, 0, _oneElm__37_
# 	ori	_assign__29_,_t1__30_,0
# 	ori	y_local__4_,_assign__29_,0
# 	ori	_dat__44_,y_local__4_,0
# 	ori	2,_dat__44_,0
	jal	writeInt
# was:	jal	writeInt, 2
	ori	$2, $28, 0
# was:	ori	_assign__45_, 28, 0
	addi	$28, $28, 12
# was:	addi	28, 28, 12
	ori	$4, $2, 0
# was:	ori	_tmp__46_, _assign__45_, 0
	ori	$3, $0, 1
# was:	ori	_tmp__47_, 0, 1
	sw	$3, 0($4)
# was:	sw	_tmp__47_, 0(_tmp__46_)
	addi	$4, $4, 4
# was:	addi	_tmp__46_, _tmp__46_, 4
	addi	$4, $4, 4
# was:	addi	_tmp__46_, _tmp__46_, 4
	sw	$4, -4($4)
# was:	sw	_tmp__46_, -4(_tmp__46_)
	ori	$3, $0, 8
# was:	ori	_tmp__47_, 0, 8
	sw	$3, 0($4)
# was:	sw	_tmp__47_, 0(_tmp__46_)
	addi	$4, $4, 4
# was:	addi	_tmp__46_, _tmp__46_, 4
# 	ori	c_local__1_,_assign__45_,0
# 	ori	_t1__49_,c_local__1_,0
	addi	$16, $0, 1
# was:	addi	_pointer__58_, 0, 1
	sll	$16, $16, 2
# was:	sll	_pointer__58_, _pointer__58_, 2
	addi	$18, $0, 1
# was:	addi	_t4__52_, 0, 1
_getDim__53_:
	lw	$3, 0($2)
# was:	lw	_t3__51_, 0(_t1__49_)
	mul	$18, $18, $3
# was:	mul	_t4__52_, _t4__52_, _t3__51_
	addi	$2, $2, 4
# was:	addi	_t1__49_, _t1__49_, 4
	addi	$16, $16, -4
# was:	addi	_pointer__58_, _pointer__58_, -4
	bne	$16, $0, _getDim__53_
# was:	bne	_pointer__58_, 0, _getDim__53_
	addi	$17, $0, 4
# was:	addi	_nextElm__57_, 0, 4
	addi	$16, $0, 1
# was:	addi	_pointer__58_, 0, 1
	sll	$16, $16, 2
# was:	sll	_pointer__58_, _pointer__58_, 2
	add	$2, $2, $16
# was:	add	_t1__49_, _t1__49_, _pointer__58_
	add	$16, $2, $17
# was:	add	_pointer__58_, _t1__49_, _nextElm__57_
	addi	$3, $18, -2
# was:	addi	_t2__50_, _t4__52_, -2
	beq	$3, $0, _endRed__55_
# was:	beq	_t2__50_, 0, _endRed__55_
	lw	$3, 4($2)
# was:	lw	_t3__51_, 4(_t1__49_)
	lw	$2, 0($2)
# was:	lw	_t1__49_, 0(_t1__49_)
	addi	$18, $18, -2
# was:	addi	_t4__52_, _t4__52_, -2
_reduce__54_:
# 	ori	_funarg__61_,_t1__49_,0
# 	ori	_funarg__62_,_t3__51_,0
# 	ori	3,_funarg__62_,0
# 	ori	2,_funarg__61_,0
	jal	plus
# was:	jal	plus, 23
# 	ori	_t1__49_,2,0
	beq	$18, $0, _endRed__55_
# was:	beq	_t4__52_, 0, _endRed__55_
	lw	$3, 4($16)
# was:	lw	_t3__51_, 4(_pointer__58_)
	add	$16, $16, $17
# was:	add	_pointer__58_, _pointer__58_, _nextElm__57_
	addi	$18, $18, -1
# was:	addi	_t4__52_, _t4__52_, -1
	j	_reduce__54_
_oneElm__56_:
	lw	$2, 0($2)
# was:	lw	_t1__49_, 0(_t1__49_)
	addi	$18, $18, -2
# was:	addi	_t4__52_, _t4__52_, -2
_endRed__55_:
	xor	$3, $18, $1
# was:	xor	_t2__50_, _t4__52_, 1
	beq	$3, $0, _oneElm__56_
# was:	beq	_t2__50_, 0, _oneElm__56_
# 	ori	_assign__48_,_t1__49_,0
# 	ori	z_local__2_,_assign__48_,0
# 	ori	_dat__63_,z_local__2_,0
# 	ori	2,_dat__63_,0
	jal	writeInt
# was:	jal	writeInt, 2
main_exit:
	lw	$18, 8($29)
	lw	$17, 4($29)
	lw	$16, 0($29)
	addi	$29, $29, 16
	lw	$31, -4($29)
	jr	$31
# Function xor
xor:
	sw	$31, -4($29)
	addi	$29, $29, -4
# 	ori	x_arg__64_,2,0
# 	ori	y_arg__65_,3,0
	ori	$2, $3, 0
# was:	ori	_return__66_, y_arg__65_, 0
# 	ori	2,_return__66_,0
	j	xor_exit
xor_exit:
	addi	$29, $29, 4
	lw	$31, -4($29)
	jr	$31
# Function plus
plus:
	sw	$31, -4($29)
	addi	$29, $29, -4
# 	ori	x_arg__67_,2,0
# 	ori	y_arg__68_,3,0
# 	ori	plus1__70_,x_arg__67_,0
# 	ori	plus2__71_,y_arg__68_,0
	add	$2, $2, $3
# was:	add	_return__69_, plus1__70_, plus2__71_
# 	ori	2,_return__69_,0
	j	plus_exit
plus_exit:
	addi	$29, $29, 4
	lw	$31, -4($29)
	jr	$31
ord:
	andi	$2, $2, 255
	jr	$31
chr:
	andi	$2, $2, 255
	jr	$31
len:
	lw	$2, 0($2)
	jr	$31
writeInt:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 1
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
readInt:
	ori	$2, $0, 5
	syscall
	jr	$31
writeChar:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 11
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
readChar:
	addi	$29, $29, -8
	sw	$4, 0($29)
	sw	$5, 4($29)
	ori	$2, $0, 12
	syscall
	ori	$5, $2, 0
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	ori	$2, $5, 0
	lw	$4, 0($29)
	lw	$5, 4($29)
	addi	$29, $29, 8
	jr	$31
_IllegalArrSizeError_:
	la	$4, _IllegalArrSizeString_
	ori	$2, $0, 4
	syscall
	ori	$4, $5, 0
	ori	$2, $0, 1
	syscall
	la	$4, _cr_
	ori	$2, $0, 4
	syscall
	j	_stop_
_IllegalArrIndexError_:
	la	$4, _IllegalArrIndexString_
	ori	$2, $0, 4
	syscall
	ori	$4, $5, 0
	ori	$2, $0, 1
	syscall
	la	$4, _cr_
	ori	$2, $0, 4
	syscall
	j	_stop_
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_IllegalArrSizeString_:
	.asciiz	"Error: Array size less or equal to 0 at line "
	.align	2
_IllegalArrIndexString_:
	.asciiz	"Error: Array index out of bounds at line "
# String Literals
	.align	2
_heap_:
	.space	100000
