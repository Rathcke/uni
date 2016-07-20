	.text	0x00400000
	.globl	main
	la	$28, _heap_
	la	$4, bye__st_27_
# was:	la	bye__st_27__addr, bye__st_27_
	ori	$3, $0, 6
# was:	ori	bye__st_27__init, 0, 6
	sw	$3, 0($4)
# was:	sw	bye__st_27__init, 0(bye__st_27__addr)
	addi	$3, $4, 8
# was:	addi	bye__st_27__init, bye__st_27__addr, 8
	sw	$3, 4($4)
# was:	sw	bye__st_27__init, 4(bye__st_27__addr)
	la	$4, iisneit_22_
# was:	la	iisneit_22__addr, iisneit_22_
	ori	$3, $0, 21
# was:	ori	iisneit_22__init, 0, 21
	sw	$3, 0($4)
# was:	sw	iisneit_22__init, 0(iisneit_22__addr)
	addi	$3, $4, 8
# was:	addi	iisneit_22__init, iisneit_22__addr, 8
	sw	$3, 4($4)
# was:	sw	iisneit_22__init, 4(iisneit_22__addr)
	la	$4, iis__st_15_
# was:	la	iis__st_15__addr, iis__st_15_
	ori	$3, $0, 6
# was:	ori	iis__st_15__init, 0, 6
	sw	$3, 0($4)
# was:	sw	iis__st_15__init, 0(iis__st_15__addr)
	addi	$3, $4, 8
# was:	addi	iis__st_15__init, iis__st_15__addr, 8
	sw	$3, 4($4)
# was:	sw	iis__st_15__init, 4(iis__st_15__addr)
	la	$3, iis__st_9_
# was:	la	iis__st_9__addr, iis__st_9_
	ori	$4, $0, 7
# was:	ori	iis__st_9__init, 0, 7
	sw	$4, 0($3)
# was:	sw	iis__st_9__init, 0(iis__st_9__addr)
	addi	$4, $3, 8
# was:	addi	iis__st_9__init, iis__st_9__addr, 8
	sw	$4, 4($3)
# was:	sw	iis__st_9__init, 4(iis__st_9__addr)
	jal	main
_stop_:
	ori	$2, $0, 10
	syscall
# Function main
main:
	sw	$31, -4($29)
	addi	$29, $29, -12
	sw	$17, 4($29)
	sw	$16, 0($29)
	jal	readInt
# was:	jal	readInt, 2
# 	ori	_assign__2_,2,0
# 	ori	i_local__1_,_assign__2_,0
	ori	$3, $0, 0
# was:	ori	_caseVal__5_, 0, 0
	beq	$3, $16, _sCase1__6_
# was:	beq	_caseVal__5_, _switchVal__4_, _sCase1__6_
	ori	$3, $0, 2
# was:	ori	_caseVal__5_, 0, 2
	beq	$3, $16, _sCase2__7_
# was:	beq	_caseVal__5_, _switchVal__4_, _sCase2__7_
	la	$17, iisneit_22_
# was:	la	_dat__21_, iisneit_22_
# iisneit_22_: string "i is neither 0 nor 1."
	lw	$16, 0($17)
# was:	lw	_arr_end__23_, 0(_dat__21_)
	lw	$17, 4($17)
# was:	lw	_dat__21_, 4(_dat__21_)
	add	$16, $16, $17
# was:	add	_arr_end__23_, _arr_end__23_, _dat__21_
_write_loop_beg_24_:
	beq	$17, $16, _write_loop_end_25_
# was:	beq	_dat__21_, _arr_end__23_, _write_loop_end_25_
	lb	$2, 0($17)
# was:	lb	2, 0(_dat__21_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$17, $17, 1
# was:	addi	_dat__21_, _dat__21_, 1
	j	_write_loop_beg_24_
_write_loop_end_25_:
	j	_sExit__3_
_sCase1__6_:
	la	$16, iis__st_9_
# was:	la	_dat__8_, iis__st_9_
# iis__st_9_: string "i is 0!"
	lw	$17, 0($16)
# was:	lw	_arr_end__10_, 0(_dat__8_)
	lw	$16, 4($16)
# was:	lw	_dat__8_, 4(_dat__8_)
	add	$17, $17, $16
# was:	add	_arr_end__10_, _arr_end__10_, _dat__8_
_write_loop_beg_11_:
	beq	$16, $17, _write_loop_end_12_
# was:	beq	_dat__8_, _arr_end__10_, _write_loop_end_12_
	lb	$2, 0($16)
# was:	lb	2, 0(_dat__8_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$16, $16, 1
# was:	addi	_dat__8_, _dat__8_, 1
	j	_write_loop_beg_11_
_write_loop_end_12_:
	j	_sExit__3_
_sCase2__7_:
	la	$17, iis__st_15_
# was:	la	_dat__14_, iis__st_15_
# iis__st_15_: string "i is 1"
	lw	$16, 0($17)
# was:	lw	_arr_end__16_, 0(_dat__14_)
	lw	$17, 4($17)
# was:	lw	_dat__14_, 4(_dat__14_)
	add	$16, $16, $17
# was:	add	_arr_end__16_, _arr_end__16_, _dat__14_
_write_loop_beg_17_:
	beq	$17, $16, _write_loop_end_18_
# was:	beq	_dat__14_, _arr_end__16_, _write_loop_end_18_
	lb	$2, 0($17)
# was:	lb	2, 0(_dat__14_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$17, $17, 1
# was:	addi	_dat__14_, _dat__14_, 1
	j	_write_loop_beg_17_
_write_loop_end_18_:
	ori	$2, $0, 33
# was:	ori	_assign__19_, 0, 33
	andi	$2, $2, 255
# was:	andi	c_local__13_, _assign__19_, 255
# 	ori	_dat__20_,c_local__13_,0
# 	ori	2,_dat__20_,0
	jal	writeChar
# was:	jal	writeChar, 2
	j	_sExit__3_
_sExit__3_:
	la	$17, bye__st_27_
# was:	la	_dat__26_, bye__st_27_
# bye__st_27_: string "\nbye!\n"
	lw	$16, 0($17)
# was:	lw	_arr_end__28_, 0(_dat__26_)
	lw	$17, 4($17)
# was:	lw	_dat__26_, 4(_dat__26_)
	add	$16, $16, $17
# was:	add	_arr_end__28_, _arr_end__28_, _dat__26_
_write_loop_beg_29_:
	beq	$17, $16, _write_loop_end_30_
# was:	beq	_dat__26_, _arr_end__28_, _write_loop_end_30_
	lb	$2, 0($17)
# was:	lb	2, 0(_dat__26_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$17, $17, 1
# was:	addi	_dat__26_, _dat__26_, 1
	j	_write_loop_beg_29_
_write_loop_end_30_:
main_exit:
	lw	$17, 4($29)
	lw	$16, 0($29)
	addi	$29, $29, 12
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
bye__st_27_:
	.space	8
	.ascii	"\nbye!\n"
	.align	2
iisneit_22_:
	.space	8
	.ascii	"i is neither 0 nor 1."
	.align	2
iis__st_15_:
	.space	8
	.ascii	"i is 1"
	.align	2
iis__st_9_:
	.space	8
	.ascii	"i is 0!"
	.align	2
_heap_:
	.space	100000