	.text	0x00400000
	.globl	main
	la	$28, _heap_
	la	$4, Nay__st_69_
# was:	la	Nay__st_69__addr, Nay__st_69_
	ori	$3, $0, 3
# was:	ori	Nay__st_69__init, 0, 3
	sw	$3, 0($4)
# was:	sw	Nay__st_69__init, 0(Nay__st_69__addr)
	addi	$3, $4, 8
# was:	addi	Nay__st_69__init, Nay__st_69__addr, 8
	sw	$3, 4($4)
# was:	sw	Nay__st_69__init, 4(Nay__st_69__addr)
	la	$3, Yay__st_64_
# was:	la	Yay__st_64__addr, Yay__st_64_
	ori	$4, $0, 3
# was:	ori	Yay__st_64__init, 0, 3
	sw	$4, 0($3)
# was:	sw	Yay__st_64__init, 0(Yay__st_64__addr)
	addi	$4, $3, 8
# was:	addi	Yay__st_64__init, Yay__st_64__addr, 8
	sw	$4, 4($3)
# was:	sw	Yay__st_64__init, 4(Yay__st_64__addr)
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
	ori	$4, $0, 1
# was:	ori	_check__8_, 0, 1
	ori	$7, $28, 0
# was:	ori	_assign__3_, 28, 0
	addi	$28, $28, 16
# was:	addi	28, 28, 16
	ori	$3, $0, 1
# was:	ori	_tmp__4_, 0, 1
	ori	$6, $0, 2
# was:	ori	_ereg__5_, 0, 2
	slt	$5, $0, $6
# was:	slt	_cond__9_, 0, _ereg__5_
	and	$4, $4, $5
# was:	and	_check__8_, _check__8_, _cond__9_
	sw	$6, 4($7)
# was:	sw	_ereg__5_, 4(_assign__3_)
	mul	$3, $3, $6
# was:	mul	_tmp__4_, _tmp__4_, _ereg__5_
	sw	$3, 8($7)
# was:	sw	_tmp__4_, 8(_assign__3_)
	ori	$6, $0, 1
# was:	ori	_ereg__5_, 0, 1
	slt	$5, $0, $6
# was:	slt	_cond__10_, 0, _ereg__5_
	and	$4, $4, $5
# was:	and	_check__8_, _check__8_, _cond__10_
	ori	$5, $0, 8
# was:	ori	5, 0, 8
	beq	$4, $0, _IllegalArrSizeError_
# was:	beq	_check__8_, 0, _IllegalArrSizeError_
	sw	$6, 0($7)
# was:	sw	_ereg__5_, 0(_assign__3_)
	mul	$3, $3, $6
# was:	mul	_tmp__4_, _tmp__4_, _ereg__5_
	sw	$28, 12($7)
# was:	sw	28, 12(_assign__3_)
	ori	$6, $3, 0
# was:	ori	_ereg__5_, _tmp__4_, 0
	addi	$6, $6, 3
# was:	addi	_ereg__5_, _ereg__5_, 3
	sra	$6, $6, 2
# was:	sra	_ereg__5_, _ereg__5_, 2
	sll	$6, $6, 2
# was:	sll	_ereg__5_, _ereg__5_, 2
	ori	$3, $28, 0
# was:	ori	_tmp__4_, 28, 0
	add	$28, $28, $6
# was:	add	28, 28, _ereg__5_
_loop_beg__6_:
	beq	$3, $28, _loop_end__7_
# was:	beq	_tmp__4_, 28, _loop_end__7_
	sb	$0, 0($3)
# was:	sb	0, 0(_tmp__4_)
	addi	$3, $3, 1
# was:	addi	_tmp__4_, _tmp__4_, 1
	j	_loop_beg__6_
_loop_end__7_:
# 	ori	arr2_local__1_,_assign__3_,0
	ori	$3, $0, 1
# was:	ori	_safe__14_, 0, 1
	ori	$4, $0, 0
# was:	ori	_tmp__11_, 0, 0
	ori	$5, $0, -1
# was:	ori	_cond__15_, 0, -1
	slt	$5, $5, $4
# was:	slt	_cond__15_, _cond__15_, _tmp__11_
	and	$3, $3, $5
# was:	and	_safe__14_, _safe__14_, _cond__15_
	lw	$5, 4($7)
# was:	lw	_cond__15_, 4(arr2_local__1_)
	slt	$5, $4, $5
# was:	slt	_cond__15_, _tmp__11_, _cond__15_
	and	$3, $3, $5
# was:	and	_safe__14_, _safe__14_, _cond__15_
	ori	$5, $0, 0
# was:	ori	_ereg__12_, 0, 0
	ori	$6, $0, -1
# was:	ori	_cond__16_, 0, -1
	slt	$6, $6, $5
# was:	slt	_cond__16_, _cond__16_, _ereg__12_
	and	$3, $3, $6
# was:	and	_safe__14_, _safe__14_, _cond__16_
	lw	$6, 0($7)
# was:	lw	_cond__16_, 0(arr2_local__1_)
	slt	$6, $5, $6
# was:	slt	_cond__16_, _ereg__12_, _cond__16_
	and	$3, $3, $6
# was:	and	_safe__14_, _safe__14_, _cond__16_
	lw	$6, 8($7)
# was:	lw	_tmp__13_, 8(arr2_local__1_)
	mul	$5, $5, $6
# was:	mul	_ereg__12_, _ereg__12_, _tmp__13_
	add	$4, $4, $5
# was:	add	_tmp__11_, _tmp__11_, _ereg__12_
	ori	$5, $0, 9
# was:	ori	5, 0, 9
	beq	$3, $0, _IllegalArrIndexError_
# was:	beq	_safe__14_, 0, _IllegalArrIndexError_
	lw	$5, 12($7)
# was:	lw	_ereg__12_, 12(arr2_local__1_)
	add	$5, $5, $4
# was:	add	_ereg__12_, _ereg__12_, _tmp__11_
	ori	$3, $0, 97
# was:	ori	_assign__17_, 0, 97
	sb	$3, 0($5)
# was:	sb	_assign__17_, 0(_ereg__12_)
	ori	$4, $0, 1
# was:	ori	_safe__21_, 0, 1
	ori	$3, $0, 1
# was:	ori	_tmp__18_, 0, 1
	ori	$5, $0, -1
# was:	ori	_cond__22_, 0, -1
	slt	$5, $5, $3
# was:	slt	_cond__22_, _cond__22_, _tmp__18_
	and	$4, $4, $5
# was:	and	_safe__21_, _safe__21_, _cond__22_
	lw	$5, 4($7)
# was:	lw	_cond__22_, 4(arr2_local__1_)
	slt	$5, $3, $5
# was:	slt	_cond__22_, _tmp__18_, _cond__22_
	and	$4, $4, $5
# was:	and	_safe__21_, _safe__21_, _cond__22_
	ori	$5, $0, 0
# was:	ori	_ereg__19_, 0, 0
	ori	$6, $0, -1
# was:	ori	_cond__23_, 0, -1
	slt	$6, $6, $5
# was:	slt	_cond__23_, _cond__23_, _ereg__19_
	and	$4, $4, $6
# was:	and	_safe__21_, _safe__21_, _cond__23_
	lw	$6, 0($7)
# was:	lw	_cond__23_, 0(arr2_local__1_)
	slt	$6, $5, $6
# was:	slt	_cond__23_, _ereg__19_, _cond__23_
	and	$4, $4, $6
# was:	and	_safe__21_, _safe__21_, _cond__23_
	lw	$6, 8($7)
# was:	lw	_tmp__20_, 8(arr2_local__1_)
	mul	$5, $5, $6
# was:	mul	_ereg__19_, _ereg__19_, _tmp__20_
	add	$3, $3, $5
# was:	add	_tmp__18_, _tmp__18_, _ereg__19_
	ori	$5, $0, 10
# was:	ori	5, 0, 10
	beq	$4, $0, _IllegalArrIndexError_
# was:	beq	_safe__21_, 0, _IllegalArrIndexError_
	lw	$5, 12($7)
# was:	lw	_ereg__19_, 12(arr2_local__1_)
	add	$5, $5, $3
# was:	add	_ereg__19_, _ereg__19_, _tmp__18_
	ori	$3, $0, 98
# was:	ori	_assign__24_, 0, 98
	sb	$3, 0($5)
# was:	sb	_assign__24_, 0(_ereg__19_)
	ori	$3, $0, 1
# was:	ori	_check__30_, 0, 1
	ori	$4, $28, 0
# was:	ori	_assign__25_, 28, 0
	addi	$28, $28, 16
# was:	addi	28, 28, 16
	ori	$6, $0, 1
# was:	ori	_tmp__26_, 0, 1
	ori	$8, $0, 2
# was:	ori	_ereg__27_, 0, 2
	slt	$5, $0, $8
# was:	slt	_cond__31_, 0, _ereg__27_
	and	$3, $3, $5
# was:	and	_check__30_, _check__30_, _cond__31_
	sw	$8, 4($4)
# was:	sw	_ereg__27_, 4(_assign__25_)
	mul	$6, $6, $8
# was:	mul	_tmp__26_, _tmp__26_, _ereg__27_
	sw	$6, 8($4)
# was:	sw	_tmp__26_, 8(_assign__25_)
	ori	$8, $0, 1
# was:	ori	_ereg__27_, 0, 1
	slt	$5, $0, $8
# was:	slt	_cond__32_, 0, _ereg__27_
	and	$3, $3, $5
# was:	and	_check__30_, _check__30_, _cond__32_
	ori	$5, $0, 11
# was:	ori	5, 0, 11
	beq	$3, $0, _IllegalArrSizeError_
# was:	beq	_check__30_, 0, _IllegalArrSizeError_
	sw	$8, 0($4)
# was:	sw	_ereg__27_, 0(_assign__25_)
	mul	$6, $6, $8
# was:	mul	_tmp__26_, _tmp__26_, _ereg__27_
	sw	$28, 12($4)
# was:	sw	28, 12(_assign__25_)
	ori	$8, $6, 0
# was:	ori	_ereg__27_, _tmp__26_, 0
	addi	$8, $8, 3
# was:	addi	_ereg__27_, _ereg__27_, 3
	sra	$8, $8, 2
# was:	sra	_ereg__27_, _ereg__27_, 2
	sll	$8, $8, 2
# was:	sll	_ereg__27_, _ereg__27_, 2
	ori	$6, $28, 0
# was:	ori	_tmp__26_, 28, 0
	add	$28, $28, $8
# was:	add	28, 28, _ereg__27_
_loop_beg__28_:
	beq	$6, $28, _loop_end__29_
# was:	beq	_tmp__26_, 28, _loop_end__29_
	sb	$0, 0($6)
# was:	sb	0, 0(_tmp__26_)
	addi	$6, $6, 1
# was:	addi	_tmp__26_, _tmp__26_, 1
	j	_loop_beg__28_
_loop_end__29_:
	ori	$6, $4, 0
# was:	ori	arr_local__2_, _assign__25_, 0
	ori	$4, $0, 1
# was:	ori	_safe__36_, 0, 1
	ori	$3, $0, 0
# was:	ori	_tmp__33_, 0, 0
	ori	$5, $0, -1
# was:	ori	_cond__37_, 0, -1
	slt	$5, $5, $3
# was:	slt	_cond__37_, _cond__37_, _tmp__33_
	and	$4, $4, $5
# was:	and	_safe__36_, _safe__36_, _cond__37_
	lw	$5, 4($6)
# was:	lw	_cond__37_, 4(arr_local__2_)
	slt	$5, $3, $5
# was:	slt	_cond__37_, _tmp__33_, _cond__37_
	and	$4, $4, $5
# was:	and	_safe__36_, _safe__36_, _cond__37_
	ori	$5, $0, 0
# was:	ori	_ereg__34_, 0, 0
	ori	$8, $0, -1
# was:	ori	_cond__38_, 0, -1
	slt	$8, $8, $5
# was:	slt	_cond__38_, _cond__38_, _ereg__34_
	and	$4, $4, $8
# was:	and	_safe__36_, _safe__36_, _cond__38_
	lw	$8, 0($6)
# was:	lw	_cond__38_, 0(arr_local__2_)
	slt	$8, $5, $8
# was:	slt	_cond__38_, _ereg__34_, _cond__38_
	and	$4, $4, $8
# was:	and	_safe__36_, _safe__36_, _cond__38_
	lw	$8, 8($6)
# was:	lw	_tmp__35_, 8(arr_local__2_)
	mul	$5, $5, $8
# was:	mul	_ereg__34_, _ereg__34_, _tmp__35_
	add	$3, $3, $5
# was:	add	_tmp__33_, _tmp__33_, _ereg__34_
	ori	$5, $0, 12
# was:	ori	5, 0, 12
	beq	$4, $0, _IllegalArrIndexError_
# was:	beq	_safe__36_, 0, _IllegalArrIndexError_
	lw	$5, 12($6)
# was:	lw	_ereg__34_, 12(arr_local__2_)
	add	$5, $5, $3
# was:	add	_ereg__34_, _ereg__34_, _tmp__33_
	ori	$3, $0, 97
# was:	ori	_assign__39_, 0, 97
	sb	$3, 0($5)
# was:	sb	_assign__39_, 0(_ereg__34_)
	ori	$3, $0, 1
# was:	ori	_safe__43_, 0, 1
	ori	$4, $0, 1
# was:	ori	_tmp__40_, 0, 1
	ori	$5, $0, -1
# was:	ori	_cond__44_, 0, -1
	slt	$5, $5, $4
# was:	slt	_cond__44_, _cond__44_, _tmp__40_
	and	$3, $3, $5
# was:	and	_safe__43_, _safe__43_, _cond__44_
	lw	$5, 4($6)
# was:	lw	_cond__44_, 4(arr_local__2_)
	slt	$5, $4, $5
# was:	slt	_cond__44_, _tmp__40_, _cond__44_
	and	$3, $3, $5
# was:	and	_safe__43_, _safe__43_, _cond__44_
	ori	$5, $0, 0
# was:	ori	_ereg__41_, 0, 0
	ori	$8, $0, -1
# was:	ori	_cond__45_, 0, -1
	slt	$8, $8, $5
# was:	slt	_cond__45_, _cond__45_, _ereg__41_
	and	$3, $3, $8
# was:	and	_safe__43_, _safe__43_, _cond__45_
	lw	$8, 0($6)
# was:	lw	_cond__45_, 0(arr_local__2_)
	slt	$8, $5, $8
# was:	slt	_cond__45_, _ereg__41_, _cond__45_
	and	$3, $3, $8
# was:	and	_safe__43_, _safe__43_, _cond__45_
	lw	$8, 8($6)
# was:	lw	_tmp__42_, 8(arr_local__2_)
	mul	$5, $5, $8
# was:	mul	_ereg__41_, _ereg__41_, _tmp__42_
	add	$4, $4, $5
# was:	add	_tmp__40_, _tmp__40_, _ereg__41_
	ori	$5, $0, 13
# was:	ori	5, 0, 13
	beq	$3, $0, _IllegalArrIndexError_
# was:	beq	_safe__43_, 0, _IllegalArrIndexError_
	lw	$5, 12($6)
# was:	lw	_ereg__41_, 12(arr_local__2_)
	add	$5, $5, $4
# was:	add	_ereg__41_, _ereg__41_, _tmp__40_
	ori	$3, $0, 98
# was:	ori	_assign__46_, 0, 98
	sb	$3, 0($5)
# was:	sb	_assign__46_, 0(_ereg__41_)
	ori	$3, $6, 0
# was:	ori	eq1__50_, arr_local__2_, 0
# 	ori	eq2__51_,arr2_local__1_,0
	addi	$8, $0, 1
# was:	addi	_nextElm__54_, 0, 1
	ori	$9, $0, 0
# was:	ori	_if__47_, 0, 0
	addi	$10, $0, 0
# was:	addi	_endOf__53_, 0, 0
	addi	$4, $0, 1
# was:	addi	_t5__62_, 0, 1
	addi	$10, $10, 2
# was:	addi	_endOf__53_, _endOf__53_, 2
	sll	$10, $10, 2
# was:	sll	_endOf__53_, _endOf__53_, 2
_chk_shp__56_:
	beq	$10, $0, _shpOk__58_
# was:	beq	_endOf__53_, 0, _shpOk__58_
	lw	$6, 0($3)
# was:	lw	_t3__60_, 0(eq1__50_)
	lw	$5, 0($7)
# was:	lw	_t4__61_, 0(eq2__51_)
	addi	$3, $3, 4
# was:	addi	eq1__50_, eq1__50_, 4
	addi	$7, $7, 4
# was:	addi	eq2__51_, eq2__51_, 4
	addi	$10, $10, -4
# was:	addi	_endOf__53_, _endOf__53_, -4
	mul	$4, $4, $6
# was:	mul	_t5__62_, _t5__62_, _t3__60_
	bne	$6, $5, _equal__52_
# was:	bne	_t3__60_, _t4__61_, _equal__52_
	j	_chk_shp__56_
_shpOk__58_:
	addi	$10, $0, 2
# was:	addi	_endOf__53_, 0, 2
	sll	$10, $10, 2
# was:	sll	_endOf__53_, _endOf__53_, 2
	add	$3, $3, $10
# was:	add	eq1__50_, eq1__50_, _endOf__53_
	add	$7, $7, $10
# was:	add	eq2__51_, eq2__51_, _endOf__53_
	mul	$10, $4, $8
# was:	mul	_endOf__53_, _t5__62_, _nextElm__54_
_chk_elm__57_:
	beq	$10, $0, _elmOk__59_
# was:	beq	_endOf__53_, 0, _elmOk__59_
	lw	$6, 0($3)
# was:	lw	_t3__60_, 0(eq1__50_)
	lw	$5, 0($7)
# was:	lw	_t4__61_, 0(eq2__51_)
	add	$3, $3, $8
# was:	add	eq1__50_, eq1__50_, _nextElm__54_
	add	$7, $7, $8
# was:	add	eq2__51_, eq2__51_, _nextElm__54_
	sub	$10, $10, $8
# was:	sub	_endOf__53_, _endOf__53_, _nextElm__54_
	bne	$6, $5, _equal__52_
# was:	bne	_t3__60_, _t4__61_, _equal__52_
	j	_chk_elm__57_
_elmOk__59_:
	ori	$9, $0, 1
# was:	ori	_if__47_, 0, 1
_equal__52_:
	beq	$9, $0, _else__48_
# was:	beq	_if__47_, 0, _else__48_
	la	$16, Yay__st_64_
# was:	la	_dat__63_, Yay__st_64_
# Yay__st_64_: string "Yay"
	lw	$17, 0($16)
# was:	lw	_arr_end__65_, 0(_dat__63_)
	lw	$16, 4($16)
# was:	lw	_dat__63_, 4(_dat__63_)
	add	$17, $17, $16
# was:	add	_arr_end__65_, _arr_end__65_, _dat__63_
_write_loop_beg_66_:
	beq	$16, $17, _write_loop_end_67_
# was:	beq	_dat__63_, _arr_end__65_, _write_loop_end_67_
	lb	$2, 0($16)
# was:	lb	2, 0(_dat__63_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$16, $16, 1
# was:	addi	_dat__63_, _dat__63_, 1
	j	_write_loop_beg_66_
_write_loop_end_67_:
	j	_endif__49_
_else__48_:
	la	$16, Nay__st_69_
# was:	la	_dat__68_, Nay__st_69_
# Nay__st_69_: string "Nay"
	lw	$17, 0($16)
# was:	lw	_arr_end__70_, 0(_dat__68_)
	lw	$16, 4($16)
# was:	lw	_dat__68_, 4(_dat__68_)
	add	$17, $17, $16
# was:	add	_arr_end__70_, _arr_end__70_, _dat__68_
_write_loop_beg_71_:
	beq	$16, $17, _write_loop_end_72_
# was:	beq	_dat__68_, _arr_end__70_, _write_loop_end_72_
	lb	$2, 0($16)
# was:	lb	2, 0(_dat__68_)
	jal	writeChar
# was:	jal	writeChar, 2
	addi	$16, $16, 1
# was:	addi	_dat__68_, _dat__68_, 1
	j	_write_loop_beg_71_
_write_loop_end_72_:
_endif__49_:
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
Nay__st_69_:
	.space	8
	.ascii	"Nay"
	.align	2
Yay__st_64_:
	.space	8
	.ascii	"Yay"
	.align	2
_heap_:
	.space	100000
