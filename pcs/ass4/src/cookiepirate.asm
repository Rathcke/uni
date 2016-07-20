
cookiepirate:     file format elf32-i386


Disassembly of section .init:

080483b8 <_init>:
 80483b8:	53                   	push   ebx
 80483b9:	83 ec 08             	sub    esp,0x8
 80483bc:	e8 ef 00 00 00       	call   80484b0 <__x86.get_pc_thunk.bx>
 80483c1:	81 c3 5f 16 00 00    	add    ebx,0x165f
 80483c7:	8b 83 fc ff ff ff    	mov    eax,DWORD PTR [ebx-0x4]
 80483cd:	85 c0                	test   eax,eax
 80483cf:	74 05                	je     80483d6 <_init+0x1e>
 80483d1:	e8 4a 00 00 00       	call   8048420 <__gmon_start__@plt>
 80483d6:	83 c4 08             	add    esp,0x8
 80483d9:	5b                   	pop    ebx
 80483da:	c3                   	ret    

Disassembly of section .plt:

080483e0 <read@plt-0x10>:
 80483e0:	ff 35 24 9a 04 08    	push   DWORD PTR ds:0x8049a24
 80483e6:	ff 25 28 9a 04 08    	jmp    DWORD PTR ds:0x8049a28
 80483ec:	00 00                	add    BYTE PTR [eax],al
	...

080483f0 <read@plt>:
 80483f0:	ff 25 2c 9a 04 08    	jmp    DWORD PTR ds:0x8049a2c
 80483f6:	68 00 00 00 00       	push   0x0
 80483fb:	e9 e0 ff ff ff       	jmp    80483e0 <_init+0x28>

08048400 <__stack_chk_fail@plt>:
 8048400:	ff 25 30 9a 04 08    	jmp    DWORD PTR ds:0x8049a30
 8048406:	68 08 00 00 00       	push   0x8
 804840b:	e9 d0 ff ff ff       	jmp    80483e0 <_init+0x28>

08048410 <puts@plt>:
 8048410:	ff 25 34 9a 04 08    	jmp    DWORD PTR ds:0x8049a34
 8048416:	68 10 00 00 00       	push   0x10
 804841b:	e9 c0 ff ff ff       	jmp    80483e0 <_init+0x28>

08048420 <__gmon_start__@plt>:
 8048420:	ff 25 38 9a 04 08    	jmp    DWORD PTR ds:0x8049a38
 8048426:	68 18 00 00 00       	push   0x18
 804842b:	e9 b0 ff ff ff       	jmp    80483e0 <_init+0x28>

08048430 <strlen@plt>:
 8048430:	ff 25 3c 9a 04 08    	jmp    DWORD PTR ds:0x8049a3c
 8048436:	68 20 00 00 00       	push   0x20
 804843b:	e9 a0 ff ff ff       	jmp    80483e0 <_init+0x28>

08048440 <__libc_start_main@plt>:
 8048440:	ff 25 40 9a 04 08    	jmp    DWORD PTR ds:0x8049a40
 8048446:	68 28 00 00 00       	push   0x28
 804844b:	e9 90 ff ff ff       	jmp    80483e0 <_init+0x28>

08048450 <write@plt>:
 8048450:	ff 25 44 9a 04 08    	jmp    DWORD PTR ds:0x8049a44
 8048456:	68 30 00 00 00       	push   0x30
 804845b:	e9 80 ff ff ff       	jmp    80483e0 <_init+0x28>

08048460 <memset@plt>:
 8048460:	ff 25 48 9a 04 08    	jmp    DWORD PTR ds:0x8049a48
 8048466:	68 38 00 00 00       	push   0x38
 804846b:	e9 70 ff ff ff       	jmp    80483e0 <_init+0x28>

08048470 <sprintf@plt>:
 8048470:	ff 25 4c 9a 04 08    	jmp    DWORD PTR ds:0x8049a4c
 8048476:	68 40 00 00 00       	push   0x40
 804847b:	e9 60 ff ff ff       	jmp    80483e0 <_init+0x28>

Disassembly of section .text:

08048480 <_start>:
 8048480:	31 ed                	xor    ebp,ebp
 8048482:	5e                   	pop    esi
 8048483:	89 e1                	mov    ecx,esp
 8048485:	83 e4 f0             	and    esp,0xfffffff0
 8048488:	50                   	push   eax
 8048489:	54                   	push   esp
 804848a:	52                   	push   edx
 804848b:	68 b0 87 04 08       	push   0x80487b0
 8048490:	68 40 87 04 08       	push   0x8048740
 8048495:	51                   	push   ecx
 8048496:	56                   	push   esi
 8048497:	68 00 86 04 08       	push   0x8048600
 804849c:	e8 9f ff ff ff       	call   8048440 <__libc_start_main@plt>
 80484a1:	f4                   	hlt    
 80484a2:	66 90                	xchg   ax,ax
 80484a4:	66 90                	xchg   ax,ax
 80484a6:	66 90                	xchg   ax,ax
 80484a8:	66 90                	xchg   ax,ax
 80484aa:	66 90                	xchg   ax,ax
 80484ac:	66 90                	xchg   ax,ax
 80484ae:	66 90                	xchg   ax,ax

080484b0 <__x86.get_pc_thunk.bx>:
 80484b0:	8b 1c 24             	mov    ebx,DWORD PTR [esp]
 80484b3:	c3                   	ret    
 80484b4:	66 90                	xchg   ax,ax
 80484b6:	66 90                	xchg   ax,ax
 80484b8:	66 90                	xchg   ax,ax
 80484ba:	66 90                	xchg   ax,ax
 80484bc:	66 90                	xchg   ax,ax
 80484be:	66 90                	xchg   ax,ax

080484c0 <deregister_tm_clones>:
 80484c0:	b8 5b 9a 04 08       	mov    eax,0x8049a5b
 80484c5:	2d 58 9a 04 08       	sub    eax,0x8049a58
 80484ca:	83 f8 06             	cmp    eax,0x6
 80484cd:	76 1a                	jbe    80484e9 <deregister_tm_clones+0x29>
 80484cf:	b8 00 00 00 00       	mov    eax,0x0
 80484d4:	85 c0                	test   eax,eax
 80484d6:	74 11                	je     80484e9 <deregister_tm_clones+0x29>
 80484d8:	55                   	push   ebp
 80484d9:	89 e5                	mov    ebp,esp
 80484db:	83 ec 14             	sub    esp,0x14
 80484de:	68 58 9a 04 08       	push   0x8049a58
 80484e3:	ff d0                	call   eax
 80484e5:	83 c4 10             	add    esp,0x10
 80484e8:	c9                   	leave  
 80484e9:	f3 c3                	repz ret 
 80484eb:	90                   	nop
 80484ec:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]

080484f0 <register_tm_clones>:
 80484f0:	b8 58 9a 04 08       	mov    eax,0x8049a58
 80484f5:	2d 58 9a 04 08       	sub    eax,0x8049a58
 80484fa:	c1 f8 02             	sar    eax,0x2
 80484fd:	89 c2                	mov    edx,eax
 80484ff:	c1 ea 1f             	shr    edx,0x1f
 8048502:	01 d0                	add    eax,edx
 8048504:	d1 f8                	sar    eax,1
 8048506:	74 1b                	je     8048523 <register_tm_clones+0x33>
 8048508:	ba 00 00 00 00       	mov    edx,0x0
 804850d:	85 d2                	test   edx,edx
 804850f:	74 12                	je     8048523 <register_tm_clones+0x33>
 8048511:	55                   	push   ebp
 8048512:	89 e5                	mov    ebp,esp
 8048514:	83 ec 10             	sub    esp,0x10
 8048517:	50                   	push   eax
 8048518:	68 58 9a 04 08       	push   0x8049a58
 804851d:	ff d2                	call   edx
 804851f:	83 c4 10             	add    esp,0x10
 8048522:	c9                   	leave  
 8048523:	f3 c3                	repz ret 
 8048525:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]
 8048529:	8d bc 27 00 00 00 00 	lea    edi,[edi+eiz*1+0x0]

08048530 <__do_global_dtors_aux>:
 8048530:	80 3d 58 9a 04 08 00 	cmp    BYTE PTR ds:0x8049a58,0x0
 8048537:	75 13                	jne    804854c <__do_global_dtors_aux+0x1c>
 8048539:	55                   	push   ebp
 804853a:	89 e5                	mov    ebp,esp
 804853c:	83 ec 08             	sub    esp,0x8
 804853f:	e8 7c ff ff ff       	call   80484c0 <deregister_tm_clones>
 8048544:	c6 05 58 9a 04 08 01 	mov    BYTE PTR ds:0x8049a58,0x1
 804854b:	c9                   	leave  
 804854c:	f3 c3                	repz ret 
 804854e:	66 90                	xchg   ax,ax

08048550 <frame_dummy>:
 8048550:	b8 28 99 04 08       	mov    eax,0x8049928
 8048555:	8b 10                	mov    edx,DWORD PTR [eax]
 8048557:	85 d2                	test   edx,edx
 8048559:	75 05                	jne    8048560 <frame_dummy+0x10>
 804855b:	eb 93                	jmp    80484f0 <register_tm_clones>
 804855d:	8d 76 00             	lea    esi,[esi+0x0]
 8048560:	ba 00 00 00 00       	mov    edx,0x0
 8048565:	85 d2                	test   edx,edx
 8048567:	74 f2                	je     804855b <frame_dummy+0xb>
 8048569:	55                   	push   ebp
 804856a:	89 e5                	mov    ebp,esp
 804856c:	83 ec 14             	sub    esp,0x14
 804856f:	50                   	push   eax
 8048570:	ff d2                	call   edx
 8048572:	83 c4 10             	add    esp,0x10
 8048575:	c9                   	leave  
 8048576:	e9 75 ff ff ff       	jmp    80484f0 <register_tm_clones>
 804857b:	66 90                	xchg   ax,ax
 804857d:	66 90                	xchg   ax,ax
 804857f:	90                   	nop

08048580 <readline>:
 8048580:	55                   	push   ebp
 8048581:	89 e5                	mov    ebp,esp
 8048583:	83 ec 28             	sub    esp,0x28                 ; SP points at -0x28
 8048586:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]  ; Fetch arg pointer
 8048589:	89 45 fc             	mov    DWORD PTR [ebp-0x4],eax  ; Throw it into -0x4
 804858c:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0  ; Init counter to 0 in -0xc
 8048593:	b8 00 00 00 00       	mov    eax,0x0                  ; Put 0 in eax
 8048598:	8d 4d fb             	lea    ecx,[ebp-0x5]            ; Put -0x5 in ecx
 804859b:	ba 01 00 00 00       	mov    edx,0x1                  ; Put 1 in edx
 80485a0:	c7 04 24 00 00 00 00 	mov    DWORD PTR [esp],0x0      ; ..
 80485a7:	89 4c 24 04          	mov    DWORD PTR [esp+0x4],ecx  ; ..
 80485ab:	c7 44 24 08 01 00 00 	mov    DWORD PTR [esp+0x8],0x1  ; ..
 80485b2:	00                                                    ; Stuff to read from stdin
 80485b3:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax ; ..
 80485b6:	89 55 ec             	mov    DWORD PTR [ebp-0x14],edx ; ..
 80485b9:	e8 32 fe ff ff       	call   80483f0 <read@plt>       ;  -- Read --
 80485be:	3d 00 00 00 00       	cmp    eax,0x0                  ; Is eax = 0?
 80485c3:	0f 8e 30 00 00 00    	jle    80485f9 <readline+0x79>  ; Jump to end
 80485c9:	0f be 45 fb          	movsx  eax,BYTE PTR [ebp-0x5]   ; Get next char
 80485cd:	3d 0a 00 00 00       	cmp    eax,0xa                  ; Is it a new line?
 80485d2:	0f 85 05 00 00 00    	jne    80485dd <readline+0x5d>  ; Continue if not
 80485d8:	e9 1c 00 00 00       	jmp    80485f9 <readline+0x79>  ; Otherwise, jump to the end
 80485dd:	8a 45 fb             	mov    al,BYTE PTR [ebp-0x5]    ; Fetch the character
 80485e0:	8b 4d f4             	mov    ecx,DWORD PTR [ebp-0xc]  ; Get counter in ecx
 80485e3:	89 ca                	mov    edx,ecx                  ; edx = ecx
 80485e5:	81 c2 01 00 00 00    	add    edx,0x1                  ; i++
 80485eb:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx  ; Put counter back
 80485ee:	8b 55 fc             	mov    edx,DWORD PTR [ebp-0x4]  ; Put arg pointer in edx
 80485f1:	88 04 0a             	mov    BYTE PTR [edx+ecx*1],al  ; Write the byte on correct pos
 80485f4:	e9 9a ff ff ff       	jmp    8048593 <readline+0x13>  ; Loop up
 80485f9:	83 c4 28             	add    esp,0x28
 80485fc:	5d                   	pop    ebp
 80485fd:	c3                   	ret    
 80485fe:	66 90                	xchg   ax,ax

08048600 <main>:
 8048600:	55                   	push   ebp
 8048601:	89 e5                	mov    ebp,esp
 8048603:	81 ec b8 00 00 00    	sub    esp,0xb8
 8048609:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 804860c:	8b 4d 08             	mov    ecx,DWORD PTR [ebp+0x8]
 804860f:	65 8b 15 14 00 00 00 	mov    edx,DWORD PTR gs:0x14        ; Fetch cookie
 8048616:	89 55 fc             	mov    DWORD PTR [ebp-0x4],edx      ; Store it in -0x4
 8048619:	c7 85 78 ff ff ff 00 	mov    DWORD PTR [ebp-0x88],0x0
 8048620:	00 00 00 
 8048623:	89 8d 74 ff ff ff    	mov    DWORD PTR [ebp-0x8c],ecx
 8048629:	89 85 70 ff ff ff    	mov    DWORD PTR [ebp-0x90],eax
 804862f:	89 e0                	mov    eax,esp
 8048631:	8d 8d 7c ff ff ff    	lea    ecx,[ebp-0x84]
 8048637:	89 08                	mov    DWORD PTR [eax],ecx
 8048639:	c7 40 08 80 00 00 00 	mov    DWORD PTR [eax+0x8],0x80
 8048640:	c7 40 04 00 00 00 00 	mov    DWORD PTR [eax+0x4],0x0
 8048647:	89 8d 6c ff ff ff    	mov    DWORD PTR [ebp-0x94],ecx
 804864d:	e8 0e fe ff ff       	call   8048460 <memset@plt>
 8048652:	89 e1                	mov    ecx,esp
 8048654:	c7 01 d0 87 04 08    	mov    DWORD PTR [ecx],0x80487d0
 804865a:	89 85 68 ff ff ff    	mov    DWORD PTR [ebp-0x98],eax
 8048660:	e8 ab fd ff ff       	call   8048410 <puts@plt>
 8048665:	89 e1                	mov    ecx,esp
 8048667:	8b 95 6c ff ff ff    	mov    edx,DWORD PTR [ebp-0x94]
 804866d:	89 11                	mov    DWORD PTR [ecx],edx
 804866f:	89 85 64 ff ff ff    	mov    DWORD PTR [ebp-0x9c],eax
 8048675:	e8 06 ff ff ff       	call   8048580 <readline>            ; We read the input line
 804867a:	89 e0                	mov    eax,esp
 804867c:	8b 8d 6c ff ff ff    	mov    ecx,DWORD PTR [ebp-0x94]
 8048682:	89 48 08             	mov    DWORD PTR [eax+0x8],ecx
 8048685:	c7 40 04 f0 87 04 08 	mov    DWORD PTR [eax+0x4],0x80487f0
 804868c:	c7 00 5c 9a 04 08    	mov    DWORD PTR [eax],0x8049a5c
 8048692:	e8 d9 fd ff ff       	call   8048470 <sprintf@plt>         ; Make our response
 8048697:	89 e1                	mov    ecx,esp
 8048699:	c7 01 5c 9a 04 08    	mov    DWORD PTR [ecx],0x8049a5c     ; How many bytes we want
 804869f:	89 85 60 ff ff ff    	mov    DWORD PTR [ebp-0xa0],eax      ; to write to stdout
 80486a5:	e8 86 fd ff ff       	call   8048430 <strlen@plt>          ; (we exploit this as it
 80486aa:	89 e1                	mov    ecx,esp                       ; counts until NULL byte)
 80486ac:	89 41 08             	mov    DWORD PTR [ecx+0x8],eax
 80486af:	c7 41 04 5c 9a 04 08 	mov    DWORD PTR [ecx+0x4],0x8049a5c
 80486b6:	c7 01 01 00 00 00    	mov    DWORD PTR [ecx],0x1
 80486bc:	e8 8f fd ff ff       	call   8048450 <write@plt>           ; Write it to global buf
 80486c1:	89 e1                	mov    ecx,esp                       ; (to stdout)
 80486c3:	8b 95 6c ff ff ff    	mov    edx,DWORD PTR [ebp-0x94]
 80486c9:	89 11                	mov    DWORD PTR [ecx],edx
 80486cb:	89 85 5c ff ff ff    	mov    DWORD PTR [ebp-0xa4],eax
 80486d1:	e8 aa fe ff ff       	call   8048580 <readline>            ; Read the next input line
 80486d6:	89 e0                	mov    eax,esp
 80486d8:	8b 8d 6c ff ff ff    	mov    ecx,DWORD PTR [ebp-0x94]
 80486de:	89 48 08             	mov    DWORD PTR [eax+0x8],ecx
 80486e1:	c7 40 04 3c 88 04 08 	mov    DWORD PTR [eax+0x4],0x804883c
 80486e8:	c7 00 5c 9a 04 08    	mov    DWORD PTR [eax],0x8049a5c
 80486ee:	e8 7d fd ff ff       	call   8048470 <sprintf@plt>         ; Make our response
 80486f3:	89 e1                	mov    ecx,esp
 80486f5:	c7 41 08 1e 00 00 00 	mov    DWORD PTR [ecx+0x8],0x1e
 80486fc:	c7 41 04 4b 88 04 08 	mov    DWORD PTR [ecx+0x4],0x804884b
 8048703:	c7 01 01 00 00 00    	mov    DWORD PTR [ecx],0x1
 8048709:	89 85 58 ff ff ff    	mov    DWORD PTR [ebp-0xa8],eax
 804870f:	e8 3c fd ff ff       	call   8048450 <write@plt>           ; Write it to global buf
 8048714:	65 8b 0d 14 00 00 00 	mov    ecx,DWORD PTR gs:0x14         ; Check cookie is correct
 804871b:	3b 4d fc             	cmp    ecx,DWORD PTR [ebp-0x4]
 804871e:	89 85 54 ff ff ff    	mov    DWORD PTR [ebp-0xac],eax
 8048724:	0f 85 0d 00 00 00    	jne    8048737 <main+0x137>
 804872a:	b8 00 00 00 00       	mov    eax,0x0
 804872f:	81 c4 b8 00 00 00    	add    esp,0xb8
 8048735:	5d                   	pop    ebp
 8048736:	c3                   	ret    
 8048737:	e8 c4 fc ff ff       	call   8048400 <__stack_chk_fail@plt>
 804873c:	66 90                	xchg   ax,ax
 804873e:	66 90                	xchg   ax,ax

08048740 <__libc_csu_init>:
 8048740:	55                   	push   ebp
 8048741:	57                   	push   edi
 8048742:	31 ff                	xor    edi,edi
 8048744:	56                   	push   esi
 8048745:	53                   	push   ebx
 8048746:	e8 65 fd ff ff       	call   80484b0 <__x86.get_pc_thunk.bx>
 804874b:	81 c3 d5 12 00 00    	add    ebx,0x12d5
 8048751:	83 ec 1c             	sub    esp,0x1c
 8048754:	8b 6c 24 30          	mov    ebp,DWORD PTR [esp+0x30]
 8048758:	8d b3 04 ff ff ff    	lea    esi,[ebx-0xfc]
 804875e:	e8 55 fc ff ff       	call   80483b8 <_init>
 8048763:	8d 83 00 ff ff ff    	lea    eax,[ebx-0x100]
 8048769:	29 c6                	sub    esi,eax
 804876b:	c1 fe 02             	sar    esi,0x2
 804876e:	85 f6                	test   esi,esi
 8048770:	74 27                	je     8048799 <__libc_csu_init+0x59>
 8048772:	8d b6 00 00 00 00    	lea    esi,[esi+0x0]
 8048778:	8b 44 24 38          	mov    eax,DWORD PTR [esp+0x38]
 804877c:	89 2c 24             	mov    DWORD PTR [esp],ebp
 804877f:	89 44 24 08          	mov    DWORD PTR [esp+0x8],eax
 8048783:	8b 44 24 34          	mov    eax,DWORD PTR [esp+0x34]
 8048787:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 804878b:	ff 94 bb 00 ff ff ff 	call   DWORD PTR [ebx+edi*4-0x100]
 8048792:	83 c7 01             	add    edi,0x1
 8048795:	39 f7                	cmp    edi,esi
 8048797:	75 df                	jne    8048778 <__libc_csu_init+0x38>
 8048799:	83 c4 1c             	add    esp,0x1c
 804879c:	5b                   	pop    ebx
 804879d:	5e                   	pop    esi
 804879e:	5f                   	pop    edi
 804879f:	5d                   	pop    ebp
 80487a0:	c3                   	ret    
 80487a1:	eb 0d                	jmp    80487b0 <__libc_csu_fini>
 80487a3:	90                   	nop
 80487a4:	90                   	nop
 80487a5:	90                   	nop
 80487a6:	90                   	nop
 80487a7:	90                   	nop
 80487a8:	90                   	nop
 80487a9:	90                   	nop
 80487aa:	90                   	nop
 80487ab:	90                   	nop
 80487ac:	90                   	nop
 80487ad:	90                   	nop
 80487ae:	90                   	nop
 80487af:	90                   	nop

080487b0 <__libc_csu_fini>:
 80487b0:	f3 c3                	repz ret 

Disassembly of section .fini:

080487b4 <_fini>:
 80487b4:	53                   	push   ebx
 80487b5:	83 ec 08             	sub    esp,0x8
 80487b8:	e8 f3 fc ff ff       	call   80484b0 <__x86.get_pc_thunk.bx>
 80487bd:	81 c3 63 12 00 00    	add    ebx,0x1263
 80487c3:	83 c4 08             	add    esp,0x8
 80487c6:	5b                   	pop    ebx
 80487c7:	c3                   	ret    
