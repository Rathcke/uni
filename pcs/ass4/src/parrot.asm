
parrot:     file format elf32-i386


Disassembly of section .init:

0804834c <_init>:
 804834c:	55                   	push   ebp
 804834d:	89 e5                	mov    ebp,esp
 804834f:	53                   	push   ebx
 8048350:	83 ec 04             	sub    esp,0x4
 8048353:	e8 00 00 00 00       	call   8048358 <_init+0xc>
 8048358:	5b                   	pop    ebx
 8048359:	81 c3 bc 14 00 00    	add    ebx,0x14bc
 804835f:	8b 93 fc ff ff ff    	mov    edx,DWORD PTR [ebx-0x4]
 8048365:	85 d2                	test   edx,edx
 8048367:	74 05                	je     804836e <_init+0x22>
 8048369:	e8 42 00 00 00       	call   80483b0 <__gmon_start__@plt>
 804836e:	58                   	pop    eax
 804836f:	5b                   	pop    ebx
 8048370:	c9                   	leave  
 8048371:	c3                   	ret    

Disassembly of section .plt:

08048380 <printf@plt-0x10>:
 8048380:	ff 35 18 98 04 08    	push   DWORD PTR ds:0x8049818
 8048386:	ff 25 1c 98 04 08    	jmp    DWORD PTR ds:0x804981c
 804838c:	00 00                	add    BYTE PTR [eax],al
	...

08048390 <printf@plt>:
 8048390:	ff 25 20 98 04 08    	jmp    DWORD PTR ds:0x8049820
 8048396:	68 00 00 00 00       	push   0x0
 804839b:	e9 e0 ff ff ff       	jmp    8048380 <_init+0x34>

080483a0 <__stack_chk_fail@plt>:
 80483a0:	ff 25 24 98 04 08    	jmp    DWORD PTR ds:0x8049824
 80483a6:	68 08 00 00 00       	push   0x8
 80483ab:	e9 d0 ff ff ff       	jmp    8048380 <_init+0x34>

080483b0 <__gmon_start__@plt>:
 80483b0:	ff 25 28 98 04 08    	jmp    DWORD PTR ds:0x8049828
 80483b6:	68 10 00 00 00       	push   0x10
 80483bb:	e9 c0 ff ff ff       	jmp    8048380 <_init+0x34>

080483c0 <__libc_start_main@plt>:
 80483c0:	ff 25 2c 98 04 08    	jmp    DWORD PTR ds:0x804982c
 80483c6:	68 18 00 00 00       	push   0x18
 80483cb:	e9 b0 ff ff ff       	jmp    8048380 <_init+0x34>

080483d0 <fprintf@plt>:
 80483d0:	ff 25 30 98 04 08    	jmp    DWORD PTR ds:0x8049830
 80483d6:	68 20 00 00 00       	push   0x20
 80483db:	e9 a0 ff ff ff       	jmp    8048380 <_init+0x34>

Disassembly of section .text:

080483e0 <_start>:
 80483e0:	31 ed                	xor    ebp,ebp
 80483e2:	5e                   	pop    esi
 80483e3:	89 e1                	mov    ecx,esp
 80483e5:	83 e4 f0             	and    esp,0xfffffff0
 80483e8:	50                   	push   eax
 80483e9:	54                   	push   esp
 80483ea:	52                   	push   edx
 80483eb:	68 b0 85 04 08       	push   0x80485b0
 80483f0:	68 c0 85 04 08       	push   0x80485c0
 80483f5:	51                   	push   ecx
 80483f6:	56                   	push   esi
 80483f7:	68 51 85 04 08       	push   0x8048551
 80483fc:	e8 bf ff ff ff       	call   80483c0 <__libc_start_main@plt>
 8048401:	f4                   	hlt    
 8048402:	90                   	nop
 8048403:	90                   	nop
 8048404:	90                   	nop
 8048405:	90                   	nop
 8048406:	90                   	nop
 8048407:	90                   	nop
 8048408:	90                   	nop
 8048409:	90                   	nop
 804840a:	90                   	nop
 804840b:	90                   	nop
 804840c:	90                   	nop
 804840d:	90                   	nop
 804840e:	90                   	nop
 804840f:	90                   	nop

08048410 <deregister_tm_clones>:
 8048410:	b8 3f 98 04 08       	mov    eax,0x804983f
 8048415:	2d 3c 98 04 08       	sub    eax,0x804983c
 804841a:	83 f8 06             	cmp    eax,0x6
 804841d:	77 02                	ja     8048421 <deregister_tm_clones+0x11>
 804841f:	f3 c3                	repz ret 
 8048421:	b8 00 00 00 00       	mov    eax,0x0
 8048426:	85 c0                	test   eax,eax
 8048428:	74 f5                	je     804841f <deregister_tm_clones+0xf>
 804842a:	55                   	push   ebp
 804842b:	89 e5                	mov    ebp,esp
 804842d:	83 ec 18             	sub    esp,0x18
 8048430:	c7 04 24 3c 98 04 08 	mov    DWORD PTR [esp],0x804983c
 8048437:	ff d0                	call   eax
 8048439:	c9                   	leave  
 804843a:	c3                   	ret    
 804843b:	90                   	nop
 804843c:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]

08048440 <register_tm_clones>:
 8048440:	b8 3c 98 04 08       	mov    eax,0x804983c
 8048445:	2d 3c 98 04 08       	sub    eax,0x804983c
 804844a:	c1 f8 02             	sar    eax,0x2
 804844d:	89 c2                	mov    edx,eax
 804844f:	c1 ea 1f             	shr    edx,0x1f
 8048452:	01 d0                	add    eax,edx
 8048454:	d1 f8                	sar    eax,1
 8048456:	75 02                	jne    804845a <register_tm_clones+0x1a>
 8048458:	f3 c3                	repz ret 
 804845a:	ba 00 00 00 00       	mov    edx,0x0
 804845f:	85 d2                	test   edx,edx
 8048461:	74 f5                	je     8048458 <register_tm_clones+0x18>
 8048463:	55                   	push   ebp
 8048464:	89 e5                	mov    ebp,esp
 8048466:	83 ec 18             	sub    esp,0x18
 8048469:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 804846d:	c7 04 24 3c 98 04 08 	mov    DWORD PTR [esp],0x804983c
 8048474:	ff d2                	call   edx
 8048476:	c9                   	leave  
 8048477:	c3                   	ret    
 8048478:	90                   	nop
 8048479:	8d b4 26 00 00 00 00 	lea    esi,[esi+eiz*1+0x0]

08048480 <__do_global_dtors_aux>:
 8048480:	80 3d 40 98 04 08 00 	cmp    BYTE PTR ds:0x8049840,0x0
 8048487:	75 13                	jne    804849c <__do_global_dtors_aux+0x1c>
 8048489:	55                   	push   ebp
 804848a:	89 e5                	mov    ebp,esp
 804848c:	83 ec 08             	sub    esp,0x8
 804848f:	e8 7c ff ff ff       	call   8048410 <deregister_tm_clones>
 8048494:	c6 05 40 98 04 08 01 	mov    BYTE PTR ds:0x8049840,0x1
 804849b:	c9                   	leave  
 804849c:	f3 c3                	repz ret 
 804849e:	66 90                	xchg   ax,ax

080484a0 <frame_dummy>:
 80484a0:	a1 1c 97 04 08       	mov    eax,ds:0x804971c
 80484a5:	85 c0                	test   eax,eax
 80484a7:	74 1e                	je     80484c7 <frame_dummy+0x27>
 80484a9:	b8 00 00 00 00       	mov    eax,0x0
 80484ae:	85 c0                	test   eax,eax
 80484b0:	74 15                	je     80484c7 <frame_dummy+0x27>
 80484b2:	55                   	push   ebp
 80484b3:	89 e5                	mov    ebp,esp
 80484b5:	83 ec 18             	sub    esp,0x18
 80484b8:	c7 04 24 1c 97 04 08 	mov    DWORD PTR [esp],0x804971c
 80484bf:	ff d0                	call   eax
 80484c1:	c9                   	leave  
 80484c2:	e9 79 ff ff ff       	jmp    8048440 <register_tm_clones>
 80484c7:	e9 74 ff ff ff       	jmp    8048440 <register_tm_clones>

080484cc <parse>:
 80484cc:	55                   	push   ebp
 80484cd:	89 e5                	mov    ebp,esp
 80484cf:	83 ec 68             	sub    esp,0x68                     ; SP points at -0x68
 80484d2:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]      ; Fetch arg pointer
 80484d5:	89 45 b4             	mov    DWORD PTR [ebp-0x4c],eax     ; Pointer to arg in -0x4c
 80484d8:	65 a1 14 00 00 00    	mov    eax,gs:0x14                  ; Fetch cookie
 80484de:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax      ; Cookiepointer in -0xc
 80484e1:	31 c0                	xor    eax,eax                      ; Clear register
 80484e3:	8b 45 b4             	mov    eax,DWORD PTR [ebp-0x4c]     ; Fetch arg pointer
 80484e6:	89 45 c4             	mov    DWORD PTR [ebp-0x3c],eax     ; Put arg pointer in -0x3c
 80484e9:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0     ; Put 0 in -x10 (Counter)
 80484f0:	eb 1b                	jmp    804850d <parse+0x41>         ;  -- Parse --
 80484f2:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]     ; eax=counter (loop begin)
 80484f5:	8b 4d c4             	mov    ecx,DWORD PTR [ebp-0x3c]     ; ecx = arg pointer
 80484f8:	8b 55 f0             	mov    edx,DWORD PTR [ebp-0x10]     ; Fetch counter to edx
 80484fb:	01 ca                	add    edx,ecx                      ; Add counter i to pointer
 80484fd:	0f b6 12             	movzx  edx,BYTE PTR [edx]           ; Get the byte i
 8048500:	88 54 05 c8          	mov    BYTE PTR [ebp+eax*1-0x38],dl ; Store byte in buffer
 8048504:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]     ; Get counter i
 8048507:	83 c0 01             	add    eax,0x1                      ; i++
 804850a:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax     ; Put counter back
 804850d:	8b 55 c4             	mov    edx,DWORD PTR [ebp-0x3c]     ; edx = arg pointer
 8048510:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]     ; eax = counter pointer
 8048513:	01 d0                	add    eax,edx                      ; Get the next byte
 8048515:	0f b6 00             	movzx  eax,BYTE PTR [eax]           ; Do tricks
 8048518:	84 c0                	test   al,al                        ; Is it a null byte?
 804851a:	75 d6                	jne    80484f2 <parse+0x26>         ; If not, loop (loop end)
 804851c:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]     ; Get counter
 804851f:	8d 55 c4             	lea    edx,[ebp-0x3c]               ; Get argument in edx
 8048522:	83 c2 04             	add    edx,0x4                      ; Make it point at buffer
 8048525:	89 54 24 08          	mov    DWORD PTR [esp+0x8],edx      ; Pointer to buffer
 8048529:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax      ; Counter (Bytes to write)
 804852d:	c7 04 24 40 86 04 08 	mov    DWORD PTR [esp],0x8048640    ; Change SP appropiately
 8048534:	e8 57 fe ff ff       	call   8048390 <printf@plt>         ; Exec printf with eax/edx
 8048539:	b8 00 00 00 00       	mov    eax,0x0
 804853e:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 8048541:	65 33 15 14 00 00 00 	xor    edx,DWORD PTR gs:0x14
 8048548:	74 05                	je     804854f <parse+0x83>
 804854a:	e8 51 fe ff ff       	call   80483a0 <__stack_chk_fail@plt>
 804854f:	c9                   	leave  
 8048550:	c3                   	ret    

08048551 <main>:
 8048551:	55                   	push   ebp
 8048552:	89 e5                	mov    ebp,esp
 8048554:	83 e4 f0             	and    esp,0xfffffff0
 8048557:	83 ec 10             	sub    esp,0x10
 804855a:	83 7d 08 02          	cmp    DWORD PTR [ebp+0x8],0x2
 804855e:	74 25                	je     8048585 <main+0x34>
 8048560:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048563:	8b 10                	mov    edx,DWORD PTR [eax]
 8048565:	a1 3c 98 04 08       	mov    eax,ds:0x804983c
 804856a:	89 54 24 08          	mov    DWORD PTR [esp+0x8],edx
 804856e:	c7 44 24 04 5e 86 04 	mov    DWORD PTR [esp+0x4],0x804865e
 8048575:	08 
 8048576:	89 04 24             	mov    DWORD PTR [esp],eax
 8048579:	e8 52 fe ff ff       	call   80483d0 <fprintf@plt>
 804857e:	b8 01 00 00 00       	mov    eax,0x1
 8048583:	eb 20                	jmp    80485a5 <main+0x54>
 8048585:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048588:	83 c0 04             	add    eax,0x4
 804858b:	8b 00                	mov    eax,DWORD PTR [eax]
 804858d:	89 04 24             	mov    DWORD PTR [esp],eax
 8048590:	e8 37 ff ff ff       	call   80484cc <parse>
 8048595:	85 c0                	test   eax,eax
 8048597:	74 07                	je     80485a0 <main+0x4f>
 8048599:	b8 01 00 00 00       	mov    eax,0x1
 804859e:	eb 05                	jmp    80485a5 <main+0x54>
 80485a0:	b8 00 00 00 00       	mov    eax,0x0
 80485a5:	c9                   	leave  
 80485a6:	c3                   	ret    
 80485a7:	90                   	nop
 80485a8:	90                   	nop
 80485a9:	90                   	nop
 80485aa:	90                   	nop
 80485ab:	90                   	nop
 80485ac:	90                   	nop
 80485ad:	90                   	nop
 80485ae:	90                   	nop
 80485af:	90                   	nop

080485b0 <__libc_csu_fini>:
 80485b0:	55                   	push   ebp
 80485b1:	89 e5                	mov    ebp,esp
 80485b3:	5d                   	pop    ebp
 80485b4:	c3                   	ret    
 80485b5:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]
 80485b9:	8d bc 27 00 00 00 00 	lea    edi,[edi+eiz*1+0x0]

080485c0 <__libc_csu_init>:
 80485c0:	55                   	push   ebp
 80485c1:	89 e5                	mov    ebp,esp
 80485c3:	57                   	push   edi
 80485c4:	56                   	push   esi
 80485c5:	53                   	push   ebx
 80485c6:	e8 4f 00 00 00       	call   804861a <__i686.get_pc_thunk.bx>
 80485cb:	81 c3 49 12 00 00    	add    ebx,0x1249
 80485d1:	83 ec 1c             	sub    esp,0x1c
 80485d4:	e8 73 fd ff ff       	call   804834c <_init>
 80485d9:	8d bb 04 ff ff ff    	lea    edi,[ebx-0xfc]
 80485df:	8d 83 00 ff ff ff    	lea    eax,[ebx-0x100]
 80485e5:	29 c7                	sub    edi,eax
 80485e7:	c1 ff 02             	sar    edi,0x2
 80485ea:	85 ff                	test   edi,edi
 80485ec:	74 24                	je     8048612 <__libc_csu_init+0x52>
 80485ee:	31 f6                	xor    esi,esi
 80485f0:	8b 45 10             	mov    eax,DWORD PTR [ebp+0x10]
 80485f3:	89 44 24 08          	mov    DWORD PTR [esp+0x8],eax
 80485f7:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 80485fa:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 80485fe:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048601:	89 04 24             	mov    DWORD PTR [esp],eax
 8048604:	ff 94 b3 00 ff ff ff 	call   DWORD PTR [ebx+esi*4-0x100]
 804860b:	83 c6 01             	add    esi,0x1
 804860e:	39 fe                	cmp    esi,edi
 8048610:	72 de                	jb     80485f0 <__libc_csu_init+0x30>
 8048612:	83 c4 1c             	add    esp,0x1c
 8048615:	5b                   	pop    ebx
 8048616:	5e                   	pop    esi
 8048617:	5f                   	pop    edi
 8048618:	5d                   	pop    ebp
 8048619:	c3                   	ret    

0804861a <__i686.get_pc_thunk.bx>:
 804861a:	8b 1c 24             	mov    ebx,DWORD PTR [esp]
 804861d:	c3                   	ret    
 804861e:	90                   	nop
 804861f:	90                   	nop

Disassembly of section .fini:

08048620 <_fini>:
 8048620:	55                   	push   ebp
 8048621:	89 e5                	mov    ebp,esp
 8048623:	53                   	push   ebx
 8048624:	83 ec 04             	sub    esp,0x4
 8048627:	e8 00 00 00 00       	call   804862c <_fini+0xc>
 804862c:	5b                   	pop    ebx
 804862d:	81 c3 e8 11 00 00    	add    ebx,0x11e8
 8048633:	59                   	pop    ecx
 8048634:	5b                   	pop    ebx
 8048635:	c9                   	leave  
 8048636:	c3                   	ret    
