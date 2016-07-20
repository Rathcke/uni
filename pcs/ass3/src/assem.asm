
bomb:     file format elf32-i386


Disassembly of section .init:

080483f8 <.init>:
 80483f8:	53                   	push   ebx
 80483f9:	83 ec 08             	sub    esp,0x8
 80483fc:	e8 3f 01 00 00       	call   8048540 <nanosleep@plt+0x40>
 8048401:	81 c3 07 29 00 00    	add    ebx,0x2907
 8048407:	8b 83 fc ff ff ff    	mov    eax,DWORD PTR [ebx-0x4]
 804840d:	85 c0                	test   eax,eax
 804840f:	74 05                	je     8048416 <strcmp@plt-0x1a>
 8048411:	e8 7a 00 00 00       	call   8048490 <__gmon_start__@plt>
 8048416:	83 c4 08             	add    esp,0x8
 8048419:	5b                   	pop    ebx
 804841a:	c3                   	ret

Disassembly of section .plt:

08048420 <strcmp@plt-0x10>:
 8048420:	ff 35 0c ad 04 08    	push   DWORD PTR ds:0x804ad0c
 8048426:	ff 25 10 ad 04 08    	jmp    DWORD PTR ds:0x804ad10
 804842c:	00 00                	add    BYTE PTR [eax],al
	...

08048430 <strcmp@plt>:
 8048430:	ff 25 14 ad 04 08    	jmp    DWORD PTR ds:0x804ad14
 8048436:	68 00 00 00 00       	push   0x0
 804843b:	e9 e0 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048440 <read@plt>:
 8048440:	ff 25 18 ad 04 08    	jmp    DWORD PTR ds:0x804ad18
 8048446:	68 08 00 00 00       	push   0x8
 804844b:	e9 d0 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048450 <memcpy@plt>:
 8048450:	ff 25 1c ad 04 08    	jmp    DWORD PTR ds:0x804ad1c
 8048456:	68 10 00 00 00       	push   0x10
 804845b:	e9 c0 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048460 <signal@plt>:
 8048460:	ff 25 20 ad 04 08    	jmp    DWORD PTR ds:0x804ad20
 8048466:	68 18 00 00 00       	push   0x18
 804846b:	e9 b0 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048470 <alarm@plt>:
 8048470:	ff 25 24 ad 04 08    	jmp    DWORD PTR ds:0x804ad24
 8048476:	68 20 00 00 00       	push   0x20
 804847b:	e9 a0 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048480 <puts@plt>:
 8048480:	ff 25 28 ad 04 08    	jmp    DWORD PTR ds:0x804ad28
 8048486:	68 28 00 00 00       	push   0x28
 804848b:	e9 90 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048490 <__gmon_start__@plt>:
 8048490:	ff 25 2c ad 04 08    	jmp    DWORD PTR ds:0x804ad2c
 8048496:	68 30 00 00 00       	push   0x30
 804849b:	e9 80 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484a0 <exit@plt>:
 80484a0:	ff 25 30 ad 04 08    	jmp    DWORD PTR ds:0x804ad30
 80484a6:	68 38 00 00 00       	push   0x38
 80484ab:	e9 70 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484b0 <mmap@plt>:
 80484b0:	ff 25 34 ad 04 08    	jmp    DWORD PTR ds:0x804ad34
 80484b6:	68 40 00 00 00       	push   0x40
 80484bb:	e9 60 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484c0 <__libc_start_main@plt>:
 80484c0:	ff 25 38 ad 04 08    	jmp    DWORD PTR ds:0x804ad38
 80484c6:	68 48 00 00 00       	push   0x48
 80484cb:	e9 50 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484d0 <write@plt>:
 80484d0:	ff 25 3c ad 04 08    	jmp    DWORD PTR ds:0x804ad3c
 80484d6:	68 50 00 00 00       	push   0x50
 80484db:	e9 40 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484e0 <memset@plt>:
 80484e0:	ff 25 40 ad 04 08    	jmp    DWORD PTR ds:0x804ad40
 80484e6:	68 58 00 00 00       	push   0x58
 80484eb:	e9 30 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

080484f0 <sscanf@plt>:
 80484f0:	ff 25 44 ad 04 08    	jmp    DWORD PTR ds:0x804ad44
 80484f6:	68 60 00 00 00       	push   0x60
 80484fb:	e9 20 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

08048500 <nanosleep@plt>:
 8048500:	ff 25 48 ad 04 08    	jmp    DWORD PTR ds:0x804ad48
 8048506:	68 68 00 00 00       	push   0x68
 804850b:	e9 10 ff ff ff       	jmp    8048420 <strcmp@plt-0x10>

Disassembly of section .text:

08048510 <say-0xfb>:
 8048510:	31 ed                	xor    ebp,ebp
 8048512:	5e                   	pop    esi
 8048513:	89 e1                	mov    ecx,esp
 8048515:	83 e4 f0             	and    esp,0xfffffff0
 8048518:	50                   	push   eax
 8048519:	54                   	push   esp
 804851a:	52                   	push   edx
 804851b:	68 90 93 04 08       	push   0x8049390
 8048520:	68 20 93 04 08       	push   0x8049320
 8048525:	51                   	push   ecx
 8048526:	56                   	push   esi
 8048527:	68 a0 90 04 08       	push   0x80490a0
 804852c:	e8 8f ff ff ff       	call   80484c0 <__libc_start_main@plt>
 8048531:	f4                   	hlt
 8048532:	66 90                	xchg   ax,ax
 8048534:	66 90                	xchg   ax,ax
 8048536:	66 90                	xchg   ax,ax
 8048538:	66 90                	xchg   ax,ax
 804853a:	66 90                	xchg   ax,ax
 804853c:	66 90                	xchg   ax,ax
 804853e:	66 90                	xchg   ax,ax
 8048540:	8b 1c 24             	mov    ebx,DWORD PTR [esp]
 8048543:	c3                   	ret
 8048544:	66 90                	xchg   ax,ax
 8048546:	66 90                	xchg   ax,ax
 8048548:	66 90                	xchg   ax,ax
 804854a:	66 90                	xchg   ax,ax
 804854c:	66 90                	xchg   ax,ax
 804854e:	66 90                	xchg   ax,ax
 8048550:	b8 43 bb 04 08       	mov    eax,0x804bb43
 8048555:	2d 40 bb 04 08       	sub    eax,0x804bb40
 804855a:	83 f8 06             	cmp    eax,0x6
 804855d:	76 1a                	jbe    8048579 <nanosleep@plt+0x79>
 804855f:	b8 00 00 00 00       	mov    eax,0x0
 8048564:	85 c0                	test   eax,eax
 8048566:	74 11                	je     8048579 <nanosleep@plt+0x79>
 8048568:	55                   	push   ebp
 8048569:	89 e5                	mov    ebp,esp
 804856b:	83 ec 14             	sub    esp,0x14
 804856e:	68 40 bb 04 08       	push   0x804bb40
 8048573:	ff d0                	call   eax
 8048575:	83 c4 10             	add    esp,0x10
 8048578:	c9                   	leave
 8048579:	f3 c3                	repz ret
 804857b:	90                   	nop
 804857c:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]
 8048580:	b8 40 bb 04 08       	mov    eax,0x804bb40
 8048585:	2d 40 bb 04 08       	sub    eax,0x804bb40
 804858a:	c1 f8 02             	sar    eax,0x2
 804858d:	89 c2                	mov    edx,eax
 804858f:	c1 ea 1f             	shr    edx,0x1f
 8048592:	01 d0                	add    eax,edx
 8048594:	d1 f8                	sar    eax,1
 8048596:	74 1b                	je     80485b3 <nanosleep@plt+0xb3>
 8048598:	ba 00 00 00 00       	mov    edx,0x0
 804859d:	85 d2                	test   edx,edx
 804859f:	74 12                	je     80485b3 <nanosleep@plt+0xb3>
 80485a1:	55                   	push   ebp
 80485a2:	89 e5                	mov    ebp,esp
 80485a4:	83 ec 10             	sub    esp,0x10
 80485a7:	50                   	push   eax
 80485a8:	68 40 bb 04 08       	push   0x804bb40
 80485ad:	ff d2                	call   edx
 80485af:	83 c4 10             	add    esp,0x10
 80485b2:	c9                   	leave
 80485b3:	f3 c3                	repz ret
 80485b5:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]
 80485b9:	8d bc 27 00 00 00 00 	lea    edi,[edi+eiz*1+0x0]
 80485c0:	80 3d 40 bb 04 08 00 	cmp    BYTE PTR ds:0x804bb40,0x0
 80485c7:	75 13                	jne    80485dc <nanosleep@plt+0xdc>
 80485c9:	55                   	push   ebp
 80485ca:	89 e5                	mov    ebp,esp
 80485cc:	83 ec 08             	sub    esp,0x8
 80485cf:	e8 7c ff ff ff       	call   8048550 <nanosleep@plt+0x50>
 80485d4:	c6 05 40 bb 04 08 01 	mov    BYTE PTR ds:0x804bb40,0x1
 80485db:	c9                   	leave
 80485dc:	f3 c3                	repz ret
 80485de:	66 90                	xchg   ax,ax
 80485e0:	b8 18 ac 04 08       	mov    eax,0x804ac18
 80485e5:	8b 10                	mov    edx,DWORD PTR [eax]
 80485e7:	85 d2                	test   edx,edx
 80485e9:	75 05                	jne    80485f0 <nanosleep@plt+0xf0>
 80485eb:	eb 93                	jmp    8048580 <nanosleep@plt+0x80>
 80485ed:	8d 76 00             	lea    esi,[esi+0x0]
 80485f0:	ba 00 00 00 00       	mov    edx,0x0
 80485f5:	85 d2                	test   edx,edx
 80485f7:	74 f2                	je     80485eb <nanosleep@plt+0xeb>
 80485f9:	55                   	push   ebp
 80485fa:	89 e5                	mov    ebp,esp
 80485fc:	83 ec 14             	sub    esp,0x14
 80485ff:	50                   	push   eax
 8048600:	ff d2                	call   edx
 8048602:	83 c4 10             	add    esp,0x10
 8048605:	c9                   	leave
 8048606:	e9 75 ff ff ff       	jmp    8048580 <nanosleep@plt+0x80>

0804860b <say>:
 804860b:	55                   	push   ebp
 804860c:	89 e5                	mov    ebp,esp
 804860e:	83 ec 18             	sub    esp,0x18
 8048611:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0
 8048618:	c7 45 f4 00 2d 31 01 	mov    DWORD PTR [ebp-0xc],0x1312d00
 804861f:	eb 2a                	jmp    804864b <say+0x40>
 8048621:	83 ec 08             	sub    esp,0x8
 8048624:	6a 00                	push   0x0
 8048626:	8d 45 f0             	lea    eax,[ebp-0x10]
 8048629:	50                   	push   eax
 804862a:	e8 d1 fe ff ff       	call   8048500 <nanosleep@plt>
 804862f:	83 c4 10             	add    esp,0x10
 8048632:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048635:	8d 50 01             	lea    edx,[eax+0x1]
 8048638:	89 55 08             	mov    DWORD PTR [ebp+0x8],edx
 804863b:	83 ec 04             	sub    esp,0x4
 804863e:	6a 01                	push   0x1
 8048640:	50                   	push   eax
 8048641:	6a 01                	push   0x1
 8048643:	e8 88 fe ff ff       	call   80484d0 <write@plt>
 8048648:	83 c4 10             	add    esp,0x10
 804864b:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 804864e:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048651:	84 c0                	test   al,al
 8048653:	75 cc                	jne    8048621 <say+0x16>
 8048655:	c9                   	leave
 8048656:	c3                   	ret

08048657 <explode>:
 8048657:	55                   	push   ebp
 8048658:	89 e5                	mov    ebp,esp
 804865a:	83 ec 08             	sub    esp,0x8
 804865d:	83 ec 0c             	sub    esp,0xc
 8048660:	68 b0 93 04 08       	push   0x80493b0
 8048665:	e8 16 fe ff ff       	call   8048480 <puts@plt>
 804866a:	83 c4 10             	add    esp,0x10
 804866d:	83 ec 0c             	sub    esp,0xc
 8048670:	68 bc 93 04 08       	push   0x80493bc
 8048675:	e8 91 ff ff ff       	call   804860b <say>
 804867a:	83 c4 10             	add    esp,0x10
 804867d:	83 ec 0c             	sub    esp,0xc
 8048680:	6a 01                	push   0x1
 8048682:	e8 19 fe ff ff       	call   80484a0 <exit@plt>
 8048687:	55                   	push   ebp
 8048688:	89 e5                	mov    ebp,esp
 804868a:	83 ec 08             	sub    esp,0x8
 804868d:	83 ec 0c             	sub    esp,0xc
 8048690:	68 e4 93 04 08       	push   0x80493e4
 8048695:	e8 71 ff ff ff       	call   804860b <say>
 804869a:	83 c4 10             	add    esp,0x10
 804869d:	e8 b5 ff ff ff       	call   8048657 <explode>
 80486a2:	c9                   	leave
 80486a3:	c3                   	ret
 80486a4:	55                   	push   ebp
 80486a5:	89 e5                	mov    ebp,esp
 80486a7:	83 ec 08             	sub    esp,0x8
 80486aa:	83 ec 08             	sub    esp,0x8
 80486ad:	68 87 86 04 08       	push   0x8048687
 80486b2:	6a 0e                	push   0xe
 80486b4:	e8 a7 fd ff ff       	call   8048460 <signal@plt>
 80486b9:	83 c4 10             	add    esp,0x10
 80486bc:	c9                   	leave
 80486bd:	c3                   	ret

080486be <readline>:
 80486be:	55                   	push   ebp
 80486bf:	89 e5                	mov    ebp,esp
 80486c1:	83 ec 18             	sub    esp,0x18
 80486c4:	83 ec 04             	sub    esp,0x4
 80486c7:	ff 75 0c             	push   DWORD PTR [ebp+0xc]
 80486ca:	6a 00                	push   0x0
 80486cc:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 80486cf:	e8 0c fe ff ff       	call   80484e0 <memset@plt>
 80486d4:	83 c4 10             	add    esp,0x10
 80486d7:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0
 80486de:	eb 2f                	jmp    804870f <readline+0x51>
 80486e0:	83 ec 04             	sub    esp,0x4
 80486e3:	6a 01                	push   0x1
 80486e5:	8d 45 f3             	lea    eax,[ebp-0xd]
 80486e8:	50                   	push   eax
 80486e9:	6a 00                	push   0x0
 80486eb:	e8 50 fd ff ff       	call   8048440 <read@plt>
 80486f0:	83 c4 10             	add    esp,0x10
 80486f3:	0f b6 45 f3          	movzx  eax,BYTE PTR [ebp-0xd]
 80486f7:	3c 0a                	cmp    al,0xa
 80486f9:	75 02                	jne    80486fd <readline+0x3f>
 80486fb:	eb 1a                	jmp    8048717 <readline+0x59>
 80486fd:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 8048700:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048703:	01 c2                	add    edx,eax
 8048705:	0f b6 45 f3          	movzx  eax,BYTE PTR [ebp-0xd]
 8048709:	88 02                	mov    BYTE PTR [edx],al
 804870b:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1
 804870f:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048712:	3b 45 0c             	cmp    eax,DWORD PTR [ebp+0xc]
 8048715:	72 c9                	jb     80486e0 <readline+0x22>
 8048717:	c9                   	leave
 8048718:	c3                   	ret

08048719 <phase_1>:
 8048719:	55                   	push   ebp
 804871a:	89 e5                	mov    ebp,esp
 804871c:	83 ec 18             	sub    esp,0x18
 804871f:	c7 45 f4 e7 93 04 08 	mov    DWORD PTR [ebp-0xc],0x80493e7
 8048726:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048729:	8d 50 01             	lea    edx,[eax+0x1]
 804872c:	89 55 08             	mov    DWORD PTR [ebp+0x8],edx
 804872f:	0f b6 08             	movzx  ecx,BYTE PTR [eax]
 8048732:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048735:	8d 50 01             	lea    edx,[eax+0x1]
 8048738:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx
 804873b:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 804873e:	38 c1                	cmp    cl,al
 8048740:	74 05                	je     8048747 <phase_1+0x2e>
 8048742:	e8 10 ff ff ff       	call   8048657 <explode>
 8048747:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 804874a:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 804874d:	84 c0                	test   al,al
 804874f:	75 d5                	jne    8048726 <phase_1+0xd>
 8048751:	83 ec 0c             	sub    esp,0xc
 8048754:	68 fe 93 04 08       	push   0x80493fe
 8048759:	e8 ad fe ff ff       	call   804860b <say>
 804875e:	83 c4 10             	add    esp,0x10
 8048761:	c9                   	leave
 8048762:	c3                   	ret

08048763 <phase_2>:
 8048763:	55                   	push   ebp
 8048764:	89 e5                	mov    ebp,esp
 8048766:	83 ec 28             	sub    esp,0x28
 8048769:	8d 45 dc             	lea    eax,[ebp-0x24]
 804876c:	83 c0 14             	add    eax,0x14                      ; Push arguments
 804876f:	50                   	push   eax
 8048770:	8d 45 dc             	lea    eax,[ebp-0x24]
 8048773:	83 c0 10             	add    eax,0x10
 8048776:	50                   	push   eax
 8048777:	8d 45 dc             	lea    eax,[ebp-0x24]
 804877a:	83 c0 0c             	add    eax,0xc
 804877d:	50                   	push   eax
 804877e:	8d 45 dc             	lea    eax,[ebp-0x24]
 8048781:	83 c0 08             	add    eax,0x8
 8048784:	50                   	push   eax
 8048785:	8d 45 dc             	lea    eax,[ebp-0x24]
 8048788:	83 c0 04             	add    eax,0x4
 804878b:	50                   	push   eax
 804878c:	8d 45 dc             	lea    eax,[ebp-0x24]
 804878f:	50                   	push   eax
 8048790:	68 18 94 04 08       	push   0x8049418
 8048795:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 8048798:	e8 53 fd ff ff       	call   80484f0 <sscanf@plt>
 804879d:	83 c4 20             	add    esp,0x20
 80487a0:	83 f8 05             	cmp    eax,0x5                       ; Do we have 6+ args?
 80487a3:	7f 05                	jg     80487aa <phase_2+0x47>        ; If yes, go to %7aa
 80487a5:	e8 ad fe ff ff       	call   8048657 <explode>             ; Else we explode
 80487aa:	8b 45 dc             	mov    eax,DWORD PTR [ebp-0x24]      ; Put first arg in eax
 80487ad:	83 f8 09             	cmp    eax,0x9                       ; Is first arg = 9?
 80487b0:	74 05                	je     80487b7 <phase_2+0x54>        ; If yes, jump to %7b7
 80487b2:	e8 a0 fe ff ff       	call   8048657 <explode>             ; If not, we explode
 80487b7:	c7 45 f4 01 00 00 00 	mov    DWORD PTR [ebp-0xc],0x1       ; Set a counter "i" to 1
 80487be:	eb 22                	jmp    80487e2 <phase_2+0x7f>        ; Jump to %7e2
 80487c0:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]       ; Set eax = i
 80487c3:	8b 54 85 dc          	mov    edx,DWORD PTR [ebp+eax*4-0x24]; Get the next arg in edx
 80487c7:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]       ; Set eax = i
 80487ca:	83 e8 01             	sub    eax,0x1                       ; Set eax = eax-1
 80487cd:	8b 44 85 dc          	mov    eax,DWORD PTR [ebp+eax*4-0x24]; Set eax to previous arg
 80487d1:	0f af 45 f4          	imul   eax,DWORD PTR [ebp-0xc]       ; Set eax = eax * i
 80487d5:	39 c2                	cmp    edx,eax                       ; Is eax = edx?
 80487d7:	74 05                	je     80487de <phase_2+0x7b>        ; If yes, go to %7de
 80487d9:	e8 79 fe ff ff       	call   8048657 <explode>             ; If not, we explode
 80487de:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1       ; Increment counter by 1
 80487e2:	83 7d f4 05          	cmp    DWORD PTR [ebp-0xc],0x5       ; Is counter <= 5?
 80487e6:	7e d8                	jle    80487c0 <phase_2+0x5d>        ; If yes, go to %7c0
 80487e8:	83 ec 0c             	sub    esp,0xc
 80487eb:	68 2a 94 04 08       	push   0x804942a
 80487f0:	e8 16 fe ff ff       	call   804860b <say>
 80487f5:	83 c4 10             	add    esp,0x10
 80487f8:	c9                   	leave
 80487f9:	c3                   	ret

080487fa <phase_3>:
 80487fa:	55                   	push   ebp
 80487fb:	89 e5                	mov    ebp,esp
 80487fd:	83 ec 18             	sub    esp,0x18
 8048800:	8d 45 ec             	lea    eax,[ebp-0x14]
 8048803:	50                   	push   eax
 8048804:	8d 45 f0             	lea    eax,[ebp-0x10]
 8048807:	50                   	push   eax
 8048808:	68 44 94 04 08       	push   0x8049444
 804880d:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 8048810:	e8 db fc ff ff       	call   80484f0 <sscanf@plt>
 8048815:	83 c4 10             	add    esp,0x10
 8048818:	83 f8 01             	cmp    eax,0x1                  ; Do we have 2+ args?
 804881b:	7f 05                	jg     8048822 <phase_3+0x28>   ; If yes, goto %822
 804881d:	e8 35 fe ff ff       	call   8048657 <explode>        ; If not, we explode
 8048822:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10] ; Set eax to first arg
 8048825:	3d e7 03 00 00       	cmp    eax,0x3e7                ; Is eax > 999?
 804882a:	77 05                	ja     8048831 <phase_3+0x37>   ; If yes, goto %831
 804882c:	e8 26 fe ff ff       	call   8048657 <explode>        ; If not, we explode
 8048831:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0  ; Init counter "i" to 0
 8048838:	eb 27                	jmp    8048861 <phase_3+0x67>   ; Goto %861
 804883a:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10] ; Set eax to first arg
 804883d:	83 e0 01             	and    eax,0x1                  ; Get last bit of eax
 8048840:	85 c0                	test   eax,eax                  ; Set flags
 8048842:	74 11                	je     8048855 <phase_3+0x5b>   ; If eax was even, goto %855
 8048844:	8b 55 f0             	mov    edx,DWORD PTR [ebp-0x10] ; Set var edx to first arg
 8048847:	89 d0                	mov    eax,edx                  ; Set eax to edx
 8048849:	01 c0                	add    eax,eax                  ; Set eax = eax+eax
 804884b:	01 d0                	add    eax,edx                  ; Set eax = eax+edx
 804884d:	83 c0 01             	add    eax,0x1                  ; Set eax = eax+1
 8048850:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax ; Set first arg to eax
 8048853:	eb 08                	jmp    804885d <phase_3+0x63>   ; Goto %85d
 8048855:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10] ; Set eax to first arg
 8048858:	d1 e8                	shr    eax,1                    ; Set eax = eax/2
 804885a:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax ; Set first arg to eax
 804885d:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1  ; Increment counter "i" by 1
 8048861:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10] ; Set var eax to first arg
 8048864:	83 f8 01             	cmp    eax,0x1                  ; Is eax = 1?
 8048867:	75 d1                	jne    804883a <phase_3+0x40>   ; If not, goto %83a
 8048869:	8b 45 ec             	mov    eax,DWORD PTR [ebp-0x14] ; Set eax to second arg
 804886c:	3b 45 f4             	cmp    eax,DWORD PTR [ebp-0xc]  ; Is eax = i?
 804886f:	74 05                	je     8048876 <phase_3+0x7c>   ; If yes, goto %876
 8048871:	e8 e1 fd ff ff       	call   8048657 <explode>        ; Else we explode
 8048876:	83 ec 0c             	sub    esp,0xc
 8048879:	68 4a 94 04 08       	push   0x804944a
 804887e:	e8 88 fd ff ff       	call   804860b <say>
 8048883:	83 c4 10             	add    esp,0x10
 8048886:	c9                   	leave
 8048887:	c3                   	ret

08048888 <func4>:
 8048888:	55                   	push   ebp
 8048889:	89 e5                	mov    ebp,esp
 804888b:	53                   	push   ebx
 804888c:	83 ec 04             	sub    esp,0x4
 804888f:	83 7d 08 02          	cmp    DWORD PTR [ebp+0x8],0x2 ; Is the argument > 2?
 8048893:	7f 07                	jg     804889c <func4+0x14>    ; If yes, goto %89c
 8048895:	b8 01 00 00 00       	mov    eax,0x1                 ; If not, put 1 in eax
 804889a:	eb 3c                	jmp    80488d8 <func4+0x50>    ; And jump to %8d8 (return)
 804889c:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8] ; Put argument in eax
 804889f:	83 e8 01             	sub    eax,0x1                 ; Set arg = arg - 1
 80488a2:	83 ec 0c             	sub    esp,0xc
 80488a5:	50                   	push   eax
 80488a6:	e8 dd ff ff ff       	call   8048888 <func4>         ; Call func with new arg
 80488ab:	83 c4 10             	add    esp,0x10
 80488ae:	89 c3                	mov    ebx,eax
 80488b0:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 80488b3:	83 e8 02             	sub    eax,0x2                 ; Set arg = arg - 2
 80488b6:	83 ec 0c             	sub    esp,0xc
 80488b9:	50                   	push   eax
 80488ba:	e8 c9 ff ff ff       	call   8048888 <func4>         ; Call func with new arg
 80488bf:	83 c4 10             	add    esp,0x10
 80488c2:	01 c3                	add    ebx,eax
 80488c4:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 80488c7:	83 e8 03             	sub    eax,0x3                 ; Set arg = arg - 3
 80488ca:	83 ec 0c             	sub    esp,0xc
 80488cd:	50                   	push   eax
 80488ce:	e8 b5 ff ff ff       	call   8048888 <func4>         ; Call func with new arg
 80488d3:	83 c4 10             	add    esp,0x10
 80488d6:	01 d8                	add    eax,ebx
 80488d8:	8b 5d fc             	mov    ebx,DWORD PTR [ebp-0x4] ; Return with result
 80488db:	c9                   	leave
 80488dc:	c3                   	ret

080488dd <phase_4>:
 80488dd:	55                   	push   ebp
 80488de:	89 e5                	mov    ebp,esp
 80488e0:	83 ec 18             	sub    esp,0x18
 80488e3:	83 ec 04             	sub    esp,0x4
 80488e6:	8d 45 f4             	lea    eax,[ebp-0xc]
 80488e9:	50                   	push   eax
 80488ea:	68 64 94 04 08       	push   0x8049464
 80488ef:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 80488f2:	e8 f9 fb ff ff       	call   80484f0 <sscanf@plt>
 80488f7:	83 c4 10             	add    esp,0x10
 80488fa:	85 c0                	test   eax,eax                 ; Is eax positive?
 80488fc:	7f 05                	jg     8048903 <phase_4+0x26>  ; If yes, goto %903
 80488fe:	e8 54 fd ff ff       	call   8048657 <explode>       ; If not, we explode
 8048903:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048906:	83 ec 0c             	sub    esp,0xc
 8048909:	50                   	push   eax                     ; Push eax (as an arg)
 804890a:	e8 79 ff ff ff       	call   8048888 <func4>         ; Call func4 with eax
 804890f:	83 c4 10             	add    esp,0x10
 8048912:	3d 31 6a 04 00       	cmp    eax,0x46a31             ; Is the result = 289329?
 8048917:	74 05                	je     804891e <phase_4+0x41>  ; If yes, goto %91e
 8048919:	e8 39 fd ff ff       	call   8048657 <explode>       ; If not, we explode
 804891e:	83 ec 0c             	sub    esp,0xc
 8048921:	68 67 94 04 08       	push   0x8049467
 8048926:	e8 e0 fc ff ff       	call   804860b <say>
 804892b:	83 c4 10             	add    esp,0x10
 804892e:	c9                   	leave
 804892f:	c3                   	ret

08048930 <phase_5>:
 8048930:	55                   	push   ebp
 8048931:	89 e5                	mov    ebp,esp
 8048933:	53                   	push   ebx
 8048934:	83 ec 24             	sub    esp,0x24
 8048937:	8d 45 d9             	lea    eax,[ebp-0x27]                ; Set eax to word -0x27
 804893a:	b9 1b 00 00 00       	mov    ecx,0x1b                      ; Set ecx = 27
 804893f:	bb 00 00 00 00       	mov    ebx,0x0                       ; Set ebx = 0
 8048944:	89 18                	mov    DWORD PTR [eax],ebx           ; Put ebx into eax addr
 8048946:	89 5c 08 fc          	mov    DWORD PTR [eax+ecx*1-0x4],ebx ; Put ebx into -0x4
 804894a:	8d 50 04             	lea    edx,[eax+0x4]                 ; Set edx to word -0x23
 804894d:	83 e2 fc             	and    edx,0xfffffffc                ; Rm last 2 bits in edx
 8048950:	29 d0                	sub    eax,edx                       ; Set eax = eax-edx
 8048952:	01 c1                	add    ecx,eax                       ; Set ecx = ecx+eax
 8048954:	83 e1 fc             	and    ecx,0xfffffffc                ; rm last 2 bits in ecx
 8048957:	83 e1 fc             	and    ecx,0xfffffffc                ; ... and again
 804895a:	b8 00 00 00 00       	mov    eax,0x0                       ; Set eax = 0
 804895f:	89 1c 02             	mov    DWORD PTR [edx+eax*1],ebx     ;
 8048962:	83 c0 04             	add    eax,0x4                       ; Set eax = eax + 4
 8048965:	39 c8                	cmp    eax,ecx                       ; Is eax < ecx?
 8048967:	72 f6                	jb     804895f <phase_5+0x2f>        ; If so, goto %95f
 8048969:	01 c2                	add    edx,eax                       ; Set edx = edx+eax
 804896b:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0       ; Set counter "i" to 0
 8048972:	eb 26                	jmp    804899a <phase_5+0x6a>        ; Goto %99a
 8048974:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]       ; Set edx = i
 8048977:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]       ; Set eax to param 1
 804897a:	01 d0                	add    eax,edx                       ; Set eax = eax + edx
 804897c:	0f b6 00             	movzx  eax,BYTE PTR [eax]            ; Set eax = 1st byte
 804897f:	0f be c0             	movsx  eax,al                        ; Sign extend it in eax
 8048982:	83 e0 0f             	and    eax,0xf                       ; Get last 4 bits in eax
 8048985:	0f b6 80 c0 ad 04 08 	movzx  eax,BYTE PTR [eax+0x804adc0]
 804898c:	8d 4d d9             	lea    ecx,[ebp-0x27]                ; Set ecx = val in -0x27
 804898f:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]       ; Set edx = i
 8048992:	01 ca                	add    edx,ecx                       ; Set edx = edx + ecx
 8048994:	88 02                	mov    BYTE PTR [edx],al             ; Get the byte in eax
 8048996:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1       ; i++
 804899a:	83 7d f4 19          	cmp    DWORD PTR [ebp-0xc],0x19      ; Is i <= 25?
 804899e:	7e d4                	jle    8048974 <phase_5+0x44>        ; If yes, goto %974
 80489a0:	83 ec 08             	sub    esp,0x8
 80489a3:	68 81 94 04 08       	push   0x8049481
 80489a8:	8d 45 d9             	lea    eax,[ebp-0x27]
 80489ab:	50                   	push   eax
 80489ac:	e8 7f fa ff ff       	call   8048430 <strcmp@plt>
 80489b1:	83 c4 10             	add    esp,0x10
 80489b4:	85 c0                	test   eax,eax
 80489b6:	74 05                	je     80489bd <phase_5+0x8d>
 80489b8:	e8 9a fc ff ff       	call   8048657 <explode>
 80489bd:	83 ec 0c             	sub    esp,0xc
 80489c0:	68 9c 94 04 08       	push   0x804949c
 80489c5:	e8 41 fc ff ff       	call   804860b <say>
 80489ca:	83 c4 10             	add    esp,0x10
 80489cd:	8b 5d fc             	mov    ebx,DWORD PTR [ebp-0x4]
 80489d0:	c9                   	leave
 80489d1:	c3                   	ret
 80489d2:	55                   	push   ebp                      ; We come from phase 6
 80489d3:	89 e5                	mov    ebp,esp
 80489d5:	83 ec 08             	sub    esp,0x8
 80489d8:	83 7d 08 00          	cmp    DWORD PTR [ebp+0x8],0x0
 80489dc:	75 02                	jne    80489e0 <phase_5+0xb0>
 80489de:	eb 40                	jmp    8048a20 <phase_5+0xf0>
 80489e0:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 80489e3:	8b 00                	mov    eax,DWORD PTR [eax]
 80489e5:	83 ec 04             	sub    esp,0x4
 80489e8:	ff 75 10             	push   DWORD PTR [ebp+0x10]
 80489eb:	ff 75 0c             	push   DWORD PTR [ebp+0xc]
 80489ee:	50                   	push   eax
 80489ef:	e8 de ff ff ff       	call   80489d2 <phase_5+0xa2>
 80489f4:	83 c4 10             	add    esp,0x10
 80489f7:	83 ec 08             	sub    esp,0x8
 80489fa:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 80489fd:	ff 75 10             	push   DWORD PTR [ebp+0x10]
 8048a00:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048a03:	ff d0                	call   eax
 8048a05:	83 c4 10             	add    esp,0x10
 8048a08:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048a0b:	8b 40 04             	mov    eax,DWORD PTR [eax+0x4]
 8048a0e:	83 ec 04             	sub    esp,0x4
 8048a11:	ff 75 10             	push   DWORD PTR [ebp+0x10]
 8048a14:	ff 75 0c             	push   DWORD PTR [ebp+0xc]
 8048a17:	50                   	push   eax
 8048a18:	e8 b5 ff ff ff       	call   80489d2 <phase_5+0xa2>
 8048a1d:	83 c4 10             	add    esp,0x10
 8048a20:	c9                   	leave
 8048a21:	c3                   	ret

08048a22 <func6>:
 8048a22:	55                   	push   ebp
 8048a23:	89 e5                	mov    ebp,esp
 8048a25:	83 ec 10             	sub    esp,0x10
 8048a28:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048a2b:	89 45 fc             	mov    DWORD PTR [ebp-0x4],eax
 8048a2e:	8b 45 fc             	mov    eax,DWORD PTR [ebp-0x4]        ; We move things around
 8048a31:	8b 00                	mov    eax,DWORD PTR [eax]            ; in here. Not entirely
 8048a33:	8d 48 01             	lea    ecx,[eax+0x1]                  ; sure what goes where,
 8048a36:	8b 55 fc             	mov    edx,DWORD PTR [ebp-0x4]        ; but maybe we can
 8048a39:	89 0a                	mov    DWORD PTR [edx],ecx            ; brute-force it?
 8048a3b:	8b 55 0c             	mov    edx,DWORD PTR [ebp+0xc]
 8048a3e:	8b 4a 08             	mov    ecx,DWORD PTR [edx+0x8]
 8048a41:	8b 55 fc             	mov    edx,DWORD PTR [ebp-0x4]
 8048a44:	89 4c 82 04          	mov    DWORD PTR [edx+eax*4+0x4],ecx
 8048a48:	c9                   	leave
 8048a49:	c3                   	ret

08048a4a <phase_6>:
 8048a4a:	55                   	push   ebp
 8048a4b:	89 e5                	mov    ebp,esp
 8048a4d:	83 ec 28             	sub    esp,0x28
 8048a50:	68 f0 ad 04 08       	push   0x804adf0
 8048a55:	68 e4 ad 04 08       	push   0x804ade4
 8048a5a:	68 64 bb 04 08       	push   0x804bb64
 8048a5f:	68 d8 ad 04 08       	push   0x804add8
 8048a64:	68 58 bb 04 08       	push   0x804bb58
 8048a69:	68 4c bb 04 08       	push   0x804bb4c
 8048a6e:	68 18 94 04 08       	push   0x8049418
 8048a73:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 8048a76:	e8 75 fa ff ff       	call   80484f0 <sscanf@plt>
 8048a7b:	83 c4 20             	add    esp,0x20
 8048a7e:	83 f8 05             	cmp    eax,0x5                        ; Do we have 6+ args?
 8048a81:	7f 05                	jg     8048a88 <phase_6+0x3e>         ; If yes, goto %a88
 8048a83:	e8 cf fb ff ff       	call   8048657 <explode>              ; If not, we explode
 8048a88:	83 ec 04             	sub    esp,0x4
 8048a8b:	8d 45 d8             	lea    eax,[ebp-0x28]
 8048a8e:	50                   	push   eax
 8048a8f:	68 22 8a 04 08       	push   0x8048a22
 8048a94:	68 e8 ad 04 08       	push   0x804ade8
 8048a99:	e8 34 ff ff ff       	call   80489d2 <phase_5+0xa2>
 8048a9e:	83 c4 10             	add    esp,0x10
 8048aa1:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0        ; Set counter i = 0
 8048aa8:	eb 15                	jmp    8048abf <phase_6+0x75>
 8048aaa:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]        ; Set eax = i
 8048aad:	8b 44 85 dc          	mov    eax,DWORD PTR [ebp+eax*4-0x24] ; Get an argument in eax
 8048ab1:	3b 45 f4             	cmp    eax,DWORD PTR [ebp-0xc]        ; Is eax = i?
 8048ab4:	74 05                	je     8048abb <phase_6+0x71>         ; If yes, goto %abb
 8048ab6:	e8 9c fb ff ff       	call   8048657 <explode>              ; If not, we explode
 8048abb:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1        ; i++
 8048abf:	83 7d f4 05          	cmp    DWORD PTR [ebp-0xc],0x5        ; is i <= 5?
 8048ac3:	7e e5                	jle    8048aaa <phase_6+0x60>         ; If yes, goto %aaa
 8048ac5:	83 ec 0c             	sub    esp,0xc
 8048ac8:	68 d4 94 04 08       	push   0x80494d4
 8048acd:	e8 39 fb ff ff       	call   804860b <say>
 8048ad2:	83 c4 10             	add    esp,0x10
 8048ad5:	c9                   	leave
 8048ad6:	c3                   	ret
 8048ad7:	55                   	push   ebp
 8048ad8:	89 e5                	mov    ebp,esp
 8048ada:	83 ec 08             	sub    esp,0x8
 8048add:	a1 f4 ad 04 08       	mov    eax,ds:0x804adf4
 8048ae2:	2d 00 00 00 20       	sub    eax,0x20000000
 8048ae7:	a3 f4 ad 04 08       	mov    ds:0x804adf4,eax
 8048aec:	83 ec 08             	sub    esp,0x8
 8048aef:	68 7c 8d 04 08       	push   0x8048d7c
 8048af4:	6a 05                	push   0x5
 8048af6:	e8 65 f9 ff ff       	call   8048460 <signal@plt>
 8048afb:	83 c4 10             	add    esp,0x10
 8048afe:	c9                   	leave
 8048aff:	c3                   	ret
 8048b00:	55                   	push   ebp
 8048b01:	89 e5                	mov    ebp,esp
 8048b03:	83 ec 08             	sub    esp,0x8
 8048b06:	83 ec 08             	sub    esp,0x8
 8048b09:	68 d7 8a 04 08       	push   0x8048ad7
 8048b0e:	6a 05                	push   0x5
 8048b10:	e8 4b f9 ff ff       	call   8048460 <signal@plt>
 8048b15:	83 c4 10             	add    esp,0x10
 8048b18:	c9                   	leave
 8048b19:	c3                   	ret

08048b1a <phase_7>:
 8048b1a:	55                   	push   ebp
 8048b1b:	89 e5                	mov    ebp,esp
 8048b1d:	83 ec 18             	sub    esp,0x18
 8048b20:	83 ec 04             	sub    esp,0x4
 8048b23:	8d 45 f0             	lea    eax,[ebp-0x10]
 8048b26:	50                   	push   eax
 8048b27:	68 64 94 04 08       	push   0x8049464
 8048b2c:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 8048b2f:	e8 bc f9 ff ff       	call   80484f0 <sscanf@plt>
 8048b34:	83 c4 10             	add    esp,0x10
 8048b37:	85 c0                	test   eax,eax
 8048b39:	7f 05                	jg     8048b40 <phase_7+0x26>
 8048b3b:	e8 17 fb ff ff       	call   8048657 <explode>
 8048b40:	cc                   	int3
 8048b41:	a1 f4 ad 04 08       	mov    eax,ds:0x804adf4
 8048b46:	89 c2                	mov    edx,eax
 8048b48:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048b4b:	39 c2                	cmp    edx,eax
 8048b4d:	76 05                	jbe    8048b54 <phase_7+0x3a>
 8048b4f:	e8 03 fb ff ff       	call   8048657 <explode>
 8048b54:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0
 8048b5b:	eb 29                	jmp    8048b86 <phase_7+0x6c>
 8048b5d:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048b60:	83 e0 01             	and    eax,0x1
 8048b63:	85 c0                	test   eax,eax
 8048b65:	74 13                	je     8048b7a <phase_7+0x60>
 8048b67:	8b 55 f0             	mov    edx,DWORD PTR [ebp-0x10]
 8048b6a:	89 d0                	mov    eax,edx
 8048b6c:	01 c0                	add    eax,eax
 8048b6e:	01 d0                	add    eax,edx
 8048b70:	83 c0 01             	add    eax,0x1
 8048b73:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax
 8048b76:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1
 8048b7a:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048b7d:	d1 e8                	shr    eax,1
 8048b7f:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax
 8048b82:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1
 8048b86:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048b89:	83 f8 01             	cmp    eax,0x1
 8048b8c:	75 cf                	jne    8048b5d <phase_7+0x43>
 8048b8e:	83 7d f4 09          	cmp    DWORD PTR [ebp-0xc],0x9
 8048b92:	76 05                	jbe    8048b99 <phase_7+0x7f>
 8048b94:	e8 be fa ff ff       	call   8048657 <explode>
 8048b99:	83 ec 0c             	sub    esp,0xc
 8048b9c:	68 10 95 04 08       	push   0x8049510
 8048ba1:	e8 65 fa ff ff       	call   804860b <say>
 8048ba6:	83 c4 10             	add    esp,0x10
 8048ba9:	c9                   	leave  
 8048baa:	c3                   	ret    
 8048bab:	55                   	push   ebp
 8048bac:	89 e5                	mov    ebp,esp
 8048bae:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048bb1:	0f b6 10             	movzx  edx,BYTE PTR [eax]
 8048bb4:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048bb7:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048bba:	31 c2                	xor    edx,eax
 8048bbc:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048bbf:	88 10                	mov    BYTE PTR [eax],dl
 8048bc1:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048bc4:	0f b6 10             	movzx  edx,BYTE PTR [eax]
 8048bc7:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048bca:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048bcd:	31 c2                	xor    edx,eax
 8048bcf:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048bd2:	88 10                	mov    BYTE PTR [eax],dl
 8048bd4:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048bd7:	0f b6 10             	movzx  edx,BYTE PTR [eax]
 8048bda:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048bdd:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048be0:	31 c2                	xor    edx,eax
 8048be2:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048be5:	88 10                	mov    BYTE PTR [eax],dl
 8048be7:	5d                   	pop    ebp
 8048be8:	c3                   	ret    

08048be9 <phase_8>:
 8048be9:	55                   	push   ebp
 8048bea:	89 e5                	mov    ebp,esp
 8048bec:	53                   	push   ebx
 8048bed:	81 ec 14 01 00 00    	sub    esp,0x114
 8048bf3:	c6 45 f5 00          	mov    BYTE PTR [ebp-0xb],0x0
 8048bf7:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0
 8048bfe:	eb 16                	jmp    8048c16 <phase_8+0x2d>
 8048c00:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048c03:	89 c1                	mov    ecx,eax
 8048c05:	8d 95 ef fe ff ff    	lea    edx,[ebp-0x111]
 8048c0b:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048c0e:	01 d0                	add    eax,edx
 8048c10:	88 08                	mov    BYTE PTR [eax],cl
 8048c12:	83 45 f0 01          	add    DWORD PTR [ebp-0x10],0x1
 8048c16:	81 7d f0 ff 00 00 00 	cmp    DWORD PTR [ebp-0x10],0xff
 8048c1d:	7e e1                	jle    8048c00 <phase_8+0x17>
 8048c1f:	c6 45 f6 00          	mov    BYTE PTR [ebp-0xa],0x0
 8048c23:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0
 8048c2a:	c6 45 f6 00          	mov    BYTE PTR [ebp-0xa],0x0
 8048c2e:	eb 67                	jmp    8048c97 <phase_8+0xae>
 8048c30:	8d 95 ef fe ff ff    	lea    edx,[ebp-0x111]
 8048c36:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048c39:	01 d0                	add    eax,edx
 8048c3b:	0f b6 18             	movzx  ebx,BYTE PTR [eax]
 8048c3e:	8b 4d f0             	mov    ecx,DWORD PTR [ebp-0x10]
 8048c41:	ba cb 6b 28 af       	mov    edx,0xaf286bcb
 8048c46:	89 c8                	mov    eax,ecx
 8048c48:	f7 e2                	mul    edx
 8048c4a:	89 c8                	mov    eax,ecx
 8048c4c:	29 d0                	sub    eax,edx
 8048c4e:	d1 e8                	shr    eax,1
 8048c50:	01 d0                	add    eax,edx
 8048c52:	c1 e8 04             	shr    eax,0x4
 8048c55:	89 c2                	mov    edx,eax
 8048c57:	89 d0                	mov    eax,edx
 8048c59:	c1 e0 03             	shl    eax,0x3
 8048c5c:	01 d0                	add    eax,edx
 8048c5e:	01 c0                	add    eax,eax
 8048c60:	01 d0                	add    eax,edx
 8048c62:	29 c1                	sub    ecx,eax
 8048c64:	89 ca                	mov    edx,ecx
 8048c66:	0f b6 82 f8 ad 04 08 	movzx  eax,BYTE PTR [edx+0x804adf8]
 8048c6d:	01 d8                	add    eax,ebx
 8048c6f:	00 45 f6             	add    BYTE PTR [ebp-0xa],al
 8048c72:	0f b6 45 f6          	movzx  eax,BYTE PTR [ebp-0xa]
 8048c76:	8d 95 ef fe ff ff    	lea    edx,[ebp-0x111]
 8048c7c:	01 c2                	add    edx,eax
 8048c7e:	8d 8d ef fe ff ff    	lea    ecx,[ebp-0x111]
 8048c84:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048c87:	01 c8                	add    eax,ecx
 8048c89:	52                   	push   edx
 8048c8a:	50                   	push   eax
 8048c8b:	e8 1b ff ff ff       	call   8048bab <phase_7+0x91>
 8048c90:	83 c4 08             	add    esp,0x8
 8048c93:	83 45 f0 01          	add    DWORD PTR [ebp-0x10],0x1
 8048c97:	81 7d f0 ff 00 00 00 	cmp    DWORD PTR [ebp-0x10],0xff
 8048c9e:	7e 90                	jle    8048c30 <phase_8+0x47>
 8048ca0:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0
 8048ca7:	c6 45 f6 00          	mov    BYTE PTR [ebp-0xa],0x0
 8048cab:	0f b6 45 f6          	movzx  eax,BYTE PTR [ebp-0xa]
 8048caf:	88 45 f7             	mov    BYTE PTR [ebp-0x9],al
 8048cb2:	e9 89 00 00 00       	jmp    8048d40 <phase_8+0x157>
 8048cb7:	80 45 f7 01          	add    BYTE PTR [ebp-0x9],0x1
 8048cbb:	0f b6 45 f7          	movzx  eax,BYTE PTR [ebp-0x9]
 8048cbf:	0f b6 84 05 ef fe ff 	movzx  eax,BYTE PTR [ebp+eax*1-0x111]
 8048cc6:	ff 
 8048cc7:	00 45 f6             	add    BYTE PTR [ebp-0xa],al
 8048cca:	0f b6 45 f6          	movzx  eax,BYTE PTR [ebp-0xa]
 8048cce:	8d 95 ef fe ff ff    	lea    edx,[ebp-0x111]
 8048cd4:	01 c2                	add    edx,eax
 8048cd6:	0f b6 45 f7          	movzx  eax,BYTE PTR [ebp-0x9]
 8048cda:	8d 8d ef fe ff ff    	lea    ecx,[ebp-0x111]
 8048ce0:	01 c8                	add    eax,ecx
 8048ce2:	52                   	push   edx
 8048ce3:	50                   	push   eax
 8048ce4:	e8 c2 fe ff ff       	call   8048bab <phase_7+0x91>
 8048ce9:	83 c4 08             	add    esp,0x8
 8048cec:	0f b6 45 f7          	movzx  eax,BYTE PTR [ebp-0x9]
 8048cf0:	0f b6 94 05 ef fe ff 	movzx  edx,BYTE PTR [ebp+eax*1-0x111]
 8048cf7:	ff 
 8048cf8:	0f b6 45 f6          	movzx  eax,BYTE PTR [ebp-0xa]
 8048cfc:	0f b6 84 05 ef fe ff 	movzx  eax,BYTE PTR [ebp+eax*1-0x111]
 8048d03:	ff 
 8048d04:	01 d0                	add    eax,edx
 8048d06:	88 45 ef             	mov    BYTE PTR [ebp-0x11],al
 8048d09:	0f b6 45 ef          	movzx  eax,BYTE PTR [ebp-0x11]
 8048d0d:	0f b6 84 05 ef fe ff 	movzx  eax,BYTE PTR [ebp+eax*1-0x111]
 8048d14:	ff 
 8048d15:	89 c1                	mov    ecx,eax
 8048d17:	8b 55 f0             	mov    edx,DWORD PTR [ebp-0x10]
 8048d1a:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048d1d:	01 d0                	add    eax,edx
 8048d1f:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048d22:	31 c1                	xor    ecx,eax
 8048d24:	89 ca                	mov    edx,ecx
 8048d26:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048d29:	05 20 ae 04 08       	add    eax,0x804ae20
 8048d2e:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048d31:	31 c2                	xor    edx,eax
 8048d33:	0f b6 45 f5          	movzx  eax,BYTE PTR [ebp-0xb]
 8048d37:	09 d0                	or     eax,edx
 8048d39:	88 45 f5             	mov    BYTE PTR [ebp-0xb],al
 8048d3c:	83 45 f0 01          	add    DWORD PTR [ebp-0x10],0x1
 8048d40:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048d43:	83 f8 29             	cmp    eax,0x29
 8048d46:	0f 86 6b ff ff ff    	jbe    8048cb7 <phase_8+0xce>
 8048d4c:	80 7d f5 00          	cmp    BYTE PTR [ebp-0xb],0x0
 8048d50:	74 05                	je     8048d57 <phase_8+0x16e>
 8048d52:	e8 00 f9 ff ff       	call   8048657 <explode>
 8048d57:	83 ec 0c             	sub    esp,0xc
 8048d5a:	68 44 95 04 08       	push   0x8049544
 8048d5f:	e8 a7 f8 ff ff       	call   804860b <say>
 8048d64:	83 c4 10             	add    esp,0x10
 8048d67:	83 ec 0c             	sub    esp,0xc
 8048d6a:	68 7c 95 04 08       	push   0x804957c
 8048d6f:	e8 97 f8 ff ff       	call   804860b <say>
 8048d74:	83 c4 10             	add    esp,0x10
 8048d77:	8b 5d fc             	mov    ebx,DWORD PTR [ebp-0x4]
 8048d7a:	c9                   	leave  
 8048d7b:	c3                   	ret    
 8048d7c:	55                   	push   ebp
 8048d7d:	89 e5                	mov    ebp,esp
 8048d7f:	53                   	push   ebx
 8048d80:	0f b6 05 68 bb 04 08 	movzx  eax,BYTE PTR ds:0x804bb68
 8048d87:	0f b6 c0             	movzx  eax,al
 8048d8a:	83 f8 01             	cmp    eax,0x1
 8048d8d:	74 3e                	je     8048dcd <phase_8+0x1e4>
 8048d8f:	83 f8 01             	cmp    eax,0x1
 8048d92:	7f 09                	jg     8048d9d <phase_8+0x1b4>
 8048d94:	85 c0                	test   eax,eax
 8048d96:	74 18                	je     8048db0 <phase_8+0x1c7>
 8048d98:	e9 c8 00 00 00       	jmp    8048e65 <phase_8+0x27c>
 8048d9d:	83 f8 02             	cmp    eax,0x2
 8048da0:	74 52                	je     8048df4 <phase_8+0x20b>
 8048da2:	83 f8 03             	cmp    eax,0x3
 8048da5:	0f 84 91 00 00 00    	je     8048e3c <phase_8+0x253>
 8048dab:	e9 b5 00 00 00       	jmp    8048e65 <phase_8+0x27c>
 8048db0:	0f b6 05 4c ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4c
 8048db7:	a2 4b ae 04 08       	mov    ds:0x804ae4b,al
 8048dbc:	0f b6 05 4b ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4b
 8048dc3:	a2 4a ae 04 08       	mov    ds:0x804ae4a,al
 8048dc8:	e9 98 00 00 00       	jmp    8048e65 <phase_8+0x27c>
 8048dcd:	0f b6 05 4c ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4c
 8048dd4:	0f b6 c0             	movzx  eax,al
 8048dd7:	ba 01 00 00 00       	mov    edx,0x1
 8048ddc:	89 c1                	mov    ecx,eax
 8048dde:	d3 e2                	shl    edx,cl
 8048de0:	89 d0                	mov    eax,edx
 8048de2:	89 c2                	mov    edx,eax
 8048de4:	0f b6 05 4a ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4a
 8048deb:	31 d0                	xor    eax,edx
 8048ded:	a2 4a ae 04 08       	mov    ds:0x804ae4a,al
 8048df2:	eb 71                	jmp    8048e65 <phase_8+0x27c>
 8048df4:	0f b6 05 4a ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4a
 8048dfb:	0f b6 d0             	movzx  edx,al
 8048dfe:	0f b6 05 4c ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4c
 8048e05:	0f b6 c0             	movzx  eax,al
 8048e08:	89 c1                	mov    ecx,eax
 8048e0a:	d3 e2                	shl    edx,cl
 8048e0c:	89 d0                	mov    eax,edx
 8048e0e:	89 c3                	mov    ebx,eax
 8048e10:	0f b6 05 4a ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4a
 8048e17:	0f b6 d0             	movzx  edx,al
 8048e1a:	0f b6 05 4c ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4c
 8048e21:	0f b6 c0             	movzx  eax,al
 8048e24:	b9 08 00 00 00       	mov    ecx,0x8
 8048e29:	29 c1                	sub    ecx,eax
 8048e2b:	89 c8                	mov    eax,ecx
 8048e2d:	89 c1                	mov    ecx,eax
 8048e2f:	d3 fa                	sar    edx,cl
 8048e31:	89 d0                	mov    eax,edx
 8048e33:	09 d8                	or     eax,ebx
 8048e35:	a2 4a ae 04 08       	mov    ds:0x804ae4a,al
 8048e3a:	eb 29                	jmp    8048e65 <phase_8+0x27c>
 8048e3c:	0f b6 05 4c ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4c
 8048e43:	0f b6 c0             	movzx  eax,al
 8048e46:	0f b6 80 80 bb 04 08 	movzx  eax,BYTE PTR [eax+0x804bb80]
 8048e4d:	0f b6 15 4a ae 04 08 	movzx  edx,BYTE PTR ds:0x804ae4a
 8048e54:	31 c2                	xor    edx,eax
 8048e56:	0f b6 05 4b ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4b
 8048e5d:	09 d0                	or     eax,edx
 8048e5f:	a2 4b ae 04 08       	mov    ds:0x804ae4b,al
 8048e64:	90                   	nop
 8048e65:	5b                   	pop    ebx
 8048e66:	5d                   	pop    ebp
 8048e67:	c3                   	ret    

08048e68 <phase_9>:
 8048e68:	55                   	push   ebp
 8048e69:	89 e5                	mov    ebp,esp
 8048e6b:	83 ec 18             	sub    esp,0x18
 8048e6e:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0
 8048e75:	eb 2b                	jmp    8048ea2 <phase_9+0x3a>
 8048e77:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048e7a:	8d 50 01             	lea    edx,[eax+0x1]
 8048e7d:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx
 8048e80:	0f b6 80 80 ae 04 08 	movzx  eax,BYTE PTR [eax+0x804ae80]
 8048e87:	a2 68 bb 04 08       	mov    ds:0x804bb68,al
 8048e8c:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048e8f:	8d 50 01             	lea    edx,[eax+0x1]
 8048e92:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx
 8048e95:	0f b6 80 80 ae 04 08 	movzx  eax,BYTE PTR [eax+0x804ae80]
 8048e9c:	a2 4c ae 04 08       	mov    ds:0x804ae4c,al
 8048ea1:	cc                   	int3   
 8048ea2:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048ea5:	3d cf 00 00 00       	cmp    eax,0xcf
 8048eaa:	76 cb                	jbe    8048e77 <phase_9+0xf>
 8048eac:	0f b6 05 4b ae 04 08 	movzx  eax,BYTE PTR ds:0x804ae4b
 8048eb3:	84 c0                	test   al,al
 8048eb5:	74 05                	je     8048ebc <phase_9+0x54>
 8048eb7:	e8 9b f7 ff ff       	call   8048657 <explode>
 8048ebc:	83 ec 0c             	sub    esp,0xc
 8048ebf:	68 e5 95 04 08       	push   0x80495e5
 8048ec4:	e8 42 f7 ff ff       	call   804860b <say>
 8048ec9:	83 c4 10             	add    esp,0x10
 8048ecc:	c9                   	leave  
 8048ecd:	c3                   	ret    
 8048ece:	55                   	push   ebp
 8048ecf:	89 e5                	mov    ebp,esp
 8048ed1:	53                   	push   ebx
 8048ed2:	83 ec 24             	sub    esp,0x24
 8048ed5:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0
 8048edc:	c7 45 f0 00 00 00 00 	mov    DWORD PTR [ebp-0x10],0x0
 8048ee3:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048ee6:	89 45 ec             	mov    DWORD PTR [ebp-0x14],eax
 8048ee9:	e9 df 00 00 00       	jmp    8048fcd <phase_9+0x165>
 8048eee:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048ef1:	8d 50 01             	lea    edx,[eax+0x1]
 8048ef4:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx
 8048ef7:	89 c2                	mov    edx,eax
 8048ef9:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048efc:	01 d0                	add    eax,edx
 8048efe:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048f01:	88 45 ea             	mov    BYTE PTR [ebp-0x16],al
 8048f04:	c6 45 eb 01          	mov    BYTE PTR [ebp-0x15],0x1
 8048f08:	e9 ae 00 00 00       	jmp    8048fbb <phase_9+0x153>
 8048f0d:	0f b6 45 ea          	movzx  eax,BYTE PTR [ebp-0x16]
 8048f11:	22 45 eb             	and    al,BYTE PTR [ebp-0x15]
 8048f14:	84 c0                	test   al,al
 8048f16:	75 28                	jne    8048f40 <phase_9+0xd8>
 8048f18:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048f1b:	8d 50 01             	lea    edx,[eax+0x1]
 8048f1e:	89 55 f0             	mov    DWORD PTR [ebp-0x10],edx
 8048f21:	89 c2                	mov    edx,eax
 8048f23:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048f26:	8d 0c 02             	lea    ecx,[edx+eax*1]
 8048f29:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048f2c:	8d 50 01             	lea    edx,[eax+0x1]
 8048f2f:	89 55 f4             	mov    DWORD PTR [ebp-0xc],edx
 8048f32:	89 c2                	mov    edx,eax
 8048f34:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048f37:	01 d0                	add    eax,edx
 8048f39:	0f b6 00             	movzx  eax,BYTE PTR [eax]
 8048f3c:	88 01                	mov    BYTE PTR [ecx],al
 8048f3e:	eb 72                	jmp    8048fb2 <phase_9+0x14a>
 8048f40:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 8048f43:	8b 45 0c             	mov    eax,DWORD PTR [ebp+0xc]
 8048f46:	01 d0                	add    eax,edx
 8048f48:	0f b7 00             	movzx  eax,WORD PTR [eax]
 8048f4b:	66 89 45 e8          	mov    WORD PTR [ebp-0x18],ax
 8048f4f:	0f b7 45 e8          	movzx  eax,WORD PTR [ebp-0x18]
 8048f53:	66 c1 e8 0b          	shr    ax,0xb
 8048f57:	83 c0 03             	add    eax,0x3
 8048f5a:	66 89 45 e6          	mov    WORD PTR [ebp-0x1a],ax
 8048f5e:	81 7d f0 00 08 00 00 	cmp    DWORD PTR [ebp-0x10],0x800
 8048f65:	7e 11                	jle    8048f78 <phase_9+0x110>
 8048f67:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048f6a:	8d 90 00 f8 ff ff    	lea    edx,[eax-0x800]
 8048f70:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048f73:	01 d0                	add    eax,edx
 8048f75:	89 45 ec             	mov    DWORD PTR [ebp-0x14],eax
 8048f78:	0f b7 45 e8          	movzx  eax,WORD PTR [ebp-0x18]
 8048f7c:	66 25 ff 07          	and    ax,0x7ff
 8048f80:	66 89 45 e4          	mov    WORD PTR [ebp-0x1c],ax
 8048f84:	0f b7 45 e6          	movzx  eax,WORD PTR [ebp-0x1a]
 8048f88:	0f b7 4d e4          	movzx  ecx,WORD PTR [ebp-0x1c]
 8048f8c:	8b 55 ec             	mov    edx,DWORD PTR [ebp-0x14]
 8048f8f:	01 d1                	add    ecx,edx
 8048f91:	8b 5d f0             	mov    ebx,DWORD PTR [ebp-0x10]
 8048f94:	8b 55 08             	mov    edx,DWORD PTR [ebp+0x8]
 8048f97:	01 da                	add    edx,ebx
 8048f99:	83 ec 04             	sub    esp,0x4
 8048f9c:	50                   	push   eax
 8048f9d:	51                   	push   ecx
 8048f9e:	52                   	push   edx
 8048f9f:	e8 ac f4 ff ff       	call   8048450 <memcpy@plt>
 8048fa4:	83 c4 10             	add    esp,0x10
 8048fa7:	83 45 f4 02          	add    DWORD PTR [ebp-0xc],0x2
 8048fab:	0f b7 45 e6          	movzx  eax,WORD PTR [ebp-0x1a]
 8048faf:	01 45 f0             	add    DWORD PTR [ebp-0x10],eax
 8048fb2:	0f be 45 eb          	movsx  eax,BYTE PTR [ebp-0x15]
 8048fb6:	01 c0                	add    eax,eax
 8048fb8:	88 45 eb             	mov    BYTE PTR [ebp-0x15],al
 8048fbb:	80 7d eb 00          	cmp    BYTE PTR [ebp-0x15],0x0
 8048fbf:	74 0c                	je     8048fcd <phase_9+0x165>
 8048fc1:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048fc4:	3b 45 10             	cmp    eax,DWORD PTR [ebp+0x10]
 8048fc7:	0f 8c 40 ff ff ff    	jl     8048f0d <phase_9+0xa5>
 8048fcd:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048fd0:	3b 45 10             	cmp    eax,DWORD PTR [ebp+0x10]
 8048fd3:	0f 8c 15 ff ff ff    	jl     8048eee <phase_9+0x86>
 8048fd9:	8b 5d fc             	mov    ebx,DWORD PTR [ebp-0x4]
 8048fdc:	c9                   	leave  
 8048fdd:	c3                   	ret    
 8048fde:	55                   	push   ebp
 8048fdf:	89 e5                	mov    ebp,esp
 8048fe1:	83 ec 18             	sub    esp,0x18
 8048fe4:	c7 45 f4 00 00 00 00 	mov    DWORD PTR [ebp-0xc],0x0
 8048feb:	eb 70                	jmp    804905d <phase_9+0x1f5>
 8048fed:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048ff0:	c1 e0 04             	shl    eax,0x4
 8048ff3:	05 30 bb 04 08       	add    eax,0x804bb30
 8048ff8:	8b 40 0c             	mov    eax,DWORD PTR [eax+0xc]
 8048ffb:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 8048ffe:	c1 e2 04             	shl    edx,0x4
 8049001:	81 c2 30 bb 04 08    	add    edx,0x804bb30
 8049007:	8b 52 04             	mov    edx,DWORD PTR [edx+0x4]
 804900a:	83 ec 08             	sub    esp,0x8
 804900d:	6a 00                	push   0x0
 804900f:	6a ff                	push   0xffffffff
 8049011:	6a 32                	push   0x32
 8049013:	6a 07                	push   0x7
 8049015:	50                   	push   eax
 8049016:	52                   	push   edx
 8049017:	e8 94 f4 ff ff       	call   80484b0 <mmap@plt>
 804901c:	83 c4 20             	add    esp,0x20
 804901f:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8049022:	c1 e0 04             	shl    eax,0x4
 8049025:	05 30 bb 04 08       	add    eax,0x804bb30
 804902a:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 804902d:	89 c1                	mov    ecx,eax
 804902f:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8049032:	c1 e0 04             	shl    eax,0x4
 8049035:	05 30 bb 04 08       	add    eax,0x804bb30
 804903a:	8b 00                	mov    eax,DWORD PTR [eax]
 804903c:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 804903f:	c1 e2 04             	shl    edx,0x4
 8049042:	81 c2 30 bb 04 08    	add    edx,0x804bb30
 8049048:	8b 52 04             	mov    edx,DWORD PTR [edx+0x4]
 804904b:	83 ec 04             	sub    esp,0x4
 804904e:	51                   	push   ecx
 804904f:	50                   	push   eax
 8049050:	52                   	push   edx
 8049051:	e8 78 fe ff ff       	call   8048ece <phase_9+0x66>
 8049056:	83 c4 10             	add    esp,0x10
 8049059:	83 45 f4 01          	add    DWORD PTR [ebp-0xc],0x1
 804905d:	83 7d f4 00          	cmp    DWORD PTR [ebp-0xc],0x0
 8049061:	74 8a                	je     8048fed <phase_9+0x185>
 8049063:	c9                   	leave  
 8049064:	c3                   	ret    
 8049065:	55                   	push   ebp
 8049066:	89 e5                	mov    ebp,esp
 8049068:	83 ec 08             	sub    esp,0x8
 804906b:	83 ec 08             	sub    esp,0x8
 804906e:	68 de 8f 04 08       	push   0x8048fde
 8049073:	6a 0b                	push   0xb
 8049075:	e8 e6 f3 ff ff       	call   8048460 <signal@plt>
 804907a:	83 c4 10             	add    esp,0x10
 804907d:	c9                   	leave  
 804907e:	c3                   	ret    

0804907f <phase_10>:
 804907f:	55                   	push   ebp
 8049080:	89 e5                	mov    ebp,esp
 8049082:	83 ec 08             	sub    esp,0x8
 8049085:	83 ec 0c             	sub    esp,0xc
 8049088:	ff 75 08             	push   DWORD PTR [ebp+0x8]
 804908b:	b8 70 0f 00 09       	mov    eax,0x9000f70
 8049090:	ff d0                	call   eax
 8049092:	83 c4 10             	add    esp,0x10
 8049095:	85 c0                	test   eax,eax
 8049097:	74 05                	je     804909e <phase_10+0x1f>
 8049099:	e8 b9 f5 ff ff       	call   8048657 <explode>
 804909e:	c9                   	leave  
 804909f:	c3                   	ret    

080490a0 <main>:
 80490a0:	8d 4c 24 04          	lea    ecx,[esp+0x4]
 80490a4:	83 e4 f0             	and    esp,0xfffffff0
 80490a7:	ff 71 fc             	push   DWORD PTR [ecx-0x4]
 80490aa:	55                   	push   ebp
 80490ab:	89 e5                	mov    ebp,esp
 80490ad:	51                   	push   ecx
 80490ae:	83 ec 04             	sub    esp,0x4
 80490b1:	83 ec 0c             	sub    esp,0xc
 80490b4:	68 f4 95 04 08       	push   0x80495f4
 80490b9:	e8 4d f5 ff ff       	call   804860b <say>
 80490be:	83 c4 10             	add    esp,0x10
 80490c1:	83 ec 0c             	sub    esp,0xc
 80490c4:	68 48 96 04 08       	push   0x8049648
 80490c9:	e8 3d f5 ff ff       	call   804860b <say>
 80490ce:	83 c4 10             	add    esp,0x10
 80490d1:	83 ec 0c             	sub    esp,0xc
 80490d4:	68 75 96 04 08       	push   0x8049675
 80490d9:	e8 2d f5 ff ff       	call   804860b <say>
 80490de:	83 c4 10             	add    esp,0x10
 80490e1:	83 ec 0c             	sub    esp,0xc
 80490e4:	6a 1e                	push   0x1e
 80490e6:	e8 85 f3 ff ff       	call   8048470 <alarm@plt>
 80490eb:	83 c4 10             	add    esp,0x10
 80490ee:	83 ec 0c             	sub    esp,0xc
 80490f1:	68 88 96 04 08       	push   0x8049688
 80490f6:	e8 10 f5 ff ff       	call   804860b <say>
 80490fb:	83 c4 10             	add    esp,0x10
 80490fe:	83 ec 08             	sub    esp,0x8
 8049101:	68 00 04 00 00       	push   0x400
 8049106:	68 80 bb 04 08       	push   0x804bb80
 804910b:	e8 ae f5 ff ff       	call   80486be <readline>
 8049110:	83 c4 10             	add    esp,0x10
 8049113:	83 ec 0c             	sub    esp,0xc
 8049116:	68 80 bb 04 08       	push   0x804bb80
 804911b:	e8 f9 f5 ff ff       	call   8048719 <phase_1>
 8049120:	83 c4 10             	add    esp,0x10
 8049123:	83 ec 0c             	sub    esp,0xc
 8049126:	68 94 96 04 08       	push   0x8049694
 804912b:	e8 db f4 ff ff       	call   804860b <say>
 8049130:	83 c4 10             	add    esp,0x10
 8049133:	83 ec 08             	sub    esp,0x8
 8049136:	68 00 04 00 00       	push   0x400
 804913b:	68 80 bb 04 08       	push   0x804bb80
 8049140:	e8 79 f5 ff ff       	call   80486be <readline>
 8049145:	83 c4 10             	add    esp,0x10
 8049148:	83 ec 0c             	sub    esp,0xc
 804914b:	68 80 bb 04 08       	push   0x804bb80
 8049150:	e8 0e f6 ff ff       	call   8048763 <phase_2>
 8049155:	83 c4 10             	add    esp,0x10
 8049158:	83 ec 0c             	sub    esp,0xc
 804915b:	68 a0 96 04 08       	push   0x80496a0
 8049160:	e8 a6 f4 ff ff       	call   804860b <say>
 8049165:	83 c4 10             	add    esp,0x10
 8049168:	83 ec 08             	sub    esp,0x8
 804916b:	68 00 04 00 00       	push   0x400
 8049170:	68 80 bb 04 08       	push   0x804bb80
 8049175:	e8 44 f5 ff ff       	call   80486be <readline>
 804917a:	83 c4 10             	add    esp,0x10
 804917d:	83 ec 0c             	sub    esp,0xc
 8049180:	68 80 bb 04 08       	push   0x804bb80
 8049185:	e8 70 f6 ff ff       	call   80487fa <phase_3>
 804918a:	83 c4 10             	add    esp,0x10
 804918d:	83 ec 0c             	sub    esp,0xc
 8049190:	68 ac 96 04 08       	push   0x80496ac
 8049195:	e8 71 f4 ff ff       	call   804860b <say>
 804919a:	83 c4 10             	add    esp,0x10
 804919d:	83 ec 08             	sub    esp,0x8
 80491a0:	68 00 04 00 00       	push   0x400
 80491a5:	68 80 bb 04 08       	push   0x804bb80
 80491aa:	e8 0f f5 ff ff       	call   80486be <readline>
 80491af:	83 c4 10             	add    esp,0x10
 80491b2:	83 ec 0c             	sub    esp,0xc
 80491b5:	68 80 bb 04 08       	push   0x804bb80
 80491ba:	e8 1e f7 ff ff       	call   80488dd <phase_4>
 80491bf:	83 c4 10             	add    esp,0x10
 80491c2:	83 ec 0c             	sub    esp,0xc
 80491c5:	68 b8 96 04 08       	push   0x80496b8
 80491ca:	e8 3c f4 ff ff       	call   804860b <say>
 80491cf:	83 c4 10             	add    esp,0x10
 80491d2:	83 ec 08             	sub    esp,0x8
 80491d5:	68 00 04 00 00       	push   0x400
 80491da:	68 80 bb 04 08       	push   0x804bb80
 80491df:	e8 da f4 ff ff       	call   80486be <readline>
 80491e4:	83 c4 10             	add    esp,0x10
 80491e7:	83 ec 0c             	sub    esp,0xc
 80491ea:	68 80 bb 04 08       	push   0x804bb80
 80491ef:	e8 3c f7 ff ff       	call   8048930 <phase_5>
 80491f4:	83 c4 10             	add    esp,0x10
 80491f7:	83 ec 0c             	sub    esp,0xc
 80491fa:	68 c4 96 04 08       	push   0x80496c4
 80491ff:	e8 07 f4 ff ff       	call   804860b <say>
 8049204:	83 c4 10             	add    esp,0x10
 8049207:	83 ec 08             	sub    esp,0x8
 804920a:	68 00 04 00 00       	push   0x400
 804920f:	68 80 bb 04 08       	push   0x804bb80
 8049214:	e8 a5 f4 ff ff       	call   80486be <readline>
 8049219:	83 c4 10             	add    esp,0x10
 804921c:	83 ec 0c             	sub    esp,0xc
 804921f:	68 80 bb 04 08       	push   0x804bb80
 8049224:	e8 21 f8 ff ff       	call   8048a4a <phase_6>
 8049229:	83 c4 10             	add    esp,0x10
 804922c:	83 ec 0c             	sub    esp,0xc
 804922f:	68 d0 96 04 08       	push   0x80496d0
 8049234:	e8 d2 f3 ff ff       	call   804860b <say>
 8049239:	83 c4 10             	add    esp,0x10
 804923c:	83 ec 08             	sub    esp,0x8
 804923f:	68 00 04 00 00       	push   0x400
 8049244:	68 80 bb 04 08       	push   0x804bb80
 8049249:	e8 70 f4 ff ff       	call   80486be <readline>
 804924e:	83 c4 10             	add    esp,0x10
 8049251:	83 ec 0c             	sub    esp,0xc
 8049254:	68 80 bb 04 08       	push   0x804bb80
 8049259:	e8 bc f8 ff ff       	call   8048b1a <phase_7>
 804925e:	83 c4 10             	add    esp,0x10
 8049261:	83 ec 0c             	sub    esp,0xc
 8049264:	68 dc 96 04 08       	push   0x80496dc
 8049269:	e8 9d f3 ff ff       	call   804860b <say>
 804926e:	83 c4 10             	add    esp,0x10
 8049271:	83 ec 08             	sub    esp,0x8
 8049274:	68 00 04 00 00       	push   0x400
 8049279:	68 80 bb 04 08       	push   0x804bb80
 804927e:	e8 3b f4 ff ff       	call   80486be <readline>
 8049283:	83 c4 10             	add    esp,0x10
 8049286:	83 ec 0c             	sub    esp,0xc
 8049289:	68 80 bb 04 08       	push   0x804bb80
 804928e:	e8 56 f9 ff ff       	call   8048be9 <phase_8>
 8049293:	83 c4 10             	add    esp,0x10
 8049296:	83 ec 0c             	sub    esp,0xc
 8049299:	68 e8 96 04 08       	push   0x80496e8
 804929e:	e8 68 f3 ff ff       	call   804860b <say>
 80492a3:	83 c4 10             	add    esp,0x10
 80492a6:	83 ec 08             	sub    esp,0x8
 80492a9:	68 00 04 00 00       	push   0x400
 80492ae:	68 80 bb 04 08       	push   0x804bb80
 80492b3:	e8 06 f4 ff ff       	call   80486be <readline>
 80492b8:	83 c4 10             	add    esp,0x10
 80492bb:	83 ec 0c             	sub    esp,0xc
 80492be:	68 80 bb 04 08       	push   0x804bb80
 80492c3:	e8 a0 fb ff ff       	call   8048e68 <phase_9>
 80492c8:	83 c4 10             	add    esp,0x10
 80492cb:	83 ec 0c             	sub    esp,0xc
 80492ce:	68 f4 96 04 08       	push   0x80496f4
 80492d3:	e8 33 f3 ff ff       	call   804860b <say>
 80492d8:	83 c4 10             	add    esp,0x10
 80492db:	83 ec 08             	sub    esp,0x8
 80492de:	68 00 04 00 00       	push   0x400
 80492e3:	68 80 bb 04 08       	push   0x804bb80
 80492e8:	e8 d1 f3 ff ff       	call   80486be <readline>
 80492ed:	83 c4 10             	add    esp,0x10
 80492f0:	83 ec 0c             	sub    esp,0xc
 80492f3:	68 80 bb 04 08       	push   0x804bb80
 80492f8:	e8 82 fd ff ff       	call   804907f <phase_10>
 80492fd:	83 c4 10             	add    esp,0x10
 8049300:	83 ec 0c             	sub    esp,0xc
 8049303:	68 04 97 04 08       	push   0x8049704
 8049308:	e8 fe f2 ff ff       	call   804860b <say>
 804930d:	83 c4 10             	add    esp,0x10
 8049310:	83 ec 0c             	sub    esp,0xc
 8049313:	6a 00                	push   0x0
 8049315:	e8 86 f1 ff ff       	call   80484a0 <exit@plt>
 804931a:	66 90                	xchg   ax,ax
 804931c:	66 90                	xchg   ax,ax
 804931e:	66 90                	xchg   ax,ax
 8049320:	55                   	push   ebp
 8049321:	57                   	push   edi
 8049322:	31 ff                	xor    edi,edi
 8049324:	56                   	push   esi
 8049325:	53                   	push   ebx
 8049326:	e8 15 f2 ff ff       	call   8048540 <nanosleep@plt+0x40>
 804932b:	81 c3 dd 19 00 00    	add    ebx,0x19dd
 8049331:	83 ec 1c             	sub    esp,0x1c
 8049334:	8b 6c 24 30          	mov    ebp,DWORD PTR [esp+0x30]
 8049338:	8d b3 0c ff ff ff    	lea    esi,[ebx-0xf4]
 804933e:	e8 b5 f0 ff ff       	call   80483f8 <strcmp@plt-0x38>
 8049343:	8d 83 fc fe ff ff    	lea    eax,[ebx-0x104]
 8049349:	29 c6                	sub    esi,eax
 804934b:	c1 fe 02             	sar    esi,0x2
 804934e:	85 f6                	test   esi,esi
 8049350:	74 27                	je     8049379 <main+0x2d9>
 8049352:	8d b6 00 00 00 00    	lea    esi,[esi+0x0]
 8049358:	8b 44 24 38          	mov    eax,DWORD PTR [esp+0x38]
 804935c:	89 2c 24             	mov    DWORD PTR [esp],ebp
 804935f:	89 44 24 08          	mov    DWORD PTR [esp+0x8],eax
 8049363:	8b 44 24 34          	mov    eax,DWORD PTR [esp+0x34]
 8049367:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 804936b:	ff 94 bb fc fe ff ff 	call   DWORD PTR [ebx+edi*4-0x104]
 8049372:	83 c7 01             	add    edi,0x1
 8049375:	39 f7                	cmp    edi,esi
 8049377:	75 df                	jne    8049358 <main+0x2b8>
 8049379:	83 c4 1c             	add    esp,0x1c
 804937c:	5b                   	pop    ebx
 804937d:	5e                   	pop    esi
 804937e:	5f                   	pop    edi
 804937f:	5d                   	pop    ebp
 8049380:	c3                   	ret    
 8049381:	eb 0d                	jmp    8049390 <main+0x2f0>
 8049383:	90                   	nop
 8049384:	90                   	nop
 8049385:	90                   	nop
 8049386:	90                   	nop
 8049387:	90                   	nop
 8049388:	90                   	nop
 8049389:	90                   	nop
 804938a:	90                   	nop
 804938b:	90                   	nop
 804938c:	90                   	nop
 804938d:	90                   	nop
 804938e:	90                   	nop
 804938f:	90                   	nop
 8049390:	f3 c3                	repz ret 

Disassembly of section .fini:

08049394 <.fini>:
 8049394:	53                   	push   ebx
 8049395:	83 ec 08             	sub    esp,0x8
 8049398:	e8 a3 f1 ff ff       	call   8048540 <nanosleep@plt+0x40>
 804939d:	81 c3 6b 19 00 00    	add    ebx,0x196b
 80493a3:	83 c4 08             	add    esp,0x8
 80493a6:	5b                   	pop    ebx
 80493a7:	c3                   	ret    
