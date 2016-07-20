
sortfile:     file format elf32-i386


Disassembly of section .init:

080483fc <_init>:
 80483fc:	53                   	push   ebx
 80483fd:	83 ec 08             	sub    esp,0x8
 8048400:	e8 00 00 00 00       	call   8048405 <_init+0x9>
 8048405:	5b                   	pop    ebx
 8048406:	81 c3 fb 1b 00 00    	add    ebx,0x1bfb
 804840c:	8b 83 fc ff ff ff    	mov    eax,DWORD PTR [ebx-0x4]
 8048412:	85 c0                	test   eax,eax
 8048414:	74 05                	je     804841b <_init+0x1f>
 8048416:	e8 75 00 00 00       	call   8048490 <__gmon_start__@plt>
 804841b:	83 c4 08             	add    esp,0x8
 804841e:	5b                   	pop    ebx
 804841f:	c3                   	ret    

Disassembly of section .plt:

08048420 <strcmp@plt-0x10>:
 8048420:	ff 35 04 a0 04 08    	push   DWORD PTR ds:0x804a004
 8048426:	ff 25 08 a0 04 08    	jmp    DWORD PTR ds:0x804a008
 804842c:	00 00                	add    BYTE PTR [eax],al
	...

08048430 <strcmp@plt>:
 8048430:	ff 25 0c a0 04 08    	jmp    DWORD PTR ds:0x804a00c
 8048436:	68 00 00 00 00       	push   0x0
 804843b:	e9 e0 ff ff ff       	jmp    8048420 <_init+0x24>

08048440 <printf@plt>:
 8048440:	ff 25 10 a0 04 08    	jmp    DWORD PTR ds:0x804a010
 8048446:	68 08 00 00 00       	push   0x8
 804844b:	e9 d0 ff ff ff       	jmp    8048420 <_init+0x24>

08048450 <memcpy@plt>:
 8048450:	ff 25 14 a0 04 08    	jmp    DWORD PTR ds:0x804a014
 8048456:	68 10 00 00 00       	push   0x10
 804845b:	e9 c0 ff ff ff       	jmp    8048420 <_init+0x24>

08048460 <malloc@plt>:
 8048460:	ff 25 18 a0 04 08    	jmp    DWORD PTR ds:0x804a018
 8048466:	68 18 00 00 00       	push   0x18
 804846b:	e9 b0 ff ff ff       	jmp    8048420 <_init+0x24>

08048470 <__fxstat@plt>:
 8048470:	ff 25 1c a0 04 08    	jmp    DWORD PTR ds:0x804a01c
 8048476:	68 20 00 00 00       	push   0x20
 804847b:	e9 a0 ff ff ff       	jmp    8048420 <_init+0x24>

08048480 <system@plt>:
 8048480:	ff 25 20 a0 04 08    	jmp    DWORD PTR ds:0x804a020
 8048486:	68 28 00 00 00       	push   0x28
 804848b:	e9 90 ff ff ff       	jmp    8048420 <_init+0x24>

08048490 <__gmon_start__@plt>:
 8048490:	ff 25 24 a0 04 08    	jmp    DWORD PTR ds:0x804a024
 8048496:	68 30 00 00 00       	push   0x30
 804849b:	e9 80 ff ff ff       	jmp    8048420 <_init+0x24>

080484a0 <exit@plt>:
 80484a0:	ff 25 28 a0 04 08    	jmp    DWORD PTR ds:0x804a028
 80484a6:	68 38 00 00 00       	push   0x38
 80484ab:	e9 70 ff ff ff       	jmp    8048420 <_init+0x24>

080484b0 <open@plt>:
 80484b0:	ff 25 2c a0 04 08    	jmp    DWORD PTR ds:0x804a02c
 80484b6:	68 40 00 00 00       	push   0x40
 80484bb:	e9 60 ff ff ff       	jmp    8048420 <_init+0x24>

080484c0 <mmap@plt>:
 80484c0:	ff 25 30 a0 04 08    	jmp    DWORD PTR ds:0x804a030
 80484c6:	68 48 00 00 00       	push   0x48
 80484cb:	e9 50 ff ff ff       	jmp    8048420 <_init+0x24>

080484d0 <__libc_start_main@plt>:
 80484d0:	ff 25 34 a0 04 08    	jmp    DWORD PTR ds:0x804a034
 80484d6:	68 50 00 00 00       	push   0x50
 80484db:	e9 40 ff ff ff       	jmp    8048420 <_init+0x24>

080484e0 <write@plt>:
 80484e0:	ff 25 38 a0 04 08    	jmp    DWORD PTR ds:0x804a038
 80484e6:	68 58 00 00 00       	push   0x58
 80484eb:	e9 30 ff ff ff       	jmp    8048420 <_init+0x24>

080484f0 <ftruncate@plt>:
 80484f0:	ff 25 3c a0 04 08    	jmp    DWORD PTR ds:0x804a03c
 80484f6:	68 60 00 00 00       	push   0x60
 80484fb:	e9 20 ff ff ff       	jmp    8048420 <_init+0x24>

Disassembly of section .text:

08048500 <_start>:
 8048500:	31 ed                	xor    ebp,ebp
 8048502:	5e                   	pop    esi
 8048503:	89 e1                	mov    ecx,esp
 8048505:	83 e4 f0             	and    esp,0xfffffff0
 8048508:	50                   	push   eax
 8048509:	54                   	push   esp
 804850a:	52                   	push   edx
 804850b:	68 d0 89 04 08       	push   0x80489d0
 8048510:	68 60 89 04 08       	push   0x8048960
 8048515:	51                   	push   ecx
 8048516:	56                   	push   esi
 8048517:	68 26 89 04 08       	push   0x8048926
 804851c:	e8 af ff ff ff       	call   80484d0 <__libc_start_main@plt>
 8048521:	f4                   	hlt    
 8048522:	66 90                	xchg   ax,ax
 8048524:	66 90                	xchg   ax,ax
 8048526:	66 90                	xchg   ax,ax
 8048528:	66 90                	xchg   ax,ax
 804852a:	66 90                	xchg   ax,ax
 804852c:	66 90                	xchg   ax,ax
 804852e:	66 90                	xchg   ax,ax

08048530 <deregister_tm_clones>:
 8048530:	b8 4b a0 04 08       	mov    eax,0x804a04b
 8048535:	2d 48 a0 04 08       	sub    eax,0x804a048
 804853a:	83 f8 06             	cmp    eax,0x6
 804853d:	77 02                	ja     8048541 <deregister_tm_clones+0x11>
 804853f:	f3 c3                	repz ret 
 8048541:	b8 00 00 00 00       	mov    eax,0x0
 8048546:	85 c0                	test   eax,eax
 8048548:	74 f5                	je     804853f <deregister_tm_clones+0xf>
 804854a:	55                   	push   ebp
 804854b:	89 e5                	mov    ebp,esp
 804854d:	83 ec 18             	sub    esp,0x18
 8048550:	c7 04 24 48 a0 04 08 	mov    DWORD PTR [esp],0x804a048
 8048557:	ff d0                	call   eax
 8048559:	c9                   	leave  
 804855a:	c3                   	ret    
 804855b:	90                   	nop
 804855c:	8d 74 26 00          	lea    esi,[esi+eiz*1+0x0]

08048560 <register_tm_clones>:
 8048560:	b8 48 a0 04 08       	mov    eax,0x804a048
 8048565:	2d 48 a0 04 08       	sub    eax,0x804a048
 804856a:	c1 f8 02             	sar    eax,0x2
 804856d:	89 c2                	mov    edx,eax
 804856f:	c1 ea 1f             	shr    edx,0x1f
 8048572:	01 d0                	add    eax,edx
 8048574:	d1 f8                	sar    eax,1
 8048576:	75 02                	jne    804857a <register_tm_clones+0x1a>
 8048578:	f3 c3                	repz ret 
 804857a:	ba 00 00 00 00       	mov    edx,0x0
 804857f:	85 d2                	test   edx,edx
 8048581:	74 f5                	je     8048578 <register_tm_clones+0x18>
 8048583:	55                   	push   ebp
 8048584:	89 e5                	mov    ebp,esp
 8048586:	83 ec 18             	sub    esp,0x18
 8048589:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 804858d:	c7 04 24 48 a0 04 08 	mov    DWORD PTR [esp],0x804a048
 8048594:	ff d2                	call   edx
 8048596:	c9                   	leave  
 8048597:	c3                   	ret    
 8048598:	90                   	nop
 8048599:	8d b4 26 00 00 00 00 	lea    esi,[esi+eiz*1+0x0]

080485a0 <__do_global_dtors_aux>:
 80485a0:	80 3d 48 a0 04 08 00 	cmp    BYTE PTR ds:0x804a048,0x0
 80485a7:	75 13                	jne    80485bc <__do_global_dtors_aux+0x1c>
 80485a9:	55                   	push   ebp
 80485aa:	89 e5                	mov    ebp,esp
 80485ac:	83 ec 08             	sub    esp,0x8
 80485af:	e8 7c ff ff ff       	call   8048530 <deregister_tm_clones>
 80485b4:	c6 05 48 a0 04 08 01 	mov    BYTE PTR ds:0x804a048,0x1
 80485bb:	c9                   	leave  
 80485bc:	f3 c3                	repz ret 
 80485be:	66 90                	xchg   ax,ax

080485c0 <frame_dummy>:
 80485c0:	a1 10 9f 04 08       	mov    eax,ds:0x8049f10
 80485c5:	85 c0                	test   eax,eax
 80485c7:	74 1e                	je     80485e7 <frame_dummy+0x27>
 80485c9:	b8 00 00 00 00       	mov    eax,0x0
 80485ce:	85 c0                	test   eax,eax
 80485d0:	74 15                	je     80485e7 <frame_dummy+0x27>
 80485d2:	55                   	push   ebp
 80485d3:	89 e5                	mov    ebp,esp
 80485d5:	83 ec 18             	sub    esp,0x18
 80485d8:	c7 04 24 10 9f 04 08 	mov    DWORD PTR [esp],0x8049f10
 80485df:	ff d0                	call   eax
 80485e1:	c9                   	leave  
 80485e2:	e9 79 ff ff ff       	jmp    8048560 <register_tm_clones>
 80485e7:	e9 74 ff ff ff       	jmp    8048560 <register_tm_clones>

080485ec <load_file>:
 80485ec:	55                   	push   ebp
 80485ed:	89 e5                	mov    ebp,esp
 80485ef:	81 ec 88 00 00 00    	sub    esp,0x88
 80485f5:	c7 04 24 30 8a 04 08 	mov    DWORD PTR [esp],0x8048a30
 80485fc:	e8 7f fe ff ff       	call   8048480 <system@plt>
 8048601:	85 c0                	test   eax,eax
 8048603:	74 18                	je     804861d <load_file+0x31>
 8048605:	c7 04 24 40 8a 04 08 	mov    DWORD PTR [esp],0x8048a40
 804860c:	e8 2f fe ff ff       	call   8048440 <printf@plt>
 8048611:	c7 04 24 01 00 00 00 	mov    DWORD PTR [esp],0x1
 8048618:	e8 83 fe ff ff       	call   80484a0 <exit@plt>
 804861d:	c7 04 24 59 8a 04 08 	mov    DWORD PTR [esp],0x8048a59
 8048624:	e8 57 fe ff ff       	call   8048480 <system@plt>
 8048629:	85 c0                	test   eax,eax
 804862b:	75 28                	jne    8048655 <load_file+0x69>
 804862d:	c7 04 24 6a 8a 04 08 	mov    DWORD PTR [esp],0x8048a6a
 8048634:	e8 47 fe ff ff       	call   8048480 <system@plt>
 8048639:	85 c0                	test   eax,eax
 804863b:	74 18                	je     8048655 <load_file+0x69>
 804863d:	c7 04 24 84 8a 04 08 	mov    DWORD PTR [esp],0x8048a84
 8048644:	e8 f7 fd ff ff       	call   8048440 <printf@plt>
 8048649:	c7 04 24 00 00 00 00 	mov    DWORD PTR [esp],0x0
 8048650:	e8 4b fe ff ff       	call   80484a0 <exit@plt>
 8048655:	c7 44 24 04 00 00 00 	mov    DWORD PTR [esp+0x4],0x0
 804865c:	00 
 804865d:	c7 04 24 9d 8a 04 08 	mov    DWORD PTR [esp],0x8048a9d
 8048664:	e8 47 fe ff ff       	call   80484b0 <open@plt>
 8048669:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax
 804866c:	8d 45 9c             	lea    eax,[ebp-0x64]
 804866f:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 8048673:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048676:	89 04 24             	mov    DWORD PTR [esp],eax
 8048679:	e8 62 03 00 00       	call   80489e0 <__fstat>
 804867e:	8b 45 c8             	mov    eax,DWORD PTR [ebp-0x38]
 8048681:	a3 50 a0 04 08       	mov    ds:0x804a050,eax
 8048686:	c7 05 54 a0 04 08 00 	mov    DWORD PTR ds:0x804a054,0x0
 804868d:	00 00 00 
 8048690:	a1 50 a0 04 08       	mov    eax,ds:0x804a050
 8048695:	c7 44 24 14 00 00 00 	mov    DWORD PTR [esp+0x14],0x0
 804869c:	00 
 804869d:	8b 55 f4             	mov    edx,DWORD PTR [ebp-0xc]
 80486a0:	89 54 24 10          	mov    DWORD PTR [esp+0x10],edx
 80486a4:	c7 44 24 0c 02 00 00 	mov    DWORD PTR [esp+0xc],0x2
 80486ab:	00 
 80486ac:	c7 44 24 08 01 00 00 	mov    DWORD PTR [esp+0x8],0x1
 80486b3:	00 
 80486b4:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 80486b8:	c7 04 24 00 00 00 00 	mov    DWORD PTR [esp],0x0
 80486bf:	e8 fc fd ff ff       	call   80484c0 <mmap@plt>
 80486c4:	a3 4c a0 04 08       	mov    ds:0x804a04c,eax
 80486c9:	c9                   	leave  
 80486ca:	c3                   	ret    

080486cb <get_line>:
 80486cb:	55                   	push   ebp
 80486cc:	89 e5                	mov    ebp,esp
 80486ce:	53                   	push   ebx
 80486cf:	83 ec 10             	sub    esp,0x10
 80486d2:	c7 45 f8 00 00 00 00 	mov    DWORD PTR [ebp-0x8],0x0    # Put 0 in -0x8 (counter)
 80486d9:	eb 29                	jmp    8048704 <get_line+0x39>    # Jump to %704
 80486db:	8b 45 f8             	mov    eax,DWORD PTR [ebp-0x8]    # Move counter into eax
 80486de:	8b 55 08             	mov    edx,DWORD PTR [ebp+0x8]    # Move start file into edx
 80486e1:	8d 0c 02             	lea    ecx,[edx+eax*1]            # --
 80486e4:	8b 1d 4c a0 04 08    	mov    ebx,DWORD PTR ds:0x804a04c # --
 80486ea:	a1 54 a0 04 08       	mov    eax,ds:0x804a054           # --
 80486ef:	89 c2                	mov    edx,eax                    # Get next byte
 80486f1:	01 da                	add    edx,ebx                    # --
 80486f3:	0f b6 12             	movzx  edx,BYTE PTR [edx]         # --
 80486f6:	88 11                	mov    BYTE PTR [ecx],dl          # --
 80486f8:	83 45 f8 01          	add    DWORD PTR [ebp-0x8],0x1    # Add one to counter
 80486fc:	83 c0 01             	add    eax,0x1                    # Add to file pos
 80486ff:	a3 54 a0 04 08       	mov    ds:0x804a054,eax           # Update file pos
 8048704:	8b 15 54 a0 04 08    	mov    edx,DWORD PTR ds:0x804a054 # Get pos in file in edx
 804870a:	a1 50 a0 04 08       	mov    eax,ds:0x804a050           # Get filesize in eax
 804870f:	39 c2                	cmp    edx,eax                    # Is edx >= eax?
 8048711:	7d 14                	jge    8048727 <get_line+0x5c>    # Jump to %727 if yes
 8048713:	8b 15 4c a0 04 08    	mov    edx,DWORD PTR ds:0x804a04c # --
 8048719:	a1 54 a0 04 08       	mov    eax,ds:0x804a054           # --
 804871e:	01 d0                	add    eax,edx                    # --
 8048720:	0f b6 00             	movzx  eax,BYTE PTR [eax]         # --
 8048723:	3c 0a                	cmp    al,0xa                     # Is al = 10 (newline)?
 8048725:	75 b4                	jne    80486db <get_line+0x10>    # Jump to #6db if not
 8048727:	a1 54 a0 04 08       	mov    eax,ds:0x804a054           # Get file pos in eax
 804872c:	83 c0 01             	add    eax,0x1                    # Add one to it
 804872f:	a3 54 a0 04 08       	mov    ds:0x804a054,eax
 8048734:	8b 45 f8             	mov    eax,DWORD PTR [ebp-0x8]
 8048737:	83 c4 10             	add    esp,0x10
 804873a:	5b                   	pop    ebx
 804873b:	5d                   	pop    ebp
 804873c:	c3                   	ret    

0804873d <get_lines>:
 804873d:	55                   	push   ebp
 804873e:	89 e5                	mov    ebp,esp
 8048740:	53                   	push   ebx
 8048741:	81 ec 24 10 00 00    	sub    esp,0x1024
 8048747:	c7 85 f0 ef ff ff 00 	mov    DWORD PTR [ebp-0x1010],0x0
 804874e:	00 00 00 
 8048751:	e9 ae 00 00 00       	jmp    8048804 <get_lines+0xc7>
 8048756:	c7 04 24 0c 00 00 00 	mov    DWORD PTR [esp],0xc
 804875d:	e8 fe fc ff ff       	call   8048460 <malloc@plt>
 8048762:	89 85 ec ef ff ff    	mov    DWORD PTR [ebp-0x1014],eax # --
 8048768:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014] # --
 804876e:	8b 95 f0 ef ff ff    	mov    edx,DWORD PTR [ebp-0x1010] # Variable stuff
 8048774:	89 50 08             	mov    DWORD PTR [eax+0x8],edx    # --
 8048777:	c7 44 24 04 00 10 00 	mov    DWORD PTR [esp+0x4],0x1000 # --
 804877e:	00 
 804877f:	8d 85 ec ef ff ff    	lea    eax,[ebp-0x1014]           # Load variable adr into eax
 8048785:	83 c0 0c             	add    eax,0xc                    # Add twelve to it
 8048788:	89 04 24             	mov    DWORD PTR [esp],eax        # Move sp to after the vars
 804878b:	e8 3b ff ff ff       	call   80486cb <get_line>         # Call <get_line>
 8048790:	89 85 f4 ef ff ff    	mov    DWORD PTR [ebp-0x100c],eax # Result (res) into -0x100c
 8048796:	8b 9d ec ef ff ff    	mov    ebx,DWORD PTR [ebp-0x1014] # Move -0x1014 into ebx
 804879c:	8b 85 f4 ef ff ff    	mov    eax,DWORD PTR [ebp-0x100c] # Make eax point at res
 80487a2:	83 c0 01             	add    eax,0x1                    # Add to eax
 80487a5:	89 04 24             	mov    DWORD PTR [esp],eax        # The sp points at next byte
 80487a8:	e8 b3 fc ff ff       	call   8048460 <malloc@plt>       # Allocate space
 80487ad:	89 03                	mov    DWORD PTR [ebx],eax        # --
 80487af:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014] # --
 80487b5:	8b 95 f4 ef ff ff    	mov    edx,DWORD PTR [ebp-0x100c] # --
 80487bb:	89 50 04             	mov    DWORD PTR [eax+0x4],edx    # --
 80487be:	8b 95 f4 ef ff ff    	mov    edx,DWORD PTR [ebp-0x100c] # --
 80487c4:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014] # get_line to memcpy
 80487ca:	8b 00                	mov    eax,DWORD PTR [eax]        # --
 80487cc:	89 54 24 08          	mov    DWORD PTR [esp+0x8],edx    # --
 80487d0:	8d 95 ec ef ff ff    	lea    edx,[ebp-0x1014]           # --
 80487d6:	83 c2 0c             	add    edx,0xc                    # --
 80487d9:	89 54 24 04          	mov    DWORD PTR [esp+0x4],edx    # --
 80487dd:	89 04 24             	mov    DWORD PTR [esp],eax        # --
 80487e0:	e8 6b fc ff ff       	call   8048450 <memcpy@plt>       # Call memcpy
 80487e5:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014] # --
 80487eb:	8b 10                	mov    edx,DWORD PTR [eax]        # --
 80487ed:	8b 85 f4 ef ff ff    	mov    eax,DWORD PTR [ebp-0x100c] # --
 80487f3:	01 d0                	add    eax,edx                    # Clean up
 80487f5:	c6 00 00             	mov    BYTE PTR [eax],0x0         # --
 80487f8:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014] # --
 80487fe:	89 85 f0 ef ff ff    	mov    DWORD PTR [ebp-0x1010],eax # --
 8048804:	8b 15 54 a0 04 08    	mov    edx,DWORD PTR ds:0x804a054 # Get file pos
 804880a:	a1 50 a0 04 08       	mov    eax,ds:0x804a050           # Get file length
 804880f:	39 c2                	cmp    edx,eax                    # Is edx < eax? (more lines?)
 8048811:	0f 8c 3f ff ff ff    	jl     8048756 <get_lines+0x19>   # If yes, jump to %756
 8048817:	8b 85 ec ef ff ff    	mov    eax,DWORD PTR [ebp-0x1014]
 804881d:	81 c4 24 10 00 00    	add    esp,0x1024
 8048823:	5b                   	pop    ebx
 8048824:	5d                   	pop    ebp
 8048825:	c3                   	ret    

08048826 <write_file>:
 8048826:	55                   	push   ebp
 8048827:	89 e5                	mov    ebp,esp
 8048829:	83 ec 28             	sub    esp,0x28
 804882c:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 804882f:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax
 8048832:	c7 44 24 08 a4 01 00 	mov    DWORD PTR [esp+0x8],0x1a4
 8048839:	00 
 804883a:	c7 44 24 04 41 00 00 	mov    DWORD PTR [esp+0x4],0x41
 8048841:	00 
 8048842:	c7 04 24 a5 8a 04 08 	mov    DWORD PTR [esp],0x8048aa5
 8048849:	e8 62 fc ff ff       	call   80484b0 <open@plt>
 804884e:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax
 8048851:	c7 44 24 04 00 00 00 	mov    DWORD PTR [esp+0x4],0x0
 8048858:	00 
 8048859:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 804885c:	89 04 24             	mov    DWORD PTR [esp],eax
 804885f:	e8 8c fc ff ff       	call   80484f0 <ftruncate@plt>
 8048864:	eb 42                	jmp    80488a8 <write_file+0x82>
 8048866:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048869:	8b 50 04             	mov    edx,DWORD PTR [eax+0x4]
 804886c:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 804886f:	8b 00                	mov    eax,DWORD PTR [eax]
 8048871:	89 54 24 08          	mov    DWORD PTR [esp+0x8],edx
 8048875:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 8048879:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 804887c:	89 04 24             	mov    DWORD PTR [esp],eax
 804887f:	e8 5c fc ff ff       	call   80484e0 <write@plt>
 8048884:	c7 44 24 08 01 00 00 	mov    DWORD PTR [esp+0x8],0x1
 804888b:	00 
 804888c:	c7 44 24 04 ae 8a 04 	mov    DWORD PTR [esp+0x4],0x8048aae
 8048893:	08 
 8048894:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048897:	89 04 24             	mov    DWORD PTR [esp],eax
 804889a:	e8 41 fc ff ff       	call   80484e0 <write@plt>
 804889f:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 80488a2:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 80488a5:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax
 80488a8:	83 7d f4 00          	cmp    DWORD PTR [ebp-0xc],0x0
 80488ac:	75 b8                	jne    8048866 <write_file+0x40>
 80488ae:	c9                   	leave  
 80488af:	c3                   	ret    

080488b0 <insertion_sort>:
 80488b0:	55                   	push   ebp
 80488b1:	89 e5                	mov    ebp,esp
 80488b3:	83 ec 38             	sub    esp,0x38
 80488b6:	c7 45 ec 00 00 00 00 	mov    DWORD PTR [ebp-0x14],0x0
 80488bd:	eb 5c                	jmp    804891b <insertion_sort+0x6b>
 80488bf:	8d 45 e4             	lea    eax,[ebp-0x1c]
 80488c2:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax
 80488c5:	eb 09                	jmp    80488d0 <insertion_sort+0x20>
 80488c7:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 80488ca:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 80488cd:	89 45 f4             	mov    DWORD PTR [ebp-0xc],eax
 80488d0:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 80488d3:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 80488d6:	85 c0                	test   eax,eax
 80488d8:	74 1d                	je     80488f7 <insertion_sort+0x47>
 80488da:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 80488dd:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 80488e0:	8b 10                	mov    edx,DWORD PTR [eax]
 80488e2:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 80488e5:	8b 00                	mov    eax,DWORD PTR [eax]
 80488e7:	89 54 24 04          	mov    DWORD PTR [esp+0x4],edx
 80488eb:	89 04 24             	mov    DWORD PTR [esp],eax
 80488ee:	e8 3d fb ff ff       	call   8048430 <strcmp@plt>
 80488f3:	85 c0                	test   eax,eax
 80488f5:	7f d0                	jg     80488c7 <insertion_sort+0x17>
 80488f7:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 80488fa:	8b 40 08             	mov    eax,DWORD PTR [eax+0x8]
 80488fd:	89 45 f0             	mov    DWORD PTR [ebp-0x10],eax
 8048900:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 8048903:	8b 50 08             	mov    edx,DWORD PTR [eax+0x8]
 8048906:	8b 45 08             	mov    eax,DWORD PTR [ebp+0x8]
 8048909:	89 50 08             	mov    DWORD PTR [eax+0x8],edx
 804890c:	8b 45 f4             	mov    eax,DWORD PTR [ebp-0xc]
 804890f:	8b 55 08             	mov    edx,DWORD PTR [ebp+0x8]
 8048912:	89 50 08             	mov    DWORD PTR [eax+0x8],edx
 8048915:	8b 45 f0             	mov    eax,DWORD PTR [ebp-0x10]
 8048918:	89 45 08             	mov    DWORD PTR [ebp+0x8],eax
 804891b:	83 7d 08 00          	cmp    DWORD PTR [ebp+0x8],0x0
 804891f:	75 9e                	jne    80488bf <insertion_sort+0xf>
 8048921:	8b 45 ec             	mov    eax,DWORD PTR [ebp-0x14]
 8048924:	c9                   	leave  
 8048925:	c3                   	ret    

08048926 <main>:
 8048926:	55                   	push   ebp
 8048927:	89 e5                	mov    ebp,esp
 8048929:	83 e4 f0             	and    esp,0xfffffff0
 804892c:	83 ec 20             	sub    esp,0x20
 804892f:	e8 b8 fc ff ff       	call   80485ec <load_file>
 8048934:	e8 04 fe ff ff       	call   804873d <get_lines>
 8048939:	89 44 24 1c          	mov    DWORD PTR [esp+0x1c],eax
 804893d:	8b 44 24 1c          	mov    eax,DWORD PTR [esp+0x1c]
 8048941:	89 04 24             	mov    DWORD PTR [esp],eax
 8048944:	e8 67 ff ff ff       	call   80488b0 <insertion_sort>
 8048949:	89 44 24 1c          	mov    DWORD PTR [esp+0x1c],eax
 804894d:	8b 44 24 1c          	mov    eax,DWORD PTR [esp+0x1c]
 8048951:	89 04 24             	mov    DWORD PTR [esp],eax
 8048954:	e8 cd fe ff ff       	call   8048826 <write_file>
 8048959:	b8 00 00 00 00       	mov    eax,0x0
 804895e:	c9                   	leave  
 804895f:	c3                   	ret    

08048960 <__libc_csu_init>:
 8048960:	55                   	push   ebp
 8048961:	57                   	push   edi
 8048962:	56                   	push   esi
 8048963:	53                   	push   ebx
 8048964:	e8 69 00 00 00       	call   80489d2 <__i686.get_pc_thunk.bx>
 8048969:	81 c3 97 16 00 00    	add    ebx,0x1697
 804896f:	83 ec 1c             	sub    esp,0x1c
 8048972:	8b 6c 24 30          	mov    ebp,DWORD PTR [esp+0x30]
 8048976:	8d bb 0c ff ff ff    	lea    edi,[ebx-0xf4]
 804897c:	e8 7b fa ff ff       	call   80483fc <_init>
 8048981:	8d 83 08 ff ff ff    	lea    eax,[ebx-0xf8]
 8048987:	29 c7                	sub    edi,eax
 8048989:	c1 ff 02             	sar    edi,0x2
 804898c:	85 ff                	test   edi,edi
 804898e:	74 29                	je     80489b9 <__libc_csu_init+0x59>
 8048990:	31 f6                	xor    esi,esi
 8048992:	8d b6 00 00 00 00    	lea    esi,[esi+0x0]
 8048998:	8b 44 24 38          	mov    eax,DWORD PTR [esp+0x38]
 804899c:	89 2c 24             	mov    DWORD PTR [esp],ebp
 804899f:	89 44 24 08          	mov    DWORD PTR [esp+0x8],eax
 80489a3:	8b 44 24 34          	mov    eax,DWORD PTR [esp+0x34]
 80489a7:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 80489ab:	ff 94 b3 08 ff ff ff 	call   DWORD PTR [ebx+esi*4-0xf8]
 80489b2:	83 c6 01             	add    esi,0x1
 80489b5:	39 fe                	cmp    esi,edi
 80489b7:	75 df                	jne    8048998 <__libc_csu_init+0x38>
 80489b9:	83 c4 1c             	add    esp,0x1c
 80489bc:	5b                   	pop    ebx
 80489bd:	5e                   	pop    esi
 80489be:	5f                   	pop    edi
 80489bf:	5d                   	pop    ebp
 80489c0:	c3                   	ret    
 80489c1:	eb 0d                	jmp    80489d0 <__libc_csu_fini>
 80489c3:	90                   	nop
 80489c4:	90                   	nop
 80489c5:	90                   	nop
 80489c6:	90                   	nop
 80489c7:	90                   	nop
 80489c8:	90                   	nop
 80489c9:	90                   	nop
 80489ca:	90                   	nop
 80489cb:	90                   	nop
 80489cc:	90                   	nop
 80489cd:	90                   	nop
 80489ce:	90                   	nop
 80489cf:	90                   	nop

080489d0 <__libc_csu_fini>:
 80489d0:	f3 c3                	repz ret 

080489d2 <__i686.get_pc_thunk.bx>:
 80489d2:	8b 1c 24             	mov    ebx,DWORD PTR [esp]
 80489d5:	c3                   	ret    
 80489d6:	66 90                	xchg   ax,ax
 80489d8:	66 90                	xchg   ax,ax
 80489da:	66 90                	xchg   ax,ax
 80489dc:	66 90                	xchg   ax,ax
 80489de:	66 90                	xchg   ax,ax

080489e0 <__fstat>:
 80489e0:	53                   	push   ebx
 80489e1:	83 ec 18             	sub    esp,0x18
 80489e4:	8b 44 24 24          	mov    eax,DWORD PTR [esp+0x24]
 80489e8:	e8 e5 ff ff ff       	call   80489d2 <__i686.get_pc_thunk.bx>
 80489ed:	81 c3 13 16 00 00    	add    ebx,0x1613
 80489f3:	c7 04 24 03 00 00 00 	mov    DWORD PTR [esp],0x3
 80489fa:	89 44 24 08          	mov    DWORD PTR [esp+0x8],eax
 80489fe:	8b 44 24 20          	mov    eax,DWORD PTR [esp+0x20]
 8048a02:	89 44 24 04          	mov    DWORD PTR [esp+0x4],eax
 8048a06:	e8 65 fa ff ff       	call   8048470 <__fxstat@plt>
 8048a0b:	83 c4 18             	add    esp,0x18
 8048a0e:	5b                   	pop    ebx
 8048a0f:	c3                   	ret    

Disassembly of section .fini:

08048a10 <_fini>:
 8048a10:	53                   	push   ebx
 8048a11:	83 ec 08             	sub    esp,0x8
 8048a14:	e8 00 00 00 00       	call   8048a19 <_fini+0x9>
 8048a19:	5b                   	pop    ebx
 8048a1a:	81 c3 e7 15 00 00    	add    ebx,0x15e7
 8048a20:	83 c4 08             	add    esp,0x8
 8048a23:	5b                   	pop    ebx
 8048a24:	c3                   	ret    
