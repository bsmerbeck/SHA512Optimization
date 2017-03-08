.arch rhody_sha256		;use rhody.cfg
.outfmt hex			;output format is hex
.memsize 2048			;specify 2K words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Memory addresses for Rhody System I/O devices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.define kcntrl	0xf0000	;keyboard control register
.define kascii	0xf0001	;keyboard ASCII code
.define vcntrl	0xf0002	;video control register
.define time0	0xf0003	;Timer 0
.define time1	0xf0004	;Timer 1
.define audl	0xf0005	;Audio Data registers: left
.define audr	0xf0006	;Audio Data registers: right
.define inport	0xf0007	;GPIO read address
.define outport	0xf0007	;GPIO write address
.define rand	0xf0008	;LFSR random number 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Define part to be included in user's program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.define tx	0x0900	;text video X (0 - 79)
.define ty	0x0901	;text video Y (0 - 59)
.define taddr	0x0902	;text video address
.define tascii	0x0903	;text video ASCII code
.define cursor	0x0904	;ASCII for text cursor
.define prompt	0x0905	;prompt length for BS left limit
.define tnum	0x0906	;text video number to be printed
.define format	0x0907	;number output format
.define gx	0x0908	;graphic video X (0 - 639)
.define gy	0x0909	;graphic video Y (0 - 479)
.define gaddr	0x090A	;graphic video address
.define color	0x090B	;color for graphic
.define x1	0x090C	;x1 for line/circle
.define y1	0x090D	;y1
.define x2	0x090E	;x2 for line
.define y2	0x090F	;y2
.define rad	0x0910	;radius for circle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SHA256 Program variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.define m512	0x0810	;16 words M 0x810-0x81F
.define buffer	0x0820	;64 words W 0x820-0x85F
.define wva	0x0861	;working variable a
.define wvb	0x0862	;working variable b
.define wvc	0x0863	;working variable c
.define wvd	0x0864	;working variable d
.define wve	0x0865	;working variable e
.define wvf	0x0866	;working variable f
.define wvg	0x0867	;working variable g
.define wvh	0x0868	;working variable h
.define h0	0x0869	;hash value 0
.define h1	0x086A	;hash value 1
.define h2	0x086B	;hash value 2
.define h3	0x086C	;hash value 3
.define h4	0x086D	;hash value 4
.define h5	0x086E	;hash value 5
.define h6	0x086F	;hash value 6
.define h7	0x0870	;hash value 7
.define t1	0x0871	;temporary value 1
.define t2	0x0872	;temporary value 2
.define count	0x0874	;hash count
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SHA256_time_trial
;Use randomly generated message for SHA-256
;count how many hashes per seconds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SHA256_time_trial:
	ldi	r0, 0
	stm	format, r0
	sys	clear
	ldi	r0, m512
	ldi	r1, test_message
	ldi	r2, 16
test1:	
	ldr	r3, r1		;copy test message to buffer
	str	r0, r3
	adi	r0, 1
	adi	r1, 1
	adi	r2, 0xffff
	jnz	test1
	call	HASH		;produce hash for the test message
	ldi	r0, h0
	ldi	r1, answer
	ldi	r2, 8
test2:	
	ldr	r3, r0
	ldr	r4, r1
	cmp	r3, r4
	jnz	wrong		;subroutine HASH is wrong
	adi	r0, 1
	adi	r1, 1
	adi	r2, 0xffff
	jnz	test2
;prepare hash count and timer
	ldi	r0, 0
	stm	count, r0
	ldi	r0, 1000	;1000 ms
	stm	time0, r0	;turn on 1-second timer
;Prepare the random inputs;;;;;;;;;;;;;;;;;;;;;;;;
sha1:	ldi	r5, m512	;pointer to message buffer
	ldi	r2, 8
sha2:	ldm	r0, rand	;get random number
	str	r5, r0	
	adi	r5, 1
	adi	r2, 0xFFFF
	jnz	sha2
	ldh	r0, 0x8000
	ldl	r0, 0x0000
	str	r5, r0		;stop byte in 9th word
	ldi	r0, 0
	adi	r5, 1
	ldi	r2, 6		;fill in '0'
sha3:	str	r5, r0	
	adi	r5, 1
	adi	r2, 0xFFFF
	jnz	sha3
	ldi	r0, 0x0100	;length=256
	str	r5, r0
	call	HASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldm	r0, count
	adi	r0, 1
	stm	count, r0	;inc hash count
	ldm	r0, time0
	cmpi	r0, 1		;time up?
	jnz	sha1
	ldi	r0, 0
	stm	tx, r0
	stm	ty, r0
	ldm	r0, count
	stm	tnum, r0
	sys	printn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
halt:	jmp	halt		;print count and stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HASH subroutine produced wrong hash
;print "dead" on 7-segment displays and stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
wrong:
	ldi	r0, 0xdead
	stm	outport, r0
wrong_stop:
	jmp	wrong_stop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pre-defined test message: this is a test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
test_message:
	word	0x74686973	;this
	word	0x20697320	; is 
	word	0x61207465	;a te
	word	0x73748000	;st
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000000
	word	0x00000070	;message lenth in bits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Hash for pre-define test message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
answer:
	word	0x2e997585
	word	0x48972a8e
	word	0x8822ad47
	word	0xfa1017ff
	word	0x72f06f3f
	word	0xf6a01685
	word	0x1f45c398
	word	0x732bc50c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
K256:	word	0x428a2f98	;k0	K Constant
	word	0x71374491	;k1
	word	0xb5c0fbcf	;k2
	word	0xe9b5dba5	;k3
	word	0x3956c25b	;k4
	word	0x59f111f1	;k5
	word	0x923f82a4	;k6
	word	0xab1c5ed5	;k7
	word	0xd807aa98	;k8
	word	0x12835b01	;k9
	word	0x243185be	;k10
	word	0x550c7dc3	;k11
	word	0x72be5d74	;k12
	word	0x80deb1fe	;k13
	word	0x9bdc06a7	;k14
	word	0xc19bf174	;k15
	word	0xe49b69c1	;k16
	word	0xefbe4786	;k17
	word	0x0fc19dc6	;k18
	word	0x240ca1cc	;k19
	word	0x2de92c6f	;k20
	word	0x4a7484aa	;k21
	word	0x5cb0a9dc	;k22
	word	0x76f988da	;k23
	word	0x983e5152	;k24
	word	0xa831c66d	;k25
	word	0xb00327c8	;k26
	word	0xbf597fc7	;k27
	word	0xc6e00bf3	;k28
	word	0xd5a79147	;k29
	word	0x06ca6351	;k30
	word	0x14292967	;k31
	word	0x27b70a85	;k32
	word	0x2e1b2138	;k33
	word	0x4d2c6dfc	;k34
	word	0x53380d13	;k35
	word	0x650a7354	;k36
	word	0x766a0abb	;k37
	word	0x81c2c92e	;k38
	word	0x92722c85	;k39
	word	0xa2bfe8a1	;k40
	word	0xa81a664b	;k41
	word	0xc24b8b70	;k42
	word	0xc76c51a3	;k43
	word	0xd192e819	;k44
	word	0xd6990624	;k45
	word	0xf40e3585	;k46
	word	0x106aa070	;k47
	word	0x19a4c116	;k48
	word	0x1e376c08	;k49
	word	0x2748774c	;k50
	word	0x34b0bcb5	;k51
	word	0x391c0cb3	;k52
	word	0x4ed8aa4a	;k53
	word	0x5b9cca4f	;k54
	word	0x682e6ff3	;k55
	word	0x748f82ee	;k56
	word	0x78a5636f	;k57
	word	0x84c87814	;k58
	word	0x8cc70208	;k59
	word	0x90befffa	;k60
	word	0xa4506ceb	;k61
	word	0xbef9a3f7	;k62
	word	0xc67178f2	;k63
	byte	0x00
Hinit:
	word	0x6a09e667	;H0	initial Hash
	word	0xbb67ae85	;H1
	word	0x3c6ef372	;H2
	word	0xa54ff53a	;H3
	word	0x510e527f	;H4
	word	0x9b05688c	;H5
	word	0x1f83d9ab	;H6
	word	0x5be0cd19	;H7
	byte	0x00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;HASH subroutine to generate the 256-bit message digest from 
;M = m512 is the 16-words input buffer
;W = buffer is the 64-word message schedule 
;h0 to h7 = hash values
;wva, wvb, ..., wvh = working variables a to h
;K = K256 the 64-words constant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HASH:	ldi	r5, m512	;copy M to W(0..15)
	ldi	r7, buffer
	ldi	r2, 16
sch1:	ldr	r0, r5
	str	r7, r0		
	adi	r5, 1
	adi	r7, 1
	adi	r2, 0xFFFF
	jnz	sch1
;Process the next 48 entrances
;R7 is currently the pointer to buffer (W)
	ldi	r2, 48		;loop count
sch2:	mov	r1, r7
	adi	r1, 0xFFFE	;t-2
	ldr	r0, r1		;r0=W(t-2)
	call	sig1		;r0=sig1(W(t-2))
	mov	r1, r7
	adi	r1, 0xFFF9	;t-7
	ldr	r3, r1		;r3=W(t-7)
	add	r3, r0		;r3=sig1(W(t-2))+W(t-7)
	mov	r1, r7
	adi	r1, 0xFFF1	;t-15
	ldr	r0, r1		;r0=W(t-15)
	call	sig0		;r0=sig0(W(t-15))
	mov	r1, r7
	adi	r1, 0xFFF0	;t-16
	ldr	r4, r1		;r4=W(t-16)
	add	r4, r0		;r4=sig1(W(t-15))+W(t-16)
	add	r3, r4		
;;;;;;;;;r3=sig1(W(t-2))+W(t-7)+sig1(W(t-15))+W(t-16)
	str	r7, r3
	adi	r7, 1
	adi	r2, 0xFFFF
	jnz	sch2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;At this point, we have prepared the 64 32-bit 
;message schedule: W
;Use R3 as the loop count since R2 will be use by functions
;initialize the working variables with initial H
;The working variables are defined in sequence and 
;work like a table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldi	r5, Hinit	;r5=pointer to init H
	ldi	r6, wva		;r6=pointer to working variables
	ldi	r7, h0		;r7=pointer to hash values
	ldi	r3, 8		;loop count
sch3:	ldr	r0, r5
	str	r6, r0
	str	r7, r0		
	adi	r5, 1
	adi	r6, 1
	adi	r7, 1
	adi	r3, 0xFFFF
	jnz	sch3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Begin the big loop of 0 to 63
;From this point on:
;	R6 is reserved as pointer to K
;	R7 is reserved as pointer to W
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldi	r3, 64		;the loop count
	ldi	r7, buffer	;r7=pointer to W
	ldi	r6, k256	;r6=pointer to K
sch4:	ldm	r0, wve		;r0=e
	call	sum1		;r0=sum1(e)
	ldm	r4, wvh		;r4=h
	add	r4, r0		;r4=h+sum1(e)
	ldm	r0, wve		;r0=e
	ldm	r1, wvf		;r1=f
	ldm	r2, wvg		;r2=g
	call	ch		;r0=Ch(e,f,g)
	add	r4, r0		;r4=h+sum1(e)+Ch(e,f,g)
	ldr	r0, r6
	add	r4, r0		;r4=h+sum1(e)+Ch(e,f,g)+K
	ldr	r0, r7
	add	r4, r0		;r4=h+sum1(e)+Ch(e,f,g)+K+W
	stm	t1, r4		;T1=h+sum1(e)+Ch(e,f,g)+K+W
	ldm	r0, wva		;r0=a
	call	sum0		
	mov	r4, r0		;r4=sum0(a)
	ldm	r0, wva		;r0=a
	ldm	r1, wvb		;r1=b
	ldm	r2, wvc		;r2=c
	call	maj		;r0=Maj(a,b,c)
	add	r4, r0		;r4=sum0(a)+Maj(a,b,c)
	stm	t2, r4		;T2=sum0(a)+Maj(a,b,c)
	ldm	r0, wvg
	stm	wvh, r0		;h <= g
	ldm	r0, wvf
	stm	wvg, r0		;g <= f
	ldm	r0, wve
	stm	wvf, r0		;f <= e
	ldm	r0, wvd
	ldm	r1, t1
	add	r0, r1
	stm	wve, r0		;e <= d + T1
	ldm	r0, wvc
	stm	wvd, r0		;d <= c
	ldm	r0, wvb
	stm	wvc, r0		;c <= b
	ldm	r0, wva
	stm	wvb, r0		;b <= a
	ldm	r0, t1
	ldm	r1, t2
	add	r0, r1
	stm	wva, r0		;a <= T1 + T2
	adi	r6, 1		;inc pointer to K
	adi	r7, 1		;inc pointer to W
	adi	r3, 0xFFFF
	jnz	sch4
;calculate the hash values
	ldi	r1, wva		;r1=pointer to working variables
	ldi	r2, h0		;r2=pointer to hash values
	ldi	r5, 8		;loop count
sch5:	ldr	r0, r1
	ldr	r4, r2
	add	r0, r4
	str	r2, r0		
	adi	r1, 1
	adi	r2, 1
	adi	r5, 0xFFFF
	jnz	sch5
	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function Ch(x, y, z)
;input: r0=x, r1=y, r2=z
;output: r0=Ch(x, y, z)
;affects: r5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ch:	push	r6
	mov	r5, r0
	and	r0, r1		;r0=x and y
	ldi	r6, 0xFFFF
	xor	r5, r6		;r5=not x
	and	r5, r2		;r5=not x and z
	xor	r0, r5
	pop	r6
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function Maj(x, y, z)
;input: r0=x, r1=y, r2=z
;output: r0=Maj(x, y, z)
;affects: r5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
maj:	mov	r5, r0
	and	r0, r1		;r0=x and y
	and	r5, r2		;r5=x and z
	and	r1, r2		;r1=y and z
	xor	r0, r5	
	xor	r0, r1
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function sum0(x)
;input: r0=x
;output: r0=sum0(x)
;affects: r5, r1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sum0:	mov	r5, r0
	mov	r1, r0
	ror	r0, 2		;r0=ROTR^2(x)
	ror	r5, 13		;r5=ROTR^13(x)
	ror	r1, 22		;r1=ROTR^22(x)
	xor	r0, r5
	xor	r0, r1
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function sum1(x)
;input: r0=x
;output: r0=sum1(x)
;affects: r5, r1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sum1:	mov	r5, r0
	mov	r1, r0
	ror	r0, 6		;r0=ROTR^6(x)
	ror	r5, 11		;r5=ROTR^11(x)
	ror	r1, 25		;r6=ROTR^25(x)
	xor	r0, r5
	xor	r0, r1
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function sig0(x)
;input: r0=x
;output: r0=sig0(x)
;affects: r4, r5, r6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sig0:	mov	r5, r0
	mov	r6, r0
	ror	r0, 7		;r0=ROTR^7(x)
	ror	r5, 18		;r5=ROTR^18(x)
	ror	r6, 3		;r6=ROTR^3(x)
	ldh	r4, 0x1FFF	
	ldl	r4, 0xFFFF	;mask out 3 MSB
	and	r6, r4		;r6=SHR^3(x)
	xor	r0, r5
	xor	r0, r6
	ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Function sig1(x)
;input: r0=x
;output: r0=sig1(x)
;affects: r4, r5, r6
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sig1:	mov	r5, r0
	mov	r6, r0
	ror	r0, 17		;r0=ROTR^17(x)
	ror	r5, 19		;r5=ROTR^19(x)
	ror	r6, 10		;r6=ROTR^10(x)
	ldh	r4, 0x003F	
	ldl	r4, 0xFFFF	;mask out 10 MSB
	and	r6, r4		;r6=SHR^10(x)
	xor	r0, r5
	xor	r0, r6
	ret


