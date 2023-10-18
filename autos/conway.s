	.meta source "\"autos/conway.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 24, 2
	add r23, r24, r25
	invoke 5, 25, 1
	add r22, r23, r26
	invoke 5, 26, 8
	add r21, r22, r27
	invoke 5, 27, 7
	add r20, r21, r28
	invoke 5, 28, 6
	add r19, r20, r29
	invoke 5, 29, 5
	add r18, r19, r30
	invoke 5, 30, 4
	add r17, r18, r31
	invoke 5, 31, 3
	set r17, r5
L2:
	invoke 5, 6, 0
	seti r7, #1
	goto_eq L6, r7, r2
	goto L3
L5:
	set r8, r5
	seti r9, #2
	goto_lt L8, r9, r5
	goto L6
	seti r10, #0
	invoke 4, 10, 0
	goto L7
L6:
L8:
	set r11, r5
	seti r12, #3
	goto_gt L11, r12, r8
	goto L9
	seti r13, #0
	invoke 4, 13, 0
	goto L10
L9:
L10:
L7:
	goto L4
L3:
L11:
	set r14, r5
	seti r15, #3
	goto_eq L14, r15, r11
	goto L12
	seti r16, #1
	invoke 4, 16, 0
	goto L13
L12:
L13:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
