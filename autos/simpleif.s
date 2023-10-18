	.meta source "\"autos/simpleif.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 18, 6
	set r18, r5
	invoke 5, 17, 2
	set r17, r6
L8:
	set r14, r5
	set r15, r6
	goto_eq L14, r15, r8
	goto L9
	seti r16, #1
	invoke 4, 16, 0
	goto L10
L9:
L10:
L2:
	set r8, r5
	add r7, r8, r9
	set r9, r6
	seti r10, #2
	goto_ne L7, r10, r2
	goto L3
L5:
	invoke 5, 11, 3
	invoke 5, 12, 5
	goto_lt L11, r12, r5
	goto L6
	seti r13, #2
	invoke 4, 13, 0
	goto L7
L6:
L7:
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
