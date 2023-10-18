	.meta source "\"autos/if1.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 11, 6
	set r11, r5
	invoke 5, 10, 2
	set r10, r6
L2:
	set r7, r5
	set r8, r6
	goto_eq L7, r8, r2
	goto L3
	seti r9, #1
	invoke 4, 9, 0
	goto L4
L3:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
