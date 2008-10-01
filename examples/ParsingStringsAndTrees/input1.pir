.sub 'main' :main
	.local num a, b, c, d
	b = 5
	a = b + 2
	
	a = 0
	print "a = "
	print a
	print "\n"
	
	a = a + 1
	
	$N5 = a * 4
	d = $N5 - b
	
	$N7 = a * b
	c = $N7 + d
	
	print "c = "
	print c
	print "\n"
	
	print "d = "
	print d
	print "\n"

.end
