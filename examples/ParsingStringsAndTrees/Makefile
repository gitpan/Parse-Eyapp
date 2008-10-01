input1.pir: Infix.pm I2PIR.pm input1.inf
	infix2pir.pl input1.inf > input1.pir
	cat input1.pir

Infix.pm: Infix.eyp 
	eyapp Infix.eyp

I2PIR.pm: I2PIR.trg
	treereg -m main I2PIR.trg

# You need parrot to run the generated .pir file
run1: input1.pir
	parrot input1.pir

clean: 
	rm -f I2PIR.pm Infix.pm input1.pir
