# N.B. need to link p1 e3 and p4 into src_ext before compiling
all:
	cd build && $(MAKE) 

test:
	cd build && $(MAKE) test

clean:
	cd build && $(MAKE) clean
