FUTHARK = futhark test --backend=c

test: bsz bsz2

bsz:
	$(FUTHARK) BSZ.fut

bsz2:
	$(FUTHARK) BSZ2.fut

clean:
	rm -rf BSZ BSZ2 *.c