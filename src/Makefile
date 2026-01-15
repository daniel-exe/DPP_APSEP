FUTHARK = futhark test --backend=opencl

test: bsz_mintree_gpu bsz_mintree_cpu bsz_seq

bsz_mintree_gpu:
	$(FUTHARK) bsz_mintree_gpu.fut

bsz_mintree_cpu:
	$(FUTHARK) bsz_mintree_cpu.fut

bsz_seq:
	$(FUTHARK) bsz_seq.fut

clean:
	rm -rf bsz_seq bsz_mintree_cpu bsz_mintree_gpu *.c