TEST = futhark test --backend=multicore
BENCH_MULTICORE = futhark bench --backend=multicore
BENCH_CUDA = futhark bench --backend=cuda

test: test_bsz_mintree_gpu test_bsz_mintree_cpu test_bsz_seq

bench_multicore:
	$(BENCH_MULTICORE) bench_cpu.fut

bench_cuda:
	$(BENCH_CUDA) bench_gpu.fut

test_bsz_mintree_gpu:
	$(TEST) bsz_mintree_gpu.fut

test_bsz_mintree_cpu:
	$(TEST) bsz_mintree_cpu.fut

test_bsz_seq:
	$(TEST) bsz_seq.fut

clean:
	rm -rf data bsz_seq bsz_mintree_cpu bsz_mintree_gpu bench_cpu bench_gpu *.c *.json