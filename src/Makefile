TEST = futhark test --backend=multicore
BENCH_MULTICORE = futhark bench --backend=multicore
BENCH_CUDA = futhark bench --backend=cuda

test: test_bsz_gpu test_bsz_cpu test_bsz_seq test_bsv_seq test_bsv

bench_multicore:
	$(BENCH_MULTICORE) bench_cpu.fut

bench_cuda:
	$(BENCH_CUDA) bench_gpu.fut



test_bsv_seq:
	$(TEST) bsv_seq.fut

test_bsv:
	$(TEST) bsv.fut

test_bsz_mintree_gpu:
	$(TEST) bsz_gpu.fut

test_bsz_mintree_cpu:
	$(TEST) bsz_cpu.fut

test_bsz_seq:
	$(TEST) bsz_seq.fut

clean:
	rm -rf data bsz_cpu_copy bsz_cpu_fast bsz_seq bsz_cpu bsz_gpu bench_cpu bench_gpu bsv_seq bsv *.c *.json