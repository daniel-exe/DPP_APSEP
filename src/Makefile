TEST = futhark test --backend=multicore
BENCH_MULTICORE = futhark bench --backend=multicore
BENCH_CUDA = futhark bench --backend=cuda

test: test_bsv test_bsz test_bsz_fast

bench_all: bench_cpu bench_gpu

bench_cpu:
	$(BENCH_MULTICORE) bench.fut

bench_gpu:
	$(BENCH_CUDA) bench.fut


test_bsv:
	$(TEST) bsv.fut

test_bsz:
	$(TEST) bsz.fut

test_bsz_fast:
	$(TEST) bsz_fast.fut

clean:
	rm -rf data bsz bsz_fast bsv bench *.c *.json
