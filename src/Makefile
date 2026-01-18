TEST = futhark test --backend=multicore
BENCH_MULTICORE = futhark bench --backend=multicore
BENCH_CUDA = futhark bench --backend=cuda

test: test_bsv test_bsz

bench_all: bench_gpu bench_cpu

bench_cpu:
	$(BENCH_MULTICORE) bench.fut

bench_gpu:
	$(BENCH_CUDA) bench.fut


test_bsv:
	$(TEST) bsv.fut

test_bsz:
	$(TEST) bsz.fut

clean:
	rm -rf data bsz bsv bench *.c *.json
