TEST = futhark test --backend=multicore
BENCH_MULTICORE = futhark bench --backend=multicore
BENCH_CUDA = futhark bench --backend=cuda

test: test_bsv test_bsz

bench_all: bench_gpu bench_cpu

bench_cpu:
	$(BENCH_MULTICORE) --json multicore_benchmarks.json \
					   bench.fut

bench_gpu:
	$(BENCH_CUDA) --json cuda_benchmarks.json \
				  bench.fut

test_bsv:
	$(TEST) bsv.fut

test_bsz:
	$(TEST) bsz.fut

plot:
	python3 plot_benchmarks.py cuda_benchmarks.json \
			--program bench.fut:bench_bsz \
			--program bench.fut:bench_bsv \
			--baseline bench.fut:bench_mintree_strict_previous \
			--outdir plots \
			--fmt png \
			--y-unit ms \
			--logx

clean:
	rm -rf data plots bsz bsv bench *.c *.json
