import matplotlib.pyplot as plt

n = [65536, 1048576, 16777216]

baseline_multicore = [33727, 58624, 1007644]
bsz_multicore = [34230, 41318, 996086]
bsv_multicore = [48130, 79025, 1061984]

baseline_cuda = [830, 2806, 56172]
bsz_cuda = [927, 3572, 58982]
bsv_cuda = [1171, 4408, 76204]


plt.figure()
plt.plot(n, baseline_cuda, marker='o', label='baseline (CUDA)')
plt.plot(n, bsz_cuda, marker='o', label='bsz (CUDA)')
plt.plot(n, bsv_cuda, marker='o', label='bsv (CUDA)')
plt.plot(n, baseline_multicore, marker='o', label='baseline (multicore)')
plt.plot(n, bsz_multicore, marker='o', label='bsz (multicore)')
plt.plot(n, bsv_multicore, marker='o', label='bsv (multicore)')
plt.xscale('log')
plt.yscale('log')
plt.xlabel('n')
plt.ylabel('Time (μs)')
plt.title('Performance')
plt.grid(True, which="both", alpha=0.5)
plt.legend()
plt.savefig("performance.png")