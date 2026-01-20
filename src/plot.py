import matplotlib.pyplot as plt

n = [65536, 1048576, 16777216]

baseline_multicore = [33727, 58624, 1007644]
bsz_multicore = [34230, 41318, 996086]
bsv_multicore = [48130, 79025, 1061984]

baseline_cuda = [830, 2806, 56172]
bsz_cuda = [927, 3572, 58982]
bsv_cuda = [1171, 4408, 76204]



plt.figure()
plt.plot(n, baseline_cuda, marker='o', label='baseline')
plt.plot(n, bsz_cuda, marker='o', label='bsz')
plt.plot(n, bsv_cuda, marker='o', label='bsv')
plt.yscale('log')
plt.xlabel('n')
plt.ylabel('Time (μs)')
plt.title('CUDA Performance')
plt.grid(True, which="both")
plt.legend()
plt.savefig("CUDA_performance.png")


plt.figure()
plt.plot(n, baseline_multicore, marker='o', label='baseline')
plt.plot(n, bsz_multicore, marker='o', label='bsz')
plt.plot(n, bsv_multicore, marker='o', label='bsv')
plt.yscale('log')
plt.xlabel('n')
plt.ylabel('Time (μs)')
plt.title('Multicore Performance')
plt.grid(True, which="both")
plt.legend()
plt.savefig("multicore_performance.png")