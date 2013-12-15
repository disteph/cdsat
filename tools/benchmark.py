# Psyche benchmarking tool
# Fall 2013, Clément Pit--Claudel & Zelda Mariet
#
# benchmark.py runs Psyche on a set of input files, using various 
# parameter combinations, and compares the performance of each run.
#
# Usage: python3 benchmark.py path_to_binary path_to_problems [path_to_problems_2 [...]]
#   path_to_binary:   Path to Psyche's binary
#   path_to_problems: Path to problems set. Can also be a list of files.
#                     Files are sorted in order of increasing size before
#                     being processed.
#
# Example: python3 tools/benchmark.py ./main.native problems/DIMACS/SatlibBench
#
# Customization:
#   The baseline to which other runs are compared is specified in the
#   BASELINE_COMMAND variable. Other commands are specified in the
#   BENCHMARKED_VARIANTS command, along with a header. For example,
#     BASELINE_COMMAND = binary + " -gplugin dpll_wl"
#     BENCHMARKED_VARIANTS = [
#       ("restarts ", BASELINE_COMMAND + " -restarts constant   -rsettings 7 0"),
#       ("restarts+", BASELINE_COMMAND + " -restarts arithmetic -rsettings 7 2")
#     ]
#   runs three instances of Psyche on every problem set.
#
# Output: Each file occupies one line. When a speedup (or slowdown) is
# deemed significant, a speedup value is displayed. If the highest
# speedup (or lowest slowdown) is significant, a little frowning face is
# shown.
#

import math
import operator
import os
import resource
import shlex
import subprocess
import sys

binary = sys.argv[1]
TIMEOUT = 20 # Timeout in seconds after which Psyche is killed

BASELINE_COMMAND = binary + " -gplugin dpll_wl"
BENCHMARKED_VARIANTS = [
	("restarts ", BASELINE_COMMAND + " -restarts constant   -rsettings 7 0"),
	("restarts+", BASELINE_COMMAND + " -restarts arithmetic -rsettings 7 2")
]

test_files = []

def is_cnf(file_path):
	return file_path.endswith(".cnf")

for path in sys.argv[2:]:	
	if os.path.isfile(path):
		if is_cnf(path):
			test_files.append(path)
			
	elif os.path.isdir(path):
		for file_name in os.listdir(path):
			file_path = os.path.join(path, file_name)
			if is_cnf(file_path):
				test_files.append(file_path)

with_file_size = map(lambda path: (os.path.getsize(path), path), test_files)
sorted_test_files = list(map(operator.itemgetter(1), sorted(with_file_size)))

print("Processing", len(sorted_test_files), "files")

DEVNULL = open(os.devnull, 'wb')
INFINITY = float("inf")

def time_exec(command, file_path, timeout):
	try:
		start = resource.getrusage(resource.RUSAGE_CHILDREN)
		ret = subprocess.call(shlex.split(command) + [file_path], stdout = DEVNULL, timeout = timeout)
		end = resource.getrusage(resource.RUSAGE_CHILDREN)
		return end.ru_utime - start.ru_utime
	except subprocess.TimeoutExpired:
		return INFINITY

def format_timeout(duration, fastest):
	return ("timeout" if duration == INFINITY else ("%.2f" % duration)) + ("*" if abs(duration - fastest) < 0.005 else "")

def ratio(baseline, variant):
	SIGNIFICANCE_THRESHOLD = 0.2
	
	ratio = baseline / variant if baseline != 0 else float("nan")
	significant = (baseline > SIGNIFICANCE_THRESHOLD or variant > SIGNIFICANCE_THRESHOLD) and not math.isnan(ratio) and abs(ratio - 1) > SIGNIFICANCE_THRESHOLD

	return ratio if significant else False

def format_speedup(baseline, variant):
	r = ratio(baseline, variant)
	return "x%.2f" % r if r != False else ""

spacing = 2
min_column_width = 5
DELTA_HEADER = "delta"

def column_width(header):
	return max(len(header) + spacing, min_column_width)

max_length = max(map(len, sorted_test_files)) + spacing
format_string = '{:<5}{:<' + str(max_length) + '}{:' + str(column_width("baseline")) + '}'

headers = ["", "file path", "baseline"]
for header, command in BENCHMARKED_VARIANTS:
	headers.append(header)
	headers.append(DELTA_HEADER)
	format_string += '{:<' + str(column_width(header)) + '}{:<' + str(column_width(DELTA_HEADER)) + '}'

def print_line(file_path, baseline, variants):
	smile_threshold = 1.2
	fastest_variant = min(variants)
	fastest = min(baseline, fastest_variant)
	best_ratio = ratio(baseline, fastest_variant)

	smile = "" if best_ratio == False else ":)" if best_ratio > smile_threshold else ":(" if smile_threshold * best_ratio < 1 else ""
	fields = [smile, file_path, format_timeout(baseline, fastest)]
	
	for variant in variants:
		fields.append(format_timeout(variant, fastest))
		fields.append(format_speedup(baseline, variant))

	print(format_string.format(*fields))

baseline_acc = 0
variants_acc = [0] * len(BENCHMARKED_VARIANTS)

def no_infinity(x, replacement):
	return x if x != INFINITY else replacement

try:
	print(format_string.format(*headers))
	for file_path in sorted_test_files:
		baseline = time_exec(BASELINE_COMMAND, file_path, TIMEOUT)
		
		variants = []
		for header, variant_command in BENCHMARKED_VARIANTS:
			variants.append(time_exec(variant_command, file_path, TIMEOUT))

		print_line(file_path, baseline, variants)

		baseline_acc = baseline_acc + no_infinity(baseline, TIMEOUT)
		variants_acc = [acc + no_infinity(new, TIMEOUT) for acc, new in zip(variants_acc, variants)]
except KeyboardInterrupt:
	pass

print("=== summary ===")
print(format_string.format(*headers))
print_line("", baseline_acc, variants_acc)
