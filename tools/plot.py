# Psyche output plotting script
# Fall 2013, Clément Pit--Claudel & Zelda Mariet
#
# plot.py filters it's input, looking for lines formatted as
#   HEADER<tab>label<tab>x<tab>y
# and plots each different label as a separate curve. Such input can be
# obtained by using Psyche's -plot parameter.
#
# Usage: python3 plot.py [input] [output]
#   input: a file name, or "-" to indicate standard input (default)
#   output: name of the file to which to save the plot. Omit to display
#           an interactive plot.
#
# Example: ./main.native -plot -restarts constant -rsettings 7 0 -gplugin dpll_wl problems/DIMACS/SatlibBench/dubois20.cnf | python3 tools/plot.py
#
# Customization:
#   Set INDEX_BY_EVENT_COUNT to True to index each y value by it's order
#   of appearance in the input stream instead of it's x value (useful
#   because Psyche's timers do not provide sufficient resolution to
#   distinguish all events.
#
#   Add a header to EXCLUDE to filter it out from the display
#
#   Add a header to BARS to draw corresponding samples as vertical lines
#
#   Add a header to LINES to plot the corresponding line as a continuous
#   curve. Only effective if INDEX_BY_EVENT_COUNT is True

import sys
import matplotlib.pyplot as pyplot

INDEX_BY_EVENT_COUNT = False

HEADER = ">> "
BARS = set(("10: Plugin-specific (restarts)",))
LINES = set(("06: Open branches",)) if INDEX_BY_EVENT_COUNT else set()
EXCLUDE = set(HEADER + label for label in ("00: Local successes", "01: Local failures", "kernel2", "kernel3", "04: Focuses", "05: Cuts", "kernel7", "kernel8", "09: Kernel operations", "10: Calls to plugin"))

COLORS = ["#edd400", "#f57900", "#c17d11", "#73d216", "#3465a4", "#75507b", "#cc0000", "#555753"] + ["#fce94f", "#fcaf3e", "#e9b96e", "#8ae234", "#729fcf", "#ad7fa8", "#ef2929", "#888a85"] + ["#c4a000", "#ce5c00", "#8f5902", "#4e9a06", "#204a87", "#5c3566", "#a40000", "#babdb6"] # Tango colors

path = sys.argv[1] if len(sys.argv) > 1 else "-"
is_stdin = path == "-"

handle = sys.stdin if is_stdin else open(path, mode="r", encoding="utf-8")

plots = {}
count = 0

for line in handle:
	if not line.startswith(HEADER):
		if is_stdin:
			sys.stdout.write(line)
		continue

	count += 1
	header, x, y = line.strip().split("\t")
	plots.setdefault(header, []).append((count if INDEX_BY_EVENT_COUNT else float(x), float(y)))

if not is_stdin:
	handle.close()

if count == 0:
	print("=== plot.py: No input! Did you forget the -plot option?")
	sys.exit(0)
	
fig = pyplot.figure()
axes = fig.add_axes([0.1, 0.1, 0.8, 0.8])
plots = { label[len(HEADER):]: tuple(zip(*points)) for label, points in plots.items() if label not in EXCLUDE }

for color, (label, (x, y)) in zip(COLORS, sorted(plots.items())):
	if label in BARS:
		extra = {"label": label}
		for x_coord in x:
			axes.axvline(x = x_coord, color = color, **extra)
			extra = {}
	else:
		axes.plot(x, y, label = label, marker = '.', linestyle = ' ' if label not in LINES else '-', markersize = 2, color = color)

#axes.set_xlim([0,6])
axes.set_yscale('log', basey = 10)
axes.legend(loc = "upper center", numpoints = 1, markerscale = 8, fontsize = 10, ncol = 2, fancybox = True).draggable()

if len(sys.argv) > 2:
	fig.set_size_inches(8, 5)
	pyplot.savefig(sys.argv[2], bbox_inches='tight')
else:
	pyplot.show()

