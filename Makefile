run-examples:
	cd examples; elm reactor

build-examples:
	cd examples; elm make LineChartExample.elm BarChartExample.elm ScatterChartExample.elm
