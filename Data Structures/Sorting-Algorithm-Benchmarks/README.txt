Sorting Algorithm Benchmarks
=============================

Implementation and empirical comparison of five sorting algorithms in Java,
with performance graphs generated using JFreeChart.

Algorithms implemented:
- Bubble Sort    — O(n²) comparison-based sort
- Merge Sort     — O(n log n) divide-and-conquer sort
- Quick Sort     — O(n log n) average; rightmost pivot strategy
- Counting Sort  — O(n + k) integer sort
- Quick Select   — O(n) average; finds the k-th smallest element

Benchmarking:
- Runs each algorithm against random and pre-sorted arrays of varying sizes
- Measures wall-clock time and plots results using the bundled Plotter/JFreeChart
  library to produce timing comparison graphs

Files:
- Sorting.java              — all algorithm implementations and benchmarking logic
- HW5 Instructions.pdf      — exercise specification
- jfreechart-1.0.17.jar     — charting library
- jcommon-1.0.21.jar        — JFreeChart dependency
- plotter.jar               — plotting helper

Language: Java
