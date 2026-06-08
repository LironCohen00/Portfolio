Priority Task Queue
===================

A dual-mode task allocation system implemented in Java, backed by both a
max-heap and a FIFO queue so tasks can be retrieved either by priority or
by insertion order.

Design:
- Task.java        — task entity with a priority value and creation timestamp
- TaskElement.java — wrapper that tracks a task's current position in the heap
- TaskHeap.java    — max-heap supporting O(log n) insert, extract-max, and
                     position-aware updates (percolate up/down)
- TaskQueue.java   — FIFO queue maintaining insertion order
- TaskAllocation.java — coordinates the heap and queue; supports:
                        • removeMax()  — dequeue highest-priority task
                        • removeFirst() — dequeue oldest task

Key concepts: heap data structure, percolation, dual-structure synchronization,
priority scheduling.

Language: Java
