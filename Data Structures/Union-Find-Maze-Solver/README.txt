Union-Find Maze Solver
======================

A maze solver that treats the maze as an image and uses a Union-Find
(Disjoint Set Union) data structure to determine solvability.

How it works:
1. The maze image is loaded pixel-by-pixel.
2. Adjacent passable pixels are unioned into connected components using
   UnionFind.java.
3. The maze is solved if the start pixel and end pixel share the same
   connected component (i.e., find(start) == find(end)).
4. Each connected component is assigned a random colour and the result
   is rendered as an image using DisplayImage.java.

Files:
- UnionFind.java    — Union-Find ADT with union-by-rank and path compression
- Maze.java         — image loading, pixel decomposition, and solvability check
- DisplayImage.java — renders the coloured component visualization

Language: Java
