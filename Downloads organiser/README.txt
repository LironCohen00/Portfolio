Downloads Organiser
===================

A Java utility that automatically sorts files in a Downloads folder into
categorized subdirectories based on file type.

How it works:
- Reads a YAML configuration file (categories.yaml) that maps MIME types
  and file extensions to category names (e.g. images, videos, documents,
  archives, code, binaries)
- Scans the Downloads folder and moves each file into the matching
  category subdirectory
- Falls back to extension-based matching if MIME type detection is inconclusive

Files:
- Main.java          — entry point, orchestrates the organising logic
- CategoryLoader.java — parses categories.yaml into a lookup map
- categories.yaml    — configurable mapping of MIME types and extensions to categories
- DownloadsOrganiser.jar — compiled executable
- RunScript.bat      — Windows batch script to run the application

Usage (Windows):
  Double-click RunScript.bat, or run:
  java -jar DownloadsOrganiser.jar

Language: Java
Dependencies: SnakeYAML (snakeyaml-2.3.jar)
