Multi-Client Chat Server
========================

A Java socket-based chat server supporting multiple simultaneous clients,
with message broadcasting to all connected participants.

Implementation:
- Client.java — handles a single client connection; receives messages and
  broadcasts them to every other connected client
- Main.java   — server entry point; sets up the ServerSocket and accepts
  incoming client connections

Key concepts: TCP sockets, multi-threading, concurrent client handling,
message broadcasting.

Language: Java
