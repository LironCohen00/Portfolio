BlockShooter Game
==================

An interactive game implemented entirely in Jack, compiled by the Jack compiler and run on the
Hack VM / hardware platform.

Gameplay:
- Six blocks are arranged in two rows at the top of the screen.
- The player controls a paddle at the bottom, moving left and right with the
  arrow keys.
- Pressing the spacebar fires a bullet upward from the player's current position.
- The bullet travels in a straight vertical line; if its x-coordinate falls
  within a block's bounding box, it destroys that block on contact.
- The game ends when all six blocks have been cleared.

Technical design:
- All objects are heap-allocated with constructor/dispose methods following
  Jack's manual memory management model (Memory.alloc / Memory.deAlloc).
- Collision detection: on each Shoot() call, the game walks the squares array
  and checks whether the bullet's x-coordinate falls inside any block's
  x-range. If so, the bullet animates upward until it reaches that block's
  y-coordinate, then erases the block and decrements squaresLeft.
- The game loop (in BlockShooter.jack) reads keyboard input each iteration via
  Keyboard.keyPressed(), dispatching to moveLeft/moveRight/Shoot accordingly.
- Sys.wait() calls throttle movement speed to keep the game playable.

Classes:
- Main.jack          — entry point; constructs and runs the game
- BlockShooter.jack  — game loop, input handling, and win condition
- Player.jack        — player paddle: position, draw, moveLeft/moveRight
- Bullet.jack        — bullet: position, draw, moveUp, erase
- Square.jack        — block: position, size, draw, erase

Language: Jack
