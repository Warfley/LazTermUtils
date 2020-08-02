# LazTermUtils
Terminal Utility Classes for Coloring and Inputs on terminals


## About
This small library contains a few classes for managing terminal I/O using ANSI escape sequences.
The goal of this unit is provide the functionality provided by the CRT unit (and some more), but optimized for only the most prevalent systems, by only using escape sequences.
It was tested under Windows 10 and Linux and allows for writing portable, pretty console applications on both systems.

The functionality include:
- Colorizing output (background, foreground)
- Modifying output (bold, italic, underlined, etc.)
- Reading input keys and modifiers
- Managing terminal window (getting size, jumping around, clearing)
- Non blocking reading of chars and keys

## Requirements
This library uses xTerm compatible ANSI escape sequences.

This works with any Terminal emulator supporting this including, but not limited to xTerm, Konsole, GNOME Terminal and the Windows Terminal in Windows 10 (not compatible with older versions of Windows)
If the terminal does not support 24 bit (true) color (e.g. the macOS system terminal emulator), you might want to set the define Col8 (only required in TerminalColor.pas), which limits the output to 8bit colors. Lower than that is not supported.

## Usage
See the `example` directory for some examples.
- `example/ReadkeyExample` shows how to read Keystrokes and process them
- `example/Colortest` colors the whole display (each cell individually) while measuring the FPS to brenchmark the performance of redrawing every cell
- `example/Textmods` prints text with different modifications. Which of them get displayed correctly depends on your Terminal emulator.
- `example/NonBlockingReadTest` shows how to use non blocking read to update your view and check for keystrokes single threaded
