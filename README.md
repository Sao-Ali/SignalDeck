# SignalDeck

SignalDeck is a terminal (TUI) telemetry dashboard written in Haskell.

It ingests metric data (stdin, file, or demo stream), parses numeric samples safely, and renders lightweight terminal visualizations such as sparklines.

Current focus:
- Learn-by-building stages (CLI, parsing, rolling windows, sparkline rendering)
- Keep pure data logic separate from IO loop/rendering
