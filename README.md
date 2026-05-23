# SignalDeck

SignalDeck is a terminal (TUI) telemetry dashboard written in Haskell.

It ingests metric data, keeps a rolling window of numeric samples, and renders lightweight terminal visualizations such as sparklines.

Current focus:
- Learn-by-building stages (CLI, parsing, rolling windows, sparkline rendering)
- Keep pure data logic separate from IO loop/rendering
- Treat each data feed as a source adapter that produces the same internal sample stream

## Current Commands

```sh
cabal run SignalDeck -- tui
cabal run SignalDeck -- demo
cabal run SignalDeck -- file /path/to/samples.txt
some_pipeline | cabal run SignalDeck
```

The core input contract is one numeric sample per line:

```sh
while true; do
  ps -A -o %cpu | awk '{sum += $1} END {print sum}'
  sleep 1
done | cabal run SignalDeck
```

SignalDeck does not own the upstream telemetry source. It ingests the stream the user connects, validates each line, keeps a rolling window, and renders the current frame.

The TUI currently runs a live demo source and supports:
- `space` to pause/resume updates
- `q` or `Esc` to quit
- rolling sample count, latest value, and sparkline trend

File mode parses a finite input file through the same ingestion logic and reports valid, invalid, and blank rows. Stdin mode is live and redraws the terminal as samples arrive.

## Pipeline Roadmap

SignalDeck is meant to visualize data from user-owned pipelines. Example streams include:
- latency from a service script
- CPU or memory from local system commands
- queue depth from an internal worker
- packet loss from network tooling
- sensor, radar, or FFT values from custom programs
- price, volume, or other finance metrics from a user-supplied feed

The next architecture step is to formalize a source adapter type around:
- a source name
- a stream of samples
- validation/error counts
- a polling interval or event trigger

That keeps upstream data collection separate from ingestion, state updates, and terminal rendering.
