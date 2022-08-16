<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

Simulation is intensive, so you likely want to run

```
cabal run sim -- plot 2000000
```

rather than using the REPL.

This will generate `clocks.pdf`.

Alternately,

```
cabal run sim -- csv 2000000
```

will generate `clocks0.csv`, `clocks1.csv`, etc.
