Team “Lambda Spice” entry in ICFP Contest 2019

## Problem

[icfpcontest2019.github.io](https://icfpcontest2019.github.io)

## Results

Lightning round

| Place | Team         | Score   |
|-------|--------------|---------|
| 20    | Lambda Spice | 2669265 |

Full contest

| Place | Team         | Score   |
|-------|--------------|---------|
| 31    | Lambda Spice | 1888737 |

## Developing

```
$ lein repl
icfpc.main=> (require 'icfpc.main :reload-all)
icfpc.main=> (solve "prob-010" {:delay 50}) ; interactive
icfpc.main=> (solve "prob-010") ; non-interactive
```

Recalc everything:

```
$ lein run
```

Submit everything:

```
$ ICFPC_KEY=... ./submit.sh
```

## Building native image

```
$ export GRAAL_HOME=...
$ lein native-image
$ ./target/icfpc2019
```