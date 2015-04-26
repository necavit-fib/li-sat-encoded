# Flow Free

#### David Martínez Rodríguez
#### LI, April, 2015

## Results

Neither of the solutions implemented in the files `flow-solver.pl` and `flow-solver-V2.pl` are able to generate a correct SAT encoding of the Flow Free game, even though I believe them to be close to the solution.

## Script

An automated script is provided in `flow.sh` to gather and compile the necessary pieces of the solver, in order to ease testing it. Calling this script without any arguments will provide a meaningful explanation of its usage. To solve a particular instance of the Flow Free problem, once the appropriate execution permissions have been granted to the script, we can execute:

```bash
./flow.sh instance.pl
```

## Contents

This directory contains the following files:

* `displayFlow.pl` - Utility script to parse the SAT model into a PostScript file.
* `entradaFlow9.pl` and `entradaFlow14.pl` - Sample problems (instances).
* `example-result.pdf` - Result example for the `entradaFlow9.pl` instance file.
* `flow-solver.pl` - The main version of the Flow solver.
* `flow-solver-V2.pl` - A second version of the Flow solver.
* `flow.sh` - An automated script to execute the solver.
* `main.pl` - The main procedure of the final solver, modularized in a separate file.
