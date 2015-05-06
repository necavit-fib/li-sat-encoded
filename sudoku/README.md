# Sudoku

#### David Martínez Rodríguez
#### LI, April, 2015

## Contents

There are two directories in the zipped file:

* `automated_solver` - Contains a shell script and a small C++ program to ease the automation of the whole process of compilation, instance inclusion and solution visualization. Within this folder, there is another directory, `instances`, where the problem instances are located.
* `plain_prolog` - Contains the Prolog program and necessary Makefile to manually compile and execute the solver.

## Automated solver

The automated script allows us to either execute the solver using the symbolic output flag:

```bash
./sudoku_solve.sh -s symbolic.out instance.pl
```

or actually solving a given instance:

```bash
./sudoku_solve.sh instance.pl
```

NOTE: the Prolog program does not halt itself - an EOF has to be sent using `ctrl-d` to stop its execution.
