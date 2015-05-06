#  Number Cicle Wrap Solver

LI - November 2013
David Martínez Rodríguez

## Contents

There are two directories in this repository:

* `automated_solver` contains a shell script to help automate the build process of the solver. There is a subdirectory named `instances` with the problem instances given.
* `plain_prolog` contains the necessary Prolog program and a Makefile to compile and manually execute the solver.


## Automated Solver

The script can be run as:

```bash
$> number_cicle_wrap.sh -s symbolic.out instance.pro
```

to symbolically execute the solver outputing the result to the `symbolic.out` file, over a given instance `instance.pro`.

If no symbolic output is desired, it can be run as:

```bash
$> number_cicle_wrap.sh instance.pro
```

to execute the solver over a given instance `instance.pro`, showing the solution of the model found.


**NOTE:** the script is prepared to be run under a Linux environment, with a Bash shell and having the following programs installed:

* `swipl` (SWI Prolog package)
* `evince` (the default Ubuntu PDF viewer)
