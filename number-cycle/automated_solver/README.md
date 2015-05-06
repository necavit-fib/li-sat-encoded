# Number Cycle Wrap Solver

**USAGE:**

Use the `number_cicle_wrap.sh` script to compile and run the solver. Execute it with no arguments to see usage of the script.

## Compilation

The files required by the script to compile the solver are:

* `number_cicle_wrap_solver.pro` - contains all the solver logic
* `main.pro` - contains generic procedures to translate symbolic CNF formulas to SAT solver infiles correctly formatted and translate SAT solver solutions back to formulas, then usedto display solution in some or other way.
* `displayRodear.pro` - contains the procedures needed to correctly display the solution found

The files created during compilation time are:

* `solver.pro` - final prolog program to be compiled
* `solve` - **executable compiled solver**

The files created during the execution of the solver are:

* `graph.ps` - if symbolic output was disabled and a real solution is found
* `symbolic.out` - if symbolic output was set
