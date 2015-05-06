#!/bin/bash

function usage {
	echo "usage: sudoku_solve [-s output_file] instance_file.pl"
    echo "        -s turns on symbolic output (CNF form) to output_file and disables problem resolution"
    exit
}

export PATH=$PATH:.

if [ -z "$1" ]; then
    echo "error: no arguments supplied"
    usage
fi

if [ $1 == "-s" ]; then
	#symbolic output
	if [ -n "$2" ]; then
		if [ -n "$3" ] && [ -f "$3" ]; then
			#just so that prolog can find the instance file:
			cp "$3" .
			
			instance=$(basename "$3" .pl) #instance is in the third argument
			#put the instance of the problem to a new file
			echo "including instance "$instance" in solver.pro"
			echo ":-include("$instance")." > solver.pro
			
			#set symbolic output true
			echo "setting symbolic output to true in solver.pro"
			echo "symbolicOutput(1)." >> solver.pro
			
			#append the solver logic to the file in which the instance is included
			echo "appending solve_sudoku.pro solver logic to file: solver.pro"
			cat solve_sudoku.pro >> solver.pro
			
			#compile the new solver
			make solve
			
			#run the solver with the given instance
			solve > "$2"
			
			#delete the copied instance file
			rm ./"$instance".pl
		else
			echo "error: instance_file does not exist or is not specified"
			usage
		fi
	else
		echo "error: output_file not specified"
		usage
	fi
else
	#solve the problem with the instance given
	if [ -n "$1" ] && [ -f "$1" ]; then
		#just so that prolog can find the instance file:
		cp "$1" .
		
		instance=$(basename "$1" .pl) #instance is in the third argument
		#put the instance of the problem to a new file
		echo "including instance "$instance" in file: solver.pro"
		echo ":-include("$instance")." > solver.pro
		
		#set symbolic output false
		echo "setting symbolic output to false in solver.pro"
		echo "symbolicOutput(0)." >> solver.pro
		
		#append the solver logic to the file in which the instance is included
		echo "appending solve_sudoku.pro solver logic to file: solver.pro"
		cat solve_sudoku.pro >> solver.pro
		
		#compile the new solver
		make solve
		#compile the solution parser
		make parse
		
		#run the solver with the given instance
		solve > solution.out
		#run the parser with the solution
		parse < solution.out
		
		#delete the copied instance file
		rm ./"$instance".pl
	else
		echo "error: instance_file does not exist or is not specified"
		usage
	fi
fi


