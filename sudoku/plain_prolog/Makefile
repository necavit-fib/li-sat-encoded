file = solve_sudoku

$(file): $(file).pro
	swipl -O -g main --stand_alone=true -o $(file) -c $(file).pro

clean:
	rm $(file)


