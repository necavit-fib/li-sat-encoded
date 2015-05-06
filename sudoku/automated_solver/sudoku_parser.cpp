#include <iostream>
#include <vector>
#include <stdlib.h>
using namespace std;

void error() {
	cout << "ERROR: sentinel not found! check the infile!" << endl;
	exit(0);
}

int main() {
	//matrix is 10*10 because of the indexs
	vector<vector<int> > sudoku = vector<vector<int> >(10, vector<int>(10));
	
	//read solution
	int i, j, value, aux;
	while (cin >> i) {
		//read j,        value,        -1
		cin >> j; cin >> value; cin >> aux;
		if (aux != -1) error();
		sudoku[i][j] = value;
	}
	
	//output well formatted solution
	for (int i = 1; i < 10; ++i) {
		for (int j = 1; j < 10; ++j) {
			cout << sudoku[i][j];
			if (j % 3 == 0) cout << "    "; //4 blanks
			else cout << "  "; //2 blanks
		}
		if (i % 3 == 0) cout << endl;
		cout << endl;
	}
}

