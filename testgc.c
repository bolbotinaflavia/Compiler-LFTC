// implementare recursiva pentru factorial
int h(float x, int n) {
	float s = 3;
	if (x < h(s + 1, n)) {
		s = 1 - (n + 3.14 * x) / 2;
	}
	else {
		while (s < n - x) {
			n = (s + x) / 10;
			s = s * n + x;
		}
	}
	return s;
}

void main(){
	put_i(4.9);		// se afiseaza 4
	
	put_i(fact(3));
	// implementare nerecursiva pentru factorial
	int r;
	r=1;
	int i;
	i=2;
	while(i<5){
		r=r*i;
		i=i+1;
		}
	put_i(r);		// se afiseaza 24
	}
