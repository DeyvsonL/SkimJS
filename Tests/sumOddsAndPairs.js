var x = 0;
y = 0;
g = function(i, k, w){
	x = 0;
	for( ;i<=k;i = i + 1){
		if(i%2 == w){
			x = i + x;
		}
	}
	if(w == 1){
		y = x;
	}
	else {
		x = x;
	}
}
sumOdds = g(5, 10, 1)
sumPairs = g(5, 10, 0);
x;