result = 1;
f = function mulItoN(number, exponent){
	for(var i = 0; ; i = i + 1){
		if(i >= exponent){
			break;
		}
		result = result * number;
	}
}
g = f;
g(2, 10);
result;