sumIToN = function (i, n){
	result = 0;
	for(; i <= n; i = i+1){
		result = result + i;
	}
	return result;
}

sumIToN(0,4);