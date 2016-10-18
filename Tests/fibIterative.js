function fibIterative(n){
	var i = 1;
	var j = 1;
	for(var k = 1; k < n; k = k+1){	
		var t;
		t = i + j;
		i = j;
		j = t;
	}
	return j;
	
};

fibIterative(20);