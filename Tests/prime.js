function prime(num) {
	result = true;
	if(num < 2) return false;
	for (i = 2; i < num; i = i+1){
		if (num % i == 0){
			result = false;
		} 
	}
	return result;
}
//resultado
prime(9);