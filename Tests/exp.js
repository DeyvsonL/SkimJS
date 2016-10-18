result = 1;
function exp(number, exponent){
	for(var i = 0; i < exponent; i = i + 1){
		result = result * number;
	}
}
exp(2, 10);
result;