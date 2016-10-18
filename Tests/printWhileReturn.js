y = 10;
function result(value){
	print = 0;
	while(value < y){
		print = (print+1) + value;
		value = value +1;
	}
	return print;
}
result(5);