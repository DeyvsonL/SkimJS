var fibResult = 0;

function fib(i){
	if (i == 0 || i == 1) {
		fibResult = fibResult + 1;
	} else {
		fib(i - 1);
		fib(i - 2);
	}

}

fib(30);

fibResult;



