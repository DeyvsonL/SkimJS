function dayOfWeek(x) {
	if(x == 1){
		return "Sunday";
	}else if(x== 2){
		return "Monday";
	}else if(x== 3){
		return "Tuesday";
	}else if(x== 4){
		return "Wednesday";
	}else if(x== 5){
		return "Thursday";
	}else if(x== 6){
		return "Friday";
	}else if(x== 7){
		return "Saturday";
	}else{
		return "Dia invalido";
	}
}
dayOfWeek(4);