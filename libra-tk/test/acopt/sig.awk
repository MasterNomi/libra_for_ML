BEGIN{
	n = 0;
	sum_diff = 0;
	sum_squared_diff = 0;
}

{
	diff = $1 - $2;
	sum_diff = sum_diff + diff;
	sum_squared_diff = sum_squared_diff + (diff * diff);
	n = n + 1;
}
END{
	mean = sum_diff / n;
	sd = sqrt((sum_squared_diff/n) - (mean*mean) );
	se_mean = sd / (sqrt(n));
	if (se_mean == 0) {print 0;}
	else {
		z = mean / se_mean;
		if (z < 1.64 && z > -1.64) {print 0;} else {print 1;}
	}
}

