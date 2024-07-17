procedure optmodel;

set <number> Projecte = {1..6};
set <number> Any = {1..5};

number npv {Projecte} = [141, 187, 121, 83, 265, 127];
number pressupost {Any} = [250, 75, 50, 50, 50];
number inversió {Projecte, Any} = [
	75, 25, 20, 15, 10,
	90, 35,  0,  0, 30,
	60, 15, 15, 15, 15,
	30, 20, 10,  5,  5
   100, 25, 20, 20, 20,
    50, 20, 10, 30, 40
];
number incompatible {Projecte, Projecte} = [
	., 1, 0, 1, 0, 0,
	., ., 1, 0, 0, 0,
	., ., ., 0, 0, 0,
	., ., ., ., 0, 0,
	., ., ., ., ., 1,
	., ., ., ., ., .,
];

var x {Projecte} binary;
maximize npv_total = sum {p in Projecte} x[p] * npv[p];

constraint r_pressupost {a in Any} : 
	sum {p in Projecte} x[p] * inversió[p, a] <= pressupost[a];

constraint r_incompatible {p in Projecte, q in (p+1)..card(Projecte)} :
	x[p] + x[q] <= 2 - incompatible[p, q];

expand;
solve with MILP;

print x.sol;
print r_pressupost.body r_pressupost.ub;
