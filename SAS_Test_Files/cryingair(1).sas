procedure optmodel;

set <string> Avio = {"Jumbo", "Petit", "Mitjà", "Gran"};
number preu {Avio} = [79, 67, 50, 35];
number benefici {Avio} = [5.8, 4.2, 3, 2.3];
number pressupost = 2000;

var quin {Avio} binary;
var x {Avio} >= 0 integer;

maximize z = sum {a in Avio} x[a] * benefici[a];

constraint no_triat {a in Avio} : x[a] <= 100*quin[a];
constraint si_triat {a in Avio} : x[a] >= quin[a];
constraint tipus : sum {a in Avio} quin[a] = 3;

constraint r_pressupost : sum {a in Avio} x[a] * preu[a] <= pressupost;
constraint min_avions : sum {a in Avio} x[a] >= 35;

constraint no_mes_petits_que_mitjans : x["Petit"] <= x["Mitjà"];
constraint no_jumbo_i_grans : quin["Jumbo"] + quin["Gran"] <= 1;
constraint quinze_percent : x["Jumbo"] <= 0.15 * sum {a in Avio} x[a];

solve;
number despesa = sum{a in Avio} x[a].sol * preu[a];
print quin.sol x.sol;
print despesa;