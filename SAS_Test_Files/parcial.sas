
procedure optmodel;

set <string> DOP = {"Empordà", "Garrigues", "Siurana", "Terra Alta"};
set <string> Supermercat = {"Girona", "Lleida", "Tarragona"};
set <string> Molí = {"A", "B"};

number capacitat_recollecció {DOP} = [6000, 7000, 8000, 7000];
number coeficient_extracció {DOP} = [.25, .3, .25, .2];

number transport_dop_molí {DOP, Molí} = [
	54, 56,
	60, 49, 
	41, 53, 
	54, 52
];

number capacitat_extracció {Molí} = [12000, 20000];
number cost_extracció {Molí} = [78, 82];

number transport_molí_super {Molí, Supermercat} = [
	47, 56, 58, 
	51, 52, 59
];

number demanda {Supermercat} = [1500, 3000, 2500];

var rec {DOP} >= 0; /* Recol·lecció en cada DOP */
var tdm {DOP, Molí} >= 0; /* Transport DOP - Molí */
var ext {Molí} >= 0; /* Extracció (en KL d'oli) de cada molí */
var tms {Molí, Supermercat} >= 0; /* Transport Molí - Supermercat */

minimize costos_totals = 
	  sum {i in DOP, j in Molí} (transport_dop_molí[i, j] * tdm[i, j])
	+ sum {i in DOP, j in Molí} (tdm[i, j] * cost_extracció[j])
	+ sum {j in Molí, k in Supermercat} (transport_molí_super[j, k] * tms[j, k])
;

/* Cada DOP envia el total de la seva recol·lecció */
constraint rec_tdm {i in DOP}  : sum {j in Molí} tdm[i, j] = rec[i];

/* L'extracció (en KL d'oli) de cada molí */
constraint tdm_ext {j in Molí} : sum {i in DOP}  tdm[i, j] * coeficient_extracció[i] = ext[j];

/* Els molins envien la totalitat de la seva extracció als supermercats */
constraint ext_tms {j in Molí} : sum {k in Supermercat} tms[j, k] = ext[j];


constraint recollecció {i in DOP} : rec[i] <= capacitat_recollecció[i];
constraint extracció {j in Molí} : sum {i in DOP} tdm[i, j] <= capacitat_extracció [j];
constraint satisfacció {k in Supermercat} : sum{j in Molí} tms[j, k] >= demanda[k];

expand;
solve;

print rec.sol;
print tdm.sol;
print ext.sol;
print tms.sol;

print rec.status;
print tdm.status;
print ext.status;
print tms.status;