
proc optmodel;
set Alums = 1..22;
number afin {Alums, Alums} = [
 -0 8 -2 5 -1 -0 7 -5 -1 5 3 4 -3 7 -2 5 -2 7 1 -0 3 -1
 -1 0 -7 -5 3 8 0 3 2 0 1 5 7 1 -1 6 5 -2 -4 5 -5 -2
 6 -2 0 -3 5 8 8 -4 4 4 6 4 8 -3 2 4 8 1 8 2 2 6
 -1 2 -1 0 -3 6 4 -0 5 6 3 -1 -2 4 7 6 -1 4 5 1 3 -1
 4 4 -0 -1 0 5 -5 2 2 6 4 -4 -6 -1 6 -4 4 1 7 -3 -7 0
 5 -5 2 5 -4 -0 1 -1 -5 -0 6 4 3 -2 3 8 3 8 8 8 5 4
 2 7 -3 3 -3 7 0 7 7 -4 5 3 5 -0 5 1 3 -8 2 3 4 0
 6 7 1 5 -8 7 3 0 7 -3 -4 4 -3 5 5 -6 -5 -5 -2 1 6 2
 -1 4 4 -2 2 5 4 4 0 -1 7 -0 1 -5 9 -4 5 7 6 5 3 8
 -1 1 7 -3 2 0 5 -5 8 0 -0 0 7 3 6 4 5 3 0 1 9 5
 -1 -1 2 6 3 7 -3 3 2 3 0 0 3 6 1 2 -1 1 4 -1 1 2
 -4 -0 2 8 6 -5 2 5 8 6 3 0 7 -1 -6 -2 0 7 0 3 4 9
 -6 -0 7 0 -0 6 5 1 -0 -2 7 8 0 5 -1 1 4 0 -3 5 6 1
 6 2 5 1 3 4 1 6 0 5 2 7 -5 -0 2 5 -5 3 3 8 5 5
 4 -4 1 7 3 -6 3 6 1 7 -2 8 -3 4 0 6 -5 7 5 -7 -5 -4
 8 5 -6 -6 6 3 9 7 -5 -6 7 1 -6 5 5 0 4 6 -0 1 8 4
 1 4 -3 -0 4 3 -1 5 -2 3 -7 5 8 1 1 -5 -0 3 5 2 8 1
 -6 5 -5 5 1 3 1 2 -5 -0 -4 2 -6 4 4 0 -4 0 7 -3 4 -5
 8 -2 2 -6 3 2 1 5 2 4 5 -1 7 6 8 -3 -1 -3 -0 2 6 5
 -1 4 4 6 -1 -6 -1 8 3 6 1 7 3 5 1 3 -2 2 4 0 -2 4
 5 -4 -5 3 1 5 3 4 4 3 5 2 -6 5 6 6 5 5 4 4 0 5
 7 -2 4 2 5 -2 8 -1 -1 4 7 -2 -2 7 1 7 -3 6 2 4 9 -0
];

var P{i in Alums, j in Alums} binary;
max Tot = sum{i in Alums, j in Alums: i < j} (afin[i,j] + afin[j,i]) * P[i,j];

constraint complete {i in Alums, j in Alums} : P[i,j] - P[j,i] = 0;
constraint r1 {i in Alums} : sum{j in Alums} P[i,j] = 1;
constraint r2 {i in Alums, j in Alums} : P[i,j] * (afin[i,j] - afin[j,i]) <= 5;

expand;
solve;
print P;   /* mostra la solució de forma poc clara */
number res{Alums, 1..3};
for {i in Alums, j in Alums: i < j} 
    if (P[i,j]=1) then do;
       res[i,1] = j;
       res[i,2] = afin[i,j];
       res[i,3] = afin[j,i];
    end;
print res; /* molt millor! */