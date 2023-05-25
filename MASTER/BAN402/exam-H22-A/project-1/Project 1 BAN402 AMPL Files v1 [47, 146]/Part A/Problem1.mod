
set FACI;   # facilities
set POLU;   # pollutants

param c{FACI} >= 0;   # cost of processing one ton at facility
param r{FACI, POLU} >= 0;   # tons of pollutant reduced at facility
param m{POLU};   # pollutant reduction


var x{FACI} >= 0;    # tons of fish processed

minimize z:   # minimize cost
   sum {i in FACI} c[i] * x[i];

subject to 
Reduction {i in POLU}: # adhere to government targets
  sum{j in FACI} r[j,i] * x[j] >= m[i];
  

  
  
 