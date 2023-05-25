set I = 1..26;
set N = 1..2000;
set B;

param f{I};
param v{I};
param c{B};
param d{I,N};
param t;

var x{I,N} binary;
var y{B,I} >= 0 integer;
var g{I} binary;
var r{I,N} binary;
#var o{N} binary;

minimize cost: sum{i in I}(f[i]*g[i]) + sum{i in I, b in B}(v[i]*y[b,i]) + sum{b in B, i in I}(y[b,i]*c[b]);

# Must be two 
subject to 2elBikes{i in I}:
	y["electrical",i] = 2*g[i];

# At least 50% of the city must be serviced	
subject to minUsers:
	sum{i in I, n in N} x[i,n] >= 0.5*card(N);

# Must populate selected stations with bikes to fulfill requirement
subject to bikeUserRatio{i in I}:
	sum{b in B} y[b,i] >= 0.05*sum{n in N}(x[i,n]);
subject to bikeUserRatio2{i in I}:	
	sum {b in B} y[b,i] <= 9999*g[i];

# Can only be assigned to a rack that is within range
subject to mustBeInRange{i in I, n in N}:
	r[i,n] >= x[i,n];

#
subject to definingRm1{i in I, n in N}:
	t >= d[i,n] - 999*(1-r[i,n]);
subject to definingRm2{i in I, n in N}:
	t <= d[i,n] + 999*r[i,n];

# Can only be assigned to one rack
subject to userRackLink{n in N}:
	sum{i in I} x[i,n] <= 1;

# Cannot be assigned to not open rack	
subject to cannotBeAssignedToNotOpenRack{i in I}:
	sum{n in N} x[i,n] <= 9999*g[i];

# Must be assigned the closest rack if it is in range
subject to closest{i in I, j in I, n in N}:
	x[i,n]*d[i,n] <= d[j,n];

#
#subject to mustAssignIfRackBuiltInRange1{n in N}: 	
#	sum{i in I} r[i,n]*g[i] >= o[n];
#subject to mustAssignIfRackBuiltInRange2{n in N}: 	
#	sum{i in I} r[i,n]*g[i] <= 999*o[n];	

#subject to mustAssignIfClosestOpenIsTrue{n in N}:	
#	sum{i in I} x[i,n] = o[n];

	
	
	
		
#--------------------------------------------------	

#subject to mustAssignIfRackBuiltInRange2{n in N}: 
#	sum{i in I} r[i,n]*g[i] <= 9999*sum{i in I} x[i,n];
	
# Dersom det finnes en bygd stasjon i nærheten må x vere lik 1	
	
# Problemet no er at uansett kor dårlig og dyrt eit stopp er så vil den opne det så lenge det er minst ein persom som har den som nærmeste

	#sum{i in I} r[i,n]*g[i] = 0 Then sum{i in I} x[i,n] = 0
	#sum{i in I} r[i,n]*g[i] > 1 Then sum{i in I} x[i,n] = 1
	


	



