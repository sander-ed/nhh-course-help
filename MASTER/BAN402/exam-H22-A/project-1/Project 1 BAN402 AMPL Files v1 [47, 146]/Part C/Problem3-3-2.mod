# Defining sets
set Region;
set Port;
set Market;

# Setting parameters
param n {Region};
param d {Market};
param c {Region, Port};
param t {Port, Market};
param u {Market};

# Defining variables
var x {Region, Port} >= 0;
var y {Port, Market} >= 0;

# Objective function
minimize total_cost: 
	sum {p in Port, r in Region} (x[r,p]*c[r,p])
		+ sum {k in Market, p in Port} (y[p,k]*t[p,k]);

# Constraints ----
subject to
	PurchaseLimit {r in Region}: sum {p in Port} x[r,p] <= n[r];
	Demand {k in Market}: sum {p in Port} y[p,k] = d[k]*(1-u[k]);
	Continuity {p in Port}: sum {r in Region} x[r,p] = sum {k in Market} y[p,k];


