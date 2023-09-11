set I; # Customers
set J; # Trucks

param d{I}; #demand for customer i
param c{I,J}; #travel distance from customer i to truck j
param f; # fixed cost that Guroble has to pay for placing a truck at a potential location
param r; # revenue per burrito sold
param k; # ingredient cost per burrito sold

var x{J} binary; # 1 if we locate a truck at location j, and = 0 otherwise, for each j
var y{I,J} binary; # 1 if the closest truck to customer i is at location j (i.e., if i is assigned to j), and 0 otherwise
var a{I,J}; # demand multiplier for customer i and truck j. This multiplier depends on the distance: the further a customer i is from a truck location j, the less willing the customer is to walk to the truck
var z{I,J} integer; # rounded down value of  y[i,j] * d[i] * a[i,j]

maximize profit:
	sum{i in I, j in J} (r-k) * z[i,j] - # z[i,j]) = y[i,j] * d[i] * a[i,j] rounded down
	sum{j in J} (f * x[j])
;

# constraints
subject to

Lower_limit {i in I}:
	sum{j in J} y[i,j] <=1;

Open_truck_at_location {i in I, j in J}:
	y[i,j] <= x[j];

Truck_limit:
	sum{j in J} x[j] = 3;
		
Demand_multiplier {i in I, j in J}:
	a[i,j] = (exp(-0.1 *c[i,j]));
	
	
# Rounding down the product of z[i,j]) = y[i,j] * d[i] * a[i,j]
Low_round{i in I, j in J}:
    z[i,j] >= y[i,j] * d[i] * a[i,j] - 0.99;
Upper_round{i in I, j in J}:
    z[i,j] <= y[i,j] * d[i] * a[i,j];
	