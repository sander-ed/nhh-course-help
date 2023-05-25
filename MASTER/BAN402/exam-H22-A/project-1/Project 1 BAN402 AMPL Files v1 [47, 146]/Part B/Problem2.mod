# Defining sets
set Crude;
set Gasoline;
set Market;
set Quality;

# Setting parameters
param price {Gasoline, Market};
param purB {Crude};
param cost {Gasoline};
param order {Crude};
param hours {Gasoline};
param maxhours;
param demand {Gasoline, Market};
param attribute {Crude, Quality};
param attributelimit {Gasoline, Quality};


# Defining variables
var X {Crude, Gasoline} >= 0;
var Y {Gasoline, Market} >= 0;

# Objective function
maximize total_profit: 
	sum {m in Market, j in Gasoline} (Y[j,m]*price[j,m])
		-sum {i in Crude, j in Gasoline} (purB[i]*X[i,j])
		-sum {j in Gasoline, m in Market} (cost[j]*Y[j,m]);

# Constraints ----
subject to
	PurchaseLimit {i in Crude}: sum {j in Gasoline} X[i,j] <= order[i];
	HourLimit: sum {m in Market, j in Gasoline} Y[j,m]*hours[j] <= maxhours;
	MinDemand {j in Gasoline, m in Market}: Y[j,m] >= demand[j,m];
	MinBlendReq {j in Gasoline}: sum {i in Crude} X[i,j]*attribute[i,"Octane"] >= sum {m in Market} Y[j,m]*attributelimit[j,"Octane"];
	MaxBlendReq {j in Gasoline}: sum {i in Crude} X[i,j]*attribute[i,"Sulphur"] <= sum {m in Market} Y[j,m]*attributelimit[j,"Sulphur"];
	Continuity1 {j in Gasoline}: sum {i in Crude} X[i,j] = sum {m in Market} Y[j,m];

