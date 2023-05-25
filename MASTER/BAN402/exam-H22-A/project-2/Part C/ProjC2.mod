set I; # Crude oils
set B; # Components
set P; # Final products

set J; # CDU: Crude Distilling Units
set D; # Depots
set K; # Markets
set M; # Running modes
set T; # Time periods (days)

# Parameters ----
param Ccrude {I, T}; 		# Cost of one unit of crude i at time t.
param R {I, B, J, M}; 		# Amount of component b obtained from refining one unit of crude i in CDU j at running mode m.
param N {B,P}; 				# Amount of component b needed in recipe for one unit of product p.
param S {P};				# Sales price of product p.
param Cref {I,J,M}; 		# Cost of refining one unit of crude i in CDU j at running mode m.
param Cap {J, M}; 			# Capacity of CDU j (max amount of total crudes it can process per period) at running mode m.
param Cprod {P}; 			# Cost of producing one unit of product p at the mixer facility.
param Ctra1; 				# Cost of transporting one unit of any component b from the refining to the blending department.
param Ctra2 {D}; 			# Cost of transporting one unit of any product p to depot d.
param Ctra3 {D,K}; 			# Cost of transporting one unit of any product p from depot d to market k.
param Cinvi; 				# Cost of storing one unit of any crude i at the refinery.
param Cinvb; 				# Cost of storing one unit of any component b at the tanks.
param Cinvp {D}; 			# Cost of storing one unit of any product p at depot d .
param delta {P,K,T}; 		# Demand for product p in market k in period t.
param Cmode {J,M}; 			# Fixed cost of operating CDU j at running mode m per period.
param Cchange; 				# Cost of changing running mode at a CDU.

# Parameters added to the data file
param Initmode {J,M};		# Initial running mode of the CDUs.
param Izero {P,D};			# Initial inventory.
param Ifinal {P,D};			# Final inventory.
param Slowqc;				# Price of the component lowqc.

# Desicion Variables ---
var u {I,T} >= 0; 		# Amount of crude oil i purchased on day t.
var z {I,J,M,T} >= 0; 	# Amount of crude oil i distilled in CDU j on day t.
var c {B,T} >= 0;		# Amount of component b produced on day t.
var y {B,T} >= 0; 		# Amount of component b sent to the blending department on day t (for blending in t+1).
var w {P,T} >= 0; 		# Amount of product p produced at the blending department on day t.
var x {P,D,T} >= 0; 	# Amount of product p sent from the blending department to depot d on day t (available at depot in t+1).
var v {P,D,K,T} >= 0; 	# Amount of product p sent from depot d to market k on day t (to satisfy demand in t+1).
var IO {I,T} >= 0; 		# Inventory of crude oil i at the refining department at the end of day t.
var IC {B,T} >= 0; 		# Inventory of component b at the refining department at the end of day t.
var IP {P,D,T} >= 0; 	# Inventory of product p at depot d at the end of day t.
var RM {J,M,T} binary; 	# Binary. 1 if running mode m is active on CDU j on day t, 0 otherwise.

# Objective Function ----
maximize total_profit: sum {p in P, d in D, k in K, t in T: 0 < t < 12} (S[p]*v[p,d,k,t])			# Product sales income.
	+sum{t in T: t > 0} (c['lowqc', t] * Slowqc)													# loqc sales income.
	-sum{i in I, t in T:t>0} (Ccrude[i,t]*u[i,t])													# Cost of crude oil.
	-sum{j in J, m in M, t in T:t>0} (Cmode[j,m]*RM[j,m,t])											# Cost of running CDU in mode m.
	-sum{j in J, m in M, t in T:t>0} (Cchange * (RM[j,m,t]-RM[j,m,t-1])^2)							# Cost of changing CDU mode.	
	-sum{i in I, j in J, m in M, t in T:t>0} (z[i,j,m,t] * Cref[i,j,m]*RM[j,m,t])					# Cost of refining crude oil.
	-sum{p in P, t in T:t>0} (Cprod[p]*w[p,t])														# Cost of producing product.
	-sum{i in I, t in T:t>0} (Cinvi*IO[i,t])														# Cost of inventory of crude.
	-sum{b in B, t in T:t>0} (Cinvb*IC[b,t])														# Cost of inventory of components.
	-sum{p in P, d in D, t in T:t>0} (Cinvp[d]*IP[p,d,t])											# Cost of inventory of product.
	-sum{b in B, t in T:t>0} (Ctra1*y[b,t])															# Cost of transport to blending.
	-sum{p in P, d in D, t in T:t>0} (Ctra2[d]*x[p,d,t])											# Cost of transport to depot.
	-sum{p in P, d in D, k in K, t in T:t>0} (Ctra3[d,k]*v[p,d,k,t])								# Cost of transport to market.
	;
;

# Constraints ----
subject to
# 	Initial values
	InitIO {i in I}: IO[i,0] = 0;												# Initial inventory of crude oil at refining dept.
	InitIC {b in B}: IC[b,0] = 0;												# Initial inventory of component b at refining dept.
	InitIP {p in P, d in D}: IP[p,d,0] = Izero[p,d];							# Initial inventory of product p at depot d.

	Inity {b in B}: y[b,0] = 0;													# Initial amount of component b sent to blending dept.
	Initx {p in P, d in D}: x[p,d,0] = 0;										# Initial amount of product p sent to depot d.
	
	InitRM {j in J, m in M, t in T: t = 0}: RM[j, m, t] = Initmode[j, m];		# Initial state of CDUs.
	
#	Ending values	
	FinIC{b in B diff {"lowqc"}}: IC[b,12] >= 80;								# Minimum ending amount of component b at refining dept.
	FinIP {p in P, d in D}: IP[p,d,12] = Ifinal[p, d];							# Minimum ending amount of product p at depot d.


# Further constraints

# Change in inventory of crude oil i at refinery in time t.
BalanceOil {i in I, t in T: t > 0}: IO[i, t] = IO[i, t-1] + u[i, t] - sum {j in J, m in M} z[i, j, m, t];

# CDU j can only be set to one mode m at the same time t.
CDUModeCon {j in J, t in T: t > 0}:	sum {m in M} RM[j, m, t] = 1;

# The amount of crude oil i distilled in CDU j in mode m on day t cannot 
# exceed the production capacity of a given CDU in mode m.
CDUCap {j in J, m in M, t in T: t > 0}: sum {i in I} z[i, j, m, t] <= Cap[j, m] * RM[j, m, t];

# Amount of component b produced on day t must be equal to the amount of crude oil i distilled in CDU j on day t times 
# amount of component b obtained from distilling one unit of crude oil i in CDU j at running mode m.
CreateComps {b in B, t in T: t > 0}: c[b, t] = sum {i in I, j in J, m in M} (z[i, j, m, t] * R[i, b, j, m]);

# Amount of component b sent to blending on day t cannot exceed the inventory of component b 
# on day t-1 plus production of component b on day t. 
BalanceComps {b in B diff {"lowqc"}, t in T: t > 0}: y[b,t] <= IC[b, t-1] + c[b,t];

# The amount of component b sent to the blending department in t-1 must be 
# equal to the amount of component b used to produce product p.
FulfillCompReqs {b in B diff {"lowqc"}, t in T: t > 0}: y[b,t-1] = sum {p in P} (N[b, p] * w[p, t]);

# Inventory of component b at refining department in period t cannot exceed the inventory in 
# the refining department in t-1 plus the production of component b minus the amount of component b sent to blending department
EquilInvComp {b in B diff {"lowqc"}, t in T: t > 0}: IC[b, t] = IC[b, t-1] + c[b, t] - y[b, t];

# As the blending department do not possess any storage, the amount of product p produced must equal 
# the amount of product p sent to depot d in time t.
ProdToDepot {p in P, t in T: t > 0}: w[p, t] = sum {d in D} x[p, d, t];

# Inventory at depot d must be equal to the inventory in depot d in t-1 plus the inflow of product p to depot d in t-1 minus 
# the amount of product p from depot d sold to market k on day t.
EquilInvProd {p in P, d in D, t in T: t > 0}: IP[p, d, t] = IP[p, d, t-1] + x[p,d,t-1] - sum{k in K} v[p, d, k, t];

# The amount of product p from all depots sold to market k in t-1 must be less or equal to the demand of product p in market k on day t.
DemandLimit {p in P, k in K, t in T: t > 0}: sum {d in D} v[p, d, k, t-1] <= delta[p, k, t];















