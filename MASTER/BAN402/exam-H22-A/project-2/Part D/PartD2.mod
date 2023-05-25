# Defining sets
set D;	# Days in the period
set H;	# Hours per day
set W;	# Weekdays in the period
set G;	# Sundays in the period

# Setting parameters
param r;			# Recharging max rate per hour
param c;			# Decharging rate while driving to work
param c2;			# Decharging rate while recreationally driving
param p {D, H};		# Price of one MWh at day in D at hour in H

# Defining variables
var x {D, H} >= 0;		# Amount charged at hour H in day D
var y {D, H} binary;	# If driving to/from work or not at hour H in day D
var b {D, H};			# Battery charge at hour H in day D
var s {D, H} binary;	# Start of recreation driving
var j {D, H} binary;	# If recreationally driving at hour H in day D
var i {D, H} binary;	# If at recreational facility at hour H in day D

# Objective function
minimize total_cost: sum {d in D, h in H} (x[d,h]*p[d,h]);

# Constraints ----
subject to
	#Battery level must be equal to last period plus inflow and outflows 
	BatteryContinuityHour {d in D, h in H:h>0}: b[d,h] = b[d,h-1] + x[d,h] - c * y[d,h] - c2 * j[d,h];
	BatteryContinuityDay {d in D:d>1}: b[d,0] = b[d-1,23] + x[d,0] - c * y[d,0] - c2 * j[d,0];
	
	#The battery charge level in the first time period of the timeframe and the last period of the timeframe must be equal to 51.2
	StartBattery: b[1,0] = 51.2;
	EndBattery: b[92,23] = 51.2;
	
	#The amount charged on day d and hour h is limited to a minimum and maximum value.
	MaxCharging {d in D, h in H}: x[d,h] <= r;
	
	#The minimum battery charge level for all day’s d and hours h is defined with a lower limit.
	MinCharge {d in D, h in H}: b[d,h] >= 12.8; 
	
	#The maximum battery capacity for all day’s d and hours h is defined with an upper limit.
	BatteryCapacity {d in D, h in H}: b[d,h] <= 64;
	
	#For all workdays, must be driving to and from work
	DriveToWork {d in W}: y[d,7] = 1;
	DriveFromWork {d in W}: y[d,16] = 1;
	
	#For all workdays, cannot charge while at the office
	CantChargeAtWork {d in W, h in 8..16}: x[d,h] = 0;
	
	#For all days, cannot charge while driving or at a recreational facility
	CantChargeWhileDriving {d in D, h in H}: (x[d,h]/r) + j[d,h] + y[d,h] + i[d,h] <= 1;
	
	#For all sundays, must start recreating once between 10 and 13
	MustStartRecreate {d in G}: sum {h in {10..13}} s[d,h] = 1;
	
	#Create a four hour period where optidriver cannot charge his car
	Recreate1 {d in G, h in H:h>4}: j[d,h] >= s[d,h];
	Recreate2 {d in G, h in H:h>4}: i[d,h] >= s[d,h-1];
	Recreate3 {d in G, h in H:h>4}: i[d,h] >= s[d,h-2];
	Recreate4 {d in G, h in H:h>4}: j[d,h] >= s[d,h-3];
	