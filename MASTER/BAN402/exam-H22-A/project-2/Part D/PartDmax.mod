# Defining sets
set D;	# Days in the period
set H;	# Hours per day
set W;	# Weekdays in the period

# Setting parameters
param r;			# Recharging max rate per hour
param c;			# Decharging rate while driving to work
param p {D, H};		# Price of one MWh at day in D at hour in H


# Defining variables
var x {D, H} >= 0;		# Amount charged at hour H in day D
var y {D, H} binary;	# If driving to/from work or not at hour H in day D
var b {D, H};			# Battery charge at hour H in day D

# Objective function
maximize total_cost: sum {d in D, h in H} (x[d,h]*p[d,h]);

# Constraints ----
subject to
	BatteryContinuityHour {d in D, h in H:h>0}: b[d,h] = b[d,h-1] + x[d,h] - c * y[d,h];
	BatteryContinuityDay {d in D:d>1}: b[d,0] = b[d-1,23] + x[d,0] - c * y[d,0];
		
	StartBattery: b[1,0] = 51.2;
	EndBattery: b[92,23] = 51.2;
	MaxCharging {d in D, h in H}: x[d,h] <= r;
	MinCharge {d in D, h in H}: b[d,h] >= 12.8; 
	BatteryCapacity {d in D, h in H}: b[d,h] <= 64;
	
	DriveToWork {d in W}: y[d,7] = 1;
	DriveFromWork {d in W}: y[d,16] = 1;
	CantChargeAtWork {d in W, h in 8..16}: x[d,h] = 0;
	CantChargeWhileDriving {d in D, h in H}: (x[d,h]/7.5)+y[d,h]<=1;
		
	NotDriving {d in D, h in {0,1,2,3,4,5,6,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23}}: y[d,h] = 0;
	NotDriving2{d in D diff W, h in H}: y[d,h] = 0; 
	


