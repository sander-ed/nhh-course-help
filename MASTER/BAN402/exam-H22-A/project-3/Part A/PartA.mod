# Task 1
reset;

# Define the set of customer segments
set I = {"g", "s"};

# Quantity of tickets sold to segment I
var Q{I} >= 0;

# Price of tickets for semtent I, note the ticket price can be subsidised
var p{I};


# Revenue is the sum of price times quantity for all segments
maximize Revenue: sum{i in I} p[i]*Q[i];


# Demand for the general segment
subject to General:
	Q["g"] <= 120000-3000*p["g"];

# Demand for the student segment
subject to Student:
	Q["s"] <= 20000-1250*p["s"];

# Maximum capacity for each segment
subject to maxCap:
	sum{i in I}Q[i] <= 55000;

# Minimum number of tickets sold to each segment
subject to minTickets{i in I}:
	Q[i] >= 55000*0.2;

# Must have the same price for both segments	
subject to samePrice:
	p["g"] = p["s"];


option solver minos;
solve;
display Q["g"], Q["s"], p;



# Task 2
reset;

# Define the set of customer segments
set I = {"g", "s"};

# Quantity of tickets sold to segment I
var Q{I} >= 0;

# Price of tickets for semtent I, note the ticket price can be subsidised
var p{I};


# Revenue is the sum of price times quantity for all segments
maximize Revenue: sum{i in I} p[i]*Q[i];


# Demand for the general segment
subject to General:
	Q["g"] <= 120000-3000*p["g"];

# Demand for the student segment
subject to Student:
	Q["s"] <= 20000-1250*p["s"];

# Maximum capacity for each segment
subject to maxCap:
	sum{i in I}Q[i] <= 55000;

# Minimum number of tickets sold to each segment
subject to minTickets{i in I}:
	Q[i] >= 55000*0.2;


option solver minos;
solve;
display Q["g"], Q["s"], p["g"], p["s"];



# Task 3a
reset;

# Define the set of customer segments
set I = {"g", "s", "r"};


# Quantity of tickets sold to segment I
var Q{I} >= 0;

# Price of tickets for semtent I, note the ticket price can be subsidised
var p{I};


# Revenue is the sum of price times quantity for all segments
maximize Revenue: sum{i in I} p[i]*Q[i];


# Demand for the general segment
subject to General:
	Q["g"] <= 120000-3000*p["g"];

# Demand for the student segment
subject to Student:
	Q["s"] <= 20000-1250*p["s"];

# Demand for the senior segment
subject to Senior:
	Q["r"] <= 15000-1400*p["r"];

# Maximum capacity for each segment
subject to maxCap:
	sum{i in I}Q[i] <= 55000;

# Minimum number of tickets sold to general segment
subject to minGeneral:
	Q["g"] >= 55000*0.2;

# Minimum number of tickets sold to student and senior segments
subject to minSpecials:
	Q["s"]+Q["r"] >= 55000*0.2;

# Must have the same price for both student and senior segments	
subject to SamePriceSpecials:
	p["s"] = p["r"];

option solver minos;
solve;
display Q["g"], Q["s"], Q["r"], p["g"], p["s"], p["r"];


# Task 3b
reset;

# Define the set of customer segments
set I = {"g", "s", "r"};


# Quantity of tickets sold to segment I
var Q{I} >= 0;

# Price of tickets for semtent I, note the ticket price can be subsidised
var p{I};


# Revenue is the sum of price times quantity for all segments
maximize Revenue: sum{i in I} p[i]*Q[i];


# Demand for the general segment
subject to General:
	Q["g"] <= 120000-3000*p["g"];

# Demand for the student segment
subject to Student:
	Q["s"] <= 20000-1250*p["s"];

# Demand for the senior segment
subject to Senior:
	Q["r"] <= 15000-1400*p["r"];
	
# Maximum capacity for each segment
subject to maxCap:
	sum{i in I}Q[i] <= 55000;

# Minimum number of tickets sold to general segment
subject to minGeneral:
	Q["g"] >= 55000*0.2;
	
# Minimum number of tickets sold to student segment	
subject to minStudent:
	Q["s"] >= 55000*0.1;

# Minimum number of tickets sold to senior segment
subject to minSenior:
	Q["r"] >= 55000*0.1;



option solver minos;
solve;
display Q["g"], Q["s"], Q["r"], p["g"], p["s"], p["r"];



# Task 3c
reset;

# Define the set of customer segments
set I = {"g", "s", "r"};


# Quantity of tickets sold to segment I
var Q{I} >= 0;

# Price of tickets for semtent I, note the ticket price can be subsidised
var p{I};


# Revenue is the sum of price times quantity for all segments
maximize Revenue: sum{i in I} p[i]*Q[i];


# Demand for the general segment
subject to General:
	Q["g"] <= 120000-3000*p["g"];

# Demand for the student segment
subject to Student:
	Q["s"] <= 20000-1250*p["s"];

# Demand for the senior segment
subject to Senior:
	Q["r"] <= 15000-1400*p["r"];

# Maximum capacity for each segment
subject to maxCap:
	sum{i in I}Q[i] <= 55000;
	
# Minimum number of tickets sold to each segment
subject to minTickets{i in I}:
	Q[i] >= 55000*0.05;

# Minimum price for all segments	
subject to minPrice{i in I}:
	p[i] >= 6;

# Price of segment I cannot be less than twice the price of all segments
subject to maxMultiple{i in I, j in I}:
	p[i] <= 2* p[j];
	
	
option solver minos;
solve;
display Q["g"], Q["s"], Q["r"], p["g"], p["s"], p["r"];






