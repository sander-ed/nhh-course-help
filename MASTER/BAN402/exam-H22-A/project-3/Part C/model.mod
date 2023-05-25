#----------------------------------------------------------------------------
# Maximize the social surplus on the electicity market with supply block bids. 
# Solve from the command prompt with the command:
# "ampl: include model.mod"
# Note the name of your data file must be given as input in lines 50 or 51 (do not modify other lines in this file; except for if you use Mac and the solver needs to be called as explained in line 43)
#-----------------------------------------------------------------------------

/*
 The results are presented in two text files defined by the parameters
 resfile and plotfile:
 1. The resfile contains prices and quantities for each time period and
    a list of accepted block bids (if any).
 2. The plotfile contains input data in a table that can be imported and 
    plotted with Excel.
    For example, to plot in Excel:
    a) Open the text file "Plot.txt".
    b) Select and copy the data of the period you wish to study, including the headers "Volume ... Demand linear".
    c) In Excel, paste the data.

       NOTE: Make sure that the numbers are treated as numbers by Excel,
             which might eventually not be the case.
             (In Excel you can change the decimal separators in
             tab File / Advanced / Use system separators.
             Set the separator to "." and reimport the data.)

    d) Select the data for the time period including the headers and click on tab "Insert".
    e) Insert a chart "Scatter with straight lines and markers".
    f) Right click on the chart and choose "Select data".
    g) Click on "Hidden and Empty Cells".
    h) Select "Connect data points with line". 
*/

reset;

option show_stats 1;
option solver minos; # Default solver (in some Mac systems call the solver as "./minos" instead of "minos". 

param resfile symbolic;
param plotfile symbolic;
param datafile symbolic;

let resfile := "results.txt";
let plotfile := "plot.txt";
#let datafile := "hour.dat";         # Select the input data file here for questions 1 and 2
let datafile := "blockbids_4.dat";   # Select the input data file here for question 3 (complete the data file name with 1,2,3,4,5)

#----------------------------------------------------------------------------
# Sets and parameters
#----------------------------------------------------------------------------

param no_time_periods default 2;
set T := 1..no_time_periods;

# Hourly bids
param no_demand_bids{T};
param no_supply_bids{T};
set D{t in T} := 1..no_demand_bids[t]; # Set of hourly demand bids
set S{t in T} := 1..no_supply_bids[t]; # Set of hourly supply bids

# Hourly bids - input in any order

param InputPD{t in T, D[t]};
param InputPS{t in T, S[t]};
param InputQD{t in T, D[t]};
param InputQS{t in T, S[t]};

# Hourly bids - sorted by price for use in models

param PD{t in T, D[t]}; # Price Demand (sorted descending for each t)
param PS{t in T, S[t]}; # Price Supply (sorted ascending for each t)
param QD{t in T, D[t]}; # Quantity Demand
param QS{t in T, S[t]}; # Quantity Supply 

# Block bids 

param no_block_bids default 0;
set B := 1..no_block_bids;
param Type{B} symbolic, within {'demand', 'supply'};
param Price{B};  # euro/MWh
param Volume{B}; # MWh/h
param beginPeriod{B} within T;
param endPeriod{B} within T;
set Periods{b in B} := beginPeriod[b] .. endPeriod[b];
param Accepted_order{B} default 0;
param Accepted_counter default 1;

#----------------------------------------------------------------------------
# Variables
#----------------------------------------------------------------------------

# Variables - hourly bids
var d{t in T, i in D[t]} >= 0, <= QD[t,i];
var s{t in T, i in S[t]} >= 0, <= QS[t,i];

# Variables - block bids
var d_block{b in B : Type[b] == "demand"} >= 0, <= Volume[b];
var s_block{b in B : Type[b] == "supply"} >= 0, <= Volume[b];
var block{b in B} binary;

#----------------------------------------------------------------------------
# Models 
#----------------------------------------------------------------------------

maximize Obj_Linear : 
  sum {t in T, i in D[t]} PD[t,i] * d[t,i] 
 -sum {t in T, i in S[t]} PS[t,i] * s[t,i] 

 +sum {b in B : Type[b] == "demand"} card(Periods[b]) * Price[b] * d_block[b] 
 -sum {b in B : Type[b] == "supply"} card(Periods[b]) * Price[b] * s_block[b] 
  ;

maximize Obj_Nordpool :
  sum {t in T} (
    # upper demand approx.
    PD[t,1]*d[t,1] + sum {i in 2..no_demand_bids[t]} 
      (PD[t,i-1]*d[t,i] + 0.5 * (PD[t,i]-PD[t,i-1])/QD[t,i] * d[t,i]^2)
    # lower supply approx.
   -( PS[t,1]*s[t,1] + sum {i in 2..no_supply_bids[t]} 
      (PS[t,i-1]*s[t,i] + 0.5 * (PS[t,i]-PS[t,i-1])/QS[t,i] * s[t,i]^2) )
  )

 +sum {b in B : Type[b] == "demand"} card(Periods[b]) * Price[b] * d_block[b] 
 -sum {b in B : Type[b] == "supply"} card(Periods[b]) * Price[b] * s_block[b] 

  ;

# Constraints

s.t. supply_excess {t in T} : 
   sum {i in S[t]} s[t,i]
  +sum {b in B : Type[b] == "supply" && t in Periods[b]} s_block[b] 
 = sum {i in D[t]} d[t,i]
  +sum {b in B : Type[b] == "demand" && t in Periods[b]} d_block[b]
  ;


s.t. satisfy_block_bids {b in B} :
  Volume[b]*block[b] = (if Type[b] == "demand" then d_block[b] else s_block[b])
  ;


# Problems: Define the problems "Linear" and "Nordpool"

problem Linear : # Step function objective. 
	s, d, Obj_Linear, supply_excess, 
	satisfy_block_bids, block, s_block, d_block;

problem Nordpool : # The main model with quadratic objective 
                   # (linearized step function gives a quadratic objective)
    s, d, Obj_Nordpool, supply_excess, 
    satisfy_block_bids, block, s_block, d_block;

#----------------------------------------------------------------------------
# Read data file and print some statistics
#----------------------------------------------------------------------------

data (datafile);

# Sort Input for model use
param SortMapSupply{t in T, i in S[t]} default 0;
param PosTakenSupply{t in T, i in S[t]} default 0;
for {t in T, k in S[t]} {
    let SortMapSupply[t,k] := 
        min {i in S[t] : PosTakenSupply[t,i] == 0 &&
                         InputPS[t,i] == min {j in S[t] : PosTakenSupply[t,j] == 0} InputPS[t,j]} i;
    let PosTakenSupply[t,SortMapSupply[t,k]] := 1;
}
let {t in T, i in S[t]} PS[t,i] := InputPS[t, SortMapSupply[t,i]];
let {t in T, i in S[t]} QS[t,i] := InputQS[t, SortMapSupply[t,i]];

param SortMapDemand{t in T, i in D[t]} default 0;
param PosTakenDemand{t in T, i in D[t]} default 0;
for {t in T, k in D[t]} {
    let SortMapDemand[t,k] := 
        min {i in D[t] : PosTakenDemand[t,i] == 0 && 
                         InputPD[t,i] == max {j in D[t] : PosTakenDemand[t,j] == 0} InputPD[t,j]} i;
    let PosTakenDemand[t,SortMapDemand[t,k]] := 1;
}
let {t in T, i in D[t]} PD[t,i] := InputPD[t, SortMapDemand[t,i]];
let {t in T, i in D[t]} QD[t,i] := InputQD[t, SortMapDemand[t,i]];

printf "Problem stats: \n" > (resfile);
printf "  Data filename: %s\n", datafile > (resfile);
printf "  # Block bids (supply): %i\n", card({b in B: Type[b] == "supply"}) > (resfile);
printf "  Avg # supply bids / hour: %.2f\n", sum {t in T} card(S[t]) / card(T) > (resfile);
printf "  Avg # demand bids / hour: %.2f\n\n", sum {t in T} card(D[t]) / card(T) > (resfile);

#----------------------------------------------------------------------------
# help parameters for printing and for the heuristic algorithm
#----------------------------------------------------------------------------

param indexS;
param indexD;
param temp;
param LinearSSWithoutBlock;
param StepSSWithoutBlock;
param Final_priceWithoutBlock{t in T} default 0;
param Final_quantityWithoutBlock{t in T} default 0;
param s_WithoutBlock{t in T, i in S[t]};
param d_WithoutBlock{t in T, i in D[t]};
param epsilon := 0.0001; 

param Final_price{t in T} default 0;
param Final_quantity{t in T} default 0;

param average_price default 0;
param best_supply_bid within B;
param best_supply_bid_price;

#----------------------------------------------------------------------------
# Nordpool heuristic with quadratic objective function
#----------------------------------------------------------------------------

let Accepted_counter := 1;
problem Nordpool;
fix {b in B} block[b] := 0;

repeat {

    solve Nordpool;

    # Calculate prices (find the intersection)
    for {t in T} {
        let indexS := max {i in S[t] : s[t, i] > epsilon} i;
        let indexD := max {i in D[t] : d[t, i] > epsilon} i;
        if(indexS > 1 and s[t, indexS] < QS[t, indexS] - epsilon) then 
            let Final_price[t] := PS[t, indexS-1] 
                + s[t, indexS] / QS[t,indexS] * (PS[t,indexS]-PS[t,indexS-1]);
        else if (indexD > 1 and d[t, indexD] < QD[t, indexD] - epsilon) then 
            let Final_price[t] := PD[t, indexD-1] 
                + d[t, indexD] / QD[t,indexD] * (PD[t,indexD]-PD[t,indexD-1]);
        else {
            print "Exception raised...";
            break;
        }
    }

    # Save the solution if it's the first round (ie without block bids)
    if(card{b in B : block[b] >0} == 0 ) then {
        let {t in T} Final_priceWithoutBlock[t] := Final_price[t];

        # Save the objective function value (it's trashed when solving Linear)
        let LinearSSWithoutBlock := Obj_Nordpool;

        # Find the corresponding step function value
        problem Linear;
        fix {b in B} block[b] := 0;
        fix {t in T, i in D[t] : PD[t,i] <= Final_price[t] - epsilon} d[t,i] := 0;
        fix {t in T, i in S[t] : PS[t,i] >= Final_price[t] + epsilon} s[t,i] := 0;
        solve Linear;
        let {t in T, i in S[t]} s_WithoutBlock[t,i] := s[t,i];
        let {t in T, i in D[t]} d_WithoutBlock[t,i] := d[t,i];
        let StepSSWithoutBlock := Obj_Linear;
        let {t in T} Final_quantityWithoutBlock[t] := sum {i in D[t]} d[t,i];
        unfix d;
        unfix s;
        unfix block;
        problem Nordpool;
    }

    let best_supply_bid_price := Obj_Nordpool * card(T);

    for {b in B : Type[b] == "supply" && block[b] == 0} {
        # b is a candidate supply bid if it has a 
        # lower hourly price than the average price over the block periods.
        # Update only if it's a lower priced block bid than the previously
        # (in this loop) accepted bid. (We choose the first bid in
        # an ascending list of block bids.)
        let average_price := sum {t in Periods[b]} Final_price[t] / card(Periods[b]);
        if(Price[b] < average_price and Price[b] < best_supply_bid_price) then {
            let best_supply_bid_price := Price[b];
            let best_supply_bid := b;
        }
    } 

    # Accept a new block bid?
    if(best_supply_bid_price < Obj_Nordpool * card(T)) then {
        fix block[best_supply_bid] := 1;
        print "accepting supply block bid ", best_supply_bid;
        let Accepted_order[best_supply_bid] := Accepted_counter;
        let Accepted_counter := Accepted_counter + 1;
    }

} while (best_supply_bid_price < Obj_Nordpool * card(T)); 

# Save the objective function value (it's trashed when solving Linear)
let temp := Obj_Nordpool;

# Find the corresponding step function value
problem Linear;
fix {b in B} block[b];
fix {t in T, i in D[t] : PD[t,i] <= Final_price[t] - epsilon} d[t,i] := 0;
fix {t in T, i in S[t] : PS[t,i] >= Final_price[t] + epsilon} s[t,i] := 0;
solve Linear;

# Set the actual quantity sold at the "final_price"
let {t in T} Final_quantity[t] := sum {i in S[t]} s[t,i]
        +sum {b in B : Type[b] == "supply" && t in Periods[b]} s_block[b];

#----------------------------------------------------------------------------
# Print solution
#----------------------------------------------------------------------------

printf "Without block bids:\n" > (resfile);

printf "  Linearized social surplus = %.2f\n", LinearSSWithoutBlock  > (resfile);
printf "  Step function social surplus = %.2f\n", StepSSWithoutBlock > (resfile);


# Table Header
printf "%4s%10s%10s%10s%10s%10s%10s\n", "hour", "Price", "Volume","PS", "PD", "s", "d" > (resfile);

# Hourly prices
for {t in T} {
    let indexS := max {i in S[t] : s_WithoutBlock[t, i] > epsilon} i;
    let indexD := max {i in D[t] : d_WithoutBlock[t, i] > epsilon} i;
    printf "%4i%10.2f%10.2f%10.2f%10.2f",
        t,Final_priceWithoutBlock[t], Final_quantityWithoutBlock[t], 
        PS[t,indexS],PD[t,indexD] > (resfile);    
    printf "%10.2f%10.2f\n", s_WithoutBlock[t,indexS], 
        d_WithoutBlock[t,indexD] > (resfile);
}

    printf "\nWhere \n\n" > (resfile);
    print "  Price: The intersection of the linearized step functions." > (resfile);
    print "  Volume: The total trade volume." > (resfile);
    print "  PS / PD: The price of the highest / lowest accepted hourly bids for supply / demand." > (resfile);
    print "  s / d: The quantities for the accepted limiting hourly bids." > (resfile);

if(card({b in B : Type[b] == "supply" && block[b] > epsilon}) != 0) then {
    printf "\nWith block bids:\n" > (resfile);

    printf "  Linearized social surplus = %.2f\n", temp  > (resfile);
    printf "  Step function social surplus = %.2f\n", Obj_Linear > (resfile);


    # Table Header
    printf "%4s%10s%10s%10s%10s%10s%10s\n", "hour", "Price", "Volume","PS", "PD", "s", "d" > (resfile);

    # Hourly prices
    for {t in T} {
        let indexS := max {i in S[t] : s[t, i] > epsilon} i;
        let indexD := max {i in D[t] : d[t, i] > epsilon} i;
        printf "%4i%10.2f%10.2f%10.2f%10.2f",
            t,Final_price[t], Final_quantity[t], PS[t,indexS],PD[t,indexD] > (resfile);    
        printf "%10.2f%10.2f\n", s[t,indexS], d[t,indexD] > (resfile);
    }

    printf "\nWhere \n\n" > (resfile);
    print "  Price: The intersection of the linearized step functions." > (resfile);
    print "  Volume: The total trade volume." > (resfile);
    print "  PS / PD: The price of the highest / lowest accepted hourly bids for supply / demand." > (resfile);
    print "  s / d: The quantities for the accepted limiting hourly bids." > (resfile);

    # Block bids
    printf "\nAccepted supply block bids: \n" > (resfile);
    printf "%10s%10s%6s%10s%6s%10s\n", 
        "Price", "Volume", "id", "begin", "end", "order" > (resfile);
    for {b in B : Type[b] == "supply" && block[b] > epsilon} {
        printf "%10.2f%10.2f%6i%10i%6i%10s\n", 
            Price[b], s_block[b], b, beginPeriod[b], endPeriod[b],
            Accepted_order[b] > (resfile);
    }
}

# Plot file
for {t in T} {
    printf "Time period %i\n", t > (plotfile);
    printf "Volume\tSupply\tDemand\tSupply linear\tDemand linear\n" > (plotfile);
    for {i in 1 .. max(no_supply_bids[t], no_demand_bids[t])} {
        if(i <= no_supply_bids[t]) then {
            printf "%.2f\t%.2f\n%.2f\t%.2f\t\t%.2f\n", 
                sum {j in 1..i-1} QS[t,j], PS[t,i],
                sum {j in 1..i} QS[t,j], PS[t,i], PS[t,i] > (plotfile);
        }
        if(i <= no_demand_bids[t]) then {
            printf "%.2f\t\t%.2f\n%.2f\t\t%.2f\t\t%.2f\n", 
                sum {j in 1..i-1} QD[t,j], PD[t,i],
                sum {j in 1..i} QD[t,j], PD[t,i], PD[t,i] > (plotfile);
        }
    }
    print > (plotfile);
}