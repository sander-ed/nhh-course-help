### The BANDY Case - BAN402 - NHH ###

#----- SETS -----
set Teams; # Set of Teams
set Days; # Set of "Days" - we are considering tuesday as one day and a weekend as one day  for practicality purposes... {1,2,...24} , where odd number is a tuesday and even number is a weekend...
set Tuesdays within Days;
set Weekends within Days;
set NoGames within Days; #days in which no matches (NYE, World Champs...)

#--- PARAMETERS ---
param HomeUnav{Teams,Days} default 0; #1 if stadium of team i is unavailable to play home on day k

#----- VARIABLES -----
var x{i in Teams,j in Teams,k in Days:i<>j} binary; # 1 if team i plays a home game against team j on day d, zero otherwise
var y{i in Teams, d in Tuesdays:i<>"NTNU"} binary;# 1 if team i plays on tuesday day d and weekend day d+1

#----- OBJECTIVE FUNCTION -----
minimize fobj:
	sum{i in Teams, d in Tuesdays: i<>"NTNU"}y[i,d];


subject to
# every team plays a home game against each of the other teams
RoundRobin{i in Teams,j in Teams: i <> j}: 
	sum{d in Days}x[i,j,d] = 1;

# every team plays at most one match on each Tuesday
MatchPerTeamOnTue{i in Teams, d in Tuesdays}: 
	sum{j in Teams: i<>j}(x[i,j,d] + x[j,i,d]) <= 1;

# every team plays at most one match on weekend except NTNU
MatchPerTeamOnWEnd{i in Teams, d in Weekends: i <> 'NTNU'}: 
	sum{j in Teams: i<>j}(x[i,j,d] + x[j,i,d]) <= 1;

# NTNU may play up to 2 matches on a weekend
MatchNTNUOnWEnd{d in Weekends}: 
	sum{j in Teams: j<>'NTNU'}(x['NTNU',j,d] + x[j,'NTNU',d]) <= 2;

#If NTNU plays two matches on a weekend, both are either at home or away (so not one at home and the other away)
MatchNTNUOnWEndSameHA{d in Weekends, i in Teams, j in Teams:i<>'NTNU' and j<> 'NTNU'}: 
	x['NTNU',i,d] + x[j,'NTNU',d] <= 1;

NYEnVMNoMatches{d in NoGames}:
	sum{j in Teams, i in Teams: i<>j}(x[i,j,d])=0;

NoHomeStadiumUnav{i in Teams, d in Days: HomeUnav[i,d]=1}: #1 if stadium of team i is unavailable to play home on day k
	sum{j in Teams: i<>j}(x[i,j,d])=0;

AtLeast1HomeNov{i in Teams}:
	sum{j in Teams, d in Days: i<>j and d<=5}(x[i,j,d])>=1;

AtLeast1HomeDec{i in Teams}:
	sum{j in Teams, d in Days: i<>j and d>=6 and d<=11}(x[i,j,d])>=1;

AtLeast1HomeJan{i in Teams}:
	sum{j in Teams, d in Days: i<>j and d>=13 and d<=18}(x[i,j,d])>=1;

AtLeast1HomeFeb{i in Teams}:
	sum{j in Teams, d in Days: i<>j and d>=19 and d<=22}(x[i,j,d])>=1;

#var y{i in Teams, d in Tuesdays:i<>"NTNU"} binary;# 1 if team i plays on tuesday day d and weekend day d+1
Logical_yx{i in Teams, d in Tuesdays:i<>"NTNU"}:
	sum{j in Teams: i<>j}(x[i,j,d] + x[i,j,d+1] + x[j,i,d] + x[j,i,d+1]) <= 1 + y[i,d];

