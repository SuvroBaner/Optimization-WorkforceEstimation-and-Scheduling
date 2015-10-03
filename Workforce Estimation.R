############### Workforce Scheduling for Anonymous Bank Call Center ########################

# This data set is from a call center of an anonymous bank in Israel.
# Data Source : http://ie.technion.ac.il/serveng/callcenterdata/index.html
# Data Set : describes telephone data, recorded over 12 month (from 1/01/99 till 31/12/99), at the telephone call-center of “Anonymous Bank” in Israel. 
# Notes: The call center of the bank provides different services which includes-
# a) Information on and transactions of checking and saving, to bank-customers 
# b) Computer generated voice information (through VRU = Voice Response Unit) 
# c) Information for prospective customers 
# d) Support for the customers of "Anonymous Bank" web-site (internet customers) 
# The call center consitutes of 
# 8 agent positions, 1 shift-supervisor position and 5 agent positions for internet services (in an adjacent room) 
# Operation Hours:
# a) During weekdays (Sunday to Thursday), the call center is staffed from 7:00am to midnight.
# b) During weekends (Friday-Saturday), it closes at 14:00 on Friday and reopens at around 20:00 on Saturday. 
# c) The automated service (VRU) operates 7 days a week, 24 hours a day

# Data Structure
# The data archives all the calls handled by the call center, over the period of 12 months from January 1999 till December 1999. 
# The data consists of 12 files, a file per month.
# Each file consists of records (lines), a record per phone call (between 20,000 to 30,000 calls per month)
# Each record has 17 fields and they are as follows -

# vru+line : 6 digits. Each entering phone-call is first routed through a VRU (Voice Response Unit automated service)
#		There are 6 VRUs labeled AA01 to AA06, each VRU has several lines labeled 1-16.  There are a total of 65 lines.
#		Each call is assigned a VRU number and a line number. e.g. AA0101
# Call_id  : 5 digits. Each entering call is assigned a call id. Although they are different, the id’s are not necessarily
# 		consecutive due to being assigned to different VRUs. e.g. 34536
# Customer_id : 0 to 12 digits. This is the identification number of the caller, which identifies the customer uniquely.
#		 the ID is zero if the caller is not identified by the system (prospective customer) e.g. 27849181
# Priority : 1 digit. The priority is taken from an off-line file. There are two types of customers: (high-)priority and regular
#		0 and 1 indicate unidentified customers or regular customers
#		2 indicates priority customers 
#		Customers are served in the order of their "Time in Queue" (i.e. for priority 0 and 1)
#		Priority customers are allocated at the outset of their call 1.5 minutes of waiting-time (in order to advance their position in the queue.) 
#		They are also exempt from paying a NIS 7 monthly fee, which regular customers must pay. 
#		Customers have not been told about the existence of priorities. 
# Type : 	2 digits. There are 6 different types of services:
#		a) PS - regular activity (coded 'PS' for 'Peilut Shotefet') 
#		b) PE - regular activity in English (coded 'PE' for 'Peilut English') 
#		c) IN - internet consulting (coded 'IN' for 'Internet') 
#		d) NE -stock exchange activity (coded 'NE' for 'Niarot Erech') 
#		e) NW - potential customer getting information
#		f) TT – customers who left a message asking the bank to return their call but, while the systemreturned their call, the calling-agent became busy hence the customers were put on hold in the queue.
# Date : 	6 digits, year-month-day, e.g 990201
# vru_entry: 6 digits. Time that the phone-call enters the call-center (i.e. enters the VRU)
#		 e.g. 7:02:47
# vru_exit :  6 digits. Time of exit from the VRU: either to the queue, or directly to receive service, or to leave the system (abandonment). 
#		e.g. 7:02:56
# vru_time :  1 to 3 digits. Time (in seconds) spent in the VRU (calculated by exit_time – entry_time) .
#		e.g. 9
# q_start : 6 digits. Time of joining the queue (being put on “hold”).
#		This entry is 00:00:00, for customers who have not reached the queue (abandoned from the VRU). 
#		e.g. 0:00:00
# q_exit : 6 digits. Time (in seconds) of exiting the queue, either to receive service or due to abandonment. 
#		e.g. 0:00:00
# q_time :  1 to 3 digits. Time spent in queue (calculated by q_exit – q_start)
#		e.g. 0
# Outcome :  4,5 or 7 digits. There are 3 possible outcomes for each phone call: 
#		a) AGENT - service
#		b) HANG - hung up
#		c) PHANTOM - a virtual call to be ignored.
# ser_start : 6 digits. Time of beginning of service by agent. 
#		e.g. 7:02:55
# ser_exit :  6 digits. Time of end of service by agent. 
#		e.g. 7:05:41
# ser_time :  1 to 3 digits. Service duration in seconds (calculated by ser_exit – ser_start)
#		e.g. 166
# Server : text. Name of the agent who served the call. This field is NO_SERVER, if no service was provided. 
#		e.g. DORIT


install.packages("lubridate")  # date functions  
install.packages("ggplot2")  # graphics package with ribbon plot
install.packages("queueing")  # queueing functions, including Erlang C
install.packages("lpSolve")    # linear programming package

library(lubridate)
library(grid)   # graphics utilities needed for split-plotting
library(ggplot2)
library(queueing)
library(lpSolve)

## We will focus upon just one month of data, i.e. February 1999.
# The number of calls (records) in that month was 33,344 and each record has 17 variables as discussed above.

call.center.input.data = read.table("data_anonymous_bank_february.txt",
		header = TRUE, colClasses = 
		c("character", "integer", "numeric", "integer",
		"character", "character", "character", "character",
		"integer", "character", "character", "integer",
		"factor", "character", "character", "integer",
		"character"))

summary(call.center.input.data)

# Let's delete the PHANTOM calls.

a = table(call.center.input.data$outcome)
a[names(a) == 'PHANTOM'] # there are 278 PHANTOM calls. You could have also checked the summary statistics for this.

call.center.data = subset(call.center.input.data, subset = (outcome != "PHANTOM"))
b = table(call.center.data$outcome)
b[names(b) == 'PHANTOM']  # there are now 0 PHANTOM calls.

# Let's delete the negative VRU calls.

sum(call.center.data$vru_time < 0) # there are 23 negative values.

call.center.data = subset(call.center.data, subset = (vru_time >= 0))
sum(call.center.data$vru_time < 0)  # there are no negative values for the vru_time

## Now, what is the Wait time? It is the time from when the call entered VRU stayed (if)
## and moved in the queue stayed (if). So. it is the vru_time + q_time

call.center.data$wait_time = call.center.data$vru_time + call.center.data$q_time
names(call.center.data)

# Note the date is of the format, yymmdd e.g. 990201 ,() so we'll make sure it is not read as 2099
# convert date string to date variable, using 'lubridate' library.

call.center.data$date = paste("19", call.center.data$date, sep = "") 
# paste() : Concatenate vectors after converting to character. e.g. 19990201

call.center.data$date = ymd(call.center.data$date)
# ymd() : Transforms dates stored in character and numeric vectors to POSIXct objects. e.g. "1999-02-01 UTC"

# Identify day of the week; 1 = Sunday ... 7 = Saturday.

call.center.data$day_of_week = wday(call.center.data$date)  # e.g. 2 (which is Monday)
call.center.data$day_of_week = factor(call.center.data$day_of_week,
		levels = c(1:7), 
		labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Lets examine the frequency of calls by day of week.

table(call.center.data$day_of_week)

# Identify the hour of entry into the system

time.list = strsplit(call.center.data$vru_entry, ":") # e.g. "7"  "02" "47"
# strsplit() : Split the elements of a character vector x into substrings according to the matches to substring split within them.

call.hour = numeric(nrow(call.center.data))  # creates a vector call.hour with numeric value of 0

for(index.for.call in 1:nrow(call.center.data))
{
	call.hour[index.for.call] = as.numeric(time.list[[index.for.call]][1])  # the first index in time.list is the hour of entry.
}

call.center.data$call_hour = call.hour

# Check frequency of calls in the month of February by hour and day of week. (note: entire month)

with(call.center.data, table(day_of_week, call_hour))

################### Data Visualization ################

# Let's select the first week of February 1999 for data visualization and analysis.
paste(call.center.data$day_of_week[1], call.center.data$date[1], sep = " ")  # "Monday 1999-02-01"

# The first week of February starts on Monday, February 1 and ended Sunday, February 7.

selected.week = subset(call.center.data, subset = (date < ymd("19990208")))

# Check frequency of calls in the selected week by hour and day of week.

with(selected.week, table(day_of_week, call_hour))

sum(with(selected.week, table(day_of_week, call_hour))) # total number of calls in the 1st week of February i.e. 8940
dim(selected.week)[1]  # same as above, almost evenly distributed over the month.


# Loop for day of week ignoring Saturdays in Israel.

day.of.week.list = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Sunday")  # character vector

### Ribbon Plots

# Wait-time ribbon plots for the six selected days. Call upon utility function wait.time.ribbon()
# Each file goes to its own external pdf file.

source("R_utility_program_4.R")  # provides wait-time ribbon plots.
source("R_utility_program_3.R")  # privide split-plotting utilities
put.title.on.plots = TRUE  # put title on wait-time ribbon plots (it is a switch.)

for(index.day in seq(along = day.of.week.list))
{
	this.day.of.week = day.of.week.list[index.day]  # returns Monday for the first time
	pdf(file = paste("fig_operations_management_ribbon_",
	tolower(this.day.of.week),".pdf", sep = ""), width = 11, height = 8.5)
	if(put.title.on.plots)
	{
		ribbon.plot.title = paste(this.day.of.week, "Call Center Operations")  # title in each file
	}
	else
	{
		ribbon.plot.title = ""
	}
	selected.day = subset(selected.week,
		subset = (day_of_week == this.day.of.week),
		select = c("call_hour", "wait_time", "ser_time", "server"))
	
	colnames(selected.day) = c("hour", "wait", "service", "server")

	### Calling the plot (utility) function ###

	wait.time.ribbon(wait.service.data = selected.day,
		title = ribbon.plot.title,
		use.text.tagging = TRUE, wait.time.goal = 30, wait.time.max = 90,
		plotting.min = 0, plotting.max = 250)
	
	dev.off()
}


# Now the plots are created, let's discuss about these plots. Let's pick the fist one.

# title : 'Monday Call Center Operations' : this is Monday, first day of the week in Feb 1999 (1st Feb): day.of.week.list
# The title object is creatd as "ribbon.plot.title"

#  wait.time.goal:  desired maximum wait time (30 seconds default)
#                   represented as bottom of yellow region
#  wait.time.max: when wait time becomes intolerable (90 seconds default)
#                 represented as top of yellow region

# Each wait-time ribbon provides a visualization of 24 wait-time distributions, one for each hour of the day.
# The bottom of the ribbon represents the 50th percentile or median of wait times during any given hour.
# i.e. 50 % of calls fall below the bottom of the ribbon.
# The top of the ribbon represents the 90th percentile of wait times.
# i.e. 10 % of calls fall above the top of the ribbon.
# The table below each plot provides additional information like number of service operators, total calls per hour,
# calls served, and calls dropped (abandoned).

# We can see from the plots that large segments of waiting time fall above the 90 seconds of the waiting time,
# which is the maximum tolerance limit (which is second horizontal line on the ribbon.)
# This means that many callers are waiting for more than 90 seconds before being served.

# The wait-time ribbons suggest that the bank may want to schedule additional call center staff to meet its service performance goals.
# Workforce scheduling involves two modeling tasks-
# a) Estimating Workforce needs.
# b) Scheduling workers to meet those demands.

# With so much of variability in requirements, it would be beneficial for the bank to have flexible shifts and shifts with various start times.

# We will use the standard queueing models to estimate the workforce requirements 
# for each hour of the day on Wednesdays.

########### Select Wednesdays in February for the QUEUEING MODEL. ###########

wednesdays = subset(call.center.data, subset = (day_of_week == "Wednesday"))

##### Compute arrival rate of calls as calls for hour ####

table(wednesdays$call_hour)  # if any hour has zero calls it wouldn't show up. So, we cant's use this.
# Note that the number 3 is skipped.

calls.for.hour = numeric(24)  # this will have the calls per hour for Wednesdays in the whole of February.

for(index.for.hour in 1:24)
{
	coded.index.for.hour = index.for.hour - 1
	# as in the 24-hour clock the fist hour was coded as zero in the input data file, so we need to subtract 1.

	this.hour.calls = subset(wednesdays, subset = (call_hour == coded.index.for.hour))
	if(nrow(this.hour.calls) > 0)
	{
		calls.for.hour[index.for.hour] = nrow(this.hour.calls)  # number of calls came in.
	}
}

# Note for index = 3, the number of calls is 0 and so "calls.for.hour" retains its inital value of 0.

# Compute arrival rate as average number of calls into VRU per hour.

hourly.arrival.rate = calls.for.hour / 4  # four Wednesdays in February.

#### Service times can vary hour-by-hour due to differences in service requests and 
# individuals calling hour-by-hour begin ###

# Let's begin by selecting calls that receive service.

wednesdays.served = subset(wednesdays, subset = (server != "NO_SERVER"))

hourly.mean.service.time = numeric(24)

served.for.hour = numeric(24)

for(index.for.hour in 1:24)
{
	coded.index.for.hour = index.for.hour - 1
	this.hour.calls = subset(wednesdays.served, subset = (call_hour == coded.index.for.hour))
	if(nrow(this.hour.calls) > 0)
	{
		served.for.hour[index.for.hour] = nrow(this.hour.calls)  # number of calls served
		hourly.mean.service.time[index.for.hour] = mean(this.hour.calls$ser_time)  # service time per hour of the day
	}
}

# Compute the hourly service rate considering that there are 4 Wednesdays in February.

hourly.served.rate = served.for.hour / 4

# Now, let's print these two statistics.

print(hourly.arrival.rate)
print(hourly.served.rate)

# Let's build a data drame for plotting arrival and service rates.

hour = 1:24  # hour for horizontal axis of line chart.
type = rep("Arrived", length = 24)
value = hourly.arrival.rate
arrival.data.frame = data.frame(hour, value, type)

type = rep("Served", length = 24)
value = hourly.served.rate
service.data.frame = data.frame(hour, value, type)

arrival.service.data.frame = rbind(arrival.data.frame, service.data.frame)

#### Visualization : Call Center Arrival and Service Rates on Wednesdays ####

plotting.object = ggplot(data = arrival.service.data.frame,
	aes(x = hour, y = value, fill = type)) +
	geom_line() +
	geom_point(size = 4, shape = 21) +
	scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25),
		labels = 
			c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22", "24")) +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
	labs(x = "Hour of Day (24-Hour Clock)", y = "Average Calls per hour") +
	scale_fill_manual(values = c("yellow", "dark green"),
		guide = guide_legend(title = NULL)) +
	theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
	theme(legend.text = element_text(size = 15)) +
	coord_fixed(ratio = 1/10)

print(plotting.object)

# We can clearly see that there are many calls unanswered. 
# We have to apply standard queueing models to estimate the work-force requirements for each hour of the day on Wednesdays.

# Examine service times per service operator
# For hours with no service time information, use the mean as value.

hourly.mean.service.time = ifelse((hourly.mean.service.time == 0),
					mean(wednesdays.served$ser_time),
					hourly.mean.service.time)

# Compute service rate noting that there are 3600 seconds in an hour.
# adding 60 seconds to each mean service time for time between calls.
# this 60 seconds is the wrap up time or time a service agent remains unavailable to answer a new call after the call has been completed.

hourly.service.rate = 3600 / (hourly.mean.service.time + 60)  # number of calls per service operator per hour

# We observe that mean service times do not vary that much hour-by-hour
# so, we will use the mean hourly service rate in queueing calculations.

mean(hourly.service.rate)
# about 15 calls per hour as the rate for one service operator.

SERVICE.RATE = 15

########## To use the Erlang-C formula (Estimating Workforce needs) we need the following information handy:

# Call Arrival Rate (lambda) ==>> hourly.arrival.rate (calls per hour)
# Call Duration (Ts) ==>> hourly.mean.service.time  (service time per hour of the day)
# Number of Agents (m)  ==>> <To be calculated>
# Calculate the traffic intensity (u)  ==>> lambda.Ts ==>> hourly.arrival.rate*hourly.mean.service.time  ()
# Calculate Agent Occupancy (Rho)  ==>> u / m (It should be b/w 0 and 1, otherwise the agents are overloaded)
# Probability of Waiting Ec(m, u) == > 

# Now, let's assume that the bank wants no more than 50 % of the callers to wait in queue before speaking with the service agents.

# C_erlang function is placed in the queueing package.
# inputs are:
#	a) c = number of servers  ('m' as mentioned above)
#	b) r = ratio of rate of arrivals and rate of service ('u' traffic intensity as mentioned above)
# returns the probability of waiting in queue because all servers are busy.
# Let's set a target for the probability of waiting in queue to be 0.50 (per hour) or every hour of the day.

# We'll compute the number of servers needed for each hour of the day knowing the hourly arrival rate.

PROBABILITY.GOAL = 0.50

servers.needed = integer(24)   # initialize to zero for all hours of the day.

for(index.for.hour in 1:24)
{
	if(hourly.arrival.rate[index.for.hour] > 0)
	{
		erlang.probability = 1.00   # initialization, that the call will wait in the queue by default.
		# so, that it enters the loop at least once to estimate the servers needed.

		while(erlang.probability > PROBABILITY.GOAL)
		{
			servers.needed[index.for.hour] = servers.needed[index.for.hour] + 1
			# calculating the probability of waiting after having added one extra server

			erlang.probability = C_erlang(c = servers.needed[index.for.hour],
				r = hourly.arrival.rate[index.for.hour] / SERVICE.RATE)
				# Note: the unit cancels , (calls/hour / calls/hour)

		} # ending while loop for defining servers needed given probability goal
	} # end of if-block for hours with calls
} # end for-loop for the hour

# The result for the servers.needed is for every hour :
# 1  1  1  0  1  1  1  4  8  9 10  9  8 16 10 10  6  7  8  8  6  6  5  4

# As per the guidelines the bank call center will be closed from 00 through 05 hours.

servers.needed[1:6] = 0

cat("\n", "---------- Hourly Operator Requirements ----------""\n")
print(servers.needed)

# Now read in case data for the structure of call center worker shifts

bank.shifts.data.frame = read.csv("data_anonymous_bank_shifts.csv")

bank.shifts.data.frame

# Every shift is for 6 hrs. The Shift1 starts from Midnight and ends at 6am. Shift2 starts from 6am
# and in every 2 hrs a new shift starts. The last shift, Shift8 starts from 6 pm and ends at midnight.

constraint.matrix = as.matrix(bank.shifts.data.frame[, 3:10])
cat("\n", "-----------  Call Center Shift Constraint Matrix ----------", "\n")
print(constraint.matrix)

### Six-hour shift salaries in Israeli shekels
# 1 ILS = 3.92 USD in April 2015.

# The sparse matrix of shifts go as an input to the mathematical programming.
# The number 1 indicates that an hour is part of a shift, 
# and the number 0 indicates that an hour is not part of a shift.

######################### Integer Programming ###########################

# The business problem of workforce scheduling concerns scheduling workers in a way that satisfies
# resource needs while minimizing costs.
# This is a constrained optimization problem requiring an integer solution.
# We use integer programming, a type of mathematical programming, to obtain the optimal workforce schedule for the bank.

# Inputs to the program include results from the queueing model, information about workforce shifts and 
# salaries paid to workers in each shift.

# The objective is to minimize the total costs

# The constrained optimization problem is
# minimize summation CiXj (over i (hours of the day) and j (Shift Number)) subject to 
# summation AijXj >= Bi
# where Aij is the sparse matrix of the shifts v/s hour of the day and Xj is the optimal number of agents every shift
# and Bi is the optimal servers needed per hour (from the queueing model)

# The below are the shift costs (six hour shift cost) in ILS (Israeli shekels)

cost.vector = c(252, 288, 180, 180, 180, 288, 288, 288)   # total 8 shifts.
call.center.schedule = lp(const.mat = constraint.matrix,
				  const.rhs = servers.needed,
				  const.dir = rep(">=", times = 8),
				  int.vec = 1:8,
				  objective = cost.vector,
				  direction = "min")

# Now let's prepare the summary of the results for the call center problem.

ShiftID = 1:8
StartTime = c(0, 6, 8, 10, 12, 2, 4, 6)  # from midnight to 6pm i.e. start time of the shifts

StartTimeLabels = c("Midnight", "6 AM", "8 AM", "10 AM", "Noon", "2 PM", "4 PM", "6 PM")

ShiftDuration = rep(6, times = 8)

HourlyShiftSalary = c(42, 48, 30, 30, 30, 48, 48, 48) 

HourlyShiftCost = call.center.schedule$objective   # it is "six-hourly" shift salary
# it is also similar to HourlyShiftSalary * 6

Solution = call.center.schedule$solution  # it is the optimum number of call center agents needed per shift to minimize the cost.
# 0 4 8 4 4 2 1 5

# Now what is the shift cost for this optimum number of agents

ShiftCost = call.center.schedule$solution * call.center.schedule$objective


# Note the value of 'sum(ShiftCost)' should be equal to the minimum cost value obtained from the cpnstrained optimization problem.

print(sum(ShiftCost))
print(call.center.schedule)

call.center.summary = data.frame(ShiftID, StartTime, StartTimeLabels, ShiftDuration, 
					   HourlyShiftSalary, HourlyShiftCost, Solution, ShiftCost)

cat("\n\n", "Call Center Summary", "\n\n")
print(call.center.summary)

print(call.center.schedule)
cat("\n\n", "Call Center Summary Minimum Cost Solution:", sum(ShiftCost), "\n\n")

####### Build data frame for plotting the solution compared with the need

hour = 1:24

type = rep("Hourly Need", length = 24)
value = servers.needed

needs.data.frame = data.frame(hour, value, type)

type = rep("Optimal Solution", length = 24)
value = schedule.fit.to.need = constraint.matrix %*% call.center.schedule$solution   # 24 x 1 matrix will be created
# above we get the optimal solution for every hour using the matrix multiplication.

solution.data.frame = data.frame(hour, value, type)

plotting.data.frame = rbind(needs.data.frame, solution.data.frame)

##### Plotting the solution........solution to match the workforce need ######

plotting.object = ggplot(data = plotting.data.frame,
	aes(x = hour, y = value, fill = type)) +
	geom_line() +
	geom_point(size = 4, shape = 21) +
	scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25),
		labels = 
			c("00", "02", "04", "06", "08", "10", "12", "14", "16", "18", "20", "22", "24")) +
	theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
	labs(x = "Hour of Day (24-hour clock)", y = "Number of Service Operators") +
	scale_fill_manual(values = c("white", "blue"),
		guide = guide_legend(title = NULL)) +
	theme(legend.position = c(1, 1), legend.justification = c(1, 1)) +
	theme(legend.text = element_text(size = 15)) +
	coord_fixed(ratio = 2/2.25)

print(plotting.object)


