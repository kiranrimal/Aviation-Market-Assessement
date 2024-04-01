
# Name: Kiran Rimal
# Airlines dataset: Tickets, Flight, and Airport code


#Setting working directory
setwd("C:/Users/Lenovo/Desktop/CAPO")

#Install relevant libraries
library(tidyverse)
library(ggplot2)

#Import data 
ticket <- read.csv("Tickets.csv")      
flight <- read.csv("Flights.csv")
airport_code <- read.csv("Airport_Codes.csv")

#Data understanding:
# step A: assessement of dataset. does this data can answer the business question?
# by this step the data set were screening by its structure, how close they are each other,
#variable selection, common key variable finding to merge three dataset.

#Understand the data
glimpse(ticket)
glimpse(flight)
glimpse(airport_code)


# Step one ---------------------------------------------------------------------

# ticket data set

#Data Understanding
glimpse(ticket)   #check data structure 
names(ticket)      # name of the variable or column name

#Filter country, roundtrip, and convert fare amount to numeric. 


#filter by country "US" and round trips==1
ticket1 <- ticket %>% 
  select(ORIGIN, ORIGIN_COUNTRY, ROUNDTRIP, PASSENGERS, ITIN_FARE, DESTINATION) %>% 
  mutate(ITIN_FARE = as.numeric(ITIN_FARE)) %>%  # convert into numeric
  filter(ORIGIN_COUNTRY== "US" & ROUNDTRIP == 1)  #choose country US and rountrips


# Check data types
glimpse(ticket1)      # glimpse of data after filter 
summary(ticket1$PASSENGERS)  #descriptive statistics of passengers 
summary(ticket1$ITIN_FARE)  # descriptive stats of Fare; it has NA's and lots '0'


#Method 1: remove outliers base on IQR 
IQR(ticket1$ITIN_FARE,na.rm=TRUE) 

#Note: data is highly skewed so chose mean and SD to decide fare range. 

hist(ticket1$ITIN_FARE)

#action: have to remove zero and NA's

# missing data:-  imputation or data removal.
#decided to remove NA's as there is sufficient data

#Outliers; there is unreasonable high and low ticket price. so only the ticket
#price that is above and below the one standard deviation from mean is selected.

# Filter data base on one standard deviation upper and lower mean 
x = mean(ticket1$ITIN_FARE,na.rm=TRUE)
y = sd(ticket1$ITIN_FARE,na.rm=TRUE) 
x+y
x-y
#Upper bound = mean + standard deviation
#Lower bound = mean - standard deviation 


#count how many values are between 128 to 817
count(filter(ticket1,ITIN_FARE >= 128 & ITIN_FARE <= 817)) 


# Filter by upper and lower bound; it will automatically cutoff the zeros and NA's
ticket1 <- ticket1 %>% 
  select(ORIGIN, DESTINATION, ROUNDTRIP, ITIN_FARE) %>% 
  filter(ITIN_FARE >= 128 & ITIN_FARE <= 817) #filter data by upper and lower range value

# How much percentage data fall in 128 to 817; without removing zeros and NA's
count(filter(ticket1,ITIN_FARE >= 128 & ITIN_FARE <= 817))/nrow(ticket1)


#  Calculate number of total round trips and average fare price of routes
ticket1 <- ticket1 %>% 
  group_by (ORIGIN,DESTINATION) %>%     #group start from origin and destination of flight
  summarise(Roundtrip= sum(ROUNDTRIP, na.rm = TRUE),   # sum of the round trips per routes
            AVG.fare = mean(ITIN_FARE,na.rm = TRUE)) %>% #average fare per round flight 
  arrange(ORIGIN)    #order by origin flight

# Exploratory analysis
summary(ticket1$AVG.fare)
boxplot(ticket1$AVG.fare)

# METADATA-----------------------------------
# Roundtrip = sum of number of round trips in quarter by routes
# AVG.fare = average amount of fare for round flight by routes




# Step two----------------------------------------------------------------------

# Flight data set

# Data understanding
glimpse(flight)
names(flight)

# Select relevant variable and deselect cancelled flights
flight1 <- flight %>% 
  select(-FL_DATE,-OP_CARRIER,-ORIGIN_CITY_NAME,-DEST_AIRPORT_ID,-TAIL_NUM,-OP_CARRIER_FL_NUM,-ORIGIN_AIRPORT_ID,-DEST_CITY_NAME) %>% 
  filter(CANCELLED==0)  #there is only 0 and 1 

# Recheck data
names(flight1)
unique(flight1$CANCELLED)  #to check is there any other values rather than 0 after filter
glimpse(flight1)            #snap of dataset

# Convert "distance" into numeric types
flight1$DISTANCE <- as.numeric(flight1$DISTANCE)

# Summary of numeric variable to find out outliers and distribution
summary(flight1$DISTANCE)   #distance of flight one way
summary(flight1$ARR_DELAY)   #mean, median, SD of arrival delay

#distance were determine by the actual US domestic routes. The shortest routes is
#31 miles and longest routes is 4983 miles. Thus, between of this are actual flights miles.
#other than this is outlier. 

#I assumed that no airplane were delayed more than 3 hrs. If they delayed than have to pay large amount. 
#The US Department of Transportation imposes a fine of up to US$27,500 per passenger
#for planes left on the tarmac for more than three hours without taking off. So, 180 minutes is 
#threshold for delay and arrival. 

#To check whether these airport are in medium and largest airport list 
filter(flight1, ORIGIN=="HNL" & DESTINATION=="JFK") %>% head(10)  #longest domestic routes

filter(flight1, ORIGIN=="PSG" & DESTINATION=="WRG") %>% head(10)   #shortest domestic routes


# Defining relevant distances, arrival and departure delay. 
flight2 <- flight1 %>% 
  filter(DISTANCE >= 31 & DISTANCE <= 4983) %>%  #filter the distance by above logic
  filter(DEP_DELAY<= 180 & ARR_DELAY <= 180) #assumption delay and arival max

# Summary of select times. 
summary(flight2$ARR_DELAY)  #descriptive stat
summary(flight2$DEP_DELAY) 
max(flight2$DEP_DELAY)

# Create new variable and
flight3 <- flight2 %>% 
  mutate(delay.d.cost = ifelse(DEP_DELAY>15, (DEP_DELAY - 15)*75, 0), # if delay is less than 15 zero, otherwise $75 per minutes
         delay.a.cost = ifelse(ARR_DELAY>15, (ARR_DELAY - 15)*75, 0)) #if delay is less than 15 zero, otherwise $75 per minutes


# delay.d.cost = delay departure cost one way
# delay.a.cost = delay arrival cost one way


# Summarise by average of delay, arrival, distance, and occupancy rate 
#average departure and arrival cost is one way cost:
flight3 <- flight3 %>% 
  group_by(ORIGIN,DESTINATION) %>% 
  summarise(avg.dep.cost = mean(delay.d.cost),  #average delay cost by routes 
            avg.arr.cost = mean(delay.a.cost),  # average arrival cost by routes
            distance = mean(DISTANCE),           # average distance by routes
            occupancy = mean(OCCUPANCY_RATE))    #average occupancy rate by routes

# avg.dep.cost = average cost of departure delay by routes
# avg.arr.cost = average cost of arrival delay by routes
# distance = mean distance by routes 
# occupancy = average seat booked in flight by routes

# data understanding
glimpse(flight3)
median(flight3$delay.a.cost)




# Step three--------------------------------------------------------------------

# airport data set

# Understanding the data
names(airport_code)
glimpse(airport_code)
unique(airport_code$TYPE)

# choose only medium and large airport in the US; and rename the IATA CODE as Origin:
airport_code1 <- airport_code %>% 
  filter(TYPE %in% c("medium_airport","large_airport")) %>% #choose medium or large airport
  filter(!IATA_CODE == "") %>%   #ignore the missing blank data
  filter(ISO_COUNTRY== "US") %>%   #choose the US country
  rename(ORIGIN = IATA_CODE) %>%   # rename IATA_code to ORIGIN
  select(TYPE, ORIGIN)

# Crosscheck  whether these airport are in medium or large airport list 
filter(airport_code1,ORIGIN=="PSG") #shortest flight distance in USA
filter(airport_code1,ORIGIN=="WRG")  # PSG to WRG 

#recheck after manipulation
names(airport_code1)
glimpse(airport_code1)
unique(airport_code1$TYPE)

#airport_code1 is ready for merge. it has only to column: Types and Origin.  


# Step four---------------------------------------------------------------------

# Data Merge 


# airport code data has IATA code which is equivalent of origin and destination code for other data set
# Firstly, In airport code data set the IATA code column name change into Origin
# secondly, merged with ticket1 data by origin.
# thirdly, again, in airport code data set the Origin column name change into destination.
# fourthly, merged airport code data with previously merged data (secondly) by destination. 
# fifth: now flight data set merge with the fourthly merged data set by origin and destination.  


# step-1: ticket and airport merge by Origin
tic_air <- merge(ticket1, airport_code1, by = 'ORIGIN')
colnames (tic_air)[5] <- "type.origin"  #this will rename the airport type column. 

#  Step 2: change the colname origin to destination on airport code to make easy to merge 
#it will make sure origin and destination have medium and largest airport only. 
airport_code3 <- airport_code1  #duplicate the data
colnames (airport_code3)[2] <- "DESTINATION"  #change the colname Origin to destination

# step 3: again, ticket and airport merge by destination. 
tic_air1 <- merge(tic_air,airport_code3, by= "DESTINATION") 

#by above process the origin airport and distination airport were merged one by one and
#have assigned the type of the airport by type origin and desination so the airport charge
#can be calculated. . 


# airport and flight data merge by common variable [ origin and destination ]
final_merge <- merge(flight3,tic_air1, by= c("ORIGIN","DESTINATION"))


# Step Five---------------------------------------------------------------------

#final merge data set used

# Cost calculation; Expenses, Revenue

# Operational cost base on type of airport
final_merge <- final_merge %>% 
  mutate(op.cost1 = ifelse(type.origin == "medium_airport",5000, 10000), #if airport is medium than 5000 otherwise 10,000. 
         op.cost2 = ifelse(TYPE == "medium_airport",5000, 10000),
         total.airport.cost = op.cost1 + op.cost2)  #round trip operational cost


# op.cost1 = cost related to airport; type.origin shows the type of airport base on origin flight airport code
# op.cost2 = cost related to airport; TYPE shows only type of airport base on destination
# total.airport.cost = operational cost of airport round flights

# correlation of departure delay and arrival delay
cor(final_merge$avg.dep.cost,final_merge$avg.arr.cost)  # to check does delay in arrival is related to dealy arrival

#the correlation is strong, 94%, so if the plane is delay departure the arrival will be delay and vice versa
#same applies for return plane as well. so the expenses cost in delay will be multiply by 2 each to incorporate 
#delay in arrival and departure.


# Total cost and expenses         
final_merge <- final_merge %>% 
  mutate(FOM = distance+distance * 8,   #FOM (fuel, Oil and maintance) for round flights. so distance is twice
         DIO = distance+distance *1.18, #DIO (depreciation, Insurance, and other cost) per round
         delay.d.cost = avg.dep.cost*2, #average departure delay cost per round so multiply by 2
         delay.a.cost = avg.arr.cost*2,  # mean departure arrival cost per round 
         total.cost = FOM + DIO + delay.d.cost + delay.a.cost + total.airport.cost)

# FOM = FUEL,OIL AND MAINTANANCE, ROUND FLIGHT
# DIO = DEPRECIATION, INSURANCE, AND OTHERS, ROUND FLIGHT
# delay.d.cost = total delay cost, round flight
# delay.a.cost = total arrival cost, round flight
# total.cost = total expenses to operate the flight


# Total revenue and Profit (round flight and per flight)
final_merge1 <- final_merge %>% 
  mutate(num_passenger = occupancy*200,          # average number of passengers in flights per routes
         revenue_tckt = num_passenger*AVG.fare,  #average revenue from per routes 
         bag_fee = 70 *(num_passenger*0.50),
         total_revenue = revenue_tckt + bag_fee,
         Profit = total_revenue - total.cost) %>% 
  arrange(desc(Profit))

# num_passenger = number of passenger that took flight
# revenue_tckt = earning from ticket sales/ fare amount 
# bag_fee = earning from charging the bag fee $70 
# total_revenue = total earning from ticket sale and bag fee
# profit = amount that left after paying all expenses

#correlation
cor(final_merge1$total.cost,final_merge1$total_revenue)
cor(final_merge1$avg.dep.cost,final_merge1$avg.arr.cost)


#Analysis and decision making---------------------------------------------------


#Q1 Top 10 Busiest routes base on round trips
busy_route <- final_merge1 %>% 
  select(ORIGIN, DESTINATION, Roundtrip) %>% 
  group_by(ORIGIN) %>% 
  arrange(desc(Roundtrip)) %>% 
  head(10)

busy_route_plot <- busy_route %>%   
  unite(Routes,ORIGIN,DESTINATION,sep= "-") #join origin and destination in one column. 

#barplot of top 10 busiest routes base on roundtrip
ggplot(busy_route_plot, aes(reorder(Routes,Roundtrip), Roundtrip)) +
       geom_bar(stat = "identity",fill="#004879", alpha=.6, width=.4) +
  coord_flip()+
  geom_text(aes(label = Roundtrip), vjust = -0.5, color = "black") +
  labs(title=" Top 10 Busiest Round Trip Routes by Number of Roundtrips Flights",
       x = "Routes", y= "Total Round Trip (Q1)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20))

#having the highest round trips alone shows the prospect of the market and it also indireclty tells
# the flow of the passeneger. We can see the highest flight was range 600 to 483 per quarter. 

#Q2 top profitable routes
profitable_routes <- final_merge1 %>% 
  select(ORIGIN, DESTINATION, occupancy, total_revenue, total.cost, Roundtrip,  Profit) %>% 
  arrange(desc(Profit)) %>% 
  head(10)


profitable_routes_plot <- profitable_routes %>% 
  unite(Routes,ORIGIN,DESTINATION,sep= "-") #join origin and destination

#barplot 
ggplot(profitable_routes_plot, aes(reorder(Routes,-Profit), Profit)) +
  geom_bar(stat = "identity",fill="#D22E1E", alpha=.6, width=.4) +
  geom_text(aes(label = scales::number(Profit,accuracy=0.01)), vjust = -0.5, color = "black") +
  labs(title=" Top 10 Profitable Routes",
       x = "Routes", y = "Profit per Round Flights") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) #plot title font size and center

# Based alone with profit the above are the best routes. These routes can managed to gain higher profit
# range from about $89,400 to $77,570 per flights. This is really good profit, but if we consider other factor than
# the answer changes. 


#Q3 top 5 best routes to invest
to_invest <- final_merge1 %>% 
  select(ORIGIN, DESTINATION, Roundtrip,delay.d.cost,delay.a.cost, revenue_tckt, occupancy, Profit) %>% 
  group_by(ORIGIN) %>% 
  arrange(desc(Roundtrip)) %>% 
  head(5) %>% 
  mutate(across(c(delay.d.cost, delay.a.cost, revenue_tckt, Profit), #comma for numeric variable
              ~format(., big.mark = ",", scientific = FALSE))) 

print(to_invest)

# Top 5 routes based on future potential of revenue:
# 1: the number of flight, we can see the largerst number of flight is taken from there.
# 2: Passenger occupancy rate is just below 64 % in five routes, it mean the revenue can increase by filling the
#  filing 36% remaining seat the company earn more revenue. 
# 3: the plane were delayed in arrival and departure, which cost lots money per flight, this could be minimize. 
# as the company motto is "on time". 

# other factors cannot be predict or can't control such as baggage number, airport fee charged by FAA,
# and ticket price cannot be increase without any reason or factor  because competitive price is needed. 


options(scipen = 999) # to shows numbers without sci notation

# Q4 Breakeven numbers of flight to cover plane cost
break_even_time <- final_merge1 %>% 
  mutate(delay_cost = delay.d.cost + delay.a.cost, #total delay cost
         profit_quarter = Roundtrip * Profit, #profit per quarter 
         airplane_cost = 90000000,           #airplane upfront cost
         breakeven_flight = airplane_cost / Profit) %>%  #no of flight that breakeven plane cost
  select(ORIGIN, DESTINATION, Roundtrip, airplane_cost, delay_cost, breakeven_flight, occupancy, Profit) %>%
  group_by(ORIGIN) %>% 
  arrange(desc(Roundtrip)) %>% 
  head(5) %>% 
  mutate(across(c(delay_cost, Profit, airplane_cost, breakeven_flight ), #choosing variable for comma
                ~format(., big.mark = ",", scientific = FALSE))) # comma on numeric variable

print(break_even_time)

# In break-even, I assumed that the positive difference of total revenue and total expense will be paid to upfront aeroplane cost. \
# after paid out, there will be on break-even, it will all profit. 
# In the top 1 routes the airlines has break-even till about 5017 round flight. Likewise others routes. 
# It is also can see that there is high number of flight in ORD-LGA, and the passenger are also have 35% seat remaining. 


#Q5 Key performance indicators

KPI <- final_merge1 %>% 
  mutate(Routes = paste(ORIGIN, DESTINATION, sep = "-"),
         load_factor = occupancy,   # how many passenger it has 
         passenger_yield = AVG.fare, # average fare is genereated by one passenger
         operating_margin = (Profit/total_revenue)*100,  #percentage of revenue left after pay expenses
         CASM = total.cost/(occupancy*200)) %>%  #cost incurred by per available seat 
  select(Routes,load_factor,passenger_yield,operating_margin,CASM, Roundtrip) %>% 
  group_by(Routes) %>% 
  arrange(desc(Roundtrip)) %>%  #descending order by round trip
  head(5)   #display top 5 

print(KPI)

#future of the airlines is assess by the:
# 1. Load factor: it mean how many passenger airlines has in average and how much is left. The more load factor is best.
# 2. Passenger yield: Earning from selling ticket, the increase in price yield higher revenue.
# 3. Operating Margin: this shows how much money goes for operating. the less is better. shows company is efficient.
# 4. CASM : cost company is bearing because of the idle seat. lower the better.
# 5. Roundtrip: this shows the capacity of turnover, the higher the round trips it will earn more revenue, eventhouh
# it has less profit. Earning profit $10,000 each where the flight were 600 is better than earning $50,000 where there is only
# 2 flight in quarter. 
  
# THE END-----------------------------------------------------------------------
