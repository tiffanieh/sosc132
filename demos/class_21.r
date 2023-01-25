#' ---
#' title: "In-class 2.1, Social Science Inquiry II (SOSC13200-W23-2)"
#' author: "Molly Offer-Westort"
#' date: "Tuesday 1/11/22"
#' ---

#' ## Reading in the data 
#' You can read in data locally
getwd()
dat <- read.csv("../data/card-krueger.csv", as.is = TRUE)

#' OR you can give the file address as a location on the internet. 
#' Here it's listed from the location on the class GitHub repository. 
file <- "https://raw.githubusercontent.com/UChicago-pol-methods/SOSC13200-W23/main/data/card-krueger.csv"
dat1 <- read.csv(file, as.is = TRUE)

identical(dat, dat1) # are the files identical?

rm(dat1) # remove the extra data set, we only want one. 


#' ## Summarizing the data set 

head(dat) # check out the data. 

dim(dat) #dimensions
names(dat) #names of column titles
str(dat) #structure of dataframe shown, including type of data value
summary(dat) #summary for each column category

#' What is the unit of observation?
#' individual restaurant


#' ## Indexing, vector as element of data.frame.

#' Extract a named vector from a data.frame. --> dataframe.name$vector.name
dat$id
str(dat$id) #summarizes internal structure of dat$id
length(dat$id) #length of vector

#' Extract a column from a data frame by index number. [row,column]
dat[,1]

str(dat[,1])

#' Extract a *row* from a data frame by index number; is this a vector? No.
dat[1,]

str(dat[1,])

#' What happened?
#' For a column, str(dat[,1]) describes the structure as having a length of 820 obs w/ same data type, whereas for a row, we get to see what each vector is for that particular row (only 1 observation, but there are 27 variables)

#' Index elements within a vector by index number.
dat$id[410]

dat$id[11:20]

#' Modify elements within a vector by index number.
id2 <- dat$id # recall the assignment operator, <-
identical(dat$id, id2)

id2[2] <- 99999 #change so that 99999 is stored in 2nd element of vector id2

head(id2)
head(dat$id)

identical(dat$id, id2)

# Getting index using `which()` and logicals
# format: which(vector.name == valueOfVector)
which(id2==99999)

id2[which(id2==99999)] #check by indexing that vector you found with which()


which(id2>400)
id2[which(id2>400)]


which(id2>400 & id2 < 410) #& used as and statement
id2[which(id2>400 & id2 < 410)]
id2[id2>400 & id2 < 410]

# end class 1 here

#' Using table(), sum(), mean(), and other summary functions

table(dat$d) #makes table out of frequencies of values in vector dat$d
sum(dat$d) #take sum of every value in vector
mean(dat$d) # mean of binary as percentage

table(dat$bk)
sum(dat$bk)
mean(dat$bk)


head(dat$mgrs)
table(dat$mgrs)
mean(dat$mgrs) #NA --> first we need to take out empty values

mean(dat$mgrs, na.rm = TRUE) #remove empty values
class(dat$mgrs) #data value type

mean(c(1,1,3,4,59))

mean(c(1,1,3,4,'59')) #can't calculate when data value type is not the same
class(c(1,1,3,4,'59')) #returns everything as a character if it detects a character
c(1,1,3,4,'59') #returns everything as characters
#coercion: character, complex, numeric, integer, logical

round(mean(dat$mgrs, na.rm = TRUE), 2) #round(argument, # of decimals)
?round


median(dat$mgrs, na.rm = TRUE)

max(dat$mgrs, na.rm = TRUE)

min(dat$mgrs, na.rm = TRUE)

quantile(dat$mgrs, na.rm = TRUE) #probability
summary(dat$mgrs)
quantile(dat$mgrs, c(0.025, 0.995), na.rm = TRUE) #put in probability -- quantile(vector, prob, na.rm=TRUE)

plot(ecdf(dat$mgrs)) #empirical cumulative distribution function

#' Checking mean against a hand-calculated value
mean(dat$mgrs, na.rm = TRUE) == sum(dat$mgrs, na.rm = TRUE)/length(dat$mgrs)

sum(dat$mgrs, na.rm = TRUE)/length(dat$mgrs)

?is.na
is.na(dat$mgrs) #tells us which elements are missing
table(is.na(dat$mgrs)) #sorts into table based on frequency of T/F for missingness

#' Variance and standard deviation
var(dat$d) #variance
sum( (dat$d - mean(dat$d))^2 /(length(dat$d)-1)) #by hand

sd(dat$d) #SD
sqrt(var(dat$d))

#' Calculating full time equivalent employment

#full time employment equvialent = full-time employees + managers + part-time employees/2 (2 of them = 1 full-time)
dat$fte2 <- dat$ft + dat$mgrs + dat$pt/2
summary(dat$fte2)
summary(dat$fte)

identical(dat$fte, dat$fte2)

names(dat)


#' What if we want to get the number of managers and assistant managers, just in
#' PA? 
pa.mgrs <- sum(dat$mgrs[which(dat$nj==0)],na.rm=TRUE) #560

#' Just in NJ?
nj.mgrs <- sum(dat$mgrs[which(dat$nj==1)],na.rm=TRUE) #2229.3

#' Just in NJ in Wave 1?
nj.mgrs1 <- sum(dat$mgrs[which(dat$d_nj==1)],na.rm=TRUE) #1123.5
#alternatively: sum(dat$mgrs[which(dat$nj==1&dat$d==1)],na.rm=TRUE)



#' Recreating table 1

nrow(dat[which(dat$d == 1),])
#number of rows in indexed values for wave 2 (dat$d == 1)
#unit of observation is individual store --> total # stores in wave 2

table(dat$nj[which(dat$d == 1)])
#freq table for nj data (if 0/not nj or 1/nj) for stores in wave 2
#shows in wave 2, freq for 0 is 79 (pa), for 1 is 331 (nj)

table(dat$status[which(dat$d == 1)]) # see readme
#freq table for status data for all indeces (stores) in Wave 2
  # 0 = refused second interview (count = 1)
  # 1 = answered 2nd interview (count = 399)
  # 2 = closed for renovations (count = 2)
  # 3 = closed "permanently" (count = 6)
  # 4 = closed for highway construction (count = 1)
  # 5 = closed due to Mall fire (count = 1)

mean(dat$status[which(dat$d == 1)]) # why might we not want to take the mean?
#we should treat these numbers as categories rather than numeric values that we perform calculations on b/c there is no ordinal/numeric meaning to the numbers assigned to each meaning

table(dat$status[which(dat$d == 1)], dat$nj[which(dat$d == 1)])
#freq table given stores are in wave 2: for status data (column 1), nj data (column 2)

table(dat$type[which(dat$d ==1)])
#freq table given stores are in wave 2 showing type of 2nd interview
#1 (phone) -- 371, 2 (personal) -- 39


#' ## Exercises
#' 

#' What percent of stores were successfully interviewed by phone in round 2?
prop.2nd <- nrow(dat[which(dat$d==1&dat$status==1&dat$type==1),])/nrow(dat[which(dat$d==1),])
#' Multiply this number by 100, and round to two decimal places. 
perc.2nd <- round(prop.2nd*100,2) #371/410 stores --> 90.49%
perc.2nd

#' (I.e., result should be in the format ##.## ).
#' 
#' What was the average wage at Wendy's in New Jersey in Wave 1? In Wave 2?
avg.wendys.wage1 <- mean(dat$wage[which(dat$wendys==1 & dat$nj==1 & dat$d==0)],na.rm=TRUE)
avg.wendys.wage2 <- mean(dat$wage[which(dat$wendys==1 & dat$nj==1 & dat$d==1)],na.rm=TRUE)

#' What is the difference between the two?
diff.wendys.wage <- avg.wendys.wage2-avg.wendys.wage1
diff.wendys.wage #difference: ~$0.33 increase

#' 
#' How many hours are KFC's open in PA, on average? Does this differ from 
#' Burger Kings?
hrs.kfc.pa <- mean(dat$hrsopen[which(dat$nj==0&dat$kfc==1)],na.rm=TRUE)
#10.90 hours on average KFC
hrs.kfc.bk <- mean(dat$hrsopen[which(dat$nj==0&dat$bk==1)],na.rm=TRUE)
#16.99 hours on average BK
diff.bk.hrs <- abs(hrs.kfc.bk-hrs.kfc.pa)
diff.bk.hrs #difference: ~6.09 hours

#' 
#' Which variable has the most missing data? For this variable, how many 
#' observations are missing for NJ, vs PA? 
#' 
var.miss <- which.max(colSums(is.na(dat))) #ncalls -- col 22
num.miss <- sum(is.na(dat[,var.miss])) # 249 missing data
nj.miss <- sum(is.na(dat$ncalls[which(dat$nj==1)])) #202 obs missing
pa.miss <- sum(is.na(dat$ncalls[which(dat$nj==0)])) #47 obs missing

#' 
#' 

