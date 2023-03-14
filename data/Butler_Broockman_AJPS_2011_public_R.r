#Set working directory
setwd("/Users/tiffanie/Documents/SOSC_132/data")

#Load data
library(foreign)
data <- read.dta("Butler_Broockman_AJPS_2011_public_dta.dta", convert.factors=F)

#################################################################
#This public data file does not include the control variables,###
#because we made a commitment to the IRB to keep the legislators#
#in the experiment anonymous. Consequently, this R file does not#
#recreate the tables in the appendices or supporting information#
#document that use these control variables.######################
#################################################################

#########
#Table 1#
#########
#race differentials:
#No primary
out.1.1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1,])
summary(out.1.1) #output gives entire first column (no partisanship signal)

#Democratic Primary
out.1.2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1,])
summary(out.1.2) #output gives entire third column (democratic signal)

#Republican Primary
out.1.3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1,])
summary(out.1.3) #output gives entire second column (republican signal)
#maybe switch this one with democratic just to match Table values

#Primary
out.1.4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1,])
summary(out.1.4) #slope gives combined effect (-0.1%) --> beta 1 * (treatment = 1) 
#why not noprimary==0 ?? or reverse?? now it feels like there's many treatments just looking at replication code

#party differentials: #add party differentials label???
###
out.1.5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1,])
summary(out.1.5) #party differential. rep signal leads to  -2.9% less likley to respond than dem to deshawn

out.1.6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1,])
summary(out.1.6) #party differential; rep signal leads to 1.1% more likely to respond than dem to jake

out.1.7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1,])
summary(out.1.7) #overall they are -0.9% likely to respond to either jake or desahwn if signal dem


#########
#Table 2# #for partisanship 
#########
#Republican Legislators
out.2.1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==1,]) #good that they distinguish between repub legislators+repub primary signal for imaginary sender
summary(out.2.1)
#-0.08144 less response to DeShawn when no partisanship signal (race diffferential)
#0.67033 for when treatment is 0 (Jake)

out.2.2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==1,])
summary(out.2.2)
#-0.05104 less response to DeShawn when Republican signal 

out.2.3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==1,])
summary(out.2.3)
#-0.04527 less response to DeShawn when Democrat signal

out.2.4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==1,])
summary(out.2.4)
#-0.04841 less response overall with primary coming up (combined effect for Repub and Democrat)

out.2.5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==1,])
summary(out.2.5)
#0.03994 party differential for those signaling republican increase in reply for DeShawn

out.2.6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==1,])
summary(out.2.6)
#0.04571 party differential for those signalingn republican increase in reply for Jake

out.2.7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==1,])
summary(out.2.7)
#0.04310 party different for those signaing republican increase in reply (combined)

#Democratic legislators
#OUT OF DEMOCRATIC legislATORS ONLY
out.2.8 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==0,])
summary(out.2.8)
#change in responsiveness to Deshawn compared to Jake when signaling no partisanship: -0.02668

out.2.9 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==0,])
summary(out.2.9)
#change in responsiveness to Deshawn compared to Jake when signaling republican: 0.00238 

out.2.10 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==0,])
summary(out.2.10)
#change in responsiveness to DeShawn compared to Jake when signaling democrat: 0.07152 
#**significant

out.2.11 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==0,])
summary(out.2.11)

#change in responsiveness to DeShawn compared to Jake when there is a partisan signal

out.2.12 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==0,])
summary(out.2.12)
#linear model of replies for the treatment rep signal, so whether or not someone has for those with primary (combined effect)

out.2.13 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==0,])
summary(out.2.13)
#linear model of replies regressed over rep signal, for jake (-1.6%)

out.2.14 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==0,])
summary(out.2.14)
#linear model of responsiveness regressed over Republican partisanship signal presence for when there is a partisan signal (combined effect)


#########
#Table 3#
#########
#White Democrats
out.3.1 <- lm(reply_atall ~ treat_deshawn, data=data[data$leg_republican==0 & data$leg_white==1 & data$treat_noprimary==1,])
summary(out.3.1)
#out of white Democrat legislators, responsiveness regressed over DeShawn treatment for those showing no partisan signal
#-0.06847 less responsiveness for DeShawn (race differential for white Democrat legs)
#Jake (W=0), 0.61157; DeShawn (W=1) 0.61157-0.06847

#Non-white Democrats
out.3.2 <- lm(reply_atall ~ treat_deshawn, data=data[data$leg_republican==0 & data$leg_notwhite==1 & data$treat_noprimary==1,])
summary(out.3.2)
#out of non-white Democrat legislators, responsiveness regressed over DeShawn treatment for those showing no partisan signal

#White Republicans
out.3.3 <- lm(reply_atall ~ treat_deshawn, data=data[data$leg_republican==1 & data$leg_white==1 & data$treat_noprimary==1,])
summary(out.3.3)
#out of white Republican legislators, responsiveness regressed over DeShawn treatment for those showing no partisan signal

#Non-white Republicans
out.3.4 <- lm(reply_atall ~ treat_deshawn, data=data[data$leg_republican==1 & data$leg_notwhite==1 & data$treat_noprimary==1,])
summary(out.3.4)
#out of Non-white Republican legislators, responsiveness regressed over DeShawn treatment for those showing no partisan signal


#############################
#Supporting Information File#
#############################
#######################
#Appendix C, Table SI2#
#######################

#Part (a): White Republicans
out.c.a1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a1)

#responsiveness regressed on DeShawn treatment, for no partisan signals and white Republican legislators
#-0.0795 less responsiveness (normally 0.66854 for Jake) -- race differential

out.c.a2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a2)
#responsiveness regressed on DeShawn treatment, for republican signal and White Republican legislators
# - 0.04590 change from Jake (race differential); slightly less than when no partisan signal, but still a decrease in responsiveness -- test for p value?

out.c.a3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a3)
#same thing as above but for Dem signal
#-0.05942 race differential; this is less than no partisanship, so pretty clear taste-based discrimination is involved?

out.c.a4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a4)
#same thing as above but for all partisan signals -0.05272 change (combined effect)

out.c.a5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a5)
#responsiveness regressed on rep signal, for DeShawn treated and white REpublicans
#increase of 0.04369 between Dem and Rep (increase if signal Rep)
#57% instead of 53%

out.c.a6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a6)
#same thing as above but for jake; 0.0317 increase in responsiveness if Jake shows rep signal
#sslightly smaller increase than for DeShawn

out.c.a7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==1 & data$leg_white==1,])
summary(out.c.a7)
#same thing but for out of all treated with signal
#give combined party differential +0.03708 if rep signal for White Republicans (unfavorable for nonpartisan?)


#Part (b): Minority Republicans
out.c.b1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b1)
#-0.3056 decrease in nonwhite Republican legislator responsiveness for nonpartisan DeShawn treated 

out.c.b2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b2)
#same as before, -0.1607 decrease for DeShawn if there is a rep signal

out.c.b3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b3)
#0.5278 increase ( a lot!!!) for DeShawn treatment for dem signal
#pvalue: 0.02945 (significant)

out.c.b4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b4)
#same but for combined party differential 

out.c.b5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b5)

out.c.b6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b6)

out.c.b7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==1 & data$leg_notwhite==1,])
summary(out.c.b7)

#Part (c): White Democrats
out.c.c1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c1)

out.c.c2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c2)

out.c.c3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c3)

out.c.c4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c4)

out.c.c5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c5)

out.c.c6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c6)

out.c.c7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_white==1,])
summary(out.c.c7)

#Part (d): Black Democrats
out.c.d1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d1)

out.c.d2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d2)

out.c.d3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d3)

out.c.d4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d4)

out.c.d5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d5)

out.c.d6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d6)

out.c.d7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_black==1,])
summary(out.c.d7)

#Part (e): Non-black Minority Democrats
out.c.e1 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_noprimary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e1)

out.c.e2 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_repprimary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e2)

out.c.e3 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_demprimary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e3)

out.c.e4 <- lm(reply_atall ~ treat_deshawn, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e4)

out.c.e5 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_deshawn==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e5)

out.c.e6 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_jake==1 & data$treat_primary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e6)

out.c.e7 <- lm(reply_atall ~ treat_repprimary, data=data[data$treat_primary==1 & data$leg_republican==0 & data$leg_notblackotherminority==1,])
summary(out.c.e7)