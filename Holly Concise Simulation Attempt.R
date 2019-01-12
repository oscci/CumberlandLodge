## the point of this simulation is to see if there is a significant difference
## between the grammatical and ungrammatical RTs and to calculate power
## t test?

# Update 12/1/19
# Previously we worked out how to simulate data in long form for different subjects and conditions.
# But the data were random numbers with a particular Gaussian distribution

# Problem is that this doesn't capture a key aspect of the study, which is that
# you expect the RTs to decline over time. It's possible that the decline only happens
# with grammatical sequences, as the sequence is learned, but it seems more likely
# that people will speed up anyhow as they get familiar with the task.

# So my solution would be to think of it like this:
# 1. For each subject there will be an intercept term, which reflects how fast they are
# at the start - it's realistic to suppose people differ on this, and this is something
# that fits with the LMM approach to analysis

# 2. We can also incorporate individual differences in overall learning rate - 
#    - this means we specify a different slope term for each subject

# 3. To capture the difference between grammatical/ungrammatical, 
#   we also specify a difference in slopes for the two conditions

# 4. And finally, we add random error to each data point

# So we will just specify RT for any given data point as the sum of:
#  Subject intercept term + Subject slope term at time t 
#      + Condition slope term + random error

# In script below I am just providing guestimates of each of these,
# but you could try different values.  If you have pilot data, that would
# give an idea of realistic values

# Your ability to detect an effect of grammaticality will depend crucially on
# how big that effect is relative to the error term

# An advantage of LMM approach is that you can estimate the subject-related
# intercept and slope effects, and take those into account, so you are in 
# effect removing unwanted variance that would otherwise mask the effect of
# interest.

# Note that, as we are now taking into account the trial number, 
# I've tweaked the simulation so that we have a loop that runs through each
# trial, rather than allocating subject, condition and RT in a block.
# This will be slower than the previous method.

# Also, this means that we need to simulate the grammatical and ungrammatical
# trials interleaved. 
# I have done this by just assuming that there is probability of 40/240 that
# any one trial will be ungrammatical. If you have a prespecified sequence
# it would be possible to use that instead.


rm(list=ls())

library(retimes) #we aren't now using this
library(tidyverse) #you may need to install this

nsims <-10 # number of simulations - for power estimate, you'd need 1000 or more
n = 10 #N subjects
ngram <- 200
nungram<-40
ntrial<-ngram+nungram

#holding vector for pvalues
pvals<-vector()
#For each run through simulation we'll compute the pvalue and add it

#First we're going to simulate a starting RT for each subject
# We'll assume these are  normally distributed, so we just specify
# a feasible value for mean and SD.  
# SD is selected bearing in mind most people score within +/- 2 SD from mean.
startRTmean <-600 
startRTsd <-80
sub.intercepts <- rnorm(n, startRTmean,startRTsd)
#- you can play with this to make it more realistic


# Now we need to specify slope for each subject
# Think of this as just corresponding to the decrease in RT on each trial.
# If someone did not learn at all, slope is zero.
# We need to think what good learning would look like.
# I worked out a plausible range for this by just thinking what would be
# the biggest effect we might see. If somebody's RT declined by 1 ms on
# every trial, and they started at 400 ms, (which would be unusually fast given
# our specified mean/SD for intercept) then by end of study they'd 
# have RT of 400-239 = 161 ms.  So this looks as if it would work: importantly, even
# the most extreme person should not get any negative values.

# So we want to specify mean and SD for the slopes to give sampled values 
# that range from 0 to 1. Again, if we assume normality, we can just use rnorm.

# The average person would have slope of .5. Most people would be within 2 sd
# of mean, so if we set SD to .24, this should give the kind of values we want.

subslopemean <- .5
subslopesd <-.24
sub.slopes <- rnorm(n, subslopemean,subslopesd)
# Note these are positive values - we'll just subtract them rather than adding,
# so they correspond to reduction in RT with time.

# Next we have to specify a term that reflects the difference in slopes 
# between conditions
# We assume no learning for ungrammatical, so slope is zero, and
# let's assume an effect for grammatical with speeding up by .2 ms for each time point
# with grammatical items.
# (NB you could specify a negative term for ungrammatical, if you think people
# may actually slow down)
cond.slopes <-c(0,.2)

# Finally, we need a term for the random error. 
# Note that the error term can be negative or positive - on any one
# trial you may randomly slow down or speed up.
# If we simulate z-scores for error, we can then just make the error 
# bigger or smaller by multiplying the z-score by a weighting factor.
# 
# The bigger the weighting we specify here, the more
# RTs will be affected by random noise. 
# We'll try a value of 30 to start with.
# Because most z-scores range from -2 to 2, this will mean that the 
# random error in RT would typically be in range -60 to 60 ms.
# We can play with this value to produce realistic looking data.


errorwt <- 30

#We now have all the terms we need to simulate RT.
# On a given trial, t, the RT for subject i in condition j will be 
#     subintercepts[i]- t*subslopes[i] - condslopes[j] + errorwt*e
#where e is a randomly selected z-score
# (Note we subtract the slope terms, because they are simulated as positive numbers
# but we want RT to decline over time)

# Set up a data frame for the simulated data in long form
# (ie subjects stacked above one another)
mydata <- data.frame(matrix(NA, nrow=n*ntrial,ncol=4))

colnames(mydata)<- c('subject','trial','type','RT')


for (mysim in 1:nsims){
  myrow <- 0 #initialise row counter
for (i in 1:n) { # this loop runs for each subject
  for (t in 1:ntrial){
    myrow<-myrow+1 #increment row counter
    mydata$subject[myrow]<-i
    mydata$trial[myrow]<-t
    mydata$type[myrow]<-1
    thiscondition <-2 #grammatical - designated as trial type 2
    myrand<-runif(1) #random number between 0 and 1, ie a probability
    if(myrand<(nungram/ntrial)){ thiscondition <- 1}#1 used to denote ungrammatical trials
      mydata$type[myrow]<-thiscondition
    #(We are using 1 and 2 as these can then be used to index cond.slopes)
      
      mydata$RT[myrow]<-sub.intercepts[i]- t*sub.slopes[i] - t*cond.slopes[thiscondition]+ errorwt*rnorm(1,0,1)
  }
}

  
#make table of means by condition for each subject
meantable<-aggregate(mydata$RT, by=list(mydata$type,mydata$subject),
                      FUN=mean, na.rm=TRUE)
colnames(meantable)<-c('Type','Subject','MeanRT')

#Check if overall mean RT differs by condition
myttest<-t.test(meantable$MeanRT~meantable$Type,paired=TRUE)

 pvals[mysim] <- myttest$p.value
} #store pvalue for this simulation and go back to simulate again

#Now count the significant pvalues
 nsignif<-length(which(pvals<0.05))
 power<-nsignif/nsims
 
print(pvals)
print(power)

#We can also plot the raw data from the first subject in the last run, 
# to get an idea of how realistic the simulation is

this.sub <- filter(mydata,subject==1)
plot(this.sub$trial,this.sub$RT,type='p',col=this.sub$type)
 
#NB If your data are like this, 
#then a linear mixed model analysis would be more appropriate, as
# it would allow you estimate the subject-specific intercepts and slopes.
