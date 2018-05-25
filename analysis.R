library(Hmisc)
library(lme4)
library(e1071)
library(RWeka)
# library(corrgram)
# library(ordinal)

setwd('~/Dropbox/projects/advising/vinson/yelp/python_version')
setwd('~/Sites/Dropbox/projects/advising/vinson/yelp/python_version')
setwd('/Users/Dave/Dropbox/vinson_Dale/yelp/python_version')

a = read.table('results.txt',sep='\t',header=TRUE)  
a = a[a$unis>100,]
a$starsQuad = (a$stars-3)^2
a$TTR = unitypes/sqrt(unis)
attach(a)

###################################################################################################################
######################################### Linear models ###########################################################
###################################################################################################################
library(Hmisc)
library(lme4)
library(e1071)
library(RWeka)
# library(corrgram)
# library(ordinal)

setwd('~/Dropbox/projects/advising/vinson/yelp/python_version')
setwd('~/Sites/Dropbox/projects/advising/vinson/yelp/python_version')
setwd('/Users/Dave/Dropbox/Dale/yelp/python_version')

a = read.table('results.txt',sep='\t',header=TRUE) 
a = a[a$unis>100,]
m = 1
plotbystars(unis,stars,'Review length (words)',F) # length of review

dev.new(width=4, height=3); par(pin=c(4,3),mai=c(m/1.5,m/1.5,m/3,m/3),cex=.75)

plotbystars(unis,stars,'Review length (words)',F) # length of review

###############   Review length (words)  ################################################
f = lm(unis~stars)
summary(f)
pairwise.t.test(unis,stars)
aggregate((within_uni_ent-mean(within_uni_ent))/sd(within_uni_ent)~stars,a, mean)

##############   within Review Entropy (Raw)  ###########################################
within = lm(within_uni_ent~stars)
summary(within)
pairwise.t.test(within_uni_ent,stars)
aggregate((within_uni_ent-mean(within_uni_ent))/sd(within_uni_ent)~stars,a, mean)
aggregate((within_uni_ent-mean(within_uni_ent))/sd(within_uni_ent)~stars,a, sd)

##############   within Review Entropy (controlled for review length)  ###################
withinc = lm(resid(lm(within_uni_ent~unis))~stars)
summary(withinc)
pairwise.t.test(resid(lm(within_uni_ent/log(unitypes,2)~unis)),stars)
aggregate((resid(lm(within_uni_ent/log(unitypes,2)~unis))-mean(resid(lm(within_uni_ent/log(unitypes,2)~unis))))/sd(resid(lm(within_uni_ent/log(unitypes,2)~unis)))~stars,a, mean)
aggregate((resid(lm(within_uni_ent/log(unitypes,2)~unis))-mean(resid(lm(within_uni_ent/log(unitypes,2)~unis))))/sd(resid(lm(within_uni_ent/log(unitypes,2)~unis)))~stars,a, sd)

##############   Across review information (raw)  #######################################
across = lm(across_uni_info~a$starsQuad) #note the use of quadratic fitting 
summary(across)
pairwise.t.test(across_uni_info,stars) #quad fit?
aggregate((across_uni_info-mean(across_uni_info))/sd(across_uni_info)~stars,a, mean) #quad  fit?
aggregate((across_uni_info-mean(across_uni_info))/sd(across_uni_info)~stars,a, sd) #quad  fit?

##############   Across review information (controlled for review length)  ###############
acrossc = lm(resid(lm(across_uni_info~unis))~a$starsQuad)
summary(acrossc)
pairwise.t.test(resid(lm(across_uni_info~unis)),stars) #quad  fit?
aggregate((resid(lm(across_uni_info~unis))-mean(resid(lm(across_uni_info~unis))))/sd(resid(lm(across_uni_info~unis)))~stars,a, mean) #quad  fit?
aggregate((resid(lm(across_uni_info~unis))-mean(resid(lm(across_uni_info~unis))))/sd(resid(lm(across_uni_info~unis)))~stars,a, sd) #quad  fit?

##############   Average conditional information (raw)  ##################################
oneback = lm(oneback_cond_info~a$starsQuad)
summary(oneback)
pairwise.t.test(oneback_cond_info,stars) #quad  fit?
aggregate((oneback_cond_info-mean(oneback_cond_info))/sd(oneback_cond_info)~stars,a, mean) #quad  fit?
aggregate((oneback_cond_info-mean(oneback_cond_info))/sd(oneback_cond_info)~stars,a, sd) #quad  fit?

##############   Average conditional information (controlled for by review length)  ######
onebackc = lm(resid(lm(oneback_cond_info~unis+across_uni_info))~a$starsQuad) #quad fit
summary(onebackc) 
pairwise.t.test(resid(lm(oneback_cond_info~unis+across_uni_info)),stars) #quad fit?
aggregate((resid(lm(oneback_cond_info~unis+across_uni_info))-mean(resid(lm(oneback_cond_info~unis+across_uni_info))))/sd(resid(lm(oneback_cond_info~unis+across_uni_info)))~stars,a, mean) #quad fit?
aggregate((resid(lm(oneback_cond_info~unis+across_uni_info))-mean(resid(lm(oneback_cond_info~unis+across_uni_info))))/sd(resid(lm(oneback_cond_info~unis+across_uni_info)))~stars,a, sd) #quad fit?

##############   Variability conditional information   ####################################
var = lm(UIDmeasure~a$starsQuad) #quad fit
summary(var)
pairwise.t.test(UIDmeasure,stars) #quad fit?
aggregate((UIDmeasure-mean(UIDmeasure))/sd(UIDmeasure)~stars,a, mean) #quad fit?
aggregate((UIDmeasure-mean(UIDmeasure))/sd(UIDmeasure)~stars,a, sd) #quad fit?

##############   Variability conditional information controlled for review length   #######
varc = lm(resid(lm(UIDmeasure~unis+oneback_cond_info))~stars)
summary(varc)
pairwise.t.test(resid(lm(UIDmeasure~unis+oneback_cond_info)),stars)
aggregate((resid(lm(UIDmeasure~unis+oneback_cond_info))-mean(resid(lm(UIDmeasure~unis+oneback_cond_info))))/sd(resid(lm(UIDmeasure~unis+oneback_cond_info)))~stars,a, mean)
aggregate((resid(lm(UIDmeasure~unis+oneback_cond_info))-mean(resid(lm(UIDmeasure~unis+oneback_cond_info))))/sd(resid(lm(UIDmeasure~unis+oneback_cond_info)))~stars,a, sd)



###################################################################################################################
######################################### plotting #### ###########################################################
###################################################################################################################
#fun little plotting tool
par(mfrow=c(1,1))


#################### Primary Plot function ###############################
plotbystars = function(data1,stars,ylabel,zit) {
  if (zit==T) { data1z = (data1-mean(data1))/sd(data1) }
  else { data1z = data1 }
  b1 = aggregate(data1z,by=list(stars),mean)
  b2 = aggregate(data1z,by=list(stars),sd)
  b3 = aggregate(data1>-100,by=list(stars),sum)
  #plot(b1$Group.1,b1$x,type='o',pch=15)
  errbar(b1$Group.1,b1$x,yplus=b1$x+1.96*b2$x/sqrt(b3$x),yminus=b1$x-1.96*b2$x/sqrt(b3$x),ylab=ylabel,xlab='Stars',type='b')	
}

#################### Plot per analysis  ##################################
m = 1
dev.new(width=4, height=3); par(pin=c(4,3),mai=c(m/1.5,m/1.5,m/3,m/3),cex=.75)

plotbystars(unis,stars,'Review length (words)',F) # length of review
plotbystars(within_uni_ent,stars,'RI-Ent (bits / review)',F) # within review entropy
plotbystars(across_uni_info,stars,'AUI (bits / word)',F) #Across review information
plotbystars(oneback_cond_info,stars,'ACI (bits / word pair)',F) #Average Conditional information
plotbystars(UIDmeasure,stars,'CIV (SD of bits / word)',F) #Variability condition information 

plotbystars(resid(lm(within_uni_ent~unis)),stars,'Residual RI-Ent (bits / review)',F) #Corrected Variablity condition information
plotbystars(resid(lm(across_uni_info~unis)),stars,'Residual AUI (bits / word)',F) #Corrected Variablity condition information
plotbystars(resid(lm(oneback_cond_info~unis)),stars,'Residual ACI (bits / word pair)',F) #Corrected Variablity condition information
plotbystars(resid(lm(UIDmeasure~unis+oneback_cond_info)),stars,'Residual CIV (SD of bits / word)',F) #Corrected Variablity condition information


#################### Fun cool useful  ########################################
# taking residuals makes sure there are patterns remaining after the predictors are "factored" out
plotbystars(resid(lm(across_uni_info~unis+unitypes+ within_uni_ent)),round(log(fun+1)),'z-TTR',T)  #fun cool useful

#################### other Plots  ########################################
plotbystars(a$TTR,stars,'z-TTR',T)
plotbystars(a$unis,stars,'z-TTR',F)
plot(unis,unitypes)
plotbystars(resid(lm(unitypes~unis)),stars,'z-TTR',T) # factor out unis

###################################################################################################################
######################################### Modeling ################################################################
###################################################################################################################

#note - consider using starsQuad for models. 

#################### model 1 (SVM (e1071 library)) ##############################
#################################################################################
# let's train a model and see if we can predict
# dat_test = a[sample(1:dim(a)[1],5000),]
for (i in 1:3) { # get a balanced set for training
  temp = a[a$starsQuad==i,]
  if (i==1) { dat_train = temp[sample(1:dim(temp)[1],5000),] }
  else { dat_train = rbind(dat_train,temp[sample(1:dim(temp)[1],5000),])}
}
dat_train = a[sample(1:dim(a)[1],10000),] # unbalanced training set (run separately)
hist(dat_train$starsQuad)

# train/test with a basic SVM (e1071 library)
dat_train[dat_train$starsQuad==4,]$starsQuad = 4 # xtremes are 5 (1,5)
dat_train[dat_train$starsQuad==0,]$starsQuad = 0 # mids are 1 (2,3,4)
dat_train[dat_train$starsQuad==1,]$starsQuad = 1 # mids are 1 (2,3,4)

model = svm(as.factor(starsQuad)~UIDmeasure*within_uni_ent*oneback_cond_info*across_uni_info+unis,data=dat_train)
model_onlylen = svm(as.factor(starsQuad)~TTR*across_uni_info+unis*oneback_cond_info,data=dat_train) # just knowing length
preds = fitted(model)
preds_lenonly = fitted(model_onlylen) #hist(as.numeric(preds))table(preds,dat_train$stars)
mean(preds==dat_train$starsQuad) # how well it did on training material
mean(preds_lenonly==dat_train$starsQuad) 
table(dat_train$starsQuad,preds_lenonly) #didn't work? 

# test time!
results = c()
results_lenonly = c()
for (i in 1:100) {
  print(i)
  dat_test = a[sample(1:dim(a)[1],100),]
  preds = predict(model,dat_test)
  preds_lenonly = predict(model_onlylen,dat_test)
  results = c(results,mean(preds==dat_test$starsQuad)) # how well it did on testing material
  results_lenonly = c(results_lenonly,mean(preds_lenonly==dat_test$starsQuad)) # how well it did on testing material
}
h = hist(results,10,plot=F)
hlo = hist(results_lenonly,10,plot=F)
plot(h$mids,h$density,type='l',col='green')
points(hlo$mids,hlo$density,type='l',col='red')
t.test(results,results_lenonly)
table(dat_train$starsQuad,preds) #didn't work? 


#################### model 2 (Bayes (e1071 library)) ############################
#################################################################################
# train/test with basic naive bayes model (e1071 library)
model = naiveBayes(as.factor(starsQuad)~UIDmeasure+within_uni_ent+oneback_cond_info+across_uni_info+unis,data=dat_train)
model_onlylen = naiveBayes(as.factor(starsQuad)~unis,data=dat_train) # just knowing length
preds = predict(model,dat_train) #hist(as.numeric(preds)) #table(preds,dat_train$stars)
preds_lenonly = predict(model_onlylen,dat_train)
mean(preds==dat_train$starsQuad) # how well they did on training items
mean(preds_lenonly==dat_train$starsQuad)

# test time!
results = c()
results_lenonly = c()
for (i in 1:1000) {
  print(i)
  dat_test = a[sample(1:dim(a)[1],100),]
  preds = predict(model,dat_test)
  preds_lenonly = predict(model_onlylen,dat_test)
  results = c(results,mean(preds==dat_test$starsQuad)) # how well it did on testing material
  results_lenonly = c(results_lenonly,mean(preds_lenonly==dat_test$starsQuad)) # how well it did on testing material
}
h = hist(results,10,plot=F)
hlo = hist(results_lenonly,10,plot=F)
plot(h$mids,h$density,type='l',col='green')
points(hlo$mids,hlo$density,type='l',col='red')
t.test(results,results_lenonly)
table(preds_lenonly,dat_test$starsQuad)

#################### model 3 (Rweka's k-nearest neighbor algorithm)  ############
#################################################################################
# try using Rweka's k-nearest neighbors algorithm
model = IBk(as.factor(starsQuad)~UIDmeasure+within_uni_ent+oneback_cond_info+across_uni_info+unis,data=dat_train)
model_lenonly = IBk(as.factor(starsQuad)~unis,data=dat_train)
preds = predict(model,dat_train) #hist(as.numeric(preds)) #table(preds,dat_train$stars)
preds_lenonly = predict(model_onlylen,dat_train)
mean(preds==dat_train$starsQuad) # how well they did on training items
mean(preds_lenonly==dat_train$starsQuad)
# test time!
results = c()
results_lenonly = c()

for (i in 1:100) {
  print(i)
  dat_test = a[sample(1:dim(a)[1],1000),]
  dat_train[dat_train$starsQuad==4,]$starsQuad = 4 # xtremes are 5 (1,5)
  dat_train[dat_train$starsQuad==0,]$starsQuad = 0 # mids are 1 (2,3,4)
  dat_train[dat_train$starsQuad==1,]$starsQuad = 1 # mids are 1 (2,3,4)
  preds = predict(model,dat_test)
  preds_lenonly = predict(model_lenonly,dat_test)
  results = c(results,mean(preds==dat_test$starsQuad)) # how well it did on testing material
  results_lenonly = c(results_lenonly,mean(preds_lenonly==dat_test$starsQuad)) # how well it did on testing material
}
h = hist(results,10,plot=F)
hlo = hist(results_lenonly,10,plot=F)
plot(h$mids,h$density,type='l',col='green')
points(hlo$mids,hlo$density,type='l',col='red')
t.test(results,results_lenonly)

table(preds_lenonly,dat_test$starsQuad)

