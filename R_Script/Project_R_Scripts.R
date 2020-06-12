'''
RESAMPLING METHODS PROJECT - COMPLEX DATA ANALYSIS MatCad

  Xabier Oyanguren Asua 1456628

'''
# Set the Wroking Directory to the project folder
setwd("/home/melanie/Desktop/MatCAD/AnalisisDatosComplejos/FinalProject")

'''
IMPORT DATA using readxl-------------------------------------------------------
'''
library(readxl)
# The worlds best 100.000 scientist dataset conatining h-index, citation number etc at the end of 2018
best_scientists<-read_excel("Best_100000_scientists.xlsx", sheet="Career")

# For the Physics Laureates, their h-index the year the prize was given to them (from 1990 to 2018)
hindexPhysTime<-read_excel("Nobel_hIndex_Physics - Years.xlsx")
# same for Chemistry Laureates
hindexChemTime<-read_excel("Nobel_hIndex_Chemistry - Years.xlsx")

# For Physics Laureates awarded from 2000 to 2010, the h-index they had when they published the award winning paper, 
#  their h-inbex the year they received the Nobel, and their h-index 10 years after
hindex3Phys<-read_excel("Nobel_hIndex_Physics.xlsx")
# same for Chemistry Laureates
hindex3Chem<-read_excel("Nobel_hIndex_Chemistry.xlsx")

keys<-read_excel("Best_100000_scientists.xlsx", sheet="Key")

# the extra data to check the model
extra<-read_excel("ExtraPrediction.xlsx")

'''
TEST A: Do we really need to care about both the h-index and the number of citations?

The problem with only regarding h-index could be that it is blind about few papers with lots of citations.
But still, let us check that among top quality scientists both quantities are positively and pretty linearly correlated.
If this is true, then in order to check the weight of the contribution of a scientists so far, we would have enough to perform the analysis
on the h-index instead of having to take them both into account.

Denoting by (h_i, c_i) the pair of h-index and total number of citations of a researcher i, s.t. h_i <- H; c_i <- C

H_0: H, C are NOT CORRELATED
H_1: H, C are MONOTONICALLY POSITIVELY CORRELATED

In order to check this, well take all the 100.000 researchers on the best_scientist dataset and perform a 
CORRELATION PERMUTATION TEST using two different statistcs:
- On the one hand well check the more generla Spearman Correlation Statistic, whcih will assert whether the variables are
monotonically correlated or not (when one increases the other does as well, and when one decreases the other too). It does
not require the relation to be linear, which in our case is not necessary, as we only want to prove that it is enough to use
the h-index, we dont want to further study the linear or non-linear model that fits the relation

- Even still, well check the Pearson correlation statistic as well, to see if they are actually linearly correlated (which
would give even more weight to the conclusion). If the relation was non-linear then depending on the regime tal

For both cases well perform an approximate test using 10000 rearrangements of h (we cant perform a complete permutation test for 105000 entries)

'''
h<-best_scientists$`h18 (ns)`
c<-best_scientists$`nc9618 (ns)` 

plot(c,h, xlab='Total number of citations', ylab='h-index') #PLOTIEU

n<-length(h)

rearrangement_number<-100000

piersonRear<-numeric(rearrangement_number)
spearmanRear<-numeric(rearrangement_number)

originalPierson<-cor(c, h, method="pearson") # 0.844244
originalSpearman<-cor(c, h, method="spearman") # 0.9076

piersonCount<-0
spearmanCount<-0

for(i in 1:rearrangement_number){
  rear<-sample(h, n)
  piersonRear[i]<-cor(c, rear, method="pearson")
  spearmanRear[i]<-cor(c, rear, method="spearman")
  if(piersonRear[i]>=originalPierson){ piersonCount=piersonCount+1}
  if(spearmanRear[i]>=originalSpearman){ spearmanCount=spearmanCount+1}
}

hist(spearmanRear, xlab="Spearman Correlation Coefficient", main="10.000 permutations (Spearman)") #plotieu
spearmanCount/rearrangement_number #Non of the rearrangements gives a superior R -> p-value=0

hist(piersonRear,  xlab="Pearson Correlation Coefficient", main="10.000 permutations (Pearson)") #plotieu
piersonCount/rearrangement_number # p-value=0

'''
We reject the null hypothesis in both cases, which means, we could even consider there is a linear correlation.
We have a positive monotonical correlation!
'''

h_c_model<-lm(h~c)
summary(h_c_model) #we get very significative slope b1 for h = b0 +b1*c of 1.466e-3

# even though, R-squared is 0.7127 which is not truly big. However, the goal of the test has been asserted. We can 
# proceed only using h-index.

'''
TEST B.1: Has there been any bias towards higher h-index candidates to be given the Nobel prize?

Well now test using a randomness permutation test for time series, whether h-indices (their implications) appear to matter
more now than 20 years ago in order to choose a Nobel Laureate in Physics or Chemistry.

First of all, we acknowledge that each year more than one person can be awarded with a Nobel, thus, in order to
have a proper time series, well need to somehow unify the h-indices of all the laureates of a same year. We will
consider the mean of each year.
'''
hYearPhysTable<-data.frame(hindexPhysTime)
hYearPhys<-aggregate(hYearPhysTable[,3], list(hindexPhysTime$`Prize Year`), mean)
plot(hYearPhys, xlab="Prize Year", ylab="Mean h-index of laureates", main="Physics Laureates") #plotieeu -> OUTLIER???
acf(hYearPhys[2], main="Physics Laureates", xlab="order") # a priori it doesnt seem to be any autocorrelation

hYearChemTable<-data.frame(hindexChemTime)
hYearChem<-aggregate(hYearChemTable[,3], list(hindexChemTime$`Prize Year`), mean)
plot(hYearChem, xlab="Prize Year", ylab="Mean h-index of laureates", main="Chemistry Laureates") #plotieeu -> OUTLIER???
acf(hYearChem[2], main="Chemistry Laureates", xlab="order") # a priori it doesnt seem to be any autocorrelation

'''
Physics
-------
Given n observations
H_0: Observations follow a white noise of the form H_i=mu + Z_i with _i
H_1: There is a temporal correlation of any of the orders in (1,2,3,4,5)

Well perform an approximate permutation test (100.000 rears)
'''
n<-length(hYearPhys$Group.1)
rearrangement_number<-100000
acTrue<-acf(hYearPhys[2])
ac1<-numeric(rearrangement_number)
ac2<-numeric(rearrangement_number)
ac3<-numeric(rearrangement_number)
ac4<-numeric(rearrangement_number)
ac5<-numeric(rearrangement_number)
ac6<-numeric(rearrangement_number)
for(i in 1:rearrangement_number){
  rear<-sample(hYearPhys$x, n)
  acfRear<-acf(rear, plot=FALSE)
  ac1[i]<-acfRear$acf[2]
  ac2[i]<-acfRear$acf[3]
  ac3[i]<-acfRear$acf[4]
  ac4[i]<-acfRear$acf[5]
  ac5[i]<-acfRear$acf[6]
  ac6[i]<-acfRear$acf[7]
}
sum(ac1<=acTrue$acf[2])/rearrangement_number # 0,33924
sum(ac2<=acTrue$acf[3])/rearrangement_number # 0,50888
sum(ac3<=acTrue$acf[4])/rearrangement_number # 0,10974
sum(ac4<=acTrue$acf[5])/rearrangement_number # 0,0631
sum(ac5>=acTrue$acf[6])/rearrangement_number # 0,13116
sum(ac6>=acTrue$acf[7])/rearrangement_number # 0.12633
'''
We clearly cannot reject the null hypothesis, therefore, we conclude that the tendecy was only appaerent
in the case of physics laureates in the last 30 years
What would happen if we removed the apparent outlier in 1991?
'''
n<-length(hYearPhys$Group.1)-1
rearrangement_number<-100000
h_noOutlier<-hYearPhys[2]
h_noOutlier<-h_noOutlier[h_noOutlier!=95]
acTrue<-acf(h_noOutlier)
ac1<-numeric(rearrangement_number)
ac2<-numeric(rearrangement_number)
ac3<-numeric(rearrangement_number)
ac4<-numeric(rearrangement_number)
ac5<-numeric(rearrangement_number)
ac6<-numeric(rearrangement_number)
for(i in 1:rearrangement_number){
  rear<-sample(h_noOutlier, n)
  acfRear<-acf(rear, plot=FALSE)
  ac1[i]<-acfRear$acf[2]
  ac2[i]<-acfRear$acf[3]
  ac3[i]<-acfRear$acf[4]
  ac4[i]<-acfRear$acf[5]
  ac5[i]<-acfRear$acf[6]
  ac6[i]<-acfRear$acf[7]
}
sum(ac1>=acTrue$acf[2])/rearrangement_number # 0.2805
sum(ac2>=acTrue$acf[3])/rearrangement_number # 0.5318
sum(ac3>=acTrue$acf[4])/rearrangement_number # 0.8267
sum(ac4>=acTrue$acf[5])/rearrangement_number # 0.976
sum(ac5>=acTrue$acf[6])/rearrangement_number # 0.0086
sum(ac6>=acTrue$acf[7])/rearrangement_number # 0.2389

'''
If we removed the outlier, there appears to be a fifth order autocorrelation!
In any case, lets see what happens with the chemistry (berdine badan igual los ke eligen el nobel)
'''

'''
Chemistry
-------
Given n observations
H_0: Observations follow a white noise of the form H_i=mu + Z_i with _i
H_1: There is a temporal correlation of any of the orders in (1,2,3,4,5)

Well perform an approximate permutation test (100.000 rears)
'''
n<-length(hYearChem$Group.1)
rearrangement_number<-100000
acTrue<-acf(hYearChem[2])
ac1<-numeric(rearrangement_number)
ac2<-numeric(rearrangement_number)
ac3<-numeric(rearrangement_number)
ac4<-numeric(rearrangement_number)
ac5<-numeric(rearrangement_number)
ac6<-numeric(rearrangement_number)
for(i in 1:rearrangement_number){
  rear<-sample(hYearChem$x, n)
  acfRear<-acf(rear, plot=FALSE)
  ac1[i]<-acfRear$acf[2]
  ac2[i]<-acfRear$acf[3]
  ac3[i]<-acfRear$acf[4]
  ac4[i]<-acfRear$acf[5]
  ac5[i]<-acfRear$acf[6]
  ac6[i]<-acfRear$acf[7]
}
sum(ac1<=acTrue$acf[2])/rearrangement_number # 0,42825
sum(ac2<=acTrue$acf[3])/rearrangement_number # 0.57683
sum(ac3<=acTrue$acf[4])/rearrangement_number # 0,07156
sum(ac4<=acTrue$acf[5])/rearrangement_number # 0,36967
sum(ac5<=acTrue$acf[6])/rearrangement_number # 0,22954
sum(ac6<=acTrue$acf[7])/rearrangement_number # 0,33354

'''
In any case, it seems that we cannot build any model such that we can predict the mean h-index of the nobel
prize winners of the next year. A pitty. The point is that we now see that the h-index of the winner appears
to be random around some central value. Even if this is true, we could still try to see whether there is
a pattern between the difference in h-index from the awarded paper was published and the h-index in the date of 
award, or even 10 years later the award (to see if there is some regular boost researchers gain due to the 
Nobel award). we could begin by looking if there is a pattern in the time they wait until their discovery papers
are awarded.

h-index of the nobels does not seem to be a year dependent phenomenom, thus, we conclude that in what follows, we can
safely, mix the different year nobels.

TEST C: The h-index of differnet disciplines are different-> we have two groups
H_0: E(h_physics)>=E(h_chemestry)
H_1: E(h_physics)<E(h_chemistry)

If we perform a Shapiro-Wilk normality test (which was found by Monte Carlo algorithms to be more significative than a
typical Kolmogorov Smirnov Test), we find:
'''
hist(hYearPhysTable$h.index.Prize.Year, xlab="h-index in Prize Year", main="Physics Laureates", breaks=10)
shapiro.test(hYearPhysTable$h.index.Prize.Year)
#pvalue 0.4

hist(hYearChemTable$h.index.Prize.Year, xlab="h-index in Prize Year", main="Chemistry Laureates", breaks=10)
shapiro.test(hYearChemTable$h.index.Prize.Year)
# pvalue 0.1549
'''
Clearly, we see that we can safely assume that data proceed from normal distributions.Thus, we
can safely assume that the h-indices follow a normal distribution in each group (72 obs in physics and 
 in chemistry). Thus, we could perform a Parametric Bootstrap Test to compare the means, assuming they 
follow a normal distribution (by the CLT) s.t
H_phys ~ N(mean(h_phys), var(h_phys))
H_chem ~ N(mean(h_chem), var(h_chem))
which we could justify by a KS test
'''
n_phys<-length(hYearPhysTable$h.index.Prize.Year)
mu_phys<-mean(hYearPhysTable$h.index.Prize.Year)
sd_phys<-sqrt(var(hYearPhysTable$h.index.Prize.Year))
ks.test(hYearPhysTable$h.index.Prize.Year, "pnorm", mean=mu_phys, sd=sd_phys)
# p-value 0.6274 -> cant reject the hypothesis!

n_chem<-length(hYearChemTable$h.index.Prize.Year)
mu_chem<-mean(hYearChemTable$h.index.Prize.Year)
sd_chem<-sqrt(var(hYearChemTable$h.index.Prize.Year))
ks.test(hYearChemTable$h.index.Prize.Year,"pnorm", mean=mu_chem, sd=sd_chem)
# p-value 0.8918 -> cant reject the hypothesis!

# we proceed then with a parametric bootstrap
simulation_number<-100000

bootstrapMeansPhys<-numeric(simulation_number)
bootstrapMeansChem<-numeric(simulation_number)

for(i in 1:simulation_number){
  physSimul<-rnorm(n_phys, mu_phys, sd_phys)
  chemSimul<-rnorm(n_chem, mu_chem, sd_chem)
  bootstrapMeansPhys[i]<-mean(physSimul)
  bootstrapMeansChem[i]<-mean(chemSimul)
}
# we get 95% bootstrap confidence intervals for both and see if they overlap
quantile(bootstrapMeansPhys, probs=c(0.025, 0.975))
#    2,5%    97,5% 
# 46,29834 56,19707

quantile(bootstrapMeansChem, probs=c(0.025, 0.975))
#     2,5%    97,5% 
# 74,68453 89,52978 
'''
There is no overlap in the CI with 95% confidence.
We see that the h-index the year they were awarded is with a significance 0.05 higher for chemistry laureates!
Therefore, we should effectively treat them as separate groups.
'''

'''
Test D: Analysis of the time until recognition
Bueno, si hace falt ba ein aurreko testan exaktamente berdine
'''
timeTillRecognitionPhys<-aggregate(hYearPhysTable[,4][hindexPhysTime$`Paper Year`!=0], list(hindexPhysTime$`Prize Year`[hindexPhysTime$`Paper Year`!=0]), mean)
timeTillRecognitionPhys[2]<-timeTillRecognitionPhys[1]-timeTillRecognitionPhys[2]
plot(timeTillRecognitionPhys, xlab="Prize Year", ylab="years waited from paper publication", main="Physics Laureates")
lines(timeTillRecognitionPhys)
acf(timeTillRecognitionPhys[2], main="Physics Laureates", xlab="order") # a priori it doesnt seem to be any autocorrelation


timeTillRecognitionChem<-aggregate(hYearChemTable[,4][hindexChemTime$`Paper Year`!=0], list(hindexChemTime$`Prize Year`[hindexChemTime$`Paper Year`!=0]), mean)
timeTillRecognitionChem[2]<-timeTillRecognitionChem[1]-timeTillRecognitionChem[2]
plot(timeTillRecognitionChem, xlab="Prize Year", ylab="years waited from paper publication", main="Chemistry Laureates") 
lines(timeTillRecognitionChem)
acf(timeTillRecognitionChem[2], main="Chemistry Laureates", xlab="order") # a priori it doesnt seem to be any autocorrelation

# Physics

n<-length(timeTillRecognitionPhys$Group.1)
rearrangement_number<-1000000
acTrue<-acf(timeTillRecognitionPhys[2])
ac1<-numeric(rearrangement_number)
ac2<-numeric(rearrangement_number)
ac3<-numeric(rearrangement_number)
ac4<-numeric(rearrangement_number)
ac5<-numeric(rearrangement_number)
for(i in 1:rearrangement_number){
  rear<-sample(timeTillRecognitionPhys$x, n)
  acfRear<-acf(rear, plot=FALSE)
  ac1[i]<-acfRear$acf[2]
  ac2[i]<-acfRear$acf[3]
  ac3[i]<-acfRear$acf[4]
  ac4[i]<-acfRear$acf[5]
  ac5[i]<-acfRear$acf[6]
}
sum(ac1>=acTrue$acf[2])/rearrangement_number # 0,26508
sum(ac2<=acTrue$acf[3])/rearrangement_number # 0,00515
sum(ac3<=acTrue$acf[4])/rearrangement_number # 0,0672
sum(ac4>=acTrue$acf[5])/rearrangement_number # 0,13335
sum(ac5>=acTrue$acf[6])/rearrangement_number # 0,03444

# Chemistry

n<-length(timeTillRecognitionChem$Group.1)
rearrangement_number<-1000000
acTrue<-acf(timeTillRecognitionChem[2])
ac1<-numeric(rearrangement_number)
ac2<-numeric(rearrangement_number)
ac3<-numeric(rearrangement_number)
ac4<-numeric(rearrangement_number)
ac5<-numeric(rearrangement_number)
for(i in 1:rearrangement_number){
  rear<-sample(timeTillRecognitionChem$x, n)
  acfRear<-acf(rear, plot=FALSE)
  ac1[i]<-acfRear$acf[2]
  ac2[i]<-acfRear$acf[3]
  ac3[i]<-acfRear$acf[4]
  ac4[i]<-acfRear$acf[5]
  ac5[i]<-acfRear$acf[6]
}
sum(ac1<=acTrue$acf[2])/rearrangement_number # 0,1152
sum(ac2<=acTrue$acf[3])/rearrangement_number # 0,53439
sum(ac3>=acTrue$acf[4])/rearrangement_number # 0,13277
sum(ac4<=acTrue$acf[5])/rearrangement_number # 0,37741
sum(ac5>=acTrue$acf[6])/rearrangement_number # 0,41646

'''
Test E:
Let us study whether we could make a linear model for the prediction of the h-index 10 years after the nobel
is awarded using the h-index the awarded year, the h-index the year they published the awarded-paper and the
year difference till recognition.

Physics
-------
'''
timeTillRecognitionPhys<-hindex3Phys$`Prize Year`-hindex3Phys$`Paper Year`
originalModelPhys<-summary(lm(hindex3Phys$`h-index 10 years later` ~ hindex3Phys$`h-index Paper Year`+hindex3Phys$`h-index Prize Year`+timeTillRecognitionPhys))
originalModelPhys
'''
                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                       23,4679     7,4510   3,150 0,004081 ** 
hindex3Phys$`h-index Paper Year`  -0,3757     0,1611  -2,331 0,027765 *  
hindex3Phys$`h-index Prize Year`   1,2139     0,1151  10,549 6,88e-11 ***
timeTillRecognitionPhys           -0,6203     0,1627  -3,812 0,000761 ***
---
Signif. codes:  0 ‘***’ 0,001 ‘**’ 0,01 ‘*’ 0,05 ‘.’ 0,1 ‘ ’ 1

Residual standard error: 11,85 on 26 degrees of freedom
Multiple R-squared:  0,8634,	Adjusted R-squared:  0,8477 
F-statistic:  54,8 on 3 and 26 DF,  p-value: 2,245e-11

We see that the three of them are significatively different of zero (aunque no se aplauda la de paper year).
We will use a non-parametric bootstrap method in order to calculate the CIs for the coefficients of the multidimensional
linear regression. In particular, as we have a standard error estimator for them, we will be able to use the
most convenient of the three methods: the Bootstrap-t method.

Gero hausnarketa bat ein balorien ikurreri eta magnitudieri buruz
'''
b_inter<-originalModelPhys$coefficients[1]
b_paper<-originalModelPhys$coefficients[2]
b_award<-originalModelPhys$coefficients[3]
b_recogt<-originalModelPhys$coefficients[4]

sd_inter<-originalModelPhys$coefficients[5]
sd_paper<-originalModelPhys$coefficients[6]
sd_award<-originalModelPhys$coefficients[7]
sd_recogt<-originalModelPhys$coefficients[8]
n<-length(hindex3Phys$`Laurete Name`)

simulation_number<-100000
indxs<-seq(1, n)
t_inter<-numeric(simulation_number)
t_paper<-numeric(simulation_number)
t_award<-numeric(simulation_number)
t_recogt<-numeric(simulation_number)

for(i in 1:simulation_number){
  rearIndex<-sample(indxs, n, replace=TRUE)
  bootsModel<-summary(lm(hindex3Phys$`h-index 10 years later`[rearIndex] ~ hindex3Phys$`h-index Paper Year`[rearIndex]+hindex3Phys$`h-index Prize Year`[rearIndex]+timeTillRecognitionPhys[rearIndex]))

  t_inter[i]<-(bootsModel$coefficients[1]-b_inter)/bootsModel$coefficients[5]
  t_paper[i]<-(bootsModel$coefficients[2]-b_paper)/bootsModel$coefficients[6]
  t_award[i]<-(bootsModel$coefficients[3]-b_award)/bootsModel$coefficients[7]
  t_recogt[i]<-(bootsModel$coefficients[4]-b_recogt)/bootsModel$coefficients[8]
}
#CI for the beta coefficients
b_inter+sd_inter*quantile(t_inter, probs = c(0.025, 0.975))
# 2,5%     97,5% 
# 3,456727 38,695626

b_paper+sd_paper*quantile(t_paper, probs = c(0.025, 0.975))
#      2,5%      97,5% 
# -0,7359812  0,1054549 

b_award+sd_award*quantile(t_award, probs = c(0.025, 0.975))
#     2,5%     97,5% 
# 0,9622919 1,4631034

b_recogt+sd_recogt*quantile(t_recogt, probs = c(0.025, 0.975))
#      2,5%      97,5% 
#-0,9789771 -0,1633193 

'''
Then we see that with significance 0.05 the h-index when they published the discovery seems uncorrelated!
For the other two, we see that the higher the h-index when they were awarded and the lower the time passed
from the publication of the awarded paper/discovery the higher the h-index of the researchers gets.
This is obvious: motivation, visibility for a longer time etc


Finally, let us predict using this information, the h-index that will have next year the nobel lauretes awarded in 2011.
We will use the t-bootstrap method as we have a formula for the standard deviation of a prediction in muldimiensional
regression given in https://www.jstor.org/stable/2682924?seq=2#metadata_info_tab_contents pg 33
'''
timeWaited<-(extra$`Prize Year`-extra$`Paper Year`)[extra$Field=="Phys"]
hindexPrize<-(extra$`h-index Prize year`)[extra$Field=="Phys"]

num_boots<-10000
lowerBounds<-numeric(length(timeWaited))
upperBounds<-numeric(length(timeWaited))

# we build the new model without the h index at papers year
originalModelPhys<-summary(lm(hindex3Phys$`h-index 10 years later` ~ hindex3Phys$`h-index Prize Year`+timeTillRecognitionPhys))
X<-model.matrix(originalModelPhys)
for(i in 1:length(timeWaited)){
  originalPrediction<-originalModelPhys$coefficients[1]+originalModelPhys$coefficients[2]*hindexPrize[i]+originalModelPhys$coefficients[3]*timeWaited[i]
  sigma<-sqrt(sum((hindex3Phys$`h-index 10 years later`-originalModelPhys$coefficients[1]+originalModelPhys$coefficients[2]*X[,2]+originalModelPhys$coefficients[3]*X[,3])^2)/(n-3))
  new_x<-c(1, hindexPrize[i], timeWaited[i])
  se_origPred<-sigma*sqrt(t(new_x)%*%solve(t(X)%*%X, tol=1e-12)%*%new_x )
  t_boots<-numeric(num_boots)
  for(k in 1:num_boots){
    rearIndex<-sample(indxs, n, replace=TRUE)
    bootsModel<-summary(lm(hindex3Phys$`h-index 10 years later`[rearIndex] ~ hindex3Phys$`h-index Prize Year`[rearIndex]+timeTillRecognitionPhys[rearIndex]))
    bX<-model.matrix(bootsModel)
    bootPrediction<-bootsModel$coefficients[1]+bootsModel$coefficients[2]*hindexPrize[i]+bootsModel$coefficients[3]*timeWaited[i]
    bsigma<-sqrt(sum((hindex3Phys$`h-index 10 years later`-bootsModel$coefficients[1]+bootsModel$coefficients[2]*bX[,2]+bootsModel$coefficients[3]*bX[,3])^2)/(n-3))
    se_bootPred<-bsigma*sqrt(t(new_x)%*%solve(t(bX)%*%bX, tol=1e-12)%*%new_x )
    t_boots[k]<-(bootPrediction-originalPrediction)/se_bootPred
  }
  lowerBounds[i]<-originalPrediction+quantile(t_boots, 0.025)*se_origPred
  upperBounds[i]<-originalPrediction+quantile(t_boots, 0.975)*se_origPred
}

fisikoak<-matrix(c((extra$`Laureate Name`)[extra$Field=="Phys"], (extra$`Prize Year`)[extra$Field=="Phys"], (extra$`Paper Year`)[extra$Field=="Phys"], (extra$`h-index Prize year`)[extra$Field=="Phys"], lowerBounds, upperBounds, (upperBounds+lowerBounds)/2, upperBounds-(upperBounds+lowerBounds)/2, (extra$`h-index in 2020`)[extra$Field=="Phys"]), ncol=9)


'''
Chemistry
-------
'''
timeTillRecognitionChem<-hindex3Chem$`Prize Year`-hindex3Chem$`Paper Year`
originalModelChem<-summary(lm(hindex3Chem$`h-index 10 years later` ~ hindex3Chem$`h-index Paper Year`+hindex3Chem$`h-index Prize Year`+timeTillRecognitionChem))
originalModelChem
'''
                                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)                      12,90468    5,93993   2,173   0,0399 *  
hindex3Chem$`h-index Paper Year` -0,09346    0,07979  -1,171   0,2530    
hindex3Chem$`h-index Prize Year`  1,09139    0,05674  19,235 4,33e-16 ***
timeTillRecognitionChem          -0,38710    0,16723  -2,315   0,0295 *  
---
Signif. codes:  0 ‘***’ 0,001 ‘**’ 0,01 ‘*’ 0,05 ‘.’ 0,1 ‘ ’ 1

Residual standard error: 7,703 on 24 degrees of freedom
Multiple R-squared:  0,9511,	Adjusted R-squared:  0,945 
F-statistic: 155,5 on 3 and 24 DF,  p-value: 7,391e-16

We see that the paper release year h-index is seen as not significative by the t-test.
We will use a non-parametric bootstrap method in order to calculate the CIs for the coefficients of the multidimensional
linear regression. In particular, as we have a standard error estimator for them, we will be able to use the
most convenient of the three methods: the Bootstrap-t method.

'''
b_inter<-originalModelChem$coefficients[1]
b_paper<-originalModelChem$coefficients[2]
b_award<-originalModelChem$coefficients[3]
b_recogt<-originalModelChem$coefficients[4]

sd_inter<-originalModelChem$coefficients[5]
sd_paper<-originalModelChem$coefficients[6]
sd_award<-originalModelChem$coefficients[7]
sd_recogt<-originalModelChem$coefficients[8]
n<-length(hindex3Chem$`Laurete Name`)

simulation_number<-100000
indxs<-seq(1, n)
t_inter<-numeric(simulation_number)
t_paper<-numeric(simulation_number)
t_award<-numeric(simulation_number)
t_recogt<-numeric(simulation_number)

for(i in 1:simulation_number){
  rearIndex<-sample(indxs, n, replace=TRUE)
  bootsModel<-summary(lm(hindex3Chem$`h-index 10 years later`[rearIndex] ~ hindex3Chem$`h-index Paper Year`[rearIndex]+hindex3Chem$`h-index Prize Year`[rearIndex]+timeTillRecognitionChem[rearIndex]))
  
  t_inter[i]<-(bootsModel$coefficients[1]-b_inter)/bootsModel$coefficients[5]
  t_paper[i]<-(bootsModel$coefficients[2]-b_paper)/bootsModel$coefficients[6]
  t_award[i]<-(bootsModel$coefficients[3]-b_award)/bootsModel$coefficients[7]
  t_recogt[i]<-(bootsModel$coefficients[4]-b_recogt)/bootsModel$coefficients[8]
}
#CI for the beta coefficients
b_inter+sd_inter*quantile(t_inter, probs = c(0.025, 0.975))
#     2,5%     97,5% 
# -3,374025 27,925792

b_paper+sd_paper*quantile(t_paper, probs = c(0.025, 0.975))
#      2,5%      97,5% 
# -0,23470763  0,05479653   

b_award+sd_award*quantile(t_award, probs = c(0.025, 0.975))
#     2,5%     97,5% 
# 0,9510107 1,2290318   

b_recogt+sd_recogt*quantile(t_recogt, probs = c(0.025, 0.975))
#      2,5%      97,5% 
#-0,68694771 -0,02177566  

'''
Same conclusions as in physics, just that the h-index at the award winning time appears to affect almost
with a same coefficient!!! Unify models?
'''
timeWaited<-(extra$`Prize Year`-extra$`Paper Year`)[extra$Field=="Chem"]
hindexPrize<-(extra$`h-index Prize year`)[extra$Field=="Chem"]

num_boots<-10000
lowerBounds<-numeric(length(timeWaited))
upperBounds<-numeric(length(timeWaited))

# we build the new model without the h index at papers year
originalModelChem<-summary(lm(hindex3Chem$`h-index 10 years later` ~ hindex3Chem$`h-index Prize Year`+timeTillRecognitionChem))
X<-model.matrix(originalModelChem)
for(i in 1:length(timeWaited)){
  originalPrediction<-originalModelChem$coefficients[1]+originalModelChem$coefficients[2]*hindexPrize[i]+originalModelChem$coefficients[3]*timeWaited[i]
  sigma<-sqrt(sum((hindex3Chem$`h-index 10 years later`-originalModelChem$coefficients[1]+originalModelChem$coefficients[2]*X[,2]+originalModelChem$coefficients[3]*X[,3])^2)/(n-3))
  new_x<-c(1, hindexPrize[i], timeWaited[i])
  se_origPred<-sigma*sqrt(t(new_x)%*%solve(t(X)%*%X, tol=1e-12)%*%new_x )
  t_boots<-numeric(num_boots)
  for(k in 1:num_boots){
    rearIndex<-sample(indxs, n, replace=TRUE)
    bootsModel<-summary(lm(hindex3Chem$`h-index 10 years later`[rearIndex] ~ hindex3Chem$`h-index Prize Year`[rearIndex]+timeTillRecognitionChem[rearIndex]))
    bX<-model.matrix(bootsModel)
    bootPrediction<-bootsModel$coefficients[1]+bootsModel$coefficients[2]*hindexPrize[i]+bootsModel$coefficients[3]*timeWaited[i]
    bsigma<-sqrt(sum((hindex3Chem$`h-index 10 years later`-bootsModel$coefficients[1]+bootsModel$coefficients[2]*bX[,2]+bootsModel$coefficients[3]*bX[,3])^2)/(n-3))
    se_bootPred<-bsigma*sqrt(t(new_x)%*%solve(t(bX)%*%bX, tol=1e-12)%*%new_x )
    t_boots[k]<-(bootPrediction-originalPrediction)/se_bootPred
  }
  lowerBounds[i]<-originalPrediction+quantile(t_boots, 0.025)*se_origPred
  upperBounds[i]<-originalPrediction+quantile(t_boots, 0.975)*se_origPred
}
kimikoak<-matrix(c((extra$`Laureate Name`)[extra$Field=="Chem"], (extra$`Prize Year`)[extra$Field=="Chem"], (extra$`Paper Year`)[extra$Field=="Chem"], (extra$`h-index Prize year`)[extra$Field=="Chem"], lowerBounds, upperBounds, (upperBounds+lowerBounds)/2, upperBounds-(upperBounds+lowerBounds)/2, (extra$`h-index in 2020`)[extra$Field=="Chem"]), ncol=9)

