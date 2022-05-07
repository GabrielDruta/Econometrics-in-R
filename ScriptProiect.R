library(lmtest)
library(mctest)
library(foreign)
library(corpcor)
library(car)

# Choose the working enviroment

setwd('C:\\Users\\GABI\\Desktop\\ProiectECO')

# Add the data set you are going to use in the Progrma

DataSet<-read.csv('Valori.csv')

# Display your data set 

View(DataSet)

names(DataSet)[1] ='Country'
names(DataSet)[2] = 'GDP'
names(DataSet)[3] ='TotalInvestment'
names(DataSet)[4] = 'InflationIndex'
names(DataSet)[5] ='UnemploymentRate'
names(DataSet)[6] ='Population'
names(DataSet)[7] ='GDPGrowth'


Regre<-subset(DataSet,select = c("Population","NoSchools","Abandon", "AvgSalary", "PIB", "Unemployment"))

is.numeric(Regre$Unemployment)
is.numeric(Regre$Abandon)
is.numeric(Regre$AvgSalary)
is.numeric(Regre$Population)
is.numeric(Regre$PIB)
is.numeric(Regre$NoSchools)

hist(Regre$Unemployment)
mean(Regre$Unemployment)
median(Regre$Unemployment)
sd(Regre$Unemployment)
var(Regre$Unemployment)

hist(Regre$NoSchools)
mean(Regre$NoSchools)
median(Regre$NoSchools)
sd(Regre$NoSchools)
var(Regre$NoSchools)

hist(Regre$Abandon)
mean(Regre$Abandon)
median(Regre$Abandon)
sd(Regre$Abandon)
var(Regre$Abandon)

hist(Regre$AvgSalary)
mean(Regre$AvgSalary)
median(Regre$AvgSalary)
sd(Regre$AvgSalary)
var(Regre$AvgSalary)

hist(Regre$Population)
mean(Regre$Population)
median(Regre$Population)
sd(Regre$Population)
var(Regre$Population)

hist(Regre$PIB)
mean(Regre$PIB)
median(Regre$PIB)
sd(Regre$PIB)
var(Regre$PIB)

barplot(Regre$Unemployment)
barplot(Regre$Abandon)
barplot(Regre$AvgSalary)
barplot(Regre$Population)
barplot(Regre$PIB)

#Regression

Regression<-lm(Regre$Unemployment ~ Regre$Abandon + Regre$NoSchools +Regre$AvgSalary + Regre$Population + Regre$PIB)
summary(Regression)

plot(Regression)

# TEST FOR AUTOCORRELATION

# DW test in order to check for autocorrelation

dwtest(Regression)
durbinWatsonTest(Regression)

# 2 is no autocorrelation.
# 0 to <2 is positive autocorrelation 
# 2 to 4 is negative autocorrelation 

#   A rule of thumb is that test statistic values in the range of 1.5 to 2.5 are relatively normal. 
#   Values outside of this range could be cause of concern for the statistical validity.
#   Our dw test value is 1.92,so even if positive autocorrelation is present there is no concern 
# for statistical invalidity because it is situated in the range of normality.

# TEST FOR MULTICOLINEARITY

# CorrelationMatrix

CorrelationMatrix = Regre[,2:5]

omcdiag(CorrelationMatrix,Regre$Unemployment)
imcdiag(CorrelationMatrix,Regre$Unemployment)

graphics::pairs(CorrelationMatrix)

# TEST FOR HETEROSCEDASTICITY & HOMOSCEDASTICITY

#  The Breush-Pagan test

bptest(Regression)
ncvTest(Regression)

#  Chisquare = 0.5677571, Df = 1, p = 0.45115
#  The chi-squared critical value for 4 degrees of freedom IS 9.49 at a 0.95 probability --- 0,05 
#  The null hypothesis is homoskedasticity
#  The alternative hypothesis is heteroskedasticity
#  Our p-value is 0.45115 which is bigger than 0,05 = > the alternative hypothesis of heteroskedasticity is rejected


# The Goldfeld-Quandt test

gqtest(Regression)

#  GQ = 3.3392, df1 = 16, df2 = 16, p-value = 0.01048
#  The F-critical in our case is 2.33
#  The null hypothesis is homoskedasticity
#  The alternative hypothesis is heteroskedasticity
#  GQ computed value is > that F critical so we reject the alternative hypothesis of heteroskedasticity





