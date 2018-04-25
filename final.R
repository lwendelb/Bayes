data = read.csv("sample_loans.csv")
data2 = read.csv("HPI_AT_metro.csv")
data3 = read.csv("loan_data.csv")
head(data3)

e = 177
Y1 = data3$delinq_status
Y1 = as.matrix(Y1)
Y = matrix(0,ncol=1,nrow=length(Y1))

for (i in 1:length(Y1)){
  #Y[i] = ifelse(Y1[i]=="X"||Y1[i]==0,0,ifelse(Y1[i]=="",NA,1))
  Y[i,1] = ifelse(Y1[i]=="X"||Y1[i]==0,0,1)
}

Y2 = matrix(as.numeric(Y)-2,ncol=1,nrow=length(Y))

uniq <- as.matrix(unique(data3$loan_number))
Ynew = matrix(0,ncol=1,nrow=length(uniq))
for (i in 1:length(uniq)){
  quant = Y2[data3$loan_number==uniq[i]]
  if(max(quant[1:min(length(quant),60)])>0){
    Ynew[i,1] = 1
  }
}


convert_ltv <- function(ltv){
  ifelse(l)
  for (i in 1:lenth(ltv)){
    if(ltv[i] <= 80){
      ltv[i] <- 0
    }
    if(80 < ltv[i] <= 85)
  }
}



######### Parker's code
library(doBy)
dat = data3
dat_sampleloans <- read.csv("sample_loans.csv")
dat_sampleloans <- dat_sampleloans[,c(2,7)]
dat = merge(dat,dat_sampleloans,by.x="loan_number", by.y="loan_number")

# convert date to numeric
dat$num_date <- as.Date(dat$date, format="%m/%d/%Y")

# find minimum date by loan
min_date_all <- summaryBy(num_date ~ loan_number, FUN=c(min), data=dat)

# merge on to main data frame
dat <- merge(dat, min_date_all, by.x="loan_number", by.y="loan_number")

# drop if more than five years after first date
dat <- dat[dat$num_date <= (dat$num_date.min + 365*5 + 1),]

# flag if the loan is ever delinquent
dat$flag <- ifelse(dat$delinq_status == "X" | dat$delinq_status == "0", 0, 1)   


# for_mcmc changed
for_mcmc <- summaryBy(flag ~ loan_number + ltv + fico + dti + acq_quarter + num_date.min + zip3, FUN=c(max), data=dat)
for_mcmc$acq_year <- as.numeric(substr(as.character(for_mcmc$acq_quarter), 1, 4))

origin <- "1970-01-01"
for_mcmc$tree <- as.Date(for_mcmc$num_date.min,origin = origin)

# flag origination-year groups
for_mcmc$early_orig <- ifelse(for_mcmc$acq_year <= 2003, 1, 0)
for_mcmc$mid_orig <- ifelse(for_mcmc$acq_year >= 2004 & for_mcmc$acq_year <= 2007, 1, 0)
for_mcmc$late_orig <- ifelse(for_mcmc$acq_year >= 2008, 1, 0)

for_mcmc$rand <- runif(nrow(for_mcmc), 0, 1)
for_mcmc <- for_mcmc[order(for_mcmc$rand),]
for_mcmc <- for_mcmc[1:1000,]

for_mcmc$ltv <- ifelse(for_mcmc$ltv<=80,80,for_mcmc$ltv)

library(lubridate)
library(zoo)
for_mcmc$quarter_temp <- quarters(for_mcmc$tree)
for_mcmc$year_temp <- year(for_mcmc$tree)
for_mcmc$yq_temp <- paste(for_mcmc$quarter_temp, "/", for_mcmc$year_temp, sep="")
for_mcmc$yq <- as.Date(as.yearqtr(for_mcmc$yq_temp, format = "Q%q/%Y"))


##### housing data things
housingdat <- read.csv("housing next five flag - zip3.csv")

for_mcmc <- merge(for_mcmc, housingdat, by.x=c("zip3","yq"), by.y=c("zip3","yq"),all.x=T)

for_mcmc <- for_mcmc[!is.na(for_mcmc$dec_next_five),]

Y = for_mcmc$flag.max
Xb = cbind(for_mcmc$ltv,for_mcmc$fico,for_mcmc$dti,for_mcmc$dec_next_five)
Xg = cbind(for_mcmc$early_orig,for_mcmc$mid_orig,for_mcmc$late_orig)
n = length(Y)

# Logistic regression:

logistic_model <- "model{

# Likelihood

for(i in 1:n){
Y[i] ~ dbern(q[i])
#logit(q[i]) <- alpha + beta[1]*X[i,1] + beta[2]*X[i,2] + beta[3]*X[i,3] + beta[4]*X[i,4]+gamma[1]*X[i,5]
#      + gamma[2]*X[i,6] + gamma[3]*X[i,7]
#}
logit(q[i]) <- alpha + inprod(Xb[i,],beta[])+inprod(Xg[i,],gamma[])
}

# Random Effects
for(j in 1:3){
  gamma[j] ~ dnorm(mug[j],taug[j])
}


beta[1:p] ~ dmnorm(mub[],sigma[,])


#Priors


alpha~ dnorm(5.2576775307,0.1)
#alpha~ dnorm(0,0.1)


for(j in 1:3){
#mug[j] ~ dnorm(0,0.01)
taug[j] ~ dgamma(0.01,0.01)
}

mug[1] ~ dnorm(0.5932233559,0.1)
mug[2] ~ dnorm(0.5718783093,0.1)
mug[3] ~ dnorm(0,0.1)

mub[1] ~ dnorm(0.0361957469,0.1)
mub[2] ~ dnorm(-0.0146455516,0.1)
mub[3] ~ dnorm(0.0006081439,0.1)
mub[4] ~ dnorm(0.1294116858,0.1)
#for(j in 1:p){
#mub[j] ~ dnorm(0,0.01)
#}

k = p+0.1
for(j1 in 1:p){for(j2 in 1:p){R[j1,j2]<-0.1*equals(j1,j2)}} #R is diagonal
sigma[1:p,1:p] ~ dwish(R[,],k)

}"


library(rjags)
p=4
R <- diag(1/(p+0.1),p)
I = diag(1,p)

datlist   <- list(Y=Y,n=n,Xb=Xb,Xg=Xg,p=dim(Xb[2]))
model <- jags.model(textConnection(logistic_model),data = datlist,n.chains=1)

update(model, 20000)

samp <- coda.samples(model, 
                     variable.names=c("alpha","beta","gamma"), 
                     n.iter=50000)

geweke.diag(samp)
plot(samp)

mod <- glm(Y ~ X, family=binomial(link="logit"))
summary(mod)
mod$coefficients
length(X[,7])
