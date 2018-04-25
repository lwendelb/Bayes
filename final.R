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




######### Parker's code
library(doBy)
dat = data3

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
for_mcmc <- summaryBy(flag ~ loan_number + ltv + fico + dti + acq_quarter, FUN=c(max), data=dat)
for_mcmc$acq_year <- as.numeric(substr(as.character(for_mcmc$acq_quarter), 1, 4))

# flag origination-year groups
for_mcmc$early_orig <- ifelse(for_mcmc$acq_year <= 2003, 1, 0)
for_mcmc$mid_orig <- ifelse(for_mcmc$acq_year >= 2004 & for_mcmc$acq_year <= 2007, 1, 0)
for_mcmc$late_orig <- ifelse(for_mcmc$acq_year >= 2008, 1, 0)

for_mcmc$rand <- runif(nrow(for_mcmc), 0, 1)
for_mcmc <- for_mcmc[order(for_mcmc$rand),]
for_mcmc <- for_mcmc[1:1000,]

Y = for_mcmc[,6]
X = cbind(for_mcmc[,2:4],for_mcmc[,8:10])

n = length(Y)

# Logistic regression:

logistic_model <- "model{

# Likelihood

for(i in 1:n){
Y[i] ~ dbern(q[i])
logit(q[i]) <- beta[1] + beta[2]*X[i,1] + beta[3]*X[i,2] + beta[4]*X[i,3] + gamma[1]*X[i,4]
      + gamma[2]*X[i,5] + gamma[3]*X[i,6]
}

# Random Effects
for(j in 1:3){
  gamma[j] ~ dnorm(mug[j],taug[j])
}

#Priors

for(j in 1:4){
beta[j] ~ dnorm(0,0.1)
}

for(j in 1:3){
mug[j] ~ dnorm(0,0.01)
taug[j] ~ dgamma(0.01,0.01)
}

}"


library(rjags)

datlist   <- list(Y=Y,n=n,X=X)
model <- jags.model(textConnection(logistic_model),data = datlist,n.chains=1)

update(model, 10000)

samp <- coda.samples(model, 
                     variable.names=c("beta","gamma"), 
                     n.iter=50000)

geweke.diag(samp)
plot(samp)
