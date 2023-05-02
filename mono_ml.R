# Estimation of paramters from the data with monotone missingness.
# Monotone missingness is a special type of missingness that allows us to maximize the likelihood by maximizing the conditional likelihoods of the each variables given the previous variables provided the vaiables are arranged in a increasing order of no. of missingness.

#`mono.ml` function provides the maximum likelihood estimators of a data which has a monotone missingness provided the data is arranged accordingly.

#------------------ mono.ml function ----------------------------------------


mono.ml<- function(df){          # df is the data frame with missing values
  
  n<- nrow(df) 
  r<- ncol(df)
  v<- colSums(is.na(df))
  p<- sum(v)/(n*r)      # p provides the proportion of data missing.
  
  mu_hat<- c()
  sig_hat<- matrix(rep(NA, r^2), nrow=r, ncol=r)
  
  for (i in 1:ncol(df)) {
    
    if(i==1){
      N<- n-v[i]
      x<- df[1:N,]
      
      l<- lm(x[,1]~1)
      al<-l$coefficients  # alpha & beta
      RSS<- sum(l$residuals^2)
      sig<- RSS/N
      mu_hat[i]<- al
      sig_hat[i,i]<- sig
    }
    
    else if(i==2){
      N<- n-v[i]
      x<- df[1:N,]
      
      l<- lm(x[,2]~ x[,1])
      al<- l$coefficients # alpha & beta
      mu_hat[i]<- al[1]+ al[-1] %*% mu_hat[1:i-1]
      sig<- al[-1] %*% sig_hat[1:i-1,1:i-1]
      sig_hat[i-1,i]<- sig
      sig_hat[i,i-1]<-sig_hat[i-1,i] # since symmetric
      
      RSS<- sum(l$residuals^2)
      sig<- RSS/N
      sig_hat[i,i]<- sig + al[-1] %*% sig_hat[1:i-1,i]
    }
    else{
      N<- n-v[i]
      x<- df[1:N,]
      
      l<- lm(x[,i]~., data=x[,1:i-1])
      al<- l$coefficients   # alpha & beta
      mu_hat[i]<- al[1]+ al[-1] %*% mu_hat[1:i-1]
      sig<- al[-1] %*% sig_hat[1:i-1,1:i-1]
      sig_hat[1:i-1,i]<- sig
      sig_hat[i,1:i-1]<-sig_hat[1:i-1,i] # since symmetric
      
      RSS<- sum(l$residuals^2)
      sig<- RSS/N
      sig_hat[i,i]<- sig + al[-1] %*% sig_hat[1:i-1,i]
    }
  }
  
  return(list(prop=p,mu=mu_hat ,sigma=sig_hat))
}




# The `mono.ml` function only use one argument as the name of the data frame after proper arrangement.\

#The function returns two values namely,

#   - `prop`: which gives the proportion of data missing.
#   - `mu`: which gives the estimates of the expected values of the variables.
#   - `sigma`: which gives the estimated variance-covariance matrix of the variables.


#-----------------------------------------------------------------------


# Example on a simulation study:

require(mvtnorm)

mu<- c(10,20,30,40,50)        # mean vector
s<- matrix(c(12,6,10,9,7,
             6,10,12,4,8,
             10,12,17,5,9,
             9,4,5,10,8,
             7,8,9,8,15), ncol=5, byrow=T)     # dispersion matrix
# checking for definiteness:  PD matrix has all the eigen values positive
round(eigen(s)$values,3)                   
# Now generate data through simulation:
set.seed(77)
n<-100              # sample size
df<- rmvnorm(n, mean=mu, sigma=s) # generated data
colnames(df)<- c("y1","y2","y3","y4","y5")
# data matrix
df<- as.data.frame(df)

df_up<- df
df_up[(n-6+1):n,2]<- NA
df_up[(n-9+1):n,3]<- NA
df_up[(n-12+1):n,4]<- NA
df_up[(n-17+1):n,5]<- NA
#summary(df_up)
tail(df_up,10)

x<-mono.ml(df_up)  
x


