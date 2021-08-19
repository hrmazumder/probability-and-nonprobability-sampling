 
####  Relative Efficiency study ############



#****************************************************************************************
### Simulation study on comparing convenience sampling vs SRS ###
#part(1): function for generating srs; part(2): function for generating convenience sample
#part(3): function for running the simulation by calling functions in part(1) and part(2)
#part(4): creating plots for relative efficiency study based on part(3) results
#****************************************************************************************

#part (1)*************************************************************************************
#First, write a function to simulate a simple random sample of size n: (same thing as built-in 'sample' command in R)

srs_samp = function(N, n, z){
           #generate one value from U(0,1) and multiply by N:
           u_N = runif(n = n, min = 0, max = 1)*N
           #now select every u_N th observation in Z:
           y1 = NULL
          for(i in u_N){
             y1 = c(y1, z[i])
            }
           return(y1)
           }

#end of function for simple random sample generation

#part (2)**************************************************************************
#writing a function that will ensure a convenience sample of size n:

conv_samp = function(s, z, N, n){

            #first try to generate sample of size n 
            try_conv_sam1 = function(s,z, N, n){
                            y2 = NULL  
                           for(j in 1:N){
                              if(s[j]==1){
                              y2 = c(y2, z[j])
                               }
            if(length(y2)==n){break} #if sample size=n, stop here (outcome here: sample size is either <n or =n)
               }
            return(y2)
            }
            #if sample size in try_conv_sam1 <= n, then do the follwoing to ensure sample size = n:
            #<= because it makes the while loop get activated to ensure final sample size in n.
            try_conv_sam2 = NULL  
            while(length(try_conv_sam1(s, z, N, n)) <= n){
                   try_conv_sam2 = c(try_conv_sam2, try_conv_sam1(s,z,N,n))
                                  if(length(try_conv_sam2) >n) {break} #stop here 
                        }
            conv_samp_final = try_conv_sam2[1:n] #pick first n (since here, length(try_conv_sam2)>=n)
            return(conv_samp_final)
            }

#*******************************************************************************************
#end of function for convenience sample generation
#*******************************************************************************************

#part (3)*********************************************************************

###function for running simulation NumSim times:

compare_srs_conv = function(NumSim, alpha, beta, gama, theta, N, n){
                   set.seed(100)
                   #creating NULL vectors to store result:
                   y1_bar = NULL
                   y2_bar = NULL
                   z_bar = NULL
                   
                 for(k in 1:NumSim){  
                    
                    #generate population:
                     x = rnorm(n = N, mean = 0, sd = 1) 
                     error = rnorm(n = N, mean = 0, sd = 1)
                     z = theta +  beta*x + error
                     z_bar[k] = sum(theta + beta*x)/N 
                    
                    #generate a srs of size n and calculate mean:
                     y1_bar[k] = mean(srs_samp(N, n, z))
    
                    #generate convenience sample of n and calculate mean:
                     p = exp(alpha + gama*x)/(1+exp(alpha + gama*x))
                     s = rbinom(n = N, size = 1, prob = p)
                     y2_bar[k] = mean(conv_samp(s, z, N, n))
                      }
                      
                   ##final Result (bias, variance, mse )
                   bias_srs =  sum(abs(y1_bar - z_bar))/NumSim
                   bias_conv =  sum(abs(y2_bar - z_bar))/NumSim
                   var_srs = sum( (y1_bar - z_bar)^2 )/NumSim
                   var_conv = sum( (y2_bar - z_bar)^2 )/NumSim
                   mse_srs = var_srs #theoritically bias of sample mean in SRS =0; thus bias^2=0 
                   mse_conv = var_conv #to make comparable to SRS, bias of sample mean set to 0
                   res = cbind(c(bias_srs, bias_conv), c(var_srs, var_conv), c(mse_srs, mse_conv) )
                   colnames(res) = c("Bias", "Variance", "MSE")
                   rownames(res) = c("SRS", "Conv.")
  
                   #relative efficiency (SRS to convenience)
                    re = as.matrix(mse_srs/mse_conv)
                    colnames(re) = "RE"
                    rownames(re) = "MSE_SRS/MSE_CONV"
                    return(list(res, re))
                    }

#****************************************************************************************
#function for running simulation ends
#**************************************************************************************** 
#call the above function (compare_srs_conv) to see the bias, MSE and Relative efficiency:
compare_srs_conv(NumSim = 100, alpha = 0.5, beta = 1, gama =1, theta = 10, N = 5000, n = 100)
compare_srs_conv(NumSim = 100, alpha = 0.5, beta = 1, gama =1, theta = 10, N = 5000, n = 100)[[2]]
compare_srs_conv(NumSim = 100, alpha = 0.5, beta = 1, gama =1, theta = 10, N = 5000, n = 100)[[1]][,1]


#part (4) **********************************************************************************

#creating plots for Relative Efficiency (RE) study by changing paramaters (alpha, beta, gamma and sample size)

#setup the graph environment
par(mfrow=c(2,2)) #ignore this if don't want to show all graphs in same plot

NumSim = 100 #we used number of iterations as 10000


#******changing beta
RE_beta = NULL
beta = seq(0.5, 5, by = 0.5)

#first obtain results by changing beta 
for(i in 1:length(beta)){
  RE_beta = c(RE_beta, compare_srs_conv(NumSim = NumSim, alpha = 5, beta = beta[i], gama =1, theta = 10, N = 5000, n = 100)[[2]])
}

#now plot
plot(beta, RE_beta, type = "o", ylab = "RE (SRS to Conv.)", xlab = "Beta", col = c("green"), lwd = 2, ylim = c(0,1.2),
     main = expression(paste("Relative Efficiency (", alpha,"=5, ", gamma,"=1)")))
abline(h=1, col = "red")

##******changing gamma
gama = seq(0.5, 5, by = 0.5)
RE_gama = NULL

for(i in 1:length(gama)){
  RE_gama = c(RE_gama, compare_srs_conv(NumSim = NumSim, alpha = 5, beta = 1, gama =gama[i], theta = 10, N = 5000, n = 100)[[2]])
}

plot(gama, RE_gama, type = "o", ylab = "RE (SRS to Conv.)", xlab = "Gamma", col = c("green"),ylim = c(0,1),
     main = expression(paste("Relative Efficiency (", alpha,"=5, ", beta,"=1)")), lwd=2)
abline(h=1, col = "red")

##*******changing alpha
alpha = seq(0.5, 5, by = 0.5)
RE_alpha = NULL

for(i in 1:length(alpha)){
  RE_alpha = c(RE_alpha, compare_srs_conv(NumSim = NumSim, alpha = alpha[i], beta = 5, gama =1, theta = 10, N = 5000, n = 100)[[2]])
}

plot(alpha, RE_alpha, type = "o", ylab = "RE (SRS to Conv.)", xlab = "Alpha", col = c("green"),ylim = c(0,1.2),
     main = expression(paste("Relative Efficiency (", beta,"=5, ", gamma,"=1)")), lwd=2)
abline(h=1, col = "red")

#******changing sample size:
n = seq(50, 250, by = 50)
RE_n = NULL

for(i in 1:length(n)){
  RE_n = c(RE_n, compare_srs_conv(NumSim = NumSim, alpha = 1, beta = 2.5, gama =1, theta = 10, N = 5000, n = n[i])[[2]])
}

plot(n, RE_n, type = "o", ylab = "RE (SRS to Conv.)", xlab = "Sample size", ylim = c(0,1),
     main = expression(paste("Relative Efficiency (",alpha,"=1, ", beta,"=2.5, ", gamma,"=1)")), lwd=2)
abline(h=1, col = "red")

