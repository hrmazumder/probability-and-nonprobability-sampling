
###########  Bias Study ############

#****************************************************************************************
### Simulation study on comparing convenience sampling vs SRS ###
#part(1): function for generating srs; part(2): function for generating convenience sample
#part(3): function for running the simulation by calling functions in part(1) and part(2)
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
compare_srs_conv(NumSim = 100, alpha = 0.5, beta = 1, gama =1, theta = 10, N = 5000, n = 100)[[1]][,3]


#part (4) **********************************************************************************

#creating plots for BIAS study by changing paramaters (alpha, beta, gamma and sample size)

#setup the graph environment
par(mfrow=c(2,2)) #ignore this if don't want to show all graphs in same plot
ylimit = c(0,1.5) #change upper limit according to obtained results
NumSim = 100 #we used 10000

#******changing beta
Bias_beta = NULL
beta = seq(0.5, 5, by = 0.5) #can be changed 

#first obtain results by changing beta 
for(i in 1:length(beta)){
  Bias_beta = rbind(Bias_beta, compare_srs_conv(NumSim = NumSim, alpha = 5, beta = beta[i], gama =1, theta = 10, N = 5000, n = 100)[[1]][,1])
}

#now plot
plot(beta, Bias_beta[,1], type = "o", ylab = "Bias", xlab = "Beta", col = c("purple"), lty = 2, lwd = 2, ylim =c(0,3),
     main = expression(paste("Bias (", alpha,"=5, ", gamma,"=1)")))
lines(beta, Bias_beta[,2], type = "o", lty=1, lwd = 2, ylim = c(0,4), col = c("gold3"))
legend("topleft", legend=c("SRS", "Convenience"), lty=c(2,1), cex=0.8, col = c("purple", "gold3"))


##******changing gamma
gama = seq(0.5, 5, by = 0.5)
Bias_gama = NULL

for(i in 1:length(gama)){
  Bias_gama = rbind(Bias_gama, compare_srs_conv(NumSim = NumSim, alpha = 5, beta = 1, gama =gama[i], theta = 10, N = 5000, n = 100)[[1]][,1])
}

plot(gama, Bias_gama[,1], type = "o", ylab = "Bias", xlab = "Gamma", col = c("purple"), ylim = ylimit,lty=2,
     main = expression(paste("Bias (", alpha,"=5, ", beta,"=1)")), lwd=2)
lines(gama, Bias_gama[,2], type = "o", lty=1, lwd=2, col = c("gold3"))
legend("topleft", legend=c("SRS", "Convenience"), lty=c(2,1), cex=0.8, col = c("purple", "gold3"))


##*******changing alpha
alpha = seq(0.5, 5, by = 0.5)
Bias_alpha = NULL

for(i in 1:length(alpha)){
  Bias_alpha = rbind(Bias_alpha, compare_srs_conv(NumSim = NumSim, alpha = alpha[i], beta = 5, gama =1, theta = 10, N = 5000, n = 100)[[1]][,1])
}

plot(alpha, Bias_alpha[,1], type = "o", ylab = "Bias", xlab = "Alpha", col = c("purple"), ylim = ylimit, lty=2,
     main = expression(paste("Bias (", beta,"=5, ", gamma,"=1)")),lwd=2)
lines(alpha, Bias_alpha[,2], type = "o", lty=1, lwd=2,col = c("gold3"))
legend("topleft", legend=c("SRS", "Convenience"), lty=c(2,1), cex=0.8, col = c("purple", "gold3"))


#******changing sample size:
n = seq(50, 250, by = 50)
Bias_n = NULL

for(i in 1:length(n)){
  Bias_n = rbind(Bias_n, compare_srs_conv(NumSim = NumSim, alpha = 1, beta = 2.5, gama =1, theta = 10, N = 5000, n = n[i])[[1]][,1])
}

plot(n, Bias_n[,1], type = "o", ylab = "Bias", xlab = "Sample size", ylim = ylimit, lty=2,
     main = expression(paste("Bias (",alpha,"=1, ", beta,"=2.5, ", gamma,"=1)")),lwd=2)
lines(n, Bias_n[,2], type = "o", lty=1, lwd=2)
legend("topleft", legend=c("SRS", "Convenience"), lty=c(2,1), cex=0.8)

#*************************************** END  **************************

#***comment: one can change parameters in the above code to generate all the plots that as shown in the output files.



