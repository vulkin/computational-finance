# Standard deviations and correlation
sig_x <- 0.10
sig_y <- 0.05
rho_xy <- 0.9

# Covariance between X and Y
sig_xy <- sig_x * sig_y * rho_xy

# Covariance matrix
Sigma_xy <- matrix(c(sig_xy),1,1)


##add staright lines to the plot
abline(h=mu_y,v=mu_x)

# Add line segments
segments(x0 = 0, y0 = -1e10, x1 = 0, y1 = 0, col="red")
segments(x0 = -1e10, y0 = 0, x1 = 0, y1 = 0, col="red")

#Compute joint probability
pmvnorm(lower=c(-Inf,-Inf),upper=c(0,0),mean=c(mu_x,mu_y),sigma=Sigma_xy)


