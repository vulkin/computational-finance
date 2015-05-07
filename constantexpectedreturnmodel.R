# Load relevant packages
library(PerformanceAnalytics);library(zoo);library(tseries);

# Get the monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo! using the tseries function get.hist.quote(). Set the sample to Jan 1998 through Dec 2009.

# Get the adjusted closing prices from Yahoo!
VBLTX_prices <- get.hist.quote(instrument="vbltx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

FMAGX_prices <- get.hist.quote(instrument="fmagx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

SBUX_prices <-  get.hist.quote(instrument="sbux", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

# Change the class of the time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package  

index(VBLTX_prices) <- as.yearmon(index(VBLTX_prices))
index(FMAGX_prices) <- as.yearmon(index(FMAGX_prices))
index(SBUX_prices) <- as.yearmon(index(SBUX_prices))
# Inspect your data

head(FMAGX_prices)
head(VBLTX_prices)
head(SBUX_prices)

start(SBUX_prices)
end(SBUX_prices)


# Create merged price data
all_prices <-merge(VBLTX_prices,FMAGX_prices,SBUX_prices)
# Rename columns
colnames(all_prices) <- c("VBLTX", "FMAGX", "SBUX")

# Calculate cc returns as difference in log prices
all_returns<-diff(log(all_prices))


return_matrix <- coredata(all_returns);

# The variable return_matrix is preloaded in your workspace

# Number of observations
n_obs <-nrow(return_matrix) 

# Estimates of sigma2hat
sigma2hat_vals <-apply(return_matrix,2,var) 

# Standard Error of sigma2hat
se_sigma2hat <- sigma2hat_vals/((n_obs/2)^.5)

se_sigma2hat


# Calculate the correlation matrix
cor_matrix <-cor(return_matrix) 

# Get the lower triangular part of that 'cor_matrix'
rhohat_vals <- c(cor_matrix[1,2],cor_matrix[1,3],cor_matrix[2,3])

# Set the names
names(rhohat_vals) <- c("VBLTX,FMAGX","VBLTX,SBUX","FMAGX,SBUX")

# Compute the estimated standard errors for correlation
se_rhohat <- (1-(rhohat_vals)^2)/(nrow(return_matrix)^.5)

se_rhohat


# Test the normality of the returns of VBLTX

jarque.bera.test(all_returns$VBLTX)

