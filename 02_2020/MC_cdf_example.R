 # Peak flow random data generation usinf ecdf distribution
# original in python 
# here: https://nbviewer.jupyter.org/urls/mountain-hydrology-research-group.github.io/data-analysis/labs/lab2/Lab%202.2.ipynb
# J. Hernandez

site_data <- c(
  660,
  673,
  781,
  578,
  1414,
  673,
  861,
  727,
  735,
  706,
  871,
  689,
  635,
  615,
  616,
  791,
  896,
  707,
  821,
  1426,
  966,
  950,
  1012,
  909,
  726,
  909,
  1226,
  1304,
  1246,
  1151,
  898,
  837,
  693,
  733,
  1672,
  734,
  860,
  579
)

set.seed(99)

# calculate CDF (empirical CDF) of the data
x_cdf = ecdf(site_data)

plot(x_cdf)

# see values that correspond to the probabilities with location 
sort(x_cdf(site_data))
sort(site_data) 

# simulate 20 values from the ecdf 100 times 
sims <- replicate(100, as.numeric(quantile(x_cdf, runif(20))))
str(sims)

# calculate 100 means 
sim_means <- apply(sims, 2, mean)
length(sim_means)

# mean distribution
hist(sim_means)

# get function for look up using the 100 simulated means 
e_sim_means <- ecdf(sim_means)
# look up the value
as.numeric(quantile(e_sim_means, .975))

# more directly  
as.numeric(quantile(sort(sim_means), .975))









