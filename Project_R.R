## Project 4: Module 4
##Student Name: Monika Gundecha

#Loading Libraries
library(triangle) 
library(ggplot2)
library(MASS)

# Part 1: Decision model to minimize the total inventory cost
unt_prc<- 78
opp_cst_pr_yr<- 0.18
cst_pr_ordr<- 1180
opp_cst_pr_mnth <- 0.015

no_ordrs_pr_yr<-seq(1,36)
reordr_time<- 12/no_ordrs_pr_yr
unts_pr_ordr <- 15000/no_ordrs_pr_yr
ordrng_cst <- cst_pr_ordr*no_ordrs_pr_yr

hldng_cst<- opp_cst_pr_mnth*unt_prc*unts_pr_ordr*no_ordrs_pr_yr*(reordr_time+1)/2

#Taking log scale as the numbers are large
ttl_invntry_cst<- ordrng_cst+hldng_cst
#Plot
plot(ttl_invntry_cst)

#Finding the index(number of orders per year) of the observation with minimum total inventory cost:
which.min(ttl_invntry_cst)

#Minimum total inventory cost:
ttl_invntry_cst[which.min(ttl_invntry_cst)]

print(ttl_invntry_cst[which.min(ttl_invntry_cst)])

## Part 2: Minimizing total inventory cost annual demand is not constant and follows triangular distribution.
ordrs_pr_yr<- rtriangle(1000, a = 13000, b = 17000, c = 15000)

#Function to calculate total inventory cost
ttl_cst_fun <- function(ordrs_pr_yr) {
  
#Finding general holding and ordering cost
hldng_cst_ii<- opp_cst_pr_yr*ordrs_pr_yr* unit_price/2
ordrng_cst_ii<- cst_pr_ordr*2
ttl_invntry_cst_ii<- hldng_cst_ii+ordrng_cst_ii
  return(ttl_invntry_cst_ii)
}

#Finding total inventory cost for each of the 1000 occurrence of number of orders per year
ttl_invntry_csts<- sapply(ordrs_pr_yr, ttl_cst_fun)

hist(ttl_invntry_csts, breaks = 15, main = "Histogram of Total Inventory Costs", xlab = "Total Inventory Cost", col = "red", border = "white")

#Average and SD of total inventory costs
avg_ttl_invntry_csts <- mean(ttl_invntry_csts)
sd_ttl_invntry_csts<- sd(ttl_invntry_csts)
#Confidence interval
cnf_intrvl_ttl_cst<- quantile(ttl_invntry_csts, c(0.025, 0.975))

# Output
cat("Average Total Inventory Cost:", avg_ttl_invntry_csts, "\n")
cat("Standard Deviation of Total Inventory Cost:", sd_ttl_invntry_csts, "\n")
cat("Confidence Interval (95%) for the  Total Inventory Cost:", cnf_intrvl_ttl_cst[1], "-", cnf_intrvl_ttl_cst[2], "\n")

#Average order quantity
avg_ordr_qty <- mean(ordrs_pr_yr) / 2
sd_ordr_qty<- sd(ordrs_pr_yr) / 2
cnf_intrvl_ordr_qty<- quantile(ordrs_pr_yr / 2, c(0.025, 0.975))

#Output
cat("Average of Order Quantity:",avg_ordr_qty, "\n")
cat("Standard Deviation of Order Quantity:",sd_ordr_qty , "\n")
cat("Confidence Interval (95%) for the  Order Quantity:", cnf_intrvl_ordr_qty[1], "-", cnf_intrvl_ordr_qty[2], "\n")

#Probability Distribution
ordr_qntty_ft <- fitdistr(ordrs_pr_yr / 2, "gamma")
print("Best-Fit Distribution for Order Quantity:")
print(ordr_qntty_ft)

#Avg number of orders:
avg_no_ordrs <- mean(ordrs_pr_yr) / (2 * avg_ordr_qty)
sd_no_ordrs <- sd(ordrs_pr_yr) / (2 * avg_ordr_qty)
cnf_intrvl_no_ordrs <- quantile(ordrs_pr_yr / (2 *ordrs_pr_yr), c(0.025, 0.975))

#Output
cat("Average Number of Orders per Year:", avg_no_ordrs, "\n")
cat("Standard Deviation of Number of Orders per Year:", sd_no_ordrs, "\n")
cat("95% Confidence Interval for Average Number of Orders per Year:", cnf_intrvl_no_ordrs[1], "-", cnf_intrvl_no_ordrs[2], "\n")

#Probability Distribution
no_ordrs_ft <- fitdistr(ordrs_pr_yr / (2 *avg_ordr_qty), "gamma")

# Output results for number of orders distribution
print("Parameters of Best-Fit Distribution for Number of Orders per Year:")
print(no_ordrs_ft)

