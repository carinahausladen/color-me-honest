"run the first chunk of mousetracking3.qmd before that"

library(runjags)
library(coda)

df %>%
  mutate(player.exphi = as.integer(player.exp == 4),
         player.mjrecon = as.integer(player.mjr <= 2),
         player.mjrped = as.integer(player.mjr %in% c(3, 5)),
         )->df

#Bayesian ordered logit regression 

ologit04 <- 'model {
for (i in 1:length(y)) {
y[i] ~ dinterval(latent[i], theta)
latent[i] ~ dlogis(PRS*x1[i] + CLR*x2[i] + PRS_CLR*x1[i]*x2[i] + EHI*x3[i] + ECON*x4[i] + PEDS*x5[i], 1)
}
for (j in 1:T) {
theta_base[j] ~ dnorm(0, .0001)
}
theta[1:T] <- sort(theta_base)
PRS ~ dnorm(0, .0001)
CLR ~ dnorm(0, .0001)
PRS_CLR ~ dnorm(0, .0001)
EHI ~ dnorm(0, .0001)
ECON ~ dnorm(0, .0001)
PEDS ~ dnorm(0, .0001)
}'

dataList <- with(subset(df, df$player.sex != 2), list(
  y = player.report_value - 1, x1 = player.treat_pressure, x2 = player.treat_color, 
  x3 = player.exphi, x4 = player.mjrecon, x5 = player.mjrped,
  T = max(player.report_value) - 1))
initList <- with(dataList, list(latent = y + 1/2, theta_base = 1:T))

set.seed(42)
fit_ologit04 <- run.jags(model = ologit04, data = dataList, inits = list(initList, initList),
                         monitor = c("PRS", "CLR", "PRS_CLR", "EHI", "ECON", "PEDS"),
                         modules = "glm", method = "parallel", 
                         adapt = 10000, burnin = 100000, sample = 10000000) #, thin = 1)


# bayesfactors 
bf_1<-mean(as.data.frame(as.mcmc(fit_ologit04))$PRS > 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$PRS <= 0)
bf_2<-mean(as.data.frame(as.mcmc(fit_ologit04))$CLR > 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$CLR <= 0)
bf_3<-mean(as.data.frame(as.mcmc(fit_ologit04))$PRS_CLR < 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$PRS_CLR >= 0)

bf_12<-mean(as.data.frame(as.mcmc(fit_ologit04))$EHI > 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$EHI <= 0)
bf_15<-mean(as.data.frame(as.mcmc(fit_ologit04))$ECON > 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$ECON <= 0)
bf_16<-mean(as.data.frame(as.mcmc(fit_ologit04))$PEDS > 0) / mean(as.data.frame(as.mcmc(fit_ologit04))$PEDS <= 0)


#----------- make table
orderedFIT<-fit_ologit04

odds <- exp(orderedFIT[["summary"]][["statistics"]][,1]) #odds ratio
est <- orderedFIT[["summary"]][["statistics"]][,1] # estimates
HPD_low <-orderedFIT[["HPD"]][,1]
HPD_up <-orderedFIT[["HPD"]][,3]
HPD <- paste("[", round(HPD_low, digits = 2), ",", round(HPD_up, digits = 2), "]", sep = "")
ESS <- round(orderedFIT[["mcse"]][["sseff"]], digits = 0) #ESS (Effective sample sizes, adjusted for autocorrelation)


table_names<-c("PRS","CLR","PRSxCLR","EHI", "ECON", "PED")  # check names(est)
bf<-c(bf_1,bf_2,bf_3,bf_12,bf_15,bf_16)
df_bayes = data.frame(odds, est, HPD, ESS, bf, row.names = table_names, stringsAsFactors = FALSE)
colnames(df_bayes)<-c("Odds Ratio", "Estimate", "[HPD 95%]", "ESS", "BF (>0)")
df_bayes$psrf<-c(fit_ologit04[["psrf"]][["psrf"]][,1])
df_bayes <- df_bayes %>%
  mutate_at(vars(1, 2, 4, 5,6), as.numeric)%>%
  mutate_at(vars(1, 2, 4, 5,6), ~ round(., 2))

rownames_to_column(df_bayes, var = "var") %>% 
  gt(rowname_col = "var")  %>%
  gtsave("table.tex")



