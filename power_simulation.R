


# I did this power simulation to show how the random draws would work.
# This hopefully illustrates the purpose of a power analysis:
# there's random varaibility in your sampling, and if you re-run
# this experiment 10000 times, you'll get a distributino of 
# proportions.
#
# It's amazing that the 95% confidence interval thing works.
# I get 388 when I do 20K simulations. Sure I'd get it with 100K
#
# So this proves 95% CI and sample size, but how do I get power from this sim?
# 
# The other application is I can randomly sample from
# any distribution and calculate power.
# This lets me model the shape of my data
# and tell me how big the sample size needs to be.

# Papers on Hypothesis testing: https://stats.stackexchange.com/questions/10510/references-containing-arguments-against-null-hypothesis-significance-testing
# 
# Review of binom.test, chisq.test, prop.test: http://www.simonqueenborough.info/R/statistics/testing-ratios

library(ggplot2)
library(purrr)
library(dplyr)

generate_sequence <- function(n_trials, prob=prob){
  s <- rbinom(n_trials, 1, prob = prob)
  cum_mean <- cumsum(s)/1:n_trials
  return(cum_mean)
}

generate_sims <- function(n_trials, prob, n_sims){
  sims <- map_df(1:n_sims, function(i) {
    tibble(sim = i, 
           prob = prob, 
           idx = 1:n_trials,
           cum_mean = generate_sequence(n_trials, prob = prob))
  })
  return(sims)
}

generate_quantiles <- function(sims, tail = 0.025){
  q <- sims %>%
    group_by(idx, prob) %>%
    summarize(
      p_ll = quantile(cum_mean, probs = tail),
      # p500 = quantile(p, probs = 0.500),
      p500 = mean(cum_mean),
      p_ul = quantile(cum_mean, probs = (1-tail)),
    ) %>%
    ungroup()
  return(q)
}

# Power = what Proportion of Null are greater than 0.025 cutoff?
# The cool thing about this is I could use any distribution!
# Power curve is better than this because it's wicked fast.

main <- function(n_trials, prob, n_sims, tail){
  s <- generate_sims(n_trials, prob, n_sims)
  q <- generate_quantiles(sims=s, tail=tail)
  return(list(q = q, sims = s))
}



n_trials <- 500
n_sims <- 1000
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims)
Ha <- main(n_trials=n_trials, prob=0.6, n_sims=n_sims)

q <- Ho$q %>% bind_rows(Ha$q)
dim(q)

ggplot(q, 
       aes(x=idx, ymin=p025, y = p500, ymax = p975, 
           color=as.factor(prob), fill=as.factor(prob))) + 
  geom_ribbon(alpha = 0.1) + 
  geom_line() +
  theme_minimal()


# where the confidence intervals no longer overlap
# when I do 20K simulations, it's at 388
which(Ha$q$p025 > Ho$q$p975)[0:20]


power.prop.test(p1=0.7, p2 = 0.5, power = .80, sig.level = 0.05)
n_trials <- 500
n_sims <- 1000
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims)
Ha <- main(n_trials=n_trials, prob=0.7, n_sims=n_sims)
which(Ha$q$p025 > Ho$q$p975)[0:20]

# Binomial test: My simulation produces the "gr" binomial test.
# I guess "gr" makes sense because I'm taking 2.5% from the Null
# and 2.5% from the Alternative
# https://stats.stackexchange.com/a/550865/158148
set.seed(1103)
pv =  replicate(10^5, binom.test(rbinom(1,92,.70),92,,alt="gr")$p.val)
mean(pv <= .05)

# Power = proportion of cum_means in Ha > cum_means in Ho?
cutoffs <- tibble(idx = 1:n_trials, cutoff = Ha$q$p025)
x <- Ho$sims %>% left_join(cutoffs, by = 'idx')
x %>%
  group_by(idx) %>%
  summarize(
    # mistaken acceptance of the null hypothesis as the result of a test procedure, false negative
    beta = mean(cum_mean > cutoff), 
    power = 1-mean(cum_mean > cutoff)
  ) %>%
  filter(idx >= 90)












# POWER
# https://stattrek.com/hypothesis-test/statistical-power.aspx
# Region of acceptance: https://stattrek.com/hypothesis-test/analyze-survey-data.aspx#example2
Ho_p = 0.5
n = 388
acceptance_region = Ho_p + 1.96*sqrt(((Ho_p) * (1-Ho_p))/n)
# What proportion 
a <- Ha$sims %>% rename(a_mean = cum_mean)
a %>%
  filter(idx == n) %>%
  summarize(mean(a_mean < acceptance_region))
Ho$q %>%
  filter(idx == 388)



# Beta = probability of a false negative: failing to reject a false null hypothesis.

a <- Ha$sims %>% rename(a_mean = cum_mean)
o <- Ho$sims %>% rename(o_mean = cum_mean)
x <- a %>%
  left_join(o, by = c("sim", "idx")) %>%
  group_by(idx) %>%
  summarize(
    beta = mean(o_mean > a_mean) # mistakenly accepting
    )


# Power: What % of the time are the null simulations > 
# Power = what Proportion of the Null is above 0.025 cuttoff
cutoffs <- tibble(idx = 1:n_trials, cutoff = Ha$q$p025)

# Power = proportion of cum_means in Ha > cum_means in Ho?
x <- Ho$sims %>%
  left_join(cutoffs, by = 'idx')
x %>%
  group_by(idx) %>%
  summarize(
    # mistaken acceptance of the null hypothesis as the result of a test procedure, false negative
    beta = mean(cum_mean > cutoff), 
    power = 1-mean(cum_mean > cutoff)
    ) %>%
  filter(idx >= 300)

# Truth is 387
power.prop.test(p1=0.6, p2 = 0.5, power = .50, sig.level = 0.05,
                alternative = 'one.sided')
power.prop.test(p1=0.6, p2 = 0.5, power = .80, sig.level = 0.05)
library(pwr)
p.out <- pwr.2p.test(h=ES.h(p1 = 0.6, p2 = 0.50), power=.80, sig.level=.05)
p.out
plot(p.out) + theme_minimal()
























###
# Stackoverflow question
###
library(ggplot2)
library(purrr)
library(dplyr)

generate_sequence <- function(n_trials, prob=prob){
  s <- rbinom(n_trials, 1, prob = prob)
  cum_mean <- cumsum(s)/1:n_trials
  return(cum_mean)
}

generate_sims <- function(n_trials, prob, n_sims){
  sims <- map_df(1:n_sims, function(i) {
    tibble(sim = i, 
           prob = prob, 
           idx = 1:n_trials,
           cum_mean = generate_sequence(n_trials, prob = prob))
  })
  return(sims)
}

generate_quantiles <- function(sims){
  q <- sims %>%
    group_by(idx, prob) %>%
    summarize(
      p025 = quantile(cum_mean, probs = 0.025),
      # p500 = quantile(p, probs = 0.500),
      p500 = mean(cum_mean),
      p975 = quantile(cum_mean, probs = 0.975),
    ) %>%
    ungroup()
  return(q)
}

# Requires sample size of 93
power.prop.test(p1=0.7, p2 = 0.5, power = .80, sig.level = 0.05)

# Conduct Simulation
set.seed(123)
n_trials <- 150
n_sims <- 10000
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims)
Ha <- main(n_trials=n_trials, prob=0.7, n_sims=n_sims)

# Plot simulation
# Quantiles
q <- Ho$q %>% bind_rows(Ha$q)
ggplot(q, aes(x=idx, ymin=p025, y = p500, ymax = p975, 
           color=as.factor(prob), fill=as.factor(prob))) + 
  geom_ribbon(alpha = 0.1) + 
  geom_line() + 
  theme_minimal() + 
  geom_vline(xintercept = 93) +
  annotate(geom='text', x = 100, y = .9,
            label="N = 93 for 80% power, alpha=5%") + 
  labs(
    title = "95% percentiles for averages of sequences",
    y = "Mean of sequence",
    x = "Index in sequence",
    fill = "Distribution",
    color = "Distribution")

# Identify the first index where the 2.5th percentile of the 0.7
# distribution is greater than the 97.5% of the 0.5 distribution
# Says sample size of 92 to 94.
which(Ha$q$p025 > Ho$q$p975)[0:20]

# Binomial test: My simulation produces the "gr" binomial test.
# I guess "gr" makes sense because I'm taking 2.5% from the Null
# and 2.5% from the Alternative
# https://stats.stackexchange.com/a/550865/158148
set.seed(123)
pv = replicate(10^5, binom.test(x=rbinom(1,92,.70),n=92,p=0.5,alt="gr")$p.val)
mean(pv <= .05) # power = 0.9861

### Power calculated from the simulation
# I can calculate power by computing the fraction of Null
# simulations greater than the 0.025% cutoff of the
# alternative distribution
cutoffs <- tibble(idx = 1:n_trials, cutoff = Ha$q$p025)
x <- Ho$sims %>% left_join(cutoffs, by = 'idx')
x %>%
  group_by(idx) %>%
  summarize(
    # mistaken acceptance of the null hypothesis as the result of a test procedure, false negative
    beta = mean(cum_mean > cutoff), 
    power = 1-mean(cum_mean > cutoff)
  ) %>%
  filter(idx >= 88)
#     idx   beta   power
#    <int>  <dbl>  <dbl>
#     88   0.0221  0.978
#     89   0.017   0.983
#     90   0.0222  0.978
#     91   0.0177  0.982
#     92   0.0136  0.986
#     93   0.0184  0.982 <--- Same number as the power 
#     94   0.0147  0.985
#     95   0.0117  0.988
#     96   0.0158  0.984
#     97   0.0127  0.987



### 
set.seed(1)

power.prop.test(p1 = 0.7, p2 = 0.5, sig.level = 0.05, power = .80)

n <- 93
n_sims <- 10000

# Quantiles test
g1 <- replicate(n_sims, mean(rbinom(n, 1, 0.7)))
g0 <- replicate(n_sims, mean(rbinom(n, 1, 0.5)))
g1_025 <- quantile(g1, 0.025)
(power <- 1 - mean(g0 > g1_025)) # 0.9809

get_p <- function(){
  s1 <- rbinom(1, n, 0.7)
  s0 <- rbinom(1, n, 0.5)
  mat <- matrix(data = c(s0, n-s0, s1, n - s1), ncol=2)
  # compare the different tests
  p_b <- binom.test(x = s1, n = n, p=0.5)$p.val
  p_bgr <- binom.test(x = s1, n = n, p=0.5, alt='gr')$p.val
  p_c <- chisq.test(mat, correct=T)$p.val
  p_cc <- chisq.test(mat, correct = F)$p.val
  p_p <- prop.test(x = c(s0, s1), n = c(n, n), correct = T)$p.val
  p_pc <- prop.test(x = c(s0, s1), n = c(n, n), correct = F)$p.val
  tibble(p_b, p_bgr, p_c, p_cc, p_p, p_pc)
}
p_values <- map_df(1:n_sims, ~ get_p())
fail_to_reject <- p_values > 0.05
beta <- apply(fail_to_reject, 2, mean)
power <- 1 - beta
# p_b    p_bgr    p_c   p_cc    p_p   p_pc 
# 0.9719 0.9898 0.7495 0.7944 0.7495 0.7944









# EDIT 2
generate_quantiles <- function(sims, tail = 0.025){
  q <- sims %>%
    group_by(idx, prob) %>%
    summarize(
      p_ll = quantile(cum_mean, probs = (tail)),
      # p500 = quantile(p, probs = 0.500),
      p500 = mean(cum_mean),
      p_ul = quantile(cum_mean, probs = (1-tail)),
    ) %>%
    ungroup()
  return(q)
}

main <- function(n_trials, prob, n_sims, tail){
  s <- generate_sims(n_trials, prob, n_sims)
  q <- generate_quantiles(sims=s, tail=tail)
  return(list(q = q, sims = s))
}

power.prop.test(p1 = 0.5, p2 = 0.6, power = .8, sig.level = 0.05)
# 387.3385
n_sims <- 10000
n_trials <- 400
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims, tail=0.025)
Ha <- main(n_trials=n_trials, prob=0.6, n_sims=n_sims, tail=0.025)
which(Ha$q$p_ll > Ho$q$p_ul)[0:10]
# 384  386  388  390  391  392  393  395


power.prop.test(p1 = 0.5, p2 = 0.7, power = .8, sig.level = 0.05)
# n = 93
n_sims <- 10000
n_trials <- 105
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims, tail=0.025)
Ha <- main(n_trials=n_trials, prob=0.7, n_sims=n_sims, tail=0.025)
which(Ha$q$p_ll > Ho$q$p_ul)[0:10]
# 87   90   92   94   95   97   98   99  100  101 


power.prop.test(p1 = 0.5, p2 = 0.7, power = .8, sig.level = 0.01)
# 138 sample size
n_sims <- 10000
n_trials <- 200
Ho <- main(n_trials=n_trials, prob=0.5, n_sims=n_sims, tail=0.005)
Ha <- main(n_trials=n_trials, prob=0.7, n_sims=n_sims, tail=0.005)
which(Ha$q$p_ll > Ho$q$p_ul)[0:10]
# 150  153  155  156  157  158  159  160  161  162 
q <- Ho$q %>% bind_rows(Ha$q)
ggplot(q, aes(x=idx, ymin=p_ll, y = p500, ymax = p_ul, 
              color=as.factor(prob), fill=as.factor(prob))) + 
  geom_ribbon(alpha = 0.1) + 
  geom_line() +
  labs(title = 'Detecting at 99% confidence a MDE of 20% requires 150 per this plot', subtitle = 'But power analysis says 138')






#### Edit 4: Compare differences of proportion
