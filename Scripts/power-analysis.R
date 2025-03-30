library(tidyverse)
library(brms)
#library(MASS)  # For mvrnorm(), note that this masks tidyverse's select() function which is quite annoying
library(lme4)
library(tidyr)
library("broom") 
#getwd()


data = read_csv('data_for_power.csv') %>% #pilot data
  dplyr::select('Item', 'prime_freq', 'target_freq')
head(data,3)

n_subjects = 25
n_items = 30



b0 = -49.54 #Intercept
b1 = -7.46 #prime
b2 = -4.79 #target
b3 = -4.42 #interaction

#standard deviations from pilot data
#these are our subject-level effects 
b0j = 10  #Intercept
b1j = 10   #Prime Frequency
b2j = 10  #Target Frequency
b3j = 10  #Interaction (Prime*Target)

b0k = 9  #Intercept
b1k = 10  #Prime Frequency
b2k = 10  #Target Frequency
b3k = 10  #Interaction (Prime*Target)

noise_sd = 200
n_simulations = 100

prime_freq_data = rep(c(0,1), each = n_items/2)
target_freq_data = rep(c(0,1), times = n_items/2)


df.power2 = expand_grid(n = c(10, 50, 250, 500, 1000),
                        simulation = 1:n_simulations) %>%
  mutate(index = 1:n(),
         .before = n) %>% 
  group_by(index, n, simulation) %>% 
  mutate(data = list(tibble(
    subject = rep(1:n, each = n_items),
    item = rep(1:n_items, times = n)
  ) %>%
    mutate(
      # Assign subject and item effects
      b0j = rep(rnorm(n = n, mean = 0, sd = b0j), each = n_items),
      b1j = rep(rnorm(n = n, mean = 0, sd = b1j), each = n_items),
      b2j = rep(rnorm(n = n, mean = 0, sd = b2j), each = n_items),
      b3j = rep(rnorm(n = n, mean = 0, sd = b3j), each = n_items),
      b0k = rep(rnorm(n = n_items, mean = 0, sd = b0k), times = n),
      b1k = rep(rnorm(n = n_items, mean = 0, sd = b1k), times = n),
      b2k = rep(rnorm(n = n_items, mean = 0, sd = b2k), times = n),
      b3k = rep(rnorm(n = n_items, mean = 0, sd = b3k), times = n),
      
      # Error term
      epsiloni = rnorm(n * n_items, mean = 0, sd = noise_sd),
      
      # Condition coding
      prime_freq = rep(prime_freq_data, times = n),
      target_freq = rep(target_freq_data, times = n),
      
      # Response time computation
      rt = (b0 + b0j + b0k) + 
        (b1 + b1j + b1k) * prime_freq + 
        (b2 + b2j + b2k) * target_freq + 
        (b3 + b3j + b3k) * (prime_freq * target_freq) + 
        epsiloni
    ))) %>%
  mutate(fit = map(.x = data,
                   .f = ~ glmer(formula = rt ~ prime_freq * target_freq + (prime_freq*target_freq||subject) + 
                               (prime_freq*target_freq||item),
                             data = .x)),
         parameters = map(.x = fit,
                          .f = ~ tidy(.x))) %>%
  dplyr::select(index, n, simulation,  parameters) %>% 
  unnest(cols = parameters) %>% 
  filter(term == "prime_freq:target_freq") %>% 
  select(index, n, simulation, p.value) %>% 
  group_by(n) %>% 
  summarize(power = sum(p.value < 0.05) / n()) %>% 
  ungroup()



b0 = -49.54 #Intercept
b1 = -7.46 #prime
b2 = -4.79 #target
b3 = -4.42 #interaction

#standard deviations from pilot data
#these are our subject-level effects 
b0j = 77.15  #Intercept
b1j = 10   #Prime Frequency
b2j = 10  #Target Frequency
b3j = 10  #Interaction (Prime*Target)

b0k = 9  #Intercept
b1k = 10  #Prime Frequency
b2k = 10  #Target Frequency
b3k = 10  #Interaction (Prime*Target)

n = n_subjects  # Number of subjects

noise_sd = 350

d = tibble(
  subject = rep(1:n, each = n_items),
  item = rep(1:n_items, times = n)
) %>%
  mutate(
    # Assign subject and item effects
    b0j = rep(rnorm(n = n, mean = 0, sd = b0j), each = n_items),
    b1j = rep(rnorm(n = n, mean = 0, sd = b1j), each = n_items),
    b2j = rep(rnorm(n = n, mean = 0, sd = b2j), each = n_items),
    b3j = rep(rnorm(n = n, mean = 0, sd = b3j), each = n_items),
    b0k = rep(rnorm(n = n_items, mean = 0, sd = b0k), times = n),
    b1k = rep(rnorm(n = n_items, mean = 0, sd = b1k), times = n),
    b2k = rep(rnorm(n = n_items, mean = 0, sd = b2k), times = n),
    b3k = rep(rnorm(n = n_items, mean = 0, sd = b3k), times = n),
    
    # Error term
    epsiloni = rnorm(n * n_items, mean = 0, sd = noise_sd),
    
    # Condition coding
    prime_freq = rep(ifelse(data$prime_freq == 'unrelated', 0, 1), times = n),
    target_freq = rep(ifelse(data$target_freq == 'not-frequent', 0, 1), times = n),
    
    # Response time computation
    rt = (b0 + b0j + b0k) + 
      (b1 + b1j + b1k) * prime_freq + 
      (b2 + b2j + b2k) * target_freq + 
      (b3 + b3j + b3k) * (prime_freq * target_freq) + 
      epsiloni
  )


d$subject = factor(d$subject)
d$item = factor(d$item)
d$target_freq = factor(d$target_freq)
d$prime_freq = factor(d$prime_freq)



sim_d = function(seed, n) {
  
  set.seed(seed)
  
  n_subjects = n
  
  b0 = -49.54 #Intercept
  b1 = -7.46 #prime
  b2 = -4.79 #target
  b3 = -4.42 #interaction
  
  #standard deviations from pilot data
  #these are our subject-level effects 
  b0j = 77.15  #Intercept
  b1j = 10   #Prime Frequency
  b2j = 10  #Target Frequency
  b3j = 10  #Interaction (Prime*Target)
  
  b0k = 9  #Intercept
  b1k = 10  #Prime Frequency
  b2k = 10  #Target Frequency
  b3k = 10  #Interaction (Prime*Target)
  
  n = n_subjects  # Number of subjects
  
  d = tibble(
    subject = rep(1:n, each = n_items),
    item = rep(1:n_items, times = n)
  ) %>%
    mutate(
      # Assign subject and item effects
      b0j = rep(rnorm(n = n, mean = 0, sd = b0j), each = n_items),
      b1j = rep(rnorm(n = n, mean = 0, sd = b1j), each = n_items),
      b2j = rep(rnorm(n = n, mean = 0, sd = b2j), each = n_items),
      b3j = rep(rnorm(n = n, mean = 0, sd = b3j), each = n_items),
      b0k = rep(rnorm(n = n_items, mean = 0, sd = b0k), times = n),
      b1k = rep(rnorm(n = n_items, mean = 0, sd = b1k), times = n),
      b2k = rep(rnorm(n = n_items, mean = 0, sd = b2k), times = n),
      b3k = rep(rnorm(n = n_items, mean = 0, sd = b3k), times = n),
      
      # Error term
      epsiloni = rnorm(n * n_items, mean = 0, sd = noise_sd),
      
      # Condition coding
      prime_freq = rep(ifelse(data$prime_freq == 'unrelated', 0, 1), times = n),
      target_freq = rep(ifelse(data$target_freq == 'not-frequent', 0, 1), times = n),
      
      # Response time computation
      rt = (b0 + b0j + b0k) + 
        (b1 + b1j + b1k) * prime_freq + 
        (b2 + b2j + b2k) * target_freq + 
        (b3 + b3j + b3k) * (prime_freq * target_freq) + 
        epsiloni
    )
  
  
  d$subject = factor(d$subject)
  d$item = factor(d$item)
  d$target_freq = factor(d$target_freq)
  d$prime_freq = factor(d$prime_freq)
  
  d = d #so that d is returned
}


## first model
t1 = Sys.time()
fit = 
  brm(data = d, cores = 4, chains = 4, iter = 6000, warmup = 3000,
      rt ~ 0 + Intercept + prime_freq*target_freq + 
        (prime_freq*target_freq||subject) + 
        (prime_freq*target_freq||item),
      #control = list(adapt_delta = 0.95),
      prior = c(set_prior("student_t(3, 0, 10)", class = "b"),
                set_prior("student_t(3, 0, 24.32)", class = "sd")), #from our model estimate
      seed = 465) #random seed
t2 = Sys.time()
t2 - t1 #11mins / 15.6mins'

################### simulation parameters #########################################

n_sim = 100
n_subjs = 1000


####################################################################################

s = #n = 50, b0 = -20, b1 = 0: power of 0.33
  tibble(seed = 1:n_sim) %>% 
  mutate(d = map(seed, sim_d, n = n_subjs)) %>% 
  mutate(fit = map2(d, seed, ~update(fit, newdata = .x, seed = .y, 
                                     chains = 4, cores = 4, iter = 4000, warmup = 2000)))



s = s %>%
  mutate(
    summary = map(fit, ~as.data.frame(fixef(.x, probs = c(0.025, 0.975)))), #Extract fixed effects
    prime_freq_check = map_lgl(summary, ~.x["prime_freq1", "Q97.5"] < 0), #Check if upper CI bound is < 0 for "prime_freq"
    target_freq_check = map_lgl(summary, ~.x["target_freq1", "Q97.5"] < 0),
    prime_target_check = map_lgl(summary, ~.x["prime_freq1:target_freq1", "Q97.5"] < 0),
    prime_freq = map_dbl(summary, ~.x["prime_freq1", "Estimate"]),
    target_freq = map_dbl(summary, ~.x["target_freq1", "Estimate"]),
    prime_target_freq = map_dbl(summary, ~.x["prime_freq1:target_freq1", "Estimate"]),
    prime_freq_2.5 = map_dbl(summary, ~.x["prime_freq1", "Q2.5"]),
    target_2.5 = map_dbl(summary, ~.x["target_freq1", "Q2.5"]),
    prime_target_2.5 = map_dbl(summary, ~.x["prime_freq1:target_freq1", "Q2.5"]),
    prime_freq_97.5 = map_dbl(summary, ~.x["prime_freq1", "Q97.5"]),
    target_97.5 = map_dbl(summary, ~.x["target_freq1", "Q97.5"]),
    prime_target_97.5 = map_dbl(summary, ~.x["prime_freq1:target_freq1", "Q97.5"])
  )

#proportion of models where "prime_freq" CI upper bound is < 0
prime_freq_below_zero = mean(s$prime_freq_check)
target_freq_below_zero = mean(s$target_freq_check)
prime_target_below_zero = mean(s$prime_target_check)

prime_2.5 = mean(s$prime_freq_2.5)
target_2.5 = mean(s$target_2.5)
prime_target_2.5 = mean(s$prime_target_2.5)

prime_97.5 = mean(s$prime_freq_97.5)
target_97.5 = mean(s$target_97.5)
prime_target_97.5 = mean(s$prime_target_97.5)

prime_est = mean(s$prime_freq)
target_est = mean(s$target_freq)
prime_target_est = mean(s$prime_target_freq)

prime_freq_below_zero
target_freq_below_zero
prime_target_below_zero

prime_2.5 
target_2.5
prime_target_2.5

prime_97.5
target_97.5
prime_target_97.5 #1000 subjs
