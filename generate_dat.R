library(tidyverse)

set.seed(1729)
industries <- c("Agriculture",
                "Construction",
                "Food Services",
                "Finance and Insurance",
                "Education",
                "Healthcare")

mu <- data_frame(INDUSTRY = industries, 
                 MU = c(log(50), log(100), log(20), log(25), log(30), log(80)),
                 MOBILITY_BETA = c(0.05, 30, 10, 0.3, 5, 10),
                 MOBILITY_ALPHA = log(c(2000, 5000, 1500, 2500, 2800, 4500)))

percents <- c(7, 23, 32, 18, 10, 17) / sum(c(7, 23, 32, 18, 10, 17))


df <- data_frame(INDUSTRY = sample(industries, size = 10000, prob = percents, replace = T)) %>% 
  left_join(mu)

df <- df %>% 
  mutate(EMP = round(exp(rnorm(n(), MU)))) %>% 
  mutate(EMP = ifelse(EMP < 10, 10, EMP)) %>% 
  mutate(ANNUAL_SALES = round(rnorm(n(), 50000*EMP, 10000*EMP))) %>% 
  mutate(ANNUAL_SALES = ifelse(ANNUAL_SALES < 100000, 100000, ANNUAL_SALES)) %>% 
  mutate(PROVINCE = sample(c('AB', 'BC'), n(), T)) %>% 
  mutate(AB_MULTIPLIER = case_when(
    .$PROVINCE == "AB" & .$INDUSTRY == "Construction" ~ 0.76,
    .$PROVINCE == "AB" & .$INDUSTRY == "Finance and Insurance" ~ 1.5,
    .$PROVINCE == "AB" & .$INDUSTRY == "Food Services" ~ 0.6,
    TRUE ~ 1
  )) %>% 
  mutate(MOBILITY = rnorm(n(), MOBILITY_BETA, MOBILITY_BETA/4)*EMP + 
           exp(rnorm(n(), MOBILITY_ALPHA, MOBILITY_ALPHA/4))) %>% 
  mutate(ANNUAL_SALES = AB_MULTIPLIER * ANNUAL_SALES, MOBILITY = AB_MULTIPLIER * MOBILITY) %>% 
  mutate(SIZE_QTILE = ntile(ANNUAL_SALES, 5)) %>% 
  left_join(data_frame(SIZE_QTILE = 1:5, PROB = c(0.8, 0.5, 0.3, 0.2, 0.005))) %>% 
  mutate(LOW_PLAN = rnorm(n()) < PROB) %>% 
  mutate(INTERNET = ifelse(LOW_PLAN, rnorm(n(), 1850, 500),
                           ifelse(0.08 * ANNUAL_SALES < 6000, rnorm(n(), 0.08, 0.02)*ANNUAL_SALES, 6000*rnorm(n(), (SIZE_QTILE+2.5)/5, 0.5)))) %>% 
  mutate(INTERNET = ifelse(INTERNET < 390, rnorm(n(), 390, 60), INTERNET)) %>% 
  mutate(MOBILITY = ifelse(runif(n()) < 0.03, 0, MOBILITY),
         INTERNET = ifelse(runif(n()) < 0.05, 0, INTERNET)) %>% 
  mutate(INDUSTRY = ifelse(runif(n()) < 0.0123, NA, INDUSTRY),
         EMP = ifelse(runif(n()) < 0.122, NA, EMP), 
         ANNUAL_SALES = ifelse(runif(n()) < 0.054, NA, ANNUAL_SALES)) %>% 
  mutate(INDUSTRY = ifelse(runif(n()) < 0.0123, NA, 9999999999),
         EMP = ifelse(runif(n()) < 0.122, NA, 9999999999), 
         ANNUAL_SALES = ifelse(runif(n()) < 0.054, NA, 9999999999)) %>% 
  mutate(CUSTOMER_ID = sample(120000:130000, 10000, replace = F))


write_csv(select(df, CUSTOMER_ID, INDUSTRY, EMP, ANNUAL_SALES, 
                 PROVINCE, MOBILITY, INTERNET), 'customer_info.csv')
