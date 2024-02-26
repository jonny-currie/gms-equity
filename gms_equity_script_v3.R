#Analysis of GMS funding in Wales 2014-2022 and equity of distribution between practices

#packages

library(tidyverse)
library(readxl)
library(janitor)
library(viridis)
library(patchwork)
library(kableExtra)
library(cowplot)
#library(smplot2)
library(boot)
library(broom)
library(knitr)
library(ggeffects)
library(installr)

#data

#3 sources:
# 1 = global sum data available 2004 - 2022
# 2 = additional payments inc performance-related ££ available 2014 - 2023
# 3 = deprivation lookup by practice

#Firstly need to get funding data into tidy format
#then merge and see % missing data

#Import global sum data

gsum <- read_xlsx(path="All Wales GSUM 2004-2022.xlsx", sheet=1, 
                  col_types = 
                    c("text", "text", "text", "text", 
                      "date", "text", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% 
  clean_names() 

gsum %>% 
  #filter(fin_yr == "2020-2021") %>%
  glimpse()

#aggregate to annual

annual_gsum <- gsum %>%
  group_by(lhb, practice, fin_yr) %>%
  summarise(quarterly_mpig = sum(quarterly_mpig), 
            quarterly_gsum = sum(quarterly_gsum)) %>%
  mutate(gsum_mpig = quarterly_mpig + quarterly_gsum) %>%
  ungroup()

#need a lookup for practice size, median of quarter
#NOTE THIS INVOLVES RAW list size, not normalised/weighted#

gsum_lookup <- gsum %>%
  ungroup() %>%
  select(lhb, practice, fin_yr, raw_practice_list) %>%
  group_by(practice, fin_yr) %>%
  summarise(raw_practice_list = median(raw_practice_list))

annual_gsum_prac_size <- left_join(annual_gsum, gsum_lookup, 
                                   by = c("practice", "fin_yr")) %>%
  select(-lhb)

head(annual_gsum_prac_size)

add_payments <- read_xlsx("All Wales Payment data 2015 - 2023.xlsx", 
                          sheet = 3, 
                          skip = 2) %>%
  select(1,2,3,4,5) %>%
  clean_names()

#Need to aggregate all payments to annual
#and recode year data to "2004-05"

unique(add_payments$practice)

add_payments <- add_payments %>%
  group_by(practice, year, type) %>%
  summarise(add_payment = sum(x)) %>%
  ungroup()

#Recode year data

add_payments <- add_payments %>%
  ungroup() %>%
  mutate(year = case_when(
    year == "2014/15" ~ "2014-2015",
    year == "2015/16" ~ "2015-2016",
    year == "2016/17" ~ "2016-2017",
    year == "2017/18" ~ "2017-2018",
    year == "2018/19" ~ "2018-2019",
    year == "2019/20" ~ "2019-2020",
    year == "2020/21" ~ "2020-2021",
    year == "2021/22" ~ "2021-2022",
    year == "2022/23" ~ "2022-2023"
  ))

#Now need to merge both funding datasets

head(annual_gsum_prac_size)
head(add_payments)

#Sync variable names

names(annual_gsum_prac_size)

annual_gsum_prac_size <- annual_gsum_prac_size %>%
  rename(year = fin_yr)

add_payments_agg <- add_payments %>%
  group_by(practice, year) %>%
  summarise(add_payment = sum(add_payment)) %>%
  ungroup()

final_funding <- left_join(annual_gsum_prac_size, add_payments_agg, 
                           by = c("practice", "year")) %>%
  filter(year %in% c("2014-2015", "2015-2016", "2016-2017", 
                     "2017-2018", "2018-2019", "2019-2020", 
                     "2020-2021", "2021-2022"))

#how many GP practices?

final_funding %>% 
  select(practice) %>% unique()

#466 practices 

#NAs seem to be in add_payment column, count how many

sum(is.na(final_funding$add_payment))
#79 rows

final_funding

#Remove as need complete data

final_funding <- final_funding %>% na.omit()

final_funding %>% 
  select(practice) %>% unique()

#456 practices

#Import deprivation lookup

depriv_lookup <- read_xlsx(
  "GP population WIMD GP workforce analysis to share May 2022.xlsx",
  sheet = 2) %>%
  clean_names() %>%
  select(practice_code, practice_quintile_based_on_percentage_of_practice_population, 
         percentage_of_practice_population_in_most_deprived_20_percent, 
         count_of_practice_population_in_most_deprived_20_percent) %>%
  rename(practice = 1, quintile = 2, raw_pc = 3, count_depriv = 4)

#Merge with funding

funding_lookup <- left_join(final_funding, depriv_lookup, 
                            by = "practice")

sum(is.na(funding_lookup$quintile))

#317 rows

funding_lookup %>% select(practice) %>% unique()

#456 practices

#Remove NAs

funding_lookup <- funding_lookup %>% na.omit()

funding_lookup %>% select(practice) %>% unique()

#380 practices

funding_lookup %>%
  ungroup() %>%
  filter(year == "2021-2022") %>%
  distinct() %>%
  count(quintile) %>%
  rename("Deprivation quintile" = quintile, 
         "Number of GP practices" = n) %>%
  kbl(caption = "Numbers of GP practices per deprivation quintile") %>%
  kable_minimal()

#Create dataset with simple funding, size and deprivation variables

simple_model_df <- funding_lookup %>%
  mutate(total_funding = gsum_mpig + add_payment) %>%
  select(practice, year, total_funding, raw_practice_list, raw_pc)

#Deflate funding data (discuss with the group)
#see https://www.gov.uk/government/collections/gdp-deflators-at-market-prices-and-money-gdp
#apply prices in 2014/15 (initial period of analysis)

deflators <- tibble(
  year = c("2014-2015", "2015-2016", "2016-2017", "2017-2018", 
           "2018-2019", "2019-2020", "2020-2021", "2021-2022"), 
  def = c(81.870, 82.459, 84.335, 85.656, 87.462, 89.527, 94.403, 93.629)) %>%
  mutate(def = (81.870/def))

sapply(simple_model_df, class)

simple_model_df <- left_join(simple_model_df, deflators, 
          by = "year") %>%
  mutate(total_funding = total_funding*def) %>%
  select(-def)

#visualise each variable
#funding

simple_model_df %>%
  ggplot(aes(y=total_funding)) + 
  geom_histogram() + 
  facet_grid(~year)

#Right skewed, though less over time

simple_model_df %>%
  mutate(total_funding = log(total_funding)) %>%
  ggplot(aes(y=total_funding)) + 
  geom_histogram() + 
  facet_grid(~year)

#more normally distributed

simple_model_df %>%
  ggplot(aes(y=raw_practice_list)) + 
  geom_histogram() + 
  facet_grid(~year)

#right skewed

simple_model_df %>%
  ggplot(aes(y=raw_pc)) + 
  geom_histogram() + 
  facet_grid(~year)

#V. right skewed
#advice from DPM to leave raw

#Visualise relationship between funding and variables

simple_model_df %>%
  ggplot(aes(x=year, y=total_funding)) + 
  geom_point()

simple_model_df %>%
  ggplot(aes(x=raw_practice_list, y=total_funding)) + 
  geom_point()

#Definite relationship

a<- simple_model_df %>%
  ggplot(aes(x=raw_pc, y=(total_funding))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

b<- simple_model_df %>%
  ggplot(aes(x=raw_pc, y=log(total_funding))) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)

a+b

#appears to be subtle negative linear relationship

#Change year/time to continuous

simple_model_df <- simple_model_df %>%
  mutate(year = case_when(
    year == "2014-2015" ~ 2014,
    year == "2015-2016" ~ 2015,
    year == "2016-2017" ~ 2016,
    year == "2017-2018" ~ 2017,
    year == "2018-2019" ~ 2018,
    year == "2019-2020" ~ 2019,
    year == "2020-2021" ~ 2020,
    year == "2021-2022" ~ 2021))

simple_model_df_log <- simple_model_df %>%
  mutate(funding_pp = total_funding / raw_practice_list,
         funding_pp_log = log(funding_pp))

model <- lm(formula = funding_pp_log ~ raw_pc*year, 
                     data=simple_model_df_log)

summary(model)

#appears there is a negative linear relationship between practice deprivation and funding pp at practice level

#coefficient is -0.116 with p-value 0.0000432

simple_model_df_log2 <- simple_model_df_log %>%
  mutate(year = year-2014)

model2 <- lm(formula = funding_pp_log ~ raw_pc*year, 
            data=simple_model_df_log2)

summary(model2)

out <- tidy(model2)

out

out %>%
  kbl(digits = 10, 
      caption = "Linear regression model output summary, relationship between GP practice funding, practice deprivation and time in Wales, 2014-2022") %>%
  kable_minimal()

#Plot of model output

simple_model_df_log2 %>%
  #mutate(funding_pp_log = exp(funding_pp_log)) %>%
  ggplot(aes(raw_pc, funding_pp_log)) + 
  geom_point() + 
  geom_abline(slope = coef(model2)[["raw_pc"]],
              intercept = coef(model2)[["(Intercept)"]], 
              colour = "red") + 
  theme_minimal() + 
  labs(y="(Log)Total practice funding per patient (£)", 
       x="% of patients in practice population living in 20% most deprived LSOAs", 
       title = "Scatterplot showing relationship between practice population deprivation and total practice funding", 
       subtitle = "GP practices in Wales, 2014-2022") + 
  scale_x_continuous(labels = scales::percent_format()) + 
  annotate(x=0.6, y=5.3, geom = "text", label = "Coefficient = -0.104898   , p = 1.07e-8", colour = "red")

# Graphs

#trend in practice funding over time

simple_model_df_log %>%
  ungroup() %>%
  ggplot(aes(x=year, y=median(total_funding/raw_practice_list))) + 
  geom_line()

simple_model_df_log %>%
  mutate(funding_pp = total_funding/raw_practice_list) %>%
  ggplot() + 
  geom_pointrange(aes(x=year, y=funding_pp),
         stat = "summary",
         fun.min = function(z) {quantile(z,0.25)},
         fun.max = function(z) {quantile(z,0.75)},
         fun = median)

simple_model_df_log %>%
  mutate(funding_pp = total_funding/raw_practice_list) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x=year, y=funding_pp)) + 
  geom_violin(aes(fill=year)) +
  geom_boxplot(width = 0.1, outlier.shape = NA) + 
  theme_minimal() + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette="Dark2") + 
  labs(x=NULL, y="Total practice funding per patient (£)", 
       title = "GP practice funding per patient in Wales, 2014-2022",
       subtitle = "Median, interquartile ranges and overall distributions",
       caption = "Source: NWSSP, 2023.")

#Median stats

simple_model_df_log %>%
  mutate(funding_pp = total_funding/raw_practice_list) %>%
  group_by(year) %>%
  summarise(iqr_funding = IQR(funding_pp), 
            med_funding = median(funding_pp)) %>%
  mutate(funding_0.25 = med_funding-iqr_funding, 
         funding_0.75 = med_funding+iqr_funding)

#Try and back transform model output and plot

plot(ggpredict(model2))

#this is plot of time vs funding (log)

coefficients <- ggpredict(model2, terms = c("raw_pc")) %>%
  as_tibble() %>%
  mutate(predicted = exp(predicted)) %>%
  rename(raw_pc = x)

simple_model_df_log2 %>%
  mutate(funding_pp_log = exp(funding_pp_log)) %>%
  ggplot(aes(raw_pc, funding_pp_log)) + 
  geom_point() + 
  geom_line(data = coefficients, 
            aes(y=predicted), 
            colour = "red") + 
  theme_minimal() + 
  labs(y="Total practice funding per patient (£)", 
       x="% of patients in practice population living in 20% most deprived LSOAs", 
       title = "Scatterplot showing relationship between practice population deprivation and total practice funding", 
       subtitle = "GP practices in Wales, 2014-2022") + 
  scale_x_continuous(labels = scales::percent_format()) + 
  annotate(x=0.6, y=170, geom = "text", label = "Coefficient = -0.104898   , p = 1.07e-8", colour = "red")

#https://stackoverflow.com/questions/71324820/how-to-backtransform-variables-transformed-with-log1p-when-creating-a-plot-using
