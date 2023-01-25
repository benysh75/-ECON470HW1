## Title: ECON 470 HW1
## Author: Ben Yang
## Date Created: 1/23/2023
## Date Edited: 1/25/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

## Read data and set workspace for knitr ---------------------------------------

full.ma.data <- read_rds("data/output/full_ma_data.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
ma.penetration.data <- read_rds("data/output/ma_penetration.rds")

## Merge data sets to create final data set ------------------------------------

final.data <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" & # remove snp, eghp
           (planid < 800 | planid >= 900)) %>% # remove 800-series
  inner_join(contract.service.area %>% # contracts approved in respective counties
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))
  
final.data <- final.data %>%
  left_join(ma.penetration.data %>% ungroup() %>% select(-ssa) %>%
              rename(state_long=state, county_long=county), 
            by=c("fips", "year"))

final.state <- final.data %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))

final.data <- final.data %>%
  left_join(plan.premiums,
            by=c("contractid","planid","state_name"="state","county","year"))

## Create objects for markdown -------------------------------------------------

# tot.obs <- dim(full.ma.data)[1]
tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))

# num.dist.plan_types <- n_distinct(full.ma.data$plan_type, na.rm = FALSE)
plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
num.dist.plan_types <- nrow(plan.type.table)

# plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% 
#   count() %>% arrange(year, -n) %>% # count
#   pivot_wider(names_from = year, values_from = n) # pivot

plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) %>% filter(plan_type != "NA") # count
plan.type.year1 <- pivot_wider(plan.type.year1, names_from = "year", values_from = "n", names_prefix = "Count_") # pivot

# plan.type.year2 <- full.ma.data %>% filter(snp == "No", eghp == "No") %>% # remove snp, eghp
#   filter(planid < 800 | planid >= 900) %>% # remove 800-series
#   group_by(plan_type, year) %>% 
#   count() %>% arrange(year, -n) %>% # count
#   pivot_wider(names_from = year, values_from = n) # pivot

final.plans <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" & # remove snp, eghp
           (planid < 800 | planid >= 900)) # remove 800-series
plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n) %>% filter(plan_type != "NA")
plan.type.year2 <- pivot_wider(plan.type.year2, names_from = "year", values_from = "n", names_prefix = "Count_") # pivot

avg.enrollment <- final.data %>% 
  group_by(fips, year) %>%
  select(fips, year, avg_enrollment) %>% 
  summarise(all_enroll = sum(avg_enrollment)) %>%
  group_by(year) %>% 
  summarise(avg_all_enroll = mean(all_enroll))

fig.avg.enrollment_plot <- avg.enrollment %>%
  ggplot(aes(x = factor(year), y = avg_all_enroll)) + geom_col() +
  labs(x = "Year", y = "Average Enrollment", title = "Average Number of Medicare Advantage Enrollees per County from 2007 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

avg.premium <- final.data %>% ungroup() %>%
  select(year, premium) %>% drop_na() %>%
  group_by(year) %>% summarise(avg_prem = mean(premium))

fig.avg.premium.plot <- avg.premium %>% 
  ggplot(aes(x = year, y = avg_prem)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2007:2015)) +
  labs(x = "Year", y = "Average Premium", title = "Average Premium from 2007 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

zero.prem.plan <- final.data %>%
  select(planid, year, premium) %>% drop_na() %>%
  group_by(year) %>% summarise(zero_ct = n_distinct(planid[premium == 0]), ct = n_distinct(planid), pct = zero_ct/ct*100)

fig.zero.prem.plan.plot <- zero.prem.plan %>% filter(year > 2007) %>%
  ggplot(aes(x = year, y = pct)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2007:2015)) +
  labs(x = "Year", y = "% of $0 Premium Plans", title = "Percentage of $0 Premium Plans from 2008 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

## Save data for markdown -------------------------------------------------

rm(list=c("full.ma.data", "plan.premiums", "contract.service.area", "ma.penetration.data", "final.data"))
save.image("Hwk1_workspace.Rdata")
