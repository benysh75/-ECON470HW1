if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

# Read data and set workspace for knitr ----------------------------------------

full.ma.data <- read_rds("data/output/full_ma_data.rds")
plan.premiums <- read_rds("data/output/plan_premiums.rds")
contract.service.area <- read_rds("data/output/contract_service_area.rds")
ma.penetration.data <- read_rds("data/output/ma_penetration.rds")

# Merge data set ----------------------------------------

final.data <- full.ma.data %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS","") &
           snp == "No" &
           (planid < 800 | planid >= 900) &
           !is.na(planid) & !is.na(fips))

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

# Create objects for markdown ----------------------------------------

tot.obs <- dim(full.ma.data)[1]

num.dist.plan_types <- n_distinct(full.ma.data$plan_type, na.rm = FALSE)

plan.type.table <- full.ma.data %>% group_by(plan_type, year) %>% 
  count() %>% arrange(-n) %>% # count
  pivot_wider(names_from = year, values_from = n) # pivot

plan.type.table2 <- full.ma.data %>% filter(snp == "No", eghp == "No") %>% # remove snp, eghp
  filter(planid < 800 | planid >= 900) %>% # remove 800-series
  group_by(plan_type, year) %>% 
  count() %>% arrange(-n) %>% # count
  pivot_wider(names_from = year, values_from = n) # pivot

avg_enrollment <- final.data %>% 
  select(state, county, year, avg_enrollment) %>% drop_na() %>%
  group_by(year) %>% summarise(average = mean(avg_enrollment))

avg_enrollment_plot <- avg_enrollment %>% filter(year > 2007) %>%
  ggplot(aes(x = year, y = average)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2008:2015)) +
  labs(x = "Year", y = "Average Enrollment", title = "Average Number of Medicare Advantage Enrollees per County from 2008 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

avg_premium <- final.data %>% 
  select(contractid, planid, fips, state, county, year, premium) %>% drop_na() %>%
  group_by(year) %>% summarise(average = mean(premium))

avg_premium_plot <- avg_premium %>% filter(year > 2007) %>%
  ggplot(aes(x = year, y = average)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2008:2015)) +
  labs(x = "Year", y = "Average Premium", title = "Average Premium from 2008 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

zero_premium_plan <- final.data %>%
  select(contractid, planid, fips, state, county, year, premium) %>% drop_na() %>%
  group_by(year) %>% summarise(zero_ct = n_distinct(planid[premium == 0]), ct = n_distinct(planid), pct = zero_ct/ct*100)

zero_premium_plan_plot <- zero_premium_plan %>% filter(year > 2007) %>%
  ggplot(aes(x = year, y = pct)) + geom_point() + geom_line() +
  scale_x_continuous(breaks = c(2008:2015)) +
  labs(x = "Year", y = "% of $0 Premium Plans", title = "Percentage of $0 Premium Plans from 2008 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text = element_text(size = 10, color = "black"))

rm(list=c("full.ma.data", "plan.premiums", "contract.service.area", "ma.penetration.data", "final.data"))
save.image("Hwk1_workspace.Rdata")
