library(readstata13)
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(haven)
library(ggplot2)
library(RColorBrewer)
library(viridis)

options(scipen=999)

# read in 2013 acs data from IPUMS
# https://usa.ipums.org/usa/
# setwd("C:/Users/Stephen/Desktop/stata/econ_643/aca/aca_2012")
setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/acs_data/acs_2014_ipums_survey")
acs_data <- read_dta("acs_2014_ipums_survey.dta")
glimpse(acs_data)
acs_data_rd <- acs_data

# create poverty guideline
# http://www.shadac.org/publications/using-shadac-health-insurance-unit-hiu-and-federal-poverty-guideline-fpg-microdata
acs_data_rd <- acs_data_rd %>% mutate(pov_gl = hiufpgbase + hiufpginc*(hiunpers - 1))

# clean inctot and create adjusted income
hiu_inc <- acs_data_rd %>% mutate(inctot = case_when(.$inctot == "9999999" ~ as.numeric(NA), TRUE ~ as.numeric(.$inctot))) %>%
        mutate(adj_inctot = inctot*adjust)

# sum adj_inctot by household
hiu_inc_sum <- hiu_inc %>% group_by(hiuid) %>% summarize(hiu_inc = sum(adj_inctot, na.rm = TRUE),
                                                         hiu_inc_unadj = sum(inctot, na.rm = TRUE))
# head(hiu_inc_sum)
# ggplot(hiu_inc_sum, aes(x = hiu_inc)) + geom_histogram(binwidth = 10000)
# length(which(hiu_inc_sum$hiu_inc == 0))
# dim(hiu_inc_sum)

# merge household income sum to acs data
acs_data_rd <- left_join(acs_data_rd, hiu_inc_sum, by = "hiuid")

# create household income as % of FPL 
acs_data_rd <- acs_data_rd %>% mutate(hiu_pov = 100*(hiu_inc / pov_gl)) 

# glimpse(acs_data_rd)

# sum nchild for each household to find those with kids
# note this sum is not the actual count of kids in household, since it double-counts husbandn and wife nchild
nchild_sums <- acs_data_rd %>% group_by(hiuid) %>% summarize(nchild_total = sum(nchild, na.rm = TRUE))
# head(nchild_sums)

# merge nchild_sums with acs data
acs_data_rd <- left_join(acs_data_rd, nchild_sums, by = "hiuid")

# glimpse(acs_data_rd)
# acs_data_rd %>% select(hiuid, hiunpers, nchild, nchild_total) %>% head(., 15)

# filter to households with no kids
acs_data_rd <- acs_data_rd %>% filter(nchild_total == 0)

# glimpse(acs_data_rd)
# sum(acs_data_rd$hhwt)

# filter out households w/ age > 65 who are eligible for medicare, which could bias private insurance rate
age_65 <- acs_data_rd %>% group_by(hiuid) %>% summarize(max_age = max(age))
acs_data_rd <- left_join(acs_data_rd, age_65, by = "hiuid")
acs_data_rd <- acs_data_rd %>% filter(max_age < 65)

# remove rows for family members to get just one observation per household
# acs_data_rd %>% filter(hiunpers > 1) %>% select(hiuid) %>% head(.)
# acs_data_rd %>% filter(hiuid == 10602) %>% data.frame(.)
acs_data_rd <- acs_data_rd %>% filter(pernum == 1)

# write 2014 to file
# setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/acs_data")
setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/acs_data/acs_2014_ipums_survey")
write_csv(acs_data_rd, "acs_2014_ipums_survey_rd.csv")
acs_data_rd <- read_csv("acs_2014_ipums_survey_rd.csv")


########################################################


# filter data to just states where mediciad eligibility for non-parent, non-pregnant, non-disabled 
# changed from 0% FPL eligibility in 2013 to 138% in 2014
# http://kff.org/data-collection/trends-in-medicaid-income-eligibility-limits/

# load data on eligibility-expanding states
setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/kff_medicaid_eligibility")
exp_states <- read_csv("kff_medicaid_eligibility_states_zero_to_138_2011_to_2015.csv")
nonexp_states <- read_csv("kff_medicaid_eligibility_states_zero_to_zero_2011_to_2015.csv")

# select states
# fips_state <- elig_states$fips_state
# fips_state <- nonelig_states$fips_state
fips_state <- rbind(exp_states, nonexp_states)


# filter for state
selected_states <- acs_data_rd %>% filter(statefip %in% fips_state$state)
acs_data_rd <- acs_data_rd %>% filter(statefip %in% fips_state$state)

# limit range of fpl
# selected_states <- selected_states %>% filter(hiu_pov < 200)
selected_states <- selected_states %>% filter(hiu_pov < 400, hiu_pov > 0)
acs_data_rd <- selected_states %>% filter(hiu_pov < 400, hiu_pov > 0)
# selected_states <- selected_states %>% filter(hiu_pov < 400)
# acs_data_rd <- selected_states %>% filter(hiu_pov < 400)

# create group for households each spanning 10 FPL pct pt 
# selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 200, 5), right = FALSE, labels = c(seq(5, 200, 5)))
selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 400, 10), right = FALSE, labels = c(seq(0, 390, 10)))
# selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 400, 5), right = FALSE, labels = c(seq(0, 395, 5)))
# selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 400, 1), right = FALSE, labels = c(seq(0, 399, 1)))
acs_data_rd$hiu_pov_group <- as.numeric(as.character(cut(acs_data_rd$hiu_pov, seq(0, 400, 10), right = FALSE, labels = c(seq(0, 390, 10)))))


# create dummy for households on insurance           
# selected_states <- selected_states %>% mutate(ins_dummy = case_when(.$hinscaid == 1 ~ 0, .$hinscaid == 2 ~ 1))
selected_states <- selected_states %>% mutate(ins_dummy = case_when(.$hcovpriv == 1 ~ 0, .$hcovpriv == 2 ~ 1))
# acs_data_rd <- acs_data_rd %>% mutate(ins_dummy = case_when(.$hinscaid == 1 ~ 0, .$hinscaid == 2 ~ 1))
acs_data_rd <- acs_data_rd %>% mutate(ins_dummy = case_when(.$hcovpriv == 1 ~ 0, .$hcovpriv == 2 ~ 1))


# create recentered running variable hiu_pov_group 
acs_data_rd <- acs_data_rd %>% mutate(hiu_pov_group_centered = hiu_pov_group - 140)

# create eligibility dummy
acs_data_rd$elig_dummy <- ifelse(acs_data_rd$hiu_pov_group_centered <= 0, 1, 0)

# create interaction of running var hiu_pov_group_centered and post-treatment elig_dummy 
# to allow for changing slope on either side of RD
acs_data_rd <- acs_data_rd %>% mutate(hiu_pov_group_pre = hiu_pov_group_centered * elig_dummy)
        


######################################################################


# create variable for share of fpl household population with medicaid for each fpl group
plot_data <- selected_states %>%
        mutate(ins_hh = ins_dummy * hhwt) %>% group_by(hiu_pov_group) %>% 
        summarize(n_obs = n(),
                  ins_total = sum(ins_hh, na.rm = TRUE),
                  hiu_pov_total = sum(hhwt, na.rm = TRUE),
                  ins_share = ins_total / hiu_pov_total) 
plot_data$hiu_pov_group <- as.numeric(as.character(plot_data$hiu_pov_group))

# merge ins_share from plot_data back into acs_data_rd
acs_data_rd_final <- left_join(acs_data_rd, plot_data, by = "hiu_pov_group")

# write acs_data_rd_final to file
setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/acs_data/acs_2014_ipums_survey")
write_csv(acs_data_rd_final, "acs_2014_private_rd_final.csv")
list.files()
plot_data <- read_csv("acs_2014_private_rd_final.csv")

# inspect
# plot_data
# nrow(plot_data)
# glimpse(plot_data)
# sum(plot_data$n_obs)
# sum(plot_data$hiu_pov_total)
# min(plot_data$n_obs)
# mean(plot_data$n_obs)
# ggplot(plot_data, aes(x = n_obs)) + geom_histogram(binwidth = 10)
# ggplot(plot_data, aes(x = hiu_pov_group, y = n_obs)) + geom_bar(stat = "identity")

# scatterplot of discontinuity

# optional filter hiu_pov_group > 50
plot_data <- plot_data %>% filter(hiu_pov_group > 50)

# scatterplot of discontinuity, color and smoother based on eligibility
plot_data$elig_dummy <- ifelse(as.numeric(as.character(plot_data$hiu_pov_group)) < 140, 1, 0)

# set color pallette
colors <- c("Blue", "Red")

# with lm smoother
ggplot(plot_data, aes(x = hiu_pov_group, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 138) + 
        scale_color_manual(values = colors) + geom_smooth(method = lm) + 
        ggtitle("All 2014 eligibility-expanding states FPL by Medicaid insurance share (10 pct. pt. groups)")

# with quadratic smoother
ggplot(plot_data, aes(x = hiu_pov_group, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 138) + 
        scale_color_manual(values = colors) + geom_smooth(method = lm) + 
        ggtitle("All 2014 eligibility-expanding states FPL by Medicaid insurance share (10 pct. pt. groups)")

# inspect
# plot_data %>% select(hiu_pov_group, hiu_pov_total, n_obs, ins_share) %>% data.frame(.)
# min(plot_data$n_obs)
# max(plot_data$n_obs)
# mean(plot_data$n_obs)
# median(plot_data$n_obs)



#####################################################


# facet_wrap plots of discontinuity by state
# create variable for share of fpl household population with medicaid for each fpl group
plot_data <- selected_states %>%
        mutate(ins_hh = ins_dummy * hhwt) %>% group_by(statefip, hiu_pov_group) %>% 
        summarize(n_obs = n(),
                  ins_total = sum(ins_hh, na.rm = TRUE),
                  hiu_pov_total = sum(hhwt, na.rm = TRUE),
                  ins_share = ins_total / hiu_pov_total) 
plot_data$hiu_pov_group <- as.numeric(as.character(plot_data$hiu_pov_group))

# create categorical variable for total households in each fpl pov
# max(plot_data$hiu_pov_total)
# min(plot_data$hiu_pov_total)
# mean(plot_data$hiu_pov_total)
# median(plot_data$hiu_pov_total)
# ggplot(plot_data, aes(x = hiu_pov_total)) + geom_histogram(binwidth = 1000)

plot_data$hiu_pov_total_cat <- cut(plot_data$hiu_pov_total, c(seq(0, 100000, 25000), max(plot_data$hiu_pov_total)), 
                                   right = FALSE, 
                                   labels = c("0 - 24,999", "25,000 - 49,999", "50,000 - 74,999", "75,000 - 99,999",
                                              ">= 100,000"))
       
# inspect
# glimpse(plot_data)
# ggplot(plot_data, aes(x = n_obs)) + geom_histogram(binwidth = 10)
# ggplot(plot_data, aes(x = hiu_pov_total)) + geom_histogram(binwidth = 1000)

# scatterplot of discontinuity with facet_wrap for each state and color for total hhwt by hiu_pov
ggplot(plot_data, aes(x = hiu_pov_group, y = ins_share, color = hiu_pov_total_cat)) + geom_point(shape = 1) + 
        geom_vline(xintercept = 138) + 
        facet_wrap(~ statefip, ncol = 5) + scale_color_viridis(discrete = TRUE) 



# scatterplot of discontinuity with facet_wrap for each state, color and smoother based on eligibility
plot_data$elig_dummy <- ifelse(as.numeric(as.character(plot_data$hiu_pov_group)) < 140, 1, 0)

# set color pallette
colors <- c("Blue", "Red")

# ggplot(plot_data, aes(x = hiu_pov_group, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
#         geom_vline(xintercept = 138) + 
#         facet_wrap(~ statefip, ncol = 5) + scale_color_manual(values = colors) + geom_smooth(method = lm) +
#         ggtitle("2014 expansion state FPL by private insurance share (10 pct. pt. groups)")

# get rid of single NA value in running variable blocks
plot_data <- acs_data_rd_final %>% filter(!is.na(acs_data_rd_final$hiu_pov_group_centered))

# nonlinear
ggplot(plot_data, aes(x = hiu_pov_group_centered, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 0) + facet_wrap(~ statefip, ncol = 5) +
        geom_smooth(method = glm, formula = y ~ x + I(x^2)) + 
        ggtitle("Share of households with Medicaid insurance in 2014, by FPL in selected expansion states") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Share of households enrolled in Medicaid") + xlab("% of Federal Poverty Limit (each dot is 10 pct. pts.)") + 
        scale_color_manual(name = "Medicaid\nEligibility", values = colors, breaks = c("1", "0"),
                           labels = c("Eligible", "Not Eligible"))

# inspect
# plot_data %>% filter(statefip == 38) %>% select(hiu_pov_group, hiu_pov_total, n_obs, ins_share) %>% data.frame(.)

#######################################################


# create plot for individual state

# select state
fips_state <- 5 # arkansas
# fips_state <- 6 # california
fips_state <- 32 # nevada


# filter for state
selected_states <- acs_data_rd %>% filter(statefip == fips_state)

# limit range of fpl
# selected_states <- selected_states %>% filter(hiu_pov < 200)
selected_states <- selected_states %>% filter(hiu_pov < 400)

# create group for households each spanning 10 FPL pct pt 
# selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 200, 5), right = FALSE, labels = c(seq(5, 200, 5)))
selected_states$hiu_pov_group <- cut(selected_states$hiu_pov, seq(0, 400, 5), right = FALSE, labels = c(seq(5, 400, 5)))

# create total count of households
# selected_states$total_hhpop <- sum(selected_states$hhwt)

# create dummy for households on medicaid                 
# selected_states$hiu_pov_rounded <- round(selected_states$hiu_pov)
selected_states <- selected_states %>% mutate(ins_dummy = case_when(.$hinscaid == 1 ~ 0, .$hinscaid == 2 ~ 1))

# selected_states %>% filter(hiu_pov_rounded < 0) %>% select(hiuid, inctot) %>% head(.)
# selected_states %>% filter(hiu_pov_rounded > 0) %>% group_by(hiu_pov_rounded) %>% tally()

# create variable for share of fpl household population with medicaid for each fpl group
plot_data <- selected_states %>% 
        mutate(ins_hh = ins_dummy * hhwt) %>% group_by(hiu_pov_group) %>% 
        summarize(n_obs = n(),
                ins_total = sum(ins_hh, na.rm = TRUE),
                  hiu_pov_total = sum(hhwt, na.rm = TRUE),
                  ins_share = ins_total / hiu_pov_total) 
plot_data$hiu_pov_group <- as.numeric(as.character(plot_data$hiu_pov_group))

# inspect
# plot_data
# min(plot_data$n_obs)
# mean(plot_data$n_obs)
# ggplot(plot_data, aes(x = n_obs)) + geom_histogram(binwidth = 10)

# glimpse(plot_data)
# head(plot_data)
# selected_states %>% filter(hiu_pov_rounded == 1) %>% data.frame(.) %>% head(.)
# pov1 <- selected_states %>% filter(hiu_pov_rounded == 1)
# pov1_sum <- pov1 %>% mutate(ins_hh = ins_dummy * hhwt) %>% group_by(hiu_pov_rounded) %>% 
#         summarize(ins_total = sum(ins_hh, na.rm = TRUE),
#                   hiu_pov_total = sum(hhwt, na.rm = TRUE),
#                   ins_share = ins_total / hiu_pov_total) 
# pov1_sum

# scatterplot of discontinuity
ggplot(plot_data, aes(x = hiu_pov_group, y = ins_share)) + geom_point() + geom_vline(xintercept = 138)





