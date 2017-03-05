library(survey)
library(stargazer)

# load data
setwd("C:/Users/Stephen/Desktop/stata/econ_671/term_paper/acs_data/acs_2014_ipums_survey")
list.files()
acs_data_rd_final <- read_csv("acs_2014_medicaid_rd_final.csv")

# create nonlinear trends
acs_data_rd_final <- acs_data_rd_final %>% mutate(hiu_pov_group_centered_sq = hiu_pov_group_centered^2) %>%
        mutate(hiu_pov_group_pre_sq = hiu_pov_group_centered_sq * elig_dummy)

# optional: trim data to just hiu_pov_group > 50
acs_data_rd_final <- acs_data_rd_final %>% filter(hiu_pov_group > 50)

# create survey data
acs_data_rd_svy <- svydesign(
        id = ~cluster,
        strata = ~strata,
        data = acs_data_rd_final,
        weights = ~hhwt
)

# run model_1 (verified same output as stata for all svy regressions)
model_1 <- svyglm(ins_share ~ elig_dummy + hiu_pov_group_centered, acs_data_rd_svy)
summary(model_1)

# run model_2 with pre-treatment interaction
model_2 <- svyglm(ins_share ~ elig_dummy + hiu_pov_group_centered + hiu_pov_group_pre, acs_data_rd_svy)
summary(model_2)

# run model_3 with pre-treatment interaction and non-linear trends
model_3 <- svyglm(ins_share ~ elig_dummy + hiu_pov_group_centered + hiu_pov_group_centered_sq +  
                          hiu_pov_group_pre + hiu_pov_group_pre_sq, acs_data_rd_svy)
summary(model_3)

# create output table
# copy html output, put in sublime text, save as .html, and open in browser to render then cut/paste
stargazer(model_2, model_3, type = "html", style = "qje", dep.var.labels   = "Share of FPL group with private insurance",
          title = "Households with private insurance in selected expansion states in 2014")


################################################



# plot
# get rid of single NA value in running variable blocks
plot_data <- acs_data_rd_final %>% filter(!is.na(acs_data_rd_final$hiu_pov_group_centered))

# set colors
colors <- c("Red", "Blue")

# all states with lm
ggplot(plot_data, aes(x = hiu_pov_group_centered, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 0) + 
        geom_smooth(method = lm) + 
        ggtitle("Share of households with private insurance in 2014, by FPL in selected expansion states") + 
        theme(plot.title = element_text(hjust = 0.5, lineheight=.8, face="bold")) +
        ylab("Share of households enrolled in private insurance") + xlab("% of Federal Poverty Limit (each dot is 10 pct. pts.)") + 
        scale_color_manual(name = "Medicaid\nEligibility", values = colors, breaks = c("1", "0"),
                            labels = c("Eligible", "Not Eligible"))

# all states with quadratic smoother
ggplot(plot_data, aes(x = hiu_pov_group_centered, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 0) + 
        geom_smooth(method = glm, formula = y ~ x + I(x^2)) + 
        ggtitle("Share of households with private insurance in 2014, by FPL in selected expansion states") + 
        theme(plot.title = element_text(hjust = 0.5, lineheight=.8, face="bold")) +
        ylab("Share of households enrolled in private insurance") + xlab("% of Federal Poverty Limit (each dot is 10 pct. pts.)") + 
        scale_color_manual(name = "Medicaid\nEligibility", values = colors, breaks = c("1", "0"),
                           labels = c("Eligible", "Not Eligible"))

# individual states w/ linear
ggplot(plot_data, aes(x = hiu_pov_group_centered, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 0) + facet_wrap(~ statefip, ncol = 5) +
        geom_smooth(method = lm) + 
        ggtitle("Share of households with Medicaid insurance in 2014, by FPL in selected expansion states") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Share of households enrolled in Medicaid") + xlab("% of Federal Poverty Limit (each dot is 10 pct. pts.)") + 
        scale_color_manual(name = "Medicaid\nEligibility", values = colors, breaks = c("1", "0"),
                           labels = c("Eligible", "Not Eligible"))

# individual states w/ nonlinear trend
ggplot(plot_data, aes(x = hiu_pov_group_centered, y = ins_share, color = factor(elig_dummy))) + geom_point(shape = 1) + 
        geom_vline(xintercept = 0) + facet_wrap(~ statefip, ncol = 5) +
        geom_smooth(method = glm, formula = y ~ x + I(x^2)) + 
        ggtitle("Share of households with Medicaid insurance in 2014, by FPL in selected expansion states") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Share of households enrolled in Medicaid") + xlab("% of Federal Poverty Limit (each dot is 10 pct. pts.)") + 
        scale_color_manual(name = "Medicaid\nEligibility", values = colors, breaks = c("1", "0"),
                           labels = c("Eligible", "Not Eligible"))


