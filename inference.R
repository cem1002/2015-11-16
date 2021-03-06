library(rio)
library(dplyr)
library(lubridate)
library(ggplot2)

service <- tbl_df(import("311_daily_contact_centre_trends.xlsx",
                         sheet="all"))


# Number of entries per year
service %>% group_by(year(Date)) %>% summarise(n_days = n())

# One sample t procedures
service %>% filter(year(Date) == 2011) -> service_11
mean(service_11$`Calls Answered %`)
t.test(service_11$`Calls Answered %`, mu = 0.8)

# Extract 2011 and 2012
service %>% filter(year(Date) %in% c(2011, 2012)) -> service11_12

# Boxplots
service11_12 %>% ggplot(aes(x=factor(year(Date)), y=`Average Talk Time (sec)`))+ geom_boxplot()

# Extract 2010:2013
service %>% filter(year(Date) %in% 2010:2013) -> service_full

# Boxplots
service_full %>% ggplot(aes(x=factor(year(Date)), y=`Average Talk Time (sec)`)) +
  geom_boxplot()

# Check out those outliers
service_full %>% filter(year(Date)==2010) %>% 
  select(Date, `Calls Offered`, `Service Level %`, `Average Talk Time (sec)`) %>% 
  arrange(`Average Talk Time (sec)`)

# Decision: remove them
service %>% filter(year(Date) %in% 2010:2013, 
                   `Average Talk Time (sec)` > 100) -> service_full


# Two sample t-test regular style
(t11_12 <- t.test(`Average Talk Time (sec)` ~ factor(year(Date)), 
                 data=service11_12))


# Special case: "equal variance"
(t11_12_eq <- t.test(`Average Talk Time (sec)` ~ factor(year(Date)), 
                 data=service11_12, var.equal=TRUE))


# Regression with a single 0-1 variable
t11_12_lm <- lm(`Average Talk Time (sec)` ~ factor(year(Date)), 
                data=service11_12)
summary(t11_12_lm)
t11_12_lm %>% ggplot(aes(sample = .resid)) + stat_qq()


# ANOVA for all the complete years
anova_full <- aov(`Average Talk Time (sec)` ~ factor(year(Date)), 
                  data=service_full)
summary(anova_full)
anova_full %>% ggplot(aes(sample = .resid)) + stat_qq()
anova_full %>% ggplot(aes(x = factor(.fitted), y = .resid)) + geom_boxplot()
