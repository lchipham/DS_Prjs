#Load packages
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(cowplot)
library(tidyverse)
library(lubridate)
library(patchwork)
library(car)
library(lmtest)
##----------------------------------------------------------------------------##
## 1. Monthly CPI (Percent change from year ago)
cpi_april22 <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/cpi_april22.csv")names(cpi_april22)

#Rename Variables
cpi_april22 <- cpi_april22 %>% rename('CPI' = CPIAUCSL_PC1)

# Convert character to date type
cpi_april22$DATE = as.Date(cpi_april22$DATE)
cpi_april22$CPI = as.numeric(cpi_april22$CPI)
str(cpi_april22)

# highlight recession time periods
fincrisis <- data.frame(xmin = as.Date("2007-01-01", "%Y-%m-%d"), xmax = as.Date("2010-01-01", "%Y-%m-%d"), ymin=-Inf, ymax=Inf)
covid <- data.frame(xmin = as.Date("2020-01-01", "%Y-%m-%d"), xmax = as.Date("2022-01-01", "%Y-%m-%d"), ymin=-Inf, ymax=Inf)

cpi_trend <- ggplot(cpi_april22, aes(x=DATE)) + 
  geom_line(aes(y=CPI), size = 0.6, color = "brown") +
  geom_rect(data=fincrisis, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "salmon", alpha=0.1, inherit.aes = FALSE) +
  geom_text(x=as.Date("2008-06-15", "%Y-%m-%d"), y=7, label="financial\ncrisis", family = "serif") +
  geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "orange", alpha=0.1, inherit.aes = FALSE) +
  geom_text(x=as.Date("2021-01-15", "%Y-%m-%d"), y=7, label="covid", family = "serif") +
  geom_hline(yintercept = 2) +
  geom_text(x=as.Date("1992-01-15", "%Y-%m-%d"), y=1.6, label= "2% target rate", family = "serif") +
  labs(y="Percent",title="40-year Inflation Rate for all Urban Consumers",
       subtitle="All items in US City avarage, 1983-2022") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 12),
        axis.title.x = element_blank(), panel.grid = element_blank(), axis.ticks.y = element_blank())
ggplotly(cpi_trend)
##----------------------------------------------------------------------------##
## 2. Real GDP vs Hourly Wages (Quarterly)
gdp_wage <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/gdp_wage.csv")
names(gdp_wage)

#Rename Variables
gdp_wage <- gdp_wage %>% rename('real_GDP' = GDPC1_PC1,
                                'hourly_wages' = CES0500000003_PC1)

# Convert character to date type
gdp_wage$DATE = as.Date(gdp_wage$DATE)
gdp_wage$real_GDP = as.numeric(gdp_wage$real_GDP)
gdp_wage$hourly_wages = as.numeric(gdp_wage$hourly_wages)
str(gdp_wage)

# Real GDP vs Hourly Wages
realgdp_wage <- ggplot(gdp_wage, aes(x=DATE)) + 
  geom_line(aes(y=real_GDP), size = 0.6, color = "brown") +
  geom_line(aes(y=hourly_wages), size = 0.6, color = "steelblue") +
  geom_text(x=as.Date("2021-07-01", "%Y-%m-%d"), y=0, label= "Growth in real GDP outpaced forecasts\nHourly wages on an increasing trajectory", family = "serif") +
  labs(y="Percent", title="Real Gross Domestic Product and Hourly Wages", subtitle="Quarterly, 2020-2022") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 12),
        axis.title.x = element_blank(), panel.grid = element_blank(), axis.ticks.y = element_blank())
ggplotly(realgdp_wage)

##----------------------------------------------------------------------------##
## 3. CPI vs Interest Rate vs Unemployment Rate
#Data
fred <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/Policy.csv")
names(fred)

#Rename Variables
fred <- rename(fred, 'CPI' = CPIAUCSL_PC1,
              'Unrate' = UNRATE,
              'Interest' = FEDFUNDS)

# Convert character to date type
fred$DATE = as.Date(fred$DATE)
# Convert to numeric data
fred$CPI = as.numeric(fred$CPI)
fred$Interest = as.numeric(fred$Interest)
fred$Unrate = as.numeric(fred$Unrate)
str(fred)

# Capture the data in the LAST 20 years
fred_recent_20_yrs = fred[fred$DATE >= as.Date("2000-01-01"), ]

# CPI vs Int rate vs Unrate w/ highlighted regions
infl_int_un <- ggplot(data=fred_recent_20_yrs, aes(x=DATE)) + 
  geom_line(aes(y=CPI, color = "Inflation Rate"), size = 0.69) + 
  geom_line(aes(y=Interest, color = "Interest Rate"), size = 0.69) +
  geom_line(aes(y=Unrate, color = "Unemployment Rate"), size = 0.69) +
  geom_rect(data=fincrisis, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "salmon", alpha=0.1, inherit.aes = FALSE) +
  geom_text(x=as.Date("2008-06-15", "%Y-%m-%d"), y=7, label="financial\ncrisis", family = "serif") +
  geom_rect(data=covid, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill = "salmon", alpha=0.1, inherit.aes = FALSE) +
  geom_text(x=as.Date("2021-01-15", "%Y-%m-%d"), y=7, label="covid", family = "serif") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") + 
  labs(y="Percent",
           x = "",
           title="Inflation Rate for all Urban Consumers",
           subtitle="All items in US City avarage, 2000-2022"
  ) + 
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 12),
        panel.grid = element_blank(), axis.ticks.y = element_blank()) +
  guides(color = guide_legend(title = "Measure"))  +
  scale_color_manual(values = c("Inflation Rate" = "brown", "Interest Rate" = "steelblue", "Unemployment Rate" = "orange"))

infl_int_un

##----------------------------------------------------------------------------##
##4. US INFLATION BY PRODUCT 

#CPI data
cpi <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/cpi.csv")
names(cpi)

#Rename variables used to plot
CPI <- rename(cpi, 'Used Cars' = CUSR0000SETA02,
              'Food & Bevarages' = CPIFABSL,
              'Energy' = CPIENGSL,
              'Airline Fares' = CUSR0000SETG01,
              'Shelter' = CUSR0000SAH1,
              'Medical Care' = CPIMEDSL,
              'Apparel' = CPIAPPSL)

#Change character to Date type
CPI$DATE <-as.Date(CPI$DATE,"%Y-%m-%d")

#Change character to numeric data type
CPI$`Used Cars` <- as.numeric(CPI$`Used Cars`)
CPI$`Food & Bevarages` <- as.numeric(CPI$`Food & Bevarages`)
CPI$Energy <- as.numeric(CPI$Energy)
CPI$`Airline Fares` <- as.numeric(CPI$`Airline Fares`)
CPI$Shelter <- as.numeric(CPI$Shelter)
CPI$`Medical Care` <- as.numeric(CPI$`Medical Care`)
CPI$Apparel <- as.numeric(CPI$Apparel)

#Fiter only months from 2020 onward
covid_cpi <- CPI %>% 
  filter(DATE >= "2020-01-01")

#Convert data to long format
long_form <- covid_cpi %>% 
  pivot_longer(-c(DATE, CUSR0000SEHA, CPIHOSSL), names_to = "US_CPI", values_to = "value") %>% 
  select(US_CPI, value) %>% 
  mutate(total = sum(value)) %>% 
  group_by(US_CPI) %>% 
  summarise(perc = (value / total) * 100)

#Bar plot comparing inflation by product type 
summary_inf <- ggplot(long_form, aes(x = reorder(US_CPI, perc), y = perc, fill = US_CPI)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  labs(subtitle = "COVID Impact: 2020-2022", x = "", y = "Percent (%)", fill = "Group") +
  theme_bw() +
  theme(plot.subtitle = element_text(hjust = 0.5), text = element_text(family = "serif"),
        panel.grid = element_blank(), axis.ticks.y = element_blank()) 

#Convert original data to long format
cpi_long <- CPI %>% 
  pivot_longer(-c(DATE, CUSR0000SEHA, CPIHOSSL), names_to = "US_CPI", values_to = "value") %>% 
  select(DATE, US_CPI, value) 

#US Inflation by Product - Yearly movement
yearly_movement <- ggplot(cpi_long, aes(x = DATE, y = value, fill = US_CPI)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "US Inflation by Product", subtitle = "Year-to-year (1947-2022)", x = "Date", y = "Million Dollar", fill = "Group") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), text = element_text(family = "serif"),
        panel.grid = element_blank(), legend.position = "right")

#Put plot in 1 column, 2 rows for comparison
plot_grid(yearly_movement, summary_inf, nrow = 2, ncol = 1)

#----------------------------------------------------------------------#
##5. US ECOMMERCE RETAIL SALES (2000-2022)

#Load data
ecom <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/ecom.csv")
names(ecom)

#Rename variables
ECOM <- rename(ecom, 'Dollar' = ECOMSA, 'Percent' = ECOMSA_PC1)

#Change character to numeric data type
ECOM$Percent <- as.numeric(ECOM$Percent)
ECOM$Percent[1] <- 0

#Change date (m/d/Y) to only year format
ECOM$DATE <- factor(ECOM$DATE, labels = c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

#Base plot
plot <- ggplot(ECOM) 

#Bar plot reflecting price in million $
bar <- plot +
  geom_bar(aes(x = DATE, y = Dollar, fill = DATE), stat = "identity", show.legend = F) +
  geom_line(aes(x = DATE, y = Dollar), group = 1, col = "brown", size = 0.9) +
  labs(title = "US Retail Ecommerce", subtitle = "Million Dollar", x = "", y = "Million $", fill = "Group") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), text = element_text(family = "serif"),
        panel.grid = element_blank()) 

#Bar + Line plot reflecting percent change 
line <- plot +
  geom_bar(aes(x = DATE, y = Percent), stat = "identity", show.legend = F, fill = "pink") +
  geom_line(aes(x = DATE, y = Percent), group = 1, col = "brown", size = 0.9) +
  labs(title = "US Retail Ecommerce", subtitle = "Percent Change", x = "", y = "%") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), text = element_text(family = "serif"),
        panel.grid = element_blank()) 

#Place two plots in one column to directly compare by year
plot_grid(bar, line, ncol = 1, nrow = 2)
#==================================================================#
## 6. LA Port Congestion Statistics
la_port <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/la_port_stats.csv")
names(la_port)

recent_years <- c("FY 2016/2017","FY 2017/2018", "FY 2018/2019", "FY\xca2019/2020", "FY 2020/2021")
la_2017_2022 <- la_port %>% 
  filter(Fiscal.Year %in% recent_years) 

correct_years <- c("FY 2016/2017","FY 2017/2018", "FY 2018/2019", "FY 2019/2020", "FY 2020/2021")
correct_percent <- c("9.72%","0.39%", "5.65%", "-11.64%", "27.08%")
la_2017_2022 <- cbind(la_2017_2022, correct_years, correct_percent)
names(la_2017_2022)
la_2017_2022 <- la_2017_2022[,-c(1, 9)]

ggplot(la_2017_2022, aes(x = correct_years, y = correct_percent, fill = Total.Imports)) +
  geom_bar(stat = "identity", color = "black", size = 0.4) +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "LA Port Fiscal Year Container Statistics", subtitle = "Imports and container capacity increased drastically from FY 19/20 to 20/21", y = "Twenty-foot equivalent unit (TEU)", fill = "Total Imports") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

#==================================================================#
#REGRESSION MODELING
fred <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/cpi_fedfunds.csv")

# Convert to date obj
fred$date = as.Date(fred$date)
# Convert to numeric data
fred$cpi = as.numeric(fred$cpi)
fred$fed_funds = as.numeric(fred$fed_funds)

#------------------------------------------------------------------------------#
#Demand-pull inflation; Excess demand + Supply shortage
#1. Fed funds cut
fred <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/cpi_fedfunds.csv")
theme_set(theme_minimal())
# Convert to date obj
fred$date = as.Date(fred$date)
# Convert to numeric data
fred$cpi = as.numeric(fred$cpi)
fred$fed_funds = as.numeric(fred$fed_funds)
# Capture the data in the LAST 20 years
fred_recent_20_yrs = fred[fred$date >= as.Date("2002-01-01") & fred$date < as.Date("2022-03-01"), ]
#View(fred_recent_20_yrs)
fit1 <- lm(fred_recent_20_yrs$cpi ~ fred_recent_20_yrs$fed_funds)
summary(fit1)

#2. Increased money supply
Monetary_Base <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/Monetary_Base.csv")
str(Monetary_Base)
Monetary_Base$DATE = as.Date(Monetary_Base$DATE)

# Capture the data in the LAST 20 years
mo_base_20yrss <- Monetary_Base[Monetary_Base$DATE >= as.Date("2002-01-01") & Monetary_Base$DATE < as.Date("2022-02-01"), ]
fred_recent_20_yrs <- fred_recent_20_yrs[-242,]
fit2 <- lm(fred_recent_20_yrs$cpi ~ mo_base_20yrss$BOGMBASE)
summary(fit2)

#3. Higher wages
hourly_wages <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/hourly_wages.csv")
colnames(hourly_wages) <- c('DATE', 'hourly_wage')
head(hourly_wages)

#Adjust fred & mo_base dates for consistency with hourly wages
fred_recent_newdates <- fred[fred$date >= as.Date("2006-03-01") & fred$date <= as.Date("2022-01-01"), ]
mo_base_newdates <- Monetary_Base[Monetary_Base$DATE >= as.Date("2006-03-01") & Monetary_Base$DATE <= as.Date("2022-01-01"), ]
fit3 <- lm(fred_recent_newdates$cpi ~ hourly_wages$hourly_wage)
summary(fit3)

#4. Unemployment Rate 
unrate <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/UNRATE.csv")
unrate_newdates <- unrate[unrate$DATE >= as.Date("2002-01-01") & unrate$DATE <= as.Date("2022-01-01"), ]
fit4 <- lm(fred_recent_20_yrs$cpi ~ unrate_newdates$UNRATE)
summary(fit4)

#5. GDP
realGDP <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/realGDP.csv")
colnames(realGDP) <- c('DATE', 'Real_GDP')

#Convert fred monthly to quarterly data
monthly_fred <- ts(fred_recent_20_yrs, start = c(2002, 1), frequency = 12)
quarterly_fred <- aggregate(monthly, nfrequency = 4)
quarterly_fred <- quarterly_fred[,-c(1,2)]
quarterly_df <- data.frame(measure = as.matrix(quarterly_fred), date= time(quarterly_fred))
head(quarterly_df)
dim(quarterly_df)
#Remove 1 last row of realGDP
tail(realGDP)
realGDP <- realGDP[-81,]
is.ts(quarterly_fred)

fit5 <- lm(quarterly_df$measure.cpi ~ realGDP$Real_GDP)
summary(fit5)

#6. Consumption
rpce_real <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/STA 234 Final Rcode + Data - Chi Pham/RPCE general real value.csv")
colnames(rpce_real) <- c("observation_date", "consumption")
rpce_real$observation_date = as.Date(rpce_real$observation_date)
dim(rpce_real)
rpce_real <- rpce_real[-242,]
fit6 <- lm(fred_recent_20_yrs$cpi ~ rpce_real$consumption)
summary(fit6) 

#Adjust fred & mo_base dates for consistency with hourly wages
fred_recent_newdates <- fred[fred$date >= as.Date("2006-03-01") & fred$date <= as.Date("2022-01-01"), ]
mo_base_newdates <- Monetary_Base[Monetary_Base$DATE >= as.Date("2006-03-01") & Monetary_Base$DATE <= as.Date("2022-01-01"), ]
unrate_newdates <- unrate[unrate$DATE >= as.Date("2006-03-01") & unrate$DATE <= as.Date("2022-01-01"), ]
rpce_real_newdates <- rpce_real[rpce_real$observation_date >= as.Date("2006-03-01") & rpce_real$observation_date <= as.Date("2022-01-01"), ]

combined_measures <- cbind(fred_recent_newdates$fed_funds, hourly_wages$hourly_wage, mo_base_newdates$BOGMBASE, unrate_newdates$UNRATE, rpce_real_newdates$consumption)
colnames(combined_measures) <- c('fed_funds', 'hourly_wage', 'm0_base', 'ue_rate', 'consumption')

combined_with_response <- cbind(fred_recent_newdates$cpi, combined_measures)
colnames(combined_with_response) <- c('cpi', 'fed_funds', 'hourly_wage', 'm0_base', 'ue_rate', 'consumption')
combined_with_response <- as.data.frame(combined_with_response)
class(combined_with_response)

#compute the correlation between response variable to check for multicollinearity
library(corrplot)
cor.measure <- cor(combined_measures,use="pairwise.complete.obs")
corrplot.mixed(cor.measure, order = "AOE", tl.cex = 0.66, outline = TRUE)

#There is extremely high positive correlation between Monetary Base and Hourly Wage, Monetary Base and Consumption and Monetary Base and Consumption
#To avoid multicollinearity, we will remove 2 of the least significant predictors of the 3 by comparinv their p-values in the original multiple linear regression
#--> Since  mo_base has the lowest p-value of the 3, we'll keep it and remove hourly_wage & consumption

#Original multi linear regression
multi_reg1 <- lm(fred_recent_newdates$cpi ~  fred_recent_newdates$fed_funds + hourly_wages$hourly_wage + mo_base_newdates$BOGMBASE + unrate_newdates$UNRATE + rpce_real_newdates$consumption)
summary(multi_reg1)

#Multiple reg after checking for multicollinearity
multi_reg2 <- lm(combined_with_response$cpi ~  combined_with_response$fed_funds + combined_with_response$m0_base + combined_with_response$ue_rate)
summary(multi_reg2)

#Diagnostic plots
hist(fred_recent_newdates$cpi)
par(mfrow = c(2,2))
plot(multi_reg2)

##BP test
bptest(multi_reg2)
#--> As the p-value = 0.1418 (9.87e-06) is greater than 0.05 (significance level), we fail to reject the null hypothesis. Homoscedasticity assumption thus holds true.

# Tukey's test 
Tukey.test <- residualPlot(multi_reg2, type="working", pch=18, main = "Tukey Test")
Tukey.test
#--> As the p-value = 0.7741898 is greater than 0.05 (significance level), we fail to reject the null hypothesis. Linearity assumption thus holds true

#ANOVA test
anova(multi_reg2)

#As the p-value for fed_funds and m0_base is less than the significance level 0.05, we can conclude that there are significant differences between federal funds rate and M0 monetary base highlighted with “*" in the model summary.
# F-value is the test statistic from the F test. This is the mean square of each independent variable divided by the mean square of the residuals. 
#--> AS the F-values of fed_funds and m0_base are significantly larger than that of ue_rate, it is very likely that the variation caused by fed_funds and m0_base are real and not due to chance. ppf     d s ssds 

#Plots of cpi vs each individual predictor
library(cowplot)
#install.packages("ggpubr")
library(ggpubr)
options(scipen = 999)

#Factorize mo_base into 4 levels
index.under2m <- which(combined_with_response$m0_base < 2000000)
index.2m4m <- which(combined_with_response$m0_base >= 2000000 & combined_with_response$m0_base < 4000000)
index.4m6m <- which(combined_with_response$m0_base >= 4000000 & combined_with_response$m0_base < 6000000)
index.over6m <- which(combined_with_response$m0_base > 6000000)

mo.factor <- combined_with_response$m0_base
mo.factor[index.under2m] <- "Under 2m"
mo.factor[index.2m4m] <- "2m - 4m"
mo.factor[index.4m6m] <- "4m - 6m"
mo.factor[index.over6m] <- "Over 6m"

combined_with_response$m0_base_factor <- mo.factor

#Individual scatterplots
cpi_int <- ggplot(combined_with_response, aes(x = fed_funds, y = cpi, size = cpi)) +
  geom_point(aes(color = m0_base_factor), position = "jitter", alpha = 0.6, show.legend = F) +
  scale_x_log10() +
  stat_regline_equation(label.x=0.02, label.y=5.0) +
  labs(title = "CPI vs Federal Funds Rate", subtitle = "Jan 2002 - Jan 2022", y = "CPI", x = "Fed Funds Rate") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  geom_smooth(method = "lm", col = "sienna", se = FALSE) +
  guides(size="none") +
  scale_color_manual(values = c("Over 6m" = "red", "4m - 6m" = "brown", "2m - 4m" = "sienna2", "Under 2m" = "orange"))

cpi_ue <- ggplot(combined_with_response, aes(x = ue_rate, y = cpi, size = cpi)) +
  geom_point(position = "jitter", alpha = 0.6, aes(color = m0_base_factor), show.legend = F) +
  stat_regline_equation(label.x=11, label.y=5.0) +
  labs(title = "CPI vs Unemployment Rate", subtitle = "Jan 2002 - Jan 2022", y = "CPI", x = "UE Rate") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  geom_smooth(method = "lm", col = "sienna", se = FALSE) +
  guides(size="none") +
  scale_color_manual(values = c("Over 6m" = "red", "4m - 6m" = "brown", "2m - 4m" = "sienna2", "Under 2m" = "orange"))

cpi_m0 <- ggplot(combined_with_response, aes(x = m0_base, y = cpi, size = cpi)) +
  geom_point(aes(color = m0_base_factor), position = "jitter", alpha = 0.6, show.legend = F) +
  stat_regline_equation(label.x=3500000, label.y=5.0) +
  labs(title = "CPI vs Monetary Supply", subtitle = "Jan 2002 - Jan 2022", y = "CPI", x = "M0 Monetary Base") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  geom_smooth(method = "lm", col = "sienna", se = FALSE) +
  guides(size="none") +
  scale_color_manual(values = c("Over 6m" = "red", "4m - 6m" = "brown", "2m - 4m" = "sienna2", "Under 2m" = "orange"))

cpi_cons <- ggplot(combined_with_response, aes(x = consumption, y = cpi, size = cpi)) +
  geom_point(aes(color = m0_base_factor), position = "jitter", alpha = 0.6) +
  stat_regline_equation(label.x=12000, label.y=5.0) +
  labs(title = "CPI vs Consumption", subtitle = "Jan 2002 - Jan 2022", y = "CPI", x = "Consumption", color = "M0 Base") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  geom_smooth(method = "lm", col = "sienna", se = FALSE) +
  guides(size="none") +
  scale_color_manual(values = c("Over 6m" = "red", "4m - 6m" = "brown", "2m - 4m" = "sienna2", "Under 2m" = "orange"))

#Scatterplots between CPI and 4 macroeconomic indicators
plot_grid(cpi_m0, cpi_int, cpi_ue, cpi_cons, ncol = 2, nrow = 2)
