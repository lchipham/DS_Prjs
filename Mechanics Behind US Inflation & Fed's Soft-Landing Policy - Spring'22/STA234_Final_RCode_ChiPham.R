# RESEARCH QUESTIONS:
# Since COVID pandemic, which population groups in Connecticut filed for the highest number of unemployment claims?
# What is the current UE trends in CT?

#-------------------------------------------------------------------------------#
#Load dataset
ue_claim_ct <- read.csv("~/Documents/SPRING 2022 - CONN/STA 234 - Statistical Computing with R/Continued_Claims_for_Unemployment_Benefits_in_Connecticut.csv")
dim(ue_claim_ct)
#Load required packages
library(highcharter)
library(htmltools)
library(tidyverse)
#-------------------------------------------------------------------------------#
#Change date to monthly frequency
ue_claim_ct$Date <- as.Date(ue_claim_ct$Date, format = "%m/%d/%Y")
ue_date_revised <- ue_claim_ct %>% 
  arrange(Date) 
dim(ue_date_revised)
table(ue_claim_ct$Type)

#-------------------------------------------------------------------------------#
#Continued Unemployment Claims by Industry
#industry subset
ue_industry <- subset(ue_date_revised, ue_date_revised$Type == "Continued claims by industry")
table(ue_industry$Subgroup)
industry_removed <- c("Total", "Other/Unknown", "Other services", "Management of companies", "Self employed", "Professional and technical services", "Mining/Quarrying", "Utilities", "Public administration", "Real estate")
ue_industry = ue_industry[!ue_industry$Subgroup %in% industry_removed,]
table(ue_industry$Subgroup)

#Function to create theme for each graph
make_me_pretty <- function(list) {
  hc_theme(
    colors = list,
    chart = list(backgroundColor = "white"),
    title = list(style = list(color = "black", fontFamily = "Georgia")),
    subtitle = list(style = list(color = "black", fontFamily = "Georgia")),
    legend = list(itemStyle = list(fontFamily = "Georgia", color = "black"),
                  itemHoverStyle = list(color = "gray"))
  )
}

#Theme of industry graph
industry_theme <- make_me_pretty(list = c('#fb9a99', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#d9d9d9', '#d6604d', '#ccebc5', '#ffed6f','#8dd3c7'))

#General function with set customizations for plotting similar streamgraphs by different segments
stream_plot_ue <- function(data = ue_industry, theme = industry_theme, plot_title = 'By Industry - Jan 2019 to April 2022') {
  data %>% 
    hchart('streamgraph', 
           hcaes(x = Date, y = Continued.claims, group = Subgroup),
           label = list(enabled = TRUE, minFontSize = 10, maxFontSize = 15,
                        style = list(fontFamily = "Georgia", fontWeight = 150,
                                     textOutline = "0.2px white", color = hex_to_rgba("black", 2)))) %>%
    hc_title(text = 'Continued Unemployment Claims in Connecticut',
             style = list(fontSize = '24px', fontWeight = 'bold'), align = "left") %>% 
    hc_subtitle(text = plot_title, style = list(fontSize = '16px'), align = "left") %>%
    hc_yAxis(visible = FALSE, startOnTick = FALSE, endOnTick = FALSE) %>%
    hc_xAxis(type = "datetime", gridLineWidth = 0, title = list(text = NULL)) %>%  
    hc_legend(verticalAlign = "bottom", align = "center", itemStyle =  list(fontWeight = 500)) %>% 
    hc_tooltip(table = FALSE, outside = TRUE, useHTML = TRUE,
               headerFormat = "<small>{point.key}</small><table>",
               footerFormat = "<tr><td><b>Total</b>: <b>{point.total:0.f}</b></td></tr></table>") %>% 
    hc_credits(enabled = TRUE, text = '@lchi.pham') %>% 
    hc_add_theme(theme)
}

#Streamgraph 1 - Continued UE claims by industry in CT
stream_plot_ue()

#-------------------------------------------------------------------------------#
#Continued Unemployment Claims by Education
#education subset
ue_edu <- subset(ue_date_revised, ue_date_revised$Type == "Continued claims by education")
table(ue_edu$Subgroup)
edu_removed <- c("Not available", "Some college", "Total")
ue_edu <- ue_edu[!ue_edu$Subgroup %in% edu_removed,]
new_name <- c("More than a bachelors degree")
ue_edu <- ue_edu %>% mutate(Subgroup = replace(Subgroup, which(Subgroup %in% new_name), 'Master/PhD'))

#Theme of education graph
edu_theme <- make_me_pretty(list = c('#fc8d59', '#fdcc8a', '#fef0d9', '#d7301f'))

#Streamgraph 2 - Continued UE claims by education level in CT
stream_plot_ue(ue_edu, edu_theme, 'By Education Level - Jan 2019 to April 2022')

#-------------------------------------------------------------------------------#
#Continued Unemployment Claims by Wages
#wages subset
ue_wages <- subset(ue_date_revised, ue_date_revised$Type == "Continued claims by wages")
wage_removed <- c("Not available", "Zero", "Total")
ue_wages <- ue_wages[!ue_wages$Subgroup %in% wage_removed,]

#Theme of wages graph
wage_theme <- make_me_pretty(list = c('dodgerblue', '#c6dbef', '#9ecae1', '#6baed6', 'steelblue', '#eff3ff'))

#Streamgraph 3 - Continued UE claims by wages in CT
stream_plot_ue(ue_wages, wage_theme, 'By Wages - Jan 2019 to April 2022')