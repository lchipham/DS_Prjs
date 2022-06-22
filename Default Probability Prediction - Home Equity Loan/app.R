library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel(h3("Equity Risk Calculator", align = "center")),
    h6(tags$a(href = "https://github.com/lchipham/Data-Science-Projects/blob/main/RCode_Home%20Equity%20Loan_LChiPham.Rmd", "Source code: Linh-Chi Pham"), align = "center"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("CUSTOMER INFO"),
            sliderInput("debtinc",
                        "Debt-Income Ratio",
                        min = round(0.5244992,2),
                        max = round(203.3121487,2),
                        value = 16),
            sliderInput("delinq",
                        "Delinquent Credit Lines",
                        min = 0,
                        max = 15,
                        value = 1),
            sliderInput("derog",
                        "Major Derogatory Reports",
                        min = 0,
                        max = 10,
                        value = 1),
            sliderInput("clage",
                        "Oldest Credit Line (month)",
                        min = 0,
                        max = round(1168.234,2),
                        value = 179.77),
            sliderInput("loan", 
                        "Loan Request Amount",  
                         min = 1100,
                         max = 89900,
                         value = 18608),
            sliderInput("ninq",
                        "Recent Credit Inquiries",
                        min = 0,
                        max = 17,
                        value = 2),
            sliderInput("clno",
                        "Total Credit Lines",
                        min = 0,
                        max = 71,
                        value = 21)
        ),

        # Show text output + result
        mainPanel(
           numericInput("acc_rate", "Loan Acceptance Rate (%):",  
                        min = 0,
                        max = 100,
                        value = 80, width = "200px"),
           uiOutput("text"),
           br(),
           plotOutput("percentileZ"),
           uiOutput("decision")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  default_prob <- function(X1 = 16, X2 = 1, X3 = 1, X4 = 179.77, X5 = 18608, X6 = 2, X7 = 21, pct = TRUE, plotDT = TRUE) {
    lm_mod <- -2.37457089 + 0.05227723*X1 + 0.75513589*X2 + 0.63659597*X3 - 0.00509945*X4 - 0.00001965*X5 + 0.167474596*X6 - 0.01794787*X7
    prob <- exp(lm_mod) / 1 + exp(lm_mod)
    Def_Prob <<- prob
    
    if (pct == TRUE) {
      zscore <- (Def_Prob - 0.2079319) / 0.2003866
      Zscore <<- zscore
      pct_score <- pnorm(Def_Prob, mean = 0.2079319, sd = 0.2003866)
      PCT_cus <<- pct_score
    }
    
    if (plotDT == TRUE) {
      dt <- data.frame(scores = readRDS("testProb.RData"))
      dens <- density(dt$scores)
      df <- data.frame(x=dens$x, y=dens$y)
      quantiles <- quantile(dt$scores, prob=(input$acc_rate/100))
      df$quantile <- factor(findInterval(df$x, quantiles), labels = c("Accept", "Refuse"))
      ggplot(df, aes(x, y)) + 
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quantile)) + 
        scale_x_continuous(breaks=quantiles) +
        geom_vline(xintercept = quantiles, color = "black", linetype = "dashed", size=0.69) +
        geom_text(aes(x = quantiles - 0.026, label = "cutoff line", y=2.5), colour="black", angle=90, family = "serif") +
        geom_vline(xintercept = Def_Prob, col = "brown", linetype = "dashed", size=0.69) +
        geom_text(aes(x=Def_Prob - 0.026, label="customer percentile", y=2.5), colour="brown", angle=90, family = "serif") +
        labs(title = "Distribution of Default Probability", fill = "Decision", x = "Percentile", y = "") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14), text = element_text(family = "serif"),
              panel.grid = element_blank()) +
        scale_fill_brewer(palette = "OrRd")
    }
  }
  output$text <- renderUI({
    default_prob(X1 = input$debtinc, X2 = input$delinq, X3 = input$derog, X4 = input$clage, X5 = input$loan, X6 = input$ninq, X7 = input$clno, pct = TRUE, plotDT = FALSE)
    HTML(paste0(
      "<b>", "Default Probability: ", format(round(Def_Prob, digits = 2)), "</b>",
      "<br>",
      "This customer's default probability is in the ", format(round(PCT_cus*100, digits = 2)), "th percentile.",
      "<br>"
    ))
  })
  
  output$percentileZ <- renderPlot({
    default_prob(X1 = input$debtinc, X2 = input$delinq, X3 = input$derog, X4 = input$clage, X5 = input$loan, X6 = input$ninq, X7 = input$clno, pct = TRUE, plotDT = TRUE)
  })
  
  output$decision <- renderUI({
    default_prob(X1 = input$debtinc, X2 = input$delinq, X3 = input$derog, X4 = input$clage, X5 = input$loan, X6 = input$ninq, X7 = input$clno, pct = TRUE, plotDT = FALSE)
    if (PCT_cus > (input$acc_rate/100)) {
      HTML(paste0(
        "<b>", "Bank's Decision: Refuse","</b>",
        "<br>",
        "This customer is not qualified for loan as ", format(round(PCT_cus*100, digits = 2)),"% > ", input$acc_rate, "% acceptance rate.",
        "<br>"))
    } else if (PCT_cus <= (input$acc_rate/100)){
      HTML(paste0(
        "<b>", "Bank's Decision: Accept","</b>",
        "<br>",
        "This customer is qualified for loan as ", format(round(PCT_cus*100, digits = 2)),"% <= ", input$acc_rate, "% acceptance rate.",
        "<br>"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
