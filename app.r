# From https://youtu.be/jxsKUxkiaLI?si=bumTItsDcequcWRV
library("shiny")        # Shiny
library("bslib")        # ""
library("rsconnect")    # Shinyapps.io
library("modeldata")    # Modeling
library("DataExplorer") # ""
library("plotly")       # Widgets
library("tidyverse")    # Core

# Load Data Set
online_retail_file_path <- "OnlineRetail.csv"
online_retail <- read_csv(online_retail_file_path)
#Fix Date Format
online_retail$date <- as.Date(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
online_retail$date_time <- as_datetime(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
# Calculate Invoice Amount
online_retail$InvoiceAmount <- online_retail$Quantity * online_retail$UnitPrice
# Split Costs and Revenue
costs <- online_retail[online_retail$InvoiceAmount < 0,]
revenue <- online_retail[online_retail$InvoiceAmount >= 0,]

# Prepare Pie Chart
countries <- unique(online_retail$Country)
sum_by_country <- c()
for(i in 1:length(countries)){
  sum_by_country[i] <- sum(online_retail[online_retail$Country==countries[i],]$InvoiceAmount)
}
piec <- data.frame(countries, sum_by_country)

min_date <- min(online_retail$date_time)
max_date <- max(online_retail$date_time)
# UI

ui <- navbarPage(
  title = "Bundle and Promote",
  theme = bslib::bs_theme(version=4,bootswatch="minty"),
  tabPanel(
    title = "Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h1("Explore a Dataset"),
        shiny::selectInput(
          inputId = "dataset_choice",
          label   = "Data Connection",
          choices = c("Online Retail")
        ),
        shiny::sliderInput(
          inputId="date_range",
          label="Date Range",
          min=min_date,
          max=max_date,
          value=c(min_date, max_date),
          step=1,
          ticks=TRUE,
          dragRange=TRUE
        ),
        hr(),
        h3("Apps by Business Science"),
        p("Go from beginner to building full-stack shiny apps."),
        p("Learn Shiny Today!") %>%
          a(
            href   = 'https://www.business-science.io/',
            target = "_blank",
            class  = "btn btn-lg btn-primary"
          ) %>%
          div(class="text-center")
      ),
      mainPanel(
        h2("Revenue Over Time"),
        plotOutput("revenue_over_time"),
        h2("Costs Over Time"),
        plotOutput("costs_over_time"),
        h2("Revenue/Costs by Location"),
        plotOutput("money_by_location"),
        h2("Revenue by Country"),
        plotOutput("revenue_by_country")
      )
    )
  )
)

# SERVER

server <- function(input, output) {
  #rv <- reactiveValues()
  #observe({
  #  rv$data_set <- data_list %>% pluck(input$dataset_choice)
  #})
  output$revenue_over_time <- renderPlot({
    ggplot(revenue, mapping=aes(x=date_time,y=InvoiceAmount)) + geom_line()
  })
  output$costs_over_time <- renderPlot({
    ggplot(costs, mapping=aes(x=date_time,y=InvoiceAmount)) + geom_line()
  })
  output$money_by_location <- renderPlot({
    ggplot(online_retail, mapping=aes(x=date_time,y=InvoiceAmount,colour=Country)) + geom_line()
  })
  output$revenue_by_country <- renderPlot({
    ggplot(piec, mapping=aes(x=countries, y=sum_by_country, fill=countries)) +
      geom_bar(stat="identity", width=1)
    })
}

# RUN

shinyApp(ui=ui, server=server)