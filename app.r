# From https://youtu.be/jxsKUxkiaLI?si=bumTItsDcequcWRV
library("shiny")        # Shiny
library("bslib")        # ""
library("rsconnect")    # Shinyapps.io
library("modeldata")    # Modeling
library("DataExplorer") # ""
library("plotly")       # Widgets
library("tidyverse")    # Core

load_data <- function() {
  online_retail <- readr::read_csv("OnlineRetail.csv")
 
  online_retail$date_time <- lubridate::as_datetime(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
  online_retail$InvoiceDate <- as.Date(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
 
  online_retail$InvoiceAmount <- online_retail$Quantity * online_retail$UnitPrice
  
  return(online_retail)
}

# Split Costs and Revenue
filter_cost <- function(df){return(
  dplyr::filter(df, InvoiceAmount < 0) %>%
    dplyr::mutate(InvoiceAmount = abs(InvoiceAmount))
)}
filter_revenue <- function(df){ return(dplyr::filter(df, InvoiceAmount > 0)) }

# Profit
calculate_profit <- function(df){ return(sum(dplyr::pull(df,InvoiceAmount))) }

# Get Min/Max Date
min_date <- function(df) { return(min(dplyr::pull(df,InvoiceDate))) }
max_date <- function(df) { return(max(dplyr::pull(df,InvoiceDate))) }



online_retail <- load_data()



# UI

ui <- shiny::navbarPage(
  title = "Bundle and Promote",
  theme = bslib::bs_theme(version=4,bootswatch="minty"),
  tabPanel(
    title = "Dashboard",
    sidebarLayout(
      sidebarPanel(
        h3("Filters"),
        width = 3,
        shiny::checkboxGroupInput(
          inputId = "money_plot_choice",
          label   = "Show:",
          choiceNames = c("Revenue", "Costs"),
          choiceValues = c(1, 2),
          selected = c(1, 2)
        ),
        shiny::sliderInput(
          inputId="date_range",
          label="Date Range",
          min=min_date(online_retail),
          max=max_date(online_retail),
          value=c(min_date(online_retail), max_date(online_retail)),
          step=1,
          ticks=TRUE,
          dragRange=TRUE
        )
      ),
      mainPanel(
        #textOutput("dates"),
        #tableOutput("ort"),
        #tableOutput("rvort"),
        textOutput("money"),
        
        h2("Money Over Time"),
        plotOutput("money_plot"),
        #h2("Revenue Over Time"),
        #plotOutput("revenue_over_time"),
        #h2("Costs Over Time"),
        #plotOutput("costs_over_time"),
        #h2("Costs and Revenue Over Time"),
        #plotOutput("money_over_time"),
        
        #h2("Revenue/Costs by Location"),
        #plotOutput("money_by_location"),
        #h2("Revenue by Country"),
        #plotOutput("revenue_by_country")
      )
    )
  )
)

# SERVER

server <- function(input, output) {
  money_plot_choices <- c("money_over_time","revenue_over_time","costs_over_time")
  rv <- reactiveValues()
  observe({
    rv$min_date <- input$date_range[1]
    rv$max_date <- input$date_range[2]
  })
  observe({
    # [O]nline [R]etail
    rv$or <- online_retail %>% dplyr::filter(InvoiceDate >= rv$min_date &
                                               InvoiceDate <= rv$max_date)
  })
  output$money <- shiny::renderText({
    paste(sum(as.integer(input$money_plot_choice)) %% 3 + 1,
          money_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1])
  })
  output$dates <- shiny::renderText({
    paste(rv$min_date, rv$max_date)
  })
  output$ort <- shiny::renderTable({
    as.data.frame(online_retail[1:10,])
  })
  output$rvort <- shiny::renderTable({
    temp <- rv$or[1:10,]
    temp$InvoiceDate <- format(temp$InvoiceDate)
    as.data.frame(temp %>% dplyr::select(Description, InvoiceDate), stringsAsFactors=TRUE)
  })
  
  # REVENUE / COST
  
  money_plots <- list(
    "revenue_over_time" = function(df){
    ggplot(stats::aggregate(InvoiceAmount~InvoiceDate,filter_revenue(df), sum),
           mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
    geom_line(colour="green")
    },
    "costs_over_time" = function(df){
    ggplot(stats::aggregate(InvoiceAmount~InvoiceDate,filter_cost(df), sum),
           mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
    geom_line(colour="red")
    },
    "money_over_time" = function(df) {
    stats::aggregate(InvoiceAmount~InvoiceDate+(InvoiceAmount<0), df, sum) %>% ggplot(
      mapping=aes(x=InvoiceDate,y=abs(InvoiceAmount), colour=InvoiceAmount<0)) +
    scale_colour_discrete(name="Key",labels=c("Revenue","Costs"), palette=c("green","red")) +
    geom_line()
    }
  )
  
  observe({
    rv$money_plot_choice <- money_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1]
    #rv$money_plot <- purrr::pluck(money_plot, money_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1])
  })
  output$money_plot <- renderPlot({
    purrr::pluck(money_plots, rv$money_plot_choice)(rv$or)
  })
  #output$money_by_location <- renderPlot({
  #  ggplot(rv$or, mapping=aes(x=date_time,y=InvoiceAmount,colour=Country)) + geom_line()
  #})
  #output$revenue_by_country <- renderPlot({
  #  country_data_frame <- stats::aggregate(InvoiceAmount~Country, filter_revenue(rv$or), sum)
  #  ggplot(country_data_frame, mapping=aes(x=countries, y=sum_by_country, fill=countries)) +
  #    geom_bar(stat="identity", width=1)
  #})
  
  # KPIs
  output$profit <- renderText({calculate_profit(rv$or)})
}

# ------- PROGRAM START -------

shinyApp(ui=ui, server=server)