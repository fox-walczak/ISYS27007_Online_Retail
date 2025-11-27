# From https://youtu.be/jxsKUxkiaLI?si=bumTItsDcequcWRV
library("shiny")        # Shiny
library("bslib")        # ""
library("rsconnect")    # Shinyapps.io
library("modeldata")    # Modeling
library("DataExplorer") # ""
library("plotly")       # Widgets
library("tidyverse")    # Core
library("stringr")
library("forcats")

load_data <- function() {
  online_retail <- readr::read_csv("OnlineRetail.csv")
 
  online_retail$date_time <- lubridate::as_datetime(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
  online_retail$InvoiceDate <- as.Date(online_retail$InvoiceDate, format="%m/%d/%Y %H:%M")
 
  online_retail$invoice_amount <- online_retail$Quantity * online_retail$UnitPrice
  online_retail$is_cost <- online_retail$invoice_amount < 0
  online_retail$abs_invoice_amount <- abs(online_retail$invoice_amount)
  
  return(online_retail)
}

get_top_words <- function(df) {
  all_words <- c()
  for(i in 1:nrow(df)){
    if(is.na(df$Description[i])) {
      all_words <- append(all_words, NA)
    } else {
      all_words <- append(all_words, stringr::str_split_1(df$Description[i], stringr::boundary("word")))
    }
  }
  aggregate(all_words,list(Word=all_words),length) %>%
    sort_by(~x,decreasing=TRUE) %>%
    return()
}

# Profit
calculate_profit <- function(df){ return(sum(dplyr::pull(df,invoice_amount))) }

# Get Min/Max Date
min_date <- function(df) { return(min(dplyr::pull(df,InvoiceDate))) }
max_date <- function(df) { return(max(dplyr::pull(df,InvoiceDate))) }



online_retail <- load_data()



# UI

PLOT_HEIGHT <- "200px"

money_plot_choice_selection <-
  shiny::checkboxGroupInput(
    inputId = "money_plot_choice",
    label   = "Show:",
    choiceNames = c("Revenue", "Costs"),
    choiceValues = c(1, 2)
  )
date_slider <-
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
aggregate_function_selection <-
  shiny::selectInput(
    inputId="aggregate_function",
    label="Aggregate Function:",
    choices=c("Sum","Mean","Median","Minimum","Maximum"),
    selected="Sum"
  )
location_selection <-
  shiny::checkboxGroupInput(
    inputId = "locations",
    label   = "Select Countries:",
    choices = sort(unique(online_retail$Country))
  )

ui <- shiny::navbarPage(
  title = "Online Retail",
  theme = bslib::bs_theme(version=4,bootswatch="minty"),
  tabPanel(
    title = "Bundle and Promote",
    sidebarLayout(
      sidebarPanel(
        h3("Filters"),
        width = 3,
        money_plot_choice_selection,
        date_slider,
        aggregate_function_selection,
        location_selection
      ),
      mainPanel(
        h2("Revenue/Costs Over Time"),
        plotOutput("money_plot",height=PLOT_HEIGHT),
        h2("Cashflow Over Time"),
        plotOutput("cashflow_over_time",height=PLOT_HEIGHT),
        h2("Revenue/Costs By Location"),
        plotOutput("location_plot"),
      )
    )
  )
)

# SERVER

server <- function(input, output) {
  money_plot_choices <- c("money_over_time","revenue_over_time","costs_over_time")
  location_plot_choices <- c("money_by_location","revenue_by_location","costs_by_location")
  aggregate_functions <- list(
    "Sum" = sum,
    "Mean" = mean,
    "Median" = stats::median,
    "Minimum" = min,
    "Maximum" = max
  )
  rv <- reactiveValues()
  observe({
    rv$aggregate <- aggregate_functions %>% purrr::pluck(input$aggregate_function)
    rv$min_date <- input$date_range[1]
    rv$max_date <- input$date_range[2]
    rv$or <- online_retail %>%
      dplyr::filter(InvoiceDate >= rv$min_date & InvoiceDate <= rv$max_date &
                      ((Country %in% input$locations) | (length(input$locations) == 0))
      )
  })
  
  output$test <- renderText({
    paste(input$locations)
  })
  
  # REVENUE / COST
  
  money_plots <- list(
    "revenue_over_time" = function(df){
      stats::aggregate(abs_invoice_amount~InvoiceDate, dplyr::filter(df, !is_cost), rv$aggregate) %>%
        ggplot(mapping=aes(x=InvoiceDate,y=abs_invoice_amount)) +
        geom_line(colour="green")
    },
    "costs_over_time" = function(df){
      stats::aggregate(abs_invoice_amount~InvoiceDate, dplyr::filter(df, is_cost), rv$aggregate) %>%
        ggplot(mapping=aes(x=InvoiceDate,y=abs_invoice_amount)) +
        geom_line(colour="red")
    },
    "money_over_time" = function(df) {
      stats::aggregate(abs_invoice_amount ~ InvoiceDate + is_cost, df, rv$aggregate) %>%
        ggplot(mapping=aes(x=InvoiceDate,y=abs_invoice_amount, colour=is_cost)) +
        scale_colour_discrete(name="Key",labels=c("Revenue","Costs"), palette=c("green","red")) +
        geom_line()
    }
  )
  
  observe({
    rv$money_plot_choice <- money_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1]
  })
  output$money_plot <- renderPlot({
    purrr::pluck(money_plots, rv$money_plot_choice)(rv$or)
  })
  
  # CASHFLOW
  
  cashflow_over_time = function(df) {
    stats::aggregate(invoice_amount ~ InvoiceDate, df, rv$aggregate) %>%
      ggplot(mapping=aes(x=InvoiceDate,y=invoice_amount)) +
      geom_line()
  }
  output$cashflow_over_time <- renderPlot({
    cashflow_over_time(rv$or)
  })
  
  # LOCATION
  
  location_plots <- list(
    "revenue_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount~Country, dplyr::filter(df, !is_cost), rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount)) +
        geom_col(fill="green")
    },
    "costs_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount~Country, dplyr::filter(df, is_cost), rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount)) +
        geom_col(fill="red")
    },
    "money_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount ~ Country + is_cost, df, rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount, fill=is_cost)) +
        scale_fill_discrete(name="Key",labels=c("Revenue","Costs"), palette=c("green","red")) +
        geom_col(position=position_dodge(preserve="single"))
    }
  )
  observe({
    rv$location_plot <- location_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1]
  })
  output$location_plot <- renderPlot({
    purrr::pluck(location_plots, rv$location_plot)(rv$or) +
      theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1)) +
      coord_flip()
  })
  #output$money_by_location <- renderPlot({
  #  ggplot(rv$or, mapping=aes(x=date_time,y=InvoiceAmount,colour=Country)) + geom_line()
  #})
  #output$revenue_by_country <- renderPlot({
  #  country_data_frame <- stats::aggregate(InvoiceAmount~Country, filter_revenue(rv$or), rv$aggregate)
  #  ggplot(country_data_frame, mapping=aes(x=countries, y=sum_by_country, fill=countries)) +
  #    geom_bar(stat="identity", width=1)
  #})
  
  # KPIs
  output$profit <- renderText({calculate_profit(rv$or)})
}

# ------- PROGRAM START -------

shinyApp(ui=ui, server=server)