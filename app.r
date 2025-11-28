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

# Profit = Revenue - Cost
calculate_profit <- function(df){
  return(
    sum(dplyr::pull(df,invoice_amount)) %>% signif(3)
  )}
# Profit margin = (Revenue - Cost) / Revenue
calculate_profit_margin <- function(df){
  return(
    (pull(df, invoice_amount) %>% sum() / max(filter(df, !is_cost) %>%
            pull(invoice_amount) %>% sum(), 0.000001) * 100) %>% signif(3)
  )}


money_label <- function(x) {
  tryCatch({
    o <- c()
    for(i in 1:length(x)) {
      abbreviation <- ""
      if(x[i] > 1000000) {
        o[i] <- as.integer(x[i]) / 1000000
        abbreviation <- "M"
      } else if(x[i] > 1000){
        o[i] <- as.integer(x[i]) / 1000
        abbreviation <- "K"
      }
      if(x[i] == 0){
        o[i] <-- 0
      } else {
        o[i] <- paste(o[i],abbreviation,sep="")
      }
    }
    return(o)
  }, error=function(e){ return(x) })
}

# Get Min/Max Date
min_date <- function(df) { return(min(dplyr::pull(df,InvoiceDate))) }
max_date <- function(df) { return(max(dplyr::pull(df,InvoiceDate))) }



online_retail <- load_data()



# UI

PLOT_HEIGHT <- "200px"
GREEN <- "darkgreen"
RED <- "darkred"

money_plot_choice_selection <-
  shiny::checkboxGroupInput(
    inputId = "money_plot_choice",
    label=NULL,
    choiceNames = c("Revenue", "Costs"),
    choiceValues = c(1, 2)
  )
date_slider <-
  shiny::sliderInput(
    inputId="date_range",
    label=NULL,
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
    label=NULL,
    choices=c("Sum","Mean","Median","Minimum","Maximum"),
    selected="Sum"
  )
location_selection <-
  shiny::checkboxGroupInput(
    inputId = "locations",
    label=NULL,
    choices = sort(unique(online_retail$Country))
  )

# From https://icons.getbootstrap.com/icons/percent/
profit_margin_icon <- HTML('<svg xmlns="http://www.w3.org/2000/svg" width=50% fill="currentColor" class="bi bi-percent" viewBox="0 0 16 16"> <path d="M13.442 2.558a.625.625 0 0 1 0 .884l-10 10a.625.625 0 1 1-.884-.884l10-10a.625.625 0 0 1 .884 0M4.5 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5m7 6a1.5 1.5 0 1 1 0-3 1.5 1.5 0 0 1 0 3m0 1a2.5 2.5 0 1 0 0-5 2.5 2.5 0 0 0 0 5"/> </svg>')

# From https://icons.getbootstrap.com/icons/currency-pound/
profit_icon <- HTML('<svg xmlns="http://www.w3.org/2000/svg" width=50% fill="currentColor" class="bi bi-currency-pound" viewBox="0 0 16 16"> <path d="M4 8.585h1.969c.115.465.186.939.186 1.43 0 1.385-.736 2.496-2.075 2.771V14H12v-1.24H6.492v-.129c.825-.525 1.135-1.446 1.135-2.694 0-.465-.07-.913-.168-1.352h3.29v-.972H7.22c-.186-.723-.372-1.455-.372-2.247 0-1.274 1.047-2.066 2.58-2.066a5.3 5.3 0 0 1 2.103.465V2.456A5.6 5.6 0 0 0 9.348 2C6.865 2 5.322 3.291 5.322 5.366c0 .775.195 1.515.399 2.247H4z"/> </svg>')

ui <- shiny::navbarPage(
  title = "Online Retail",
  theme = bslib::bs_theme(bootswatch="flatly",
                          base_font="'Montserrat','Roboto','Open Sans','Noto Sans','Poppins','Nunito','Segoe UI','Arial','Verdana','Consolas'"),
  tabPanel(
    title = "Bundle and Promote",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        accordion(open=c("Show...", "Aggregate Function", "Date Range"), multiple=TRUE,
                  accordion_panel( "Show...", money_plot_choice_selection ),
                  accordion_panel( "Aggregate Function", aggregate_function_selection ),
                  accordion_panel( "Date Range", date_slider ),
                  accordion_panel( "Select Countries", location_selection )
        )
      ),
      mainPanel(
        layout_columns(
          value_box( 
            title = "Profit Margin", 
            textOutput("profit_margin"), 
            showcase = profit_margin_icon
          ),
          value_box( 
            title = "Total Profit", 
            textOutput("profit"), 
            showcase = profit_icon
          )
        ),
        plotOutput("money_plot",height=PLOT_HEIGHT),
        layout_columns(
          plotOutput("profit_over_time",height=PLOT_HEIGHT),
          plotOutput("location_plot")
        )
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
  name_of <- function(ag_fn) {if(ag_fn == "Sum"){return("Total")}else{return(ag_fn)}}
  name_daily <- function(ag_fn) {if(ag_fn == "Sum"){return("Total")}else{return(paste(ag_fn,"Daily"))}}
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
        geom_line(colour=GREEN) + ggtitle(paste(name_daily(input$aggregate_function), "Revenue Over Time"))
    },
    "costs_over_time" = function(df){
      stats::aggregate(abs_invoice_amount~InvoiceDate, dplyr::filter(df, is_cost), rv$aggregate) %>%
        ggplot(mapping=aes(x=InvoiceDate,y=abs_invoice_amount)) +
        geom_line(colour=RED) + ggtitle(paste(name_daily(input$aggregate_function), "Costs Over Time"))
    },
    "money_over_time" = function(df) {
      stats::aggregate(abs_invoice_amount ~ InvoiceDate + is_cost, df, rv$aggregate) %>%
        ggplot(mapping=aes(x=InvoiceDate,y=abs_invoice_amount, colour=is_cost)) +
        scale_colour_discrete(name="Key",labels=c("Revenue","Costs"), palette=c(GREEN,RED)) +
        geom_line() + ggtitle(paste(name_daily(input$aggregate_function),"Revenue and Costs Over Time"))
    }
  )
  
  observe({
    rv$money_plot_choice <- money_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1]
  })
  output$money_plot <- renderPlot({
    purrr::pluck(money_plots, rv$money_plot_choice)(rv$or) +
      theme_bw() +
      ylab(paste(name_of(input$aggregate_function), "(£)")) +
      scale_y_continuous(label = money_label) +
      xlab("Date")
  })
  
  # PROFIT
  
  profit_over_time = function(df) {
    stats::aggregate(invoice_amount ~ InvoiceDate, df, rv$aggregate) %>%
      ggplot(mapping=aes(x=InvoiceDate,y=invoice_amount)) +
      geom_line(colour="#2c3e50") + ggtitle(paste(name_of(input$aggregate_function), "Daily Profit"))
  }
  output$profit_over_time <- renderPlot({
    profit_over_time(rv$or) + theme_bw() +
      ylab(paste(name_of(input$aggregate_function), "(£)")) +
      scale_y_continuous(label = money_label) +
      xlab("Date")
  })
  
  # LOCATION
  
  location_title <- function(type) {
    if(input$aggregate_function == "Sum") {
      return(ggtitle(paste(name_of(input$aggregate_function), type, "by Country")))
    } else {
      return(ggtitle(paste(name_of(input$aggregate_function), type, "of Transactions by Country")))
    }
  }
  
  location_plots <- list(
    "revenue_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount~Country, dplyr::filter(df, !is_cost), rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount)) +
        geom_col(fill=GREEN) + location_title("Revenue")
    },
    "costs_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount~Country, dplyr::filter(df, is_cost), rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount)) +
        geom_col(fill=RED) + location_title("Costs")
    },
    "money_by_location" = function(df) {
      stats::aggregate(abs_invoice_amount ~ Country + is_cost, df, rv$aggregate) %>%
        sort_by(~abs_invoice_amount,decreasing=TRUE) %>%
        ggplot(mapping=aes(x=fct_reorder(Country, abs_invoice_amount),y=abs_invoice_amount, fill=is_cost)) +
        scale_fill_discrete(name="Key",labels=c("Revenue","Costs"), palette=c(GREEN,RED)) +
        geom_col(position=position_dodge(preserve="single")) + location_title("Revenue and Costs")
    }
  )
  observe({
    rv$location_plot <- location_plot_choices[sum(as.integer(input$money_plot_choice)) %% 3 + 1]
  })
  output$location_plot <- renderPlot({
    purrr::pluck(location_plots, rv$location_plot)(rv$or) +
      coord_flip() +
      theme_bw() +
      scale_y_continuous(label = money_label) +
      ylab(paste(name_of(input$aggregate_function), "(£)")) +
      xlab("")
  })
  
  # KPIs
  output$profit <- renderText({
    format(calculate_profit(rv$or), big.mark=",")
  })
  output$profit_margin <- renderText({
    format(calculate_profit_margin(rv$or), nsmall=1)
  })
}

# ------- PROGRAM START -------

shinyApp(ui=ui, server=server)