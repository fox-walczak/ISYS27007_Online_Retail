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

# UI

ui <- shiny::navbarPage(
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
          min=min_date(online_retail),
          max=max_date(online_retail),
          value=c(min_date(online_retail), max_date(online_retail)),
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
  online_retail <- load_data()
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
  
  output$revenue_over_time <- renderPlot({
    ggplot(stats::aggregate(InvoiceAmount~InvoiceDate,filter_revenue(rv$or), sum),
           mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
      geom_line(colour="green")
  })
  output$costs_over_time <- renderPlot({
    ggplot(stats::aggregate(InvoiceAmount~InvoiceDate,filter_cost(rv$or), sum),
           mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
      geom_line(colour="red")
  })
  output$money_over_time <- renderPlot({
             #mutate(InvoiceAmount = dplyr::case_when(
             #  InvoiceAmount < 0 ~ 0-InvoiceAmount,
             #  InvoiceAmount >= 0 ~ InvoiceAmount)) %>%
    stats::aggregate(InvoiceAmount~InvoiceDate+(InvoiceAmount<0), rv$or, sum) %>% ggplot(
      #mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
      mapping=aes(x=InvoiceDate,y=abs(InvoiceAmount), colour=InvoiceAmount<0)) +
      scale_colour_discrete(name="Key",labels=c("Revenue","Costs"), palette=c("green","red")) +
      geom_line()
    #ggplot(stats::aggregate(InvoiceAmount~InvoiceDate, rv$or, sum) %>%
    #         mutate(InvoiceAmount = dplyr::case_when(
    #           InvoiceAmount < 0 ~ 0-InvoiceAmount,
    #           InvoiceAmount >= 0 ~ InvoiceAmount)),
    #       mapping=aes(x=InvoiceDate,y=InvoiceAmount)) +
    #       #mapping=aes(x=InvoiceDate,y=InvoiceAmount, colour=InvoiceAmount<0)) +
    #  geom_line()
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

# RUN

shinyApp(ui=ui, server=server)