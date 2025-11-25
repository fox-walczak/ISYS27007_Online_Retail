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
 
  dplyr::rename(online_retail, raw_date = InvoiceDate)
  online_retail$date <- as.Date(online_retail$raw_date, format="%m/%d/%Y %H:%M")
  online_retail$date_time <- lubridate::as_datetime(online_retail$raw_date, format="%m/%d/%Y %H:%M")
 
  online_retail$InvoiceAmount <- online_retail$Quantity * online_retail$UnitPrice

  return(online_retail)
}

# Split Costs and Revenue
get_costs <- function(df){
  return(df %>%
           dplyr::pull(InvoiceAmount) %>%
           dplyr::filter(InvoiceAmount < 0)
         )}
get_revenue <- function(df){
  return(df %>%
           dplyr::pull(InvoiceAmount) %>%
           dplyr::filter(InvoiceAmount > 0)
         )}
min_date <- function(df) {
  return(df %>%
           dplyr::pull(date) %>%
           min()
  )}
max_date <- function(df) {
  return(df %>%
           dplyr::pull(date) %>%
           max()
  )}

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
  reactive({
    load_data()
  })
  rv <- reactiveValues()
  observe({
    rv$min_date <- dplyr::pull(input, date_range[1])
    rv$max_date <- dplyr::pull(input, date_range[2])
  })
  observe({
    # orf = online retail filtered
      rv$orf <- online_retail %>% dplyr::filter(date >= rv$min_date &
                                                  date <= rv$max_date)
  })
  output$revenue_over_time <- renderPlot({
    ggplot(get_revenue(rv$orf), mapping=aes(x=date_time,y=InvoiceAmount)) + geom_line()
  })
  output$costs_over_time <- renderPlot({
    ggplot(get_costs(rv$orf), mapping=aes(x=date_time,y=InvoiceAmount)) + geom_line()
  })
  output$money_by_location <- renderPlot({
    ggplot(rv$orf, mapping=aes(x=date_time,y=InvoiceAmount,colour=Country)) + geom_line()
  })
  output$revenue_by_country <- renderPlot({
    country_data_frame <- stats::aggregate(InvoiceAmount~Country, get_revenue(rv$orf), sum)
    ggplot(country_data_frame, mapping=aes(x=countries, y=sum_by_country, fill=countries)) +
      geom_bar(stat="identity", width=1)
    })
}

# RUN

shinyApp(ui=ui, server=server)