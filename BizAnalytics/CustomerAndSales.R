library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)

# Generate sample data
set.seed(123)
dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")
n <- length(dates)

sales_data <- data.frame(
  date = dates,
  customer_id = sample(1:500, n, replace = TRUE),
  product_category = sample(c("Electronics", "Clothing", "Home & Garden", "Sports", "Books"), n, replace = TRUE),
  sales_amount = round(rnorm(n, mean = 150, sd = 75), 2),
  region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
  customer_segment = sample(c("High-Value", "Medium-Value", "Low-Value"), n, replace = TRUE)
) %>%
  mutate(
    sales_amount = pmax(sales_amount, 10),
    month = floor_date(date, "month")
  )

# UI
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Business Analytics Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Customer & Sales", tabName = "dashboard1", icon = icon("chart-line")),
      menuItem("Forecasting & Planning", tabName = "dashboard2", icon = icon("chart-area"))
    ),

    hr(),
    h4("Filters", style = "padding-left: 15px; color: white;"),

    dateRangeInput("dateRange",
                   "Date Range:",
                   start = min(sales_data$date),
                   end = max(sales_data$date)),

    selectInput("region",
                "Region:",
                choices = c("All", unique(sales_data$region)),
                selected = "All"),

    selectInput("category",
                "Product Category:",
                choices = c("All", unique(sales_data$product_category)),
                selected = "All")
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #3c8dbc; }
        .small-box { border-radius: 5px; }
        .small-box h3 { font-size: 32px; font-weight: bold; }
      "))
    ),

    tabItems(
      # Dashboard 1: Customer & Sales Performance
      tabItem(tabName = "dashboard1",
              h2("Customer & Sales Performance Dashboard"),
              p("Who are our customers, how are they behaving, and where is revenue coming from?"),

              fluidRow(
                valueBoxOutput("totalSales", width = 3),
                valueBoxOutput("numCustomers", width = 3),
                valueBoxOutput("avgOrderValue", width = 3),
                valueBoxOutput("growthRate", width = 3)
              ),

              fluidRow(
                box(
                  title = "Sales Trends Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("salesTrend", height = 300)
                )
              ),

              fluidRow(
                box(
                  title = "Sales by Product Category",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("categoryChart", height = 300)
                ),

                box(
                  title = "Customer Segmentation Analysis",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("segmentChart", height = 300)
                )
              ),

              fluidRow(
                box(
                  title = "Regional Performance",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("regionalChart", height = 300)
                )
              )
      ),

      # Dashboard 2: Forecasting & Decision Support
      tabItem(tabName = "dashboard2",
              h2("Forecasting & Decision Support Dashboard"),
              p("What is likely to happen next, and how should we respond?"),

              fluidRow(
                box(
                  title = "Scenario Planning Controls",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,

                  fluidRow(
                    column(4,
                           sliderInput("forecastHorizon",
                                       "Forecast Horizon (months):",
                                       min = 1, max = 12, value = 6)
                    ),
                    column(4,
                           selectInput("scenario",
                                       "Scenario:",
                                       choices = c("Normal", "Promotion (+20%)", "Conservative (-10%)"),
                                       selected = "Normal")
                    ),
                    column(4,
                           checkboxInput("showConfidence",
                                         "Show Confidence Intervals",
                                         value = TRUE)
                    )
                  )
                )
              ),

              fluidRow(
                box(
                  title = "Historical vs Forecasted Sales",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("forecastChart", height = 350)
                )
              ),

              fluidRow(
                box(
                  title = "Risk Indicators",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("riskIndicators", height = 250)
                ),

                box(
                  title = "Scenario Comparison",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("scenarioComparison", height = 250)
                )
              ),

              fluidRow(
                infoBoxOutput("forecastAccuracy", width = 4),
                infoBoxOutput("overstockRisk", width = 4),
                infoBoxOutput("understockRisk", width = 4)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive filtered data
  filtered_data <- reactive({
    data <- sales_data

    if (input$region != "All") {
      data <- data %>% filter(region == input$region)
    }

    if (input$category != "All") {
      data <- data %>% filter(product_category == input$category)
    }

    data %>% filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  })

  # Dashboard 1 Outputs
  output$totalSales <- renderValueBox({
    total <- sum(filtered_data()$sales_amount)
    valueBox(
      paste0("$", format(round(total), big.mark = ",")),
      "Total Sales",
      icon = icon("dollar-sign"),
      color = "blue"
    )
  })

  output$numCustomers <- renderValueBox({
    customers <- n_distinct(filtered_data()$customer_id)
    valueBox(
      format(customers, big.mark = ","),
      "Unique Customers",
      icon = icon("users"),
      color = "green"
    )
  })

  output$avgOrderValue <- renderValueBox({
    avg <- mean(filtered_data()$sales_amount)
    valueBox(
      paste0("$", round(avg, 2)),
      "Avg Order Value",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })

  output$growthRate <- renderValueBox({
    data <- filtered_data()
    current_month <- data %>%
      filter(month == max(month)) %>%
      summarise(total = sum(sales_amount)) %>%
      pull(total)

    prev_month <- data %>%
      filter(month == max(month) - months(1)) %>%
      summarise(total = sum(sales_amount)) %>%
      pull(total)

    growth <- if(prev_month > 0) ((current_month - prev_month) / prev_month) * 100 else 0

    valueBox(
      paste0(ifelse(growth >= 0, "+", ""), round(growth, 1), "%"),
      "MoM Growth",
      icon = icon("chart-line"),
      color = ifelse(growth >= 0, "green", "red")
    )
  })

  output$salesTrend <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month) %>%
      summarise(total_sales = sum(sales_amount))

    plot_ly(data, x = ~month, y = ~total_sales, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3c8dbc', width = 3),
            marker = list(size = 8, color = '#3c8dbc')) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Sales ($)"),
        hovermode = 'x unified'
      )
  })

  output$categoryChart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(product_category) %>%
      summarise(total_sales = sum(sales_amount)) %>%
      arrange(desc(total_sales))

    plot_ly(data, x = ~product_category, y = ~total_sales, type = 'bar',
            marker = list(color = '#00a65a')) %>%
      layout(
        xaxis = list(title = "Product Category"),
        yaxis = list(title = "Total Sales ($)")
      )
  })

  output$segmentChart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(customer_segment) %>%
      summarise(
        total_sales = sum(sales_amount),
        customer_count = n_distinct(customer_id),
        avg_purchase = mean(sales_amount)
      )

    plot_ly(data, x = ~customer_count, y = ~avg_purchase,
            text = ~customer_segment, type = 'scatter', mode = 'markers+text',
            marker = list(size = ~total_sales/1000, color = '#f39c12', opacity = 0.6),
            textposition = 'top center') %>%
      layout(
        xaxis = list(title = "Number of Customers"),
        yaxis = list(title = "Avg Purchase Value ($)")
      )
  })

  output$regionalChart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(region, month) %>%
      summarise(total_sales = sum(sales_amount), .groups = 'drop')

    plot_ly(data, x = ~month, y = ~total_sales, color = ~region, type = 'scatter', mode = 'lines',
            colors = c('#3c8dbc', '#00a65a', '#f39c12', '#dd4b39')) %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Sales ($)"),
        hovermode = 'x unified'
      )
  })

  # Dashboard 2 Outputs
  output$forecastChart <- renderPlotly({
    data <- filtered_data() %>%
      group_by(month) %>%
      summarise(actual_sales = sum(sales_amount))

    # Simple forecast (last value + trend)
    last_months <- tail(data, 3)
    trend <- (last_months$actual_sales[3] - last_months$actual_sales[1]) / 2
    last_value <- tail(data$actual_sales, 1)

    # Generate forecast
    forecast_months <- seq(max(data$month) + months(1),
                           max(data$month) + months(input$forecastHorizon),
                           by = "month")

    multiplier <- switch(input$scenario,
                         "Normal" = 1,
                         "Promotion (+20%)" = 1.2,
                         "Conservative (-10%)" = 0.9)

    forecast_values <- (last_value + trend * (1:input$forecastHorizon)) * multiplier

    forecast_data <- data.frame(
      month = forecast_months,
      forecast_sales = forecast_values,
      lower = forecast_values * 0.85,
      upper = forecast_values * 1.15
    )

    plot_ly() %>%
      add_lines(data = data, x = ~month, y = ~actual_sales, name = "Historical",
                line = list(color = '#3c8dbc', width = 3)) %>%
      add_lines(data = forecast_data, x = ~month, y = ~forecast_sales, name = "Forecast",
                line = list(color = '#f39c12', width = 3, dash = 'dash')) %>%
      {if(input$showConfidence)
        add_ribbons(., data = forecast_data, x = ~month, ymin = ~lower, ymax = ~upper,
                    name = "Confidence Interval", fillcolor = 'rgba(243, 156, 18, 0.2)',
                    line = list(color = 'transparent'))
        else .
      } %>%
      layout(
        xaxis = list(title = "Month"),
        yaxis = list(title = "Sales ($)"),
        hovermode = 'x unified'
      )
  })

  output$riskIndicators <- renderPlot({
    risks <- data.frame(
      category = c("Overstock Risk", "Understock Risk", "Demand Volatility"),
      risk_level = c(35, 60, 45),
      threshold = c(50, 50, 50)
    )

    ggplot(risks, aes(x = category, y = risk_level, fill = risk_level > threshold)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1) +
      scale_fill_manual(values = c("TRUE" = "#dd4b39", "FALSE" = "#00a65a")) +
      coord_flip() +
      labs(x = "", y = "Risk Level (%)") +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 12))
  })

  output$scenarioComparison <- renderPlotly({
    base_value <- 50000
    scenarios <- data.frame(
      scenario = c("Normal", "Promotion", "Conservative"),
      value = c(base_value, base_value * 1.2, base_value * 0.9)
    )

    plot_ly(scenarios, x = ~scenario, y = ~value, type = 'bar',
            marker = list(color = c('#3c8dbc', '#00a65a', '#f39c12'))) %>%
      layout(
        xaxis = list(title = "Scenario"),
        yaxis = list(title = "Projected Sales ($)")
      )
  })

  output$forecastAccuracy <- renderInfoBox({
    infoBox(
      "Forecast Accuracy",
      "87.3%",
      icon = icon("bullseye"),
      color = "blue",
      fill = TRUE
    )
  })

  output$overstockRisk <- renderInfoBox({
    infoBox(
      "Overstock Risk",
      "Low",
      icon = icon("warehouse"),
      color = "green",
      fill = TRUE
    )
  })

  output$understockRisk <- renderInfoBox({
    infoBox(
      "Understock Risk",
      "Moderate",
      icon = icon("exclamation-triangle"),
      color = "yellow",
      fill = TRUE
    )
  })
}

shinyApp(ui, server)
