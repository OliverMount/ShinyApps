library(shiny)
library(ggplot2)

# OU Process Simulation Function
simulate_OU <- function(theta, mu, sigma, x0, T, dt) {
  time <- seq(0, T, by = dt)
  n <- length(time)
  x <- numeric(n)
  x[1] <- x0
  for (i in 2:n) {
    x[i] <- x[i-1] + theta * (mu - x[i-1]) * dt + sigma * sqrt(dt) * rnorm(1)
  }
  data.frame(time = time, x = x)
}

# UI
ui <- fluidPage(
  titlePanel("Ornstein-Uhlenbeck Process Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Parameters"),
      
      sliderInput("theta", 
                  "θ (Mean Reversion Rate):",
                  min = 0.1, max = 5.0, value = 0.7, step = 0.1),
      
      sliderInput("mu",
                  "μ (Long-term Mean):",
                  min = -2, max = 2, value = 0, step = 0.1),
      
      sliderInput("sigma",
                  "σ (Volatility):",
                  min = 0.05, max = 1.0, value = 0.2, step = 0.05),
      
      sliderInput("x0",
                  "x₀ (Initial Value):",
                  min = -3, max = 3, value = -1, step = 0.1),
      
      hr(),
      
      h4("Simulation Settings"),
      
      sliderInput("T",
                  "T (Total Time):",
                  min = 5, max = 50, value = 10, step = 5),
      
      sliderInput("dt",
                  "Δt (Time Step):",
                  min = 0.001, max = 0.1, value = 0.01, step = 0.001),
      
      sliderInput("n_traj",
                  "Number of Trajectories:",
                  min = 1, max = 20, value = 5, step = 1),
      
      hr(),
      
      numericInput("seed",
                   "Random Seed:",
                   value = 123, min = 1, max = 10000),
      
      actionButton("simulate", "Simulate", class = "btn-primary"),
      
      hr(),
      
      h5("Stationary Distribution:"),
      uiOutput("stat_dist")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Trajectories",
                 plotOutput("traj_plot", height = "600px")),
        
        tabPanel("Compare θ",
                 plotOutput("theta_compare", height = "600px"),
                 p("Effect of varying mean reversion rate while keeping other parameters constant.")),
        
        tabPanel("Compare σ",
                 plotOutput("sigma_compare", height = "600px"),
                 p("Effect of varying volatility while keeping other parameters constant.")),
        
        tabPanel("Histogram",
                 plotOutput("histogram", height = "600px"),
                 p("Distribution of values from last 50% of simulation time (approaching stationary distribution)."))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values to store trajectories
  trajectories <- reactiveVal(NULL)
  
  # Simulate trajectories when button is clicked
  observeEvent(input$simulate, {
    set.seed(input$seed)
    
    traj_list <- lapply(1:input$n_traj, function(i) {
      df <- simulate_OU(input$theta, input$mu, input$sigma, 
                        input$x0, input$T, input$dt)
      df$trajectory <- paste0("Path_", i)
      df
    })
    
    trajectories(do.call(rbind, traj_list))
  })
  
  # Initialize trajectories on startup
  observe({
    if (is.null(trajectories())) {
      set.seed(input$seed)
      
      traj_list <- lapply(1:input$n_traj, function(i) {
        df <- simulate_OU(input$theta, input$mu, input$sigma, 
                          input$x0, input$T, input$dt)
        df$trajectory <- paste0("Path_", i)
        df
      })
      
      trajectories(do.call(rbind, traj_list))
    }
  })
  
  # Plot trajectories
  output$traj_plot <- renderPlot({
    req(trajectories())
    
    ggplot(trajectories(), aes(x = time, y = x, color = trajectory)) +
      geom_line(linewidth = 1) +
      geom_hline(yintercept = input$mu, linetype = "dashed", 
                 color = "red", linewidth = 1) +
      annotate("text", x = input$T * 0.9, y = input$mu, 
               label = "Long-term mean (μ)", vjust = -0.5, color = "red") +
      labs(title = "Ornstein-Uhlenbeck Process Trajectories",
           x = "Time", y = "X(t)", color = "Trajectory") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "right")
  })
  
  # Compare theta values
  output$theta_compare <- renderPlot({
    set.seed(input$seed)
    thetas <- c(0.2, 0.7, 2.0)
    
    traj_list <- lapply(seq_along(thetas), function(i) {
      df <- simulate_OU(theta = thetas[i], mu = input$mu, sigma = input$sigma,
                        x0 = input$x0, T = input$T, dt = input$dt)
      df$parameter <- paste0("θ = ", thetas[i])
      df
    })
    
    data <- do.call(rbind, traj_list)
    
    ggplot(data, aes(x = time, y = x, color = parameter)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = input$mu, linetype = "dashed", 
                 color = "red", linewidth = 0.8) +
      labs(title = "Effect of Mean Reversion Rate (θ)",
           subtitle = "Small θ → slow reversion; Large θ → fast reversion",
           x = "Time", y = "X(t)", color = "Parameter") +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("darkorange", "steelblue", "darkgreen"))
  })
  
  # Compare sigma values
  output$sigma_compare <- renderPlot({
    set.seed(input$seed)
    sigmas <- c(0.1, 0.3, 0.6)
    
    traj_list <- lapply(seq_along(sigmas), function(i) {
      df <- simulate_OU(theta = input$theta, mu = input$mu, sigma = sigmas[i],
                        x0 = input$x0, T = input$T, dt = input$dt)
      df$parameter <- paste0("σ = ", sigmas[i])
      df
    })
    
    data <- do.call(rbind, traj_list)
    
    ggplot(data, aes(x = time, y = x, color = parameter)) +
      geom_line(linewidth = 1.2) +
      geom_hline(yintercept = input$mu, linetype = "dashed", 
                 color = "red", linewidth = 0.8) +
      labs(title = "Effect of Volatility (σ)",
           subtitle = "Higher σ → more noise; Lower σ → smoother path",
           x = "Time", y = "X(t)", color = "Parameter") +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("purple", "skyblue", "darkred"))
  })
  
  # Histogram of values
  output$histogram <- renderPlot({
    req(trajectories())
    
    # Take last 50% of data (approaching stationary distribution)
    cutoff_time <- input$T * 0.5
    data_subset <- trajectories()[trajectories()$time >= cutoff_time, ]
    
    # Theoretical stationary distribution
    stat_mean <- input$mu
    stat_sd <- sqrt(input$sigma^2 / (2 * input$theta))
    
    ggplot(data_subset, aes(x = x)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50, 
                     fill = "steelblue", alpha = 0.7, color = "black") +
      stat_function(fun = dnorm, args = list(mean = stat_mean, sd = stat_sd),
                    color = "red", linewidth = 1.5, linetype = "dashed") +
      annotate("text", x = Inf, y = Inf, 
               label = paste0("Theoretical N(", round(stat_mean, 2), ", ", 
                              round(stat_sd^2, 3), ")"),
               hjust = 1.1, vjust = 2, color = "red", size = 5) +
      labs(title = "Distribution of X(t) (Last 50% of simulation)",
           subtitle = "Red dashed line shows theoretical stationary distribution",
           x = "X(t)", y = "Density") +
      theme_minimal(base_size = 14)
  })
  
  # Stationary distribution info
  output$stat_dist <- renderUI({
    stat_mean <- input$mu
    stat_var <- input$sigma^2 / (2 * input$theta)
    stat_sd <- sqrt(stat_var)
    
    HTML(paste0(
      "<p><strong>Mean:</strong> ", round(stat_mean, 3), "</p>",
      "<p><strong>Variance:</strong> ", round(stat_var, 4), "</p>",
      "<p><strong>Std Dev:</strong> ", round(stat_sd, 3), "</p>",
      "<p style='font-size: 11px; color: gray;'>",
      "X<sub>∞</sub> ~ N(μ, σ²/2θ)</p>"
    ))
  })
}

# Run app
shinyApp(ui = ui, server = server)