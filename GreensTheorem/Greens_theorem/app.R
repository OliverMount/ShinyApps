library(shiny)
library(ggplot2)
library(dplyr)

# Helper: evaluate vector field with improved precision
vector_field <- function(x, y, field, a = 1, b = 1) {
  switch(field,
         "rotational" = list(P = -y, Q = x),
         "radial" = list(P = x, Q = y),
         "sourcesink" = list(P = a * x, Q = b * y),
         "custom" = list(P = a * x - b * y, Q = b * x + a * y),
         list(P = -y, Q = x)
  )
}

# Compute circulation along parameterized contour with higher precision
compute_line_integral <- function(xfun, yfun, n, field, a = 1, b = 1) {
  t <- seq(0, 1, length.out = n + 1)
  x <- xfun(t)
  y <- yfun(t)
  
  # Use trapezoidal rule for better accuracy
  dx <- diff(x)
  dy <- diff(y)
  
  # Midpoint evaluation for better precision
  x_mid <- (head(x, -1) + tail(x, -1)) / 2
  y_mid <- (head(y, -1) + tail(y, -1)) / 2
  
  fv <- vector_field(x_mid, y_mid, field, a, b)
  
  sum(fv$P * dx + fv$Q * dy)
}

# Compute area integral of curl with adaptive grid
compute_curl_area_integral <- function(xlim, ylim, nx, ny, region_fn, field, a = 1, b = 1) {
  xs <- seq(xlim[1], xlim[2], length.out = nx)
  ys <- seq(ylim[1], ylim[2], length.out = ny)
  dx <- xs[2] - xs[1]
  dy <- ys[2] - ys[1]
  
  grid <- expand.grid(x = xs, y = ys)
  fv <- vector_field(grid$x, grid$y, field, a, b)
  
  Pmat <- matrix(fv$P, nrow = nx, ncol = ny)
  Qmat <- matrix(fv$Q, nrow = nx, ncol = ny)
  
  # Improved derivatives using central differences
  dQdx <- matrix(0, nrow = nx, ncol = ny)
  dPdy <- matrix(0, nrow = nx, ncol = ny)
  
  for (i in 2:(nx - 1)) {
    dQdx[i, ] <- (Qmat[i + 1, ] - Qmat[i - 1, ]) / (2 * dx)
  }
  for (j in 2:(ny - 1)) {
    dPdy[, j] <- (Pmat[, j + 1] - Pmat[, j - 1]) / (2 * dy)
  }
  
  # Boundary handling
  dQdx[1, ] <- (Qmat[2, ] - Qmat[1, ]) / dx
  dQdx[nx, ] <- (Qmat[nx, ] - Qmat[nx - 1, ]) / dx
  dPdy[, 1] <- (Pmat[, 2] - Pmat[, 1]) / dy
  dPdy[, ny] <- (Pmat[, ny] - Pmat[, ny - 1]) / dy
  
  curlmat <- dQdx - dPdy
  
  # Apply region mask
  inside <- mapply(region_fn, grid$x, grid$y)
  curl_inside <- as.vector(curlmat)[inside]
  
  sum(curl_inside) * dx * dy
}

# Contour factories with smoother parameterization
make_contour <- function(type, params) {
  list(
    xfun = switch(type,
                  "Circle" = function(t) params$r * cos(2 * pi * t),
                  "Ellipse" = function(t) params$a * cos(2 * pi * t),
                  "Rectangle" = function(t) {
                    s <- (t * 4) %% 4
                    ifelse(s < 1, -params$w/2 + params$w * s,
                           ifelse(s < 2, params$w/2,
                                  ifelse(s < 3, params$w/2 - params$w * (s - 2), -params$w/2)))
                  },
                  "Square" = function(t) {
                    s <- (t * 4) %% 4
                    w <- params$w
                    ifelse(s < 1, -w/2 + w * s,
                           ifelse(s < 2, w/2,
                                  ifelse(s < 3, w/2 - w * (s - 2), -w/2)))
                  }
    ),
    yfun = switch(type,
                  "Circle" = function(t) params$r * sin(2 * pi * t),
                  "Ellipse" = function(t) params$b * sin(2 * pi * t),
                  "Rectangle" = function(t) {
                    s <- (t * 4) %% 4
                    ifelse(s < 1, -params$h/2,
                           ifelse(s < 2, -params$h/2 + params$h * (s - 1),
                                  ifelse(s < 3, params$h/2, params$h/2 - params$h * (s - 3))))
                  },
                  "Square" = function(t) {
                    s <- (t * 4) %% 4
                    w <- params$w
                    ifelse(s < 1, -w/2,
                           ifelse(s < 2, -w/2 + w * (s - 1),
                                  ifelse(s < 3, w/2, w/2 - w * (s - 3))))
                  }
    )
  )
}

# Region indicator functions
region_fn_factory <- function(type, params) {
  switch(type,
         "Circle" = function(x, y) (x^2 + y^2) <= (params$r^2 + 1e-10),
         "Ellipse" = function(x, y) ((x/params$a)^2 + (y/params$b)^2) <= (1 + 1e-10),
         "Rectangle" = function(x, y) (abs(x) <= params$w/2 + 1e-10) & (abs(y) <= params$h/2 + 1e-10),
         "Square" = function(x, y) (abs(x) <= params$w/2 + 1e-10) & (abs(y) <= params$w/2 + 1e-10)
  )
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .main-header { 
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 25px;
      margin-bottom: 20px;
      border-radius: 8px;
    }
    .info-box {
      background: #f8f9fa;
      border-left: 4px solid #667eea;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
    }
    .result-box {
      background: #e8f4f8;
      border: 2px solid #2196F3;
      padding: 20px;
      margin: 15px 0;
      border-radius: 8px;
      font-family: 'Courier New', monospace;
    }
  "))),
  
  div(class = "main-header",
      h1("Green's Theorem: Interactive Visualization -- by Oliver James" ),
      p("Explore the relationship between line integrals and double integrals")
  ),
  
  sidebarLayout(
    sidebarPanel(width = 4,
                 div(class = "info-box",
                     h4("📐 Green's Theorem"),
                     p("∮ P dx + Q dy = ∬ (∂Q/∂x - ∂P/∂y) dA"),
                     p(style = "font-size: 12px; color: #666;", 
                       "The line integral around a closed curve equals the double integral of curl over the enclosed region.")
                 ),
                 
                 h4("1. Choose Vector Field F = (P, Q)"),
                 selectInput('field', NULL, 
                             choices = c(
                               "Rotational: (-y, x)" = "rotational",
                               "Radial: (x, y)" = "radial",
                               "Source/Sink: (ax, by)" = "sourcesink",
                               "Custom: (ax - by, bx + ay)" = "custom"
                             )
                 ),
                 
                 conditionalPanel(
                   "input.field == 'sourcesink' || input.field == 'custom'",
                   sliderInput('a', 'Parameter a', min = -3, max = 3, value = 1, step = 0.1),
                   sliderInput('b', 'Parameter b', min = -3, max = 3, value = 1, step = 0.1)
                 ),
                 
                 hr(),
                 h4("2. Choose Contour Shape"),
                 selectInput('contour', NULL,
                             choices = c("Circle", "Ellipse", "Rectangle", "Square")
                 ),
                 
                 conditionalPanel("input.contour == 'Circle'",
                                  sliderInput('r', 'Radius', min = 0.5, max = 3, value = 1.5, step = 0.1)
                 ),
                 conditionalPanel("input.contour == 'Ellipse'",
                                  sliderInput('a_e', 'Semi-major axis (a)', min = 0.5, max = 3, value = 1.8, step = 0.1),
                                  sliderInput('b_e', 'Semi-minor axis (b)', min = 0.5, max = 3, value = 1.2, step = 0.1)
                 ),
                 conditionalPanel("input.contour == 'Rectangle'",
                                  sliderInput('w', 'Width', min = 0.5, max = 4, value = 2.5, step = 0.1),
                                  sliderInput('h', 'Height', min = 0.5, max = 4, value = 1.8, step = 0.1)
                 ),
                 conditionalPanel("input.contour == 'Square'",
                                  sliderInput('ws', 'Side length', min = 0.5, max = 4, value = 2.2, step = 0.1)
                 ),
                 
                 hr(),
                 h4("3. Numerical Settings"),
                 sliderInput('grid', 'Vector field density', min = 10, max = 30, value = 18, step = 1),
                 sliderInput('res', 'Integration precision', min = 500, max = 5000, value = 2000, step = 500,
                             post = " points"),
                 
                 actionButton('compute', 'Calculate Integrals', 
                              class = "btn-primary btn-lg btn-block",
                              style = "margin-top: 20px;")
    ),
    
    mainPanel(width = 8,
              plotOutput('fieldPlot', height = '550px'),
              div(class = "result-box",
                  h4("📊 Computational Results"),
                  verbatimTextOutput('results')
              )
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$compute, {
    # Build contour parameters
    params <- switch(input$contour,
                     "Circle" = list(r = input$r),
                     "Ellipse" = list(a = input$a_e, b = input$b_e),
                     "Rectangle" = list(w = input$w, h = input$h),
                     "Square" = list(w = input$ws)
    )
    
    # Get field parameters
    a_val <- if (input$field %in% c("sourcesink", "custom")) input$a else 1
    b_val <- if (input$field %in% c("sourcesink", "custom")) input$b else 1
    
    # Create contour
    contour <- make_contour(input$contour, params)
    region_fn <- region_fn_factory(input$contour, params)
    
    # Compute line integral
    line_int <- compute_line_integral(contour$xfun, contour$yfun, 
                                      input$res, input$field, a_val, b_val)
    
    # Determine domain
    domain <- switch(input$contour,
                     "Circle" = input$r * 1.5,
                     "Ellipse" = max(input$a_e, input$b_e) * 1.5,
                     "Rectangle" = max(input$w, input$h) * 0.75,
                     "Square" = input$ws * 0.75
    )
    
    xlim <- c(-domain, domain)
    ylim <- c(-domain, domain)
    
    # Compute area integral with higher resolution
    nx <- input$grid * 3 + 1
    ny <- input$grid * 3 + 1
    area_int <- compute_curl_area_integral(xlim, ylim, nx, ny, 
                                           region_fn, input$field, a_val, b_val)
    
    list(
      line_int = line_int,
      area_int = area_int,
      contour = contour,
      xlim = xlim,
      ylim = ylim,
      a = a_val,
      b = b_val
    )
  })
  
  output$fieldPlot <- renderPlot({
    res <- results()
    
    # Vector field grid
    ngr <- input$grid
    xs <- seq(res$xlim[1], res$xlim[2], length.out = ngr)
    ys <- seq(res$ylim[1], res$ylim[2], length.out = ngr)
    grid <- expand.grid(x = xs, y = ys)
    
    fv <- vector_field(grid$x, grid$y, input$field, res$a, res$b)
    df <- data.frame(x = grid$x, y = grid$y, P = fv$P, Q = fv$Q)
    
    # Normalize for display
    mag <- sqrt(df$P^2 + df$Q^2)
    mag[mag == 0] <- 1
    scale <- diff(range(xs)) / 20
    df$u <- df$P / mag * scale
    df$v <- df$Q / mag * scale
    
    # Contour path
    t <- seq(0, 1, length.out = 500)
    cx <- res$contour$xfun(t)
    cy <- res$contour$yfun(t)
    pathdf <- data.frame(x = cx, y = cy)
    
    ggplot() +
      geom_segment(data = df, 
                   aes(x = x, y = y, xend = x + u, yend = y + v),
                   arrow = arrow(length = unit(0.15, 'cm'), type = "closed"),
                   color = "#555555", alpha = 0.6, linewidth = 0.4) +
      geom_path(data = pathdf, aes(x = x, y = y), 
                color = '#e74c3c', linewidth = 1.5) +
      geom_polygon(data = pathdf, aes(x = x, y = y),
                   fill = '#e74c3c', alpha = 0.1) +
      coord_fixed(xlim = res$xlim, ylim = res$ylim) +
      labs(title = "Vector Field F and Integration Contour (red)",
           x = "x", y = "y") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70")
      )
  })
  
  output$results <- renderText({
    res <- results()
    
    diff <- res$line_int - res$area_int
    rel_error <- abs(diff) / (abs(res$area_int) + 1e-15) * 100
    
    sprintf(
      "Line Integral (∮ P dx + Q dy):          %18.12f\n\nArea Integral (∬ curl F dA):            %18.12f\n\n%s\n\nAbsolute Difference:                    %18.12e\nRelative Error:                         %18.8f%%\n\n%s",
      res$line_int,
      res$area_int,
      paste(rep("─", 65), collapse = ""),
      diff,
      rel_error,
      if (rel_error < 0.1) "✓ Excellent agreement (Green's Theorem verified!)" 
      else if (rel_error < 1) "✓ Good agreement" 
      else "⚠ Consider increasing integration precision"
    )
  })
}

shinyApp(ui, server)