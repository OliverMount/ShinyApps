library(shiny)
library(ggplot2)
library(dplyr)

# Helper: evaluate vector field
vector_field <- function(x, y, field, a = 1, b = 1, swirl_centers = NULL) {
  if (field == "four_swirls" && !is.null(swirl_centers)) {
    P <- rep(0, length(x))
    Q <- rep(0, length(x))

    for (i in 1:4) {
      cx <- swirl_centers[i, 1]
      cy <- swirl_centers[i, 2]
      dx <- x - cx
      dy <- y - cy
      r_sq <- dx^2 + dy^2 + 0.1

      strength <- exp(-r_sq / 2)
      P <- P - dy * strength
      Q <- Q + dx * strength
    }

    return(list(P = P, Q = Q))
  }

  switch(field,
         "rotational" = list(P = -y, Q = x),
         "radial" = list(P = x, Q = y),
         "sourcesink" = list(P = a * x, Q = b * y),
         "custom" = list(P = a * x - b * y, Q = b * x + a * y),
         list(P = -y, Q = x)
  )
}

# Compute flux (outward flow) through boundary using normal vector
# For a curve parameterized as (x(t), y(t)), outward normal is (dy/dt, -dx/dt)
compute_flux_integral <- function(xfun, yfun, n, field, a = 1, b = 1, swirl_centers = NULL) {
  t <- seq(0, 1, length.out = n + 1)
  x <- xfun(t)
  y <- yfun(t)

  dx <- diff(x)
  dy <- diff(y)

  # Midpoint evaluation
  x_mid <- (head(x, -1) + tail(x, -1)) / 2
  y_mid <- (head(y, -1) + tail(y, -1)) / 2

  fv <- vector_field(x_mid, y_mid, field, a, b, swirl_centers)

  # Flux = F · n = F · (dy/dt, -dx/dt) = P*dy - Q*dx
  sum(fv$P * dy - fv$Q * dx)
}

# Compute area integral of divergence
compute_divergence_area_integral <- function(xlim, ylim, nx, ny, region_fn, field, a = 1, b = 1, swirl_centers = NULL) {
  xs <- seq(xlim[1], xlim[2], length.out = nx)
  ys <- seq(ylim[1], ylim[2], length.out = ny)
  dx <- xs[2] - xs[1]
  dy <- ys[2] - ys[1]

  grid <- expand.grid(x = xs, y = ys)
  fv <- vector_field(grid$x, grid$y, field, a, b, swirl_centers)

  Pmat <- matrix(fv$P, nrow = nx, ncol = ny)
  Qmat <- matrix(fv$Q, nrow = nx, ncol = ny)

  # Compute derivatives: div = dP/dx + dQ/dy
  dPdx <- matrix(0, nrow = nx, ncol = ny)
  dQdy <- matrix(0, nrow = nx, ncol = ny)

  # Central differences
  for (i in 2:(nx - 1)) {
    dPdx[i, ] <- (Pmat[i + 1, ] - Pmat[i - 1, ]) / (2 * dx)
  }
  for (j in 2:(ny - 1)) {
    dQdy[, j] <- (Qmat[, j + 1] - Qmat[, j - 1]) / (2 * dy)
  }

  # Boundary handling
  dPdx[1, ] <- (Pmat[2, ] - Pmat[1, ]) / dx
  dPdx[nx, ] <- (Pmat[nx, ] - Pmat[nx - 1, ]) / dx
  dQdy[, 1] <- (Qmat[, 2] - Qmat[, 1]) / dy
  dQdy[, ny] <- (Qmat[, ny] - Qmat[, ny - 1]) / dy

  divmat <- dPdx + dQdy

  # Apply region mask
  inside <- mapply(region_fn, grid$x, grid$y)
  div_inside <- as.vector(divmat)[inside]

  sum(div_inside) * dx * dy
}

# Contour factories
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
      background: linear-gradient(135deg, #27ae60 0%, #16a085 100%);
      color: white;
      padding: 25px;
      margin-bottom: 20px;
      border-radius: 8px;
    }
    .info-box {
      background: #f8f9fa;
      border-left: 4px solid #27ae60;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
    }
    .result-box {
      background: #e8f8f5;
      border: 2px solid #27ae60;
      padding: 20px;
      margin: 15px 0;
      border-radius: 8px;
      font-family: 'Courier New', monospace;
    }
  "))),

  div(class = "main-header",
      h1("Divergence Theorem Visualization -- Oliver James"),
      h3("Explore the relationship between flux integrals and divergence")
  ),

  sidebarLayout(
    sidebarPanel(width = 4,
                 div(class = "info-box",
                     h4("📐 Divergence Theorem"),
                     p("∮ 𝑭 · 𝒏 dS = ∬ (∇ · 𝑭) dA"),
                     p(style = "font-size: 12px; color: #666;",
                       "The flux (outward flow) through a closed curve equals the double integral of divergence over the enclosed region.")
                 ),

                 h4("1. Choose Vector Field F = (P, Q)"),
                 selectInput('field', NULL,
                             choices = c(
                               "Rotational: (-y, x)" = "rotational",
                               "Radial: (x, y)" = "radial",
                               "Source/Sink: (ax, by)" = "sourcesink",
                               "Custom: (ax - by, bx + ay)" = "custom",
                               "Four Swirls: Multiple vortices" = "four_swirls"
                             )
                 ),

                 conditionalPanel(
                   "input.field == 'sourcesink' || input.field == 'custom'",
                   sliderInput('a', 'Parameter a', min = -3, max = 3, value = 1, step = 0.1),
                   sliderInput('b', 'Parameter b', min = -3, max = 3, value = 1, step = 0.1)
                 ),

                 conditionalPanel(
                   "input.field == 'four_swirls'",
                   h5("Swirl Center Coordinates:"),
                   fluidRow(
                     column(6, numericInput('x1', 'Swirl 1 - x:', value = -1.5, step = 0.1)),
                     column(6, numericInput('y1', 'Swirl 1 - y:', value = 1.5, step = 0.1))
                   ),
                   fluidRow(
                     column(6, numericInput('x2', 'Swirl 2 - x:', value = 1.5, step = 0.1)),
                     column(6, numericInput('y2', 'Swirl 2 - y:', value = 1.5, step = 0.1))
                   ),
                   fluidRow(
                     column(6, numericInput('x3', 'Swirl 3 - x:', value = -1.5, step = 0.1)),
                     column(6, numericInput('y3', 'Swirl 3 - y:', value = -1.5, step = 0.1))
                   ),
                   fluidRow(
                     column(6, numericInput('x4', 'Swirl 4 - x:', value = 1.5, step = 0.1)),
                     column(6, numericInput('y4', 'Swirl 4 - y:', value = -1.5, step = 0.1))
                   )
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

    # Get swirl centers if applicable
    swirl_centers <- NULL
    if (input$field == "four_swirls") {
      swirl_centers <- matrix(c(
        input$x1, input$y1,
        input$x2, input$y2,
        input$x3, input$y3,
        input$x4, input$y4
      ), ncol = 2, byrow = TRUE)
    }

    # Create contour
    contour <- make_contour(input$contour, params)
    region_fn <- region_fn_factory(input$contour, params)

    # Compute flux integral (surface integral)
    flux_int <- compute_flux_integral(contour$xfun, contour$yfun,
                                      input$res, input$field, a_val, b_val, swirl_centers)

    # Determine domain
    domain <- switch(input$contour,
                     "Circle" = input$r * 1.5,
                     "Ellipse" = max(input$a_e, input$b_e) * 1.5,
                     "Rectangle" = max(input$w, input$h) * 0.75,
                     "Square" = input$ws * 0.75
    )

    xlim <- c(-domain, domain)
    ylim <- c(-domain, domain)

    # Compute divergence integral
    nx <- input$grid * 3 + 1
    ny <- input$grid * 3 + 1
    div_int <- compute_divergence_area_integral(xlim, ylim, nx, ny,
                                                region_fn, input$field, a_val, b_val, swirl_centers)

    list(
      flux_int = flux_int,
      div_int = div_int,
      contour = contour,
      xlim = xlim,
      ylim = ylim,
      a = a_val,
      b = b_val,
      swirl_centers = swirl_centers
    )
  })

  output$fieldPlot <- renderPlot({
    res <- results()

    # Vector field grid
    ngr <- input$grid
    xs <- seq(res$xlim[1], res$xlim[2], length.out = ngr)
    ys <- seq(res$ylim[1], res$ylim[2], length.out = ngr)
    grid <- expand.grid(x = xs, y = ys)

    fv <- vector_field(grid$x, grid$y, input$field, res$a, res$b, res$swirl_centers)
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

    # Compute outward normals for visualization
    dt <- 1/500
    n_display <- 16
    t_normals <- seq(0, 1 - dt, length.out = n_display)

    x_norm <- res$contour$xfun(t_normals)
    y_norm <- res$contour$yfun(t_normals)
    x_norm_next <- res$contour$xfun(t_normals + dt)
    y_norm_next <- res$contour$yfun(t_normals + dt)

    dx_dt <- (x_norm_next - x_norm) / dt
    dy_dt <- (y_norm_next - y_norm) / dt

    # Outward normal is (dy/dt, -dx/dt), normalized
    norm_mag <- sqrt(dy_dt^2 + dx_dt^2)
    nx <- dy_dt / norm_mag * 0.3
    ny <- -dx_dt / norm_mag * 0.3

    normal_df <- data.frame(
      x = x_norm,
      y = y_norm,
      xend = x_norm + nx,
      yend = y_norm + ny
    ) 
    p <- ggplot() +
      geom_segment(data = df,
                   aes(x = x, y = y, xend = x + u, yend = y + v),
                   arrow = arrow(length = unit(0.15, 'cm'), type = "closed"),
                   color = "#555555", alpha = 0.6, linewidth = 0.4) +
      geom_path(data = pathdf, aes(x = x, y = y),
                color = '#27ae60', linewidth = 1.5) +
      geom_polygon(data = pathdf, aes(x = x, y = y),
                   fill = '#27ae60', alpha = 0.1) +
      geom_segment(data = normal_df,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   arrow = arrow(length = unit(0.2, 'cm'), type = "closed"),
                   color = '#e67e22', linewidth = 1.2, alpha = 0.8) +
      coord_fixed(xlim = res$xlim, ylim = res$ylim) +
      labs(title = "Vector Field F, Boundary (green), and Outward Normals (orange)",
           x = "x", y = "y") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70")
      )

    # Add swirl centers if applicable
    if (!is.null(res$swirl_centers)) {
      swirl_df <- data.frame(
        x = res$swirl_centers[, 1],
        y = res$swirl_centers[, 2]
      )
      p <- p + geom_point(data = swirl_df, aes(x = x, y = y),
                          color = '#2c3e50', size = 4, shape = 16) +
        geom_point(data = swirl_df, aes(x = x, y = y),
                   color = '#3498db', size = 2, shape = 16)
    }

    p
  })

  output$results <- renderText({
    res <- results()

    diff <- res$flux_int - res$div_int
    rel_error <- abs(diff) / (abs(res$div_int) + 1e-15) * 100

    sprintf(
      "Flux Integral (∮ 𝑭 · 𝒏 dS):           %18.12f\n\nDivergence Integral (∬ ∇ · 𝑭 dA):   %18.12f\n\n%s\n\nAbsolute Difference:                    %18.12e\nRelative Error:                         %18.8f%%\n\n%s",
      res$flux_int,
      res$div_int,
      paste(rep("─", 65), collapse = ""),
      diff,
      rel_error,
      if (rel_error < 0.1) "✓ Excellent agreement (Divergence Theorem verified!)"
      else if (rel_error < 1) "✓ Good agreement"
      else "⚠ Consider increasing integration precision"
    )
  })
}
shinyApp(ui, server)
