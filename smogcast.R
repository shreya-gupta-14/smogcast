library(shiny)
library(ggplot2)

aq <- na.omit(airquality)
model2 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = aq)

bands <- data.frame(
  label = c("Good", "Moderate", "Sensitive groups", "Unhealthy"),
  from  = c(0, 55, 71, 86),
  to    = c(55, 71, 86, 170),
  col   = c("#2ECC71", "#F1C40F", "#E67E22", "#E74C3C")
)
band_cols <- setNames(bands$col, bands$label)

aq$band <- cut(aq$Ozone, c(0, 55, 71, 86, Inf), labels = bands$label, right = FALSE)

predict_ozone <- function(temp, wind, solar) {
  exp(predict(model2, newdata = data.frame(Temp = temp, Wind = wind, Solar.R = solar)))
}

get_info <- function(o) {
  i <- findInterval(o, c(55, 71, 86)) + 1
  advice <- c("Air is safe. Enjoy outdoor activities.",
              "Acceptable, but unusually sensitive people should limit long outdoor exertion.",
              "Children, elderly people and people with asthma should reduce outdoor activity.",
              "Everyone should avoid strenuous outdoor activity.")
  list(label = bands$label[i], color = bands$col[i], advice = advice[i])
}

ui <- fluidPage(
  titlePanel("SmogCast: Ozone Pollution Forecaster"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Set the weather"),
      sliderInput("temp", "Temperature (°F)", min = 55, max = 100, value = 80, ticks = FALSE),
      sliderInput("wind", "Wind speed (mph)", min = 2, max = 21, value = 10, ticks = FALSE),
      sliderInput("solar", "Sunlight (langleys)", min = 7, max = 335, value = 180, ticks = FALSE),
      helpText("Move the sliders and the forecast updates instantly.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Forecast", br(),
                 uiOutput("card"),
                 plotOutput("gauge", height = "140px"),
                 fluidRow(
                   column(6, plotOutput("whatif_temp", height = "300px")),
                   column(6, plotOutput("whatif_wind", height = "300px"))
                 )),
        tabPanel("Explore the data", br(),
                 plotOutput("hist", height = "300px"),
                 fluidRow(
                   column(6, plotOutput("sc_temp", height = "300px")),
                   column(6, plotOutput("sc_wind", height = "300px"))
                 )),
        tabPanel("Model accuracy", br(),
                 h4(textOutput("fit_text")),
                 plotOutput("avp", height = "350px"),
                 h4("What each factor does to ozone"),
                 tableOutput("effects"))
      )
    )
  )
)

server <- function(input, output) {
  
  pred <- reactive(predict_ozone(input$temp, input$wind, input$solar))
  
  output$card <- renderUI({
    info <- get_info(pred())
    div(style = paste0("background:", info$color, "; color:white; padding:18px; ",
                       "border-radius:10px; margin-bottom:15px;"),
        h2(paste0("Predicted ozone: ", round(pred(), 1), " ppb"), style = "margin-top:0;"),
        h4(info$label),
        p(info$advice))
  })
  
  output$gauge <- renderPlot({
    p <- min(pred(), 168)
    ggplot() +
      geom_rect(data = bands, aes(xmin = from, xmax = to, ymin = 0, ymax = 1, fill = label),
                alpha = 0.9) +
      scale_fill_manual(values = band_cols, name = NULL) +
      geom_segment(aes(x = p, xend = p, y = -0.1, yend = 1.1), linewidth = 1.5) +
      annotate("label", x = p, y = 1.35, label = paste0("Now: ", round(pred(), 1), " ppb"),
               fontface = "bold", size = 5) +
      coord_cartesian(ylim = c(-0.2, 1.6)) +
      theme_void(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  output$whatif_temp <- renderPlot({
    g <- data.frame(Temp = 55:100)
    g$Ozone <- predict_ozone(g$Temp, input$wind, input$solar)
    ggplot(g, aes(Temp, Ozone)) +
      geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = from, ymax = to, fill = label),
                inherit.aes = FALSE, alpha = 0.25) +
      scale_fill_manual(values = band_cols, name = NULL) +
      geom_line(linewidth = 1.3) +
      annotate("point", x = input$temp, y = pred(), color = "red", size = 5) +
      coord_cartesian(ylim = c(0, 170)) +
      labs(title = "What if it gets hotter?",
           subtitle = "Hotter days mean more ozone",
           x = "Temperature (°F)", y = "Predicted ozone (ppb)") +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
  })
  
  output$whatif_wind <- renderPlot({
    g <- data.frame(Wind = seq(2, 21, 0.5))
    g$Ozone <- predict_ozone(input$temp, g$Wind, input$solar)
    ggplot(g, aes(Wind, Ozone)) +
      geom_rect(data = bands, aes(xmin = -Inf, xmax = Inf, ymin = from, ymax = to, fill = label),
                inherit.aes = FALSE, alpha = 0.25) +
      scale_fill_manual(values = band_cols, name = NULL) +
      geom_line(linewidth = 1.3) +
      annotate("point", x = input$wind, y = pred(), color = "red", size = 5) +
      coord_cartesian(ylim = c(0, 170)) +
      labs(title = "What if it gets windier?",
           subtitle = "Wind blows pollution away",
           x = "Wind speed (mph)", y = "Predicted ozone (ppb)") +
      theme_minimal(base_size = 13) + theme(legend.position = "none")
  })
  
  output$hist <- renderPlot({
    ggplot(aq, aes(Ozone, fill = band)) +
      geom_histogram(binwidth = 10, color = "white") +
      scale_fill_manual(values = band_cols, name = "Air quality") +
      labs(title = "How polluted were the real days?",
           subtitle = "Most days are clean; a few very polluted days stretch the right tail",
           x = "Ozone (ppb)", y = "Number of days") +
      theme_minimal(base_size = 13)
  })
  
  output$sc_temp <- renderPlot({
    ggplot(aq, aes(Temp, Ozone)) +
      geom_point(aes(color = band), size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      scale_color_manual(values = band_cols, guide = "none") +
      labs(title = "Temperature vs ozone",
           subtitle = "Hot days, more smog",
           x = "Temperature (°F)", y = "Ozone (ppb)") +
      theme_minimal(base_size = 13)
  })
  
  output$sc_wind <- renderPlot({
    ggplot(aq, aes(Wind, Ozone)) +
      geom_point(aes(color = band), size = 3, alpha = 0.8) +
      geom_smooth(method = "lm", color = "black", se = FALSE) +
      scale_color_manual(values = band_cols, guide = "none") +
      labs(title = "Wind vs ozone",
           subtitle = "Windy days, cleaner air",
           x = "Wind speed (mph)", y = "Ozone (ppb)") +
      theme_minimal(base_size = 13)
  })
  
  output$fit_text <- renderText({
    r2 <- summary(model2)$adj.r.squared
    paste0("The model explains about ", round(r2 * 100), "% of the day-to-day change in ozone.")
  })
  
  output$avp <- renderPlot({
    d <- data.frame(Actual = aq$Ozone, Predicted = exp(fitted(model2)))
    ggplot(d, aes(Actual, Predicted)) +
      geom_point(color = "steelblue", size = 3, alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(title = "Actual vs predicted ozone",
           subtitle = "Points close to the red line = accurate predictions",
           x = "Actual ozone (ppb)", y = "Predicted ozone (ppb)") +
      theme_minimal(base_size = 13)
  })
  
  output$effects <- renderTable({
    cf <- coef(model2)[-1]
    data.frame(Factor = c("Sunlight (+1 langley)", "Wind (+1 mph)", "Temperature (+1°F)"),
               `Change in ozone` = sprintf("%+.1f%%", (exp(cf) - 1) * 100),
               check.names = FALSE)
  })
}

shinyApp(ui, server)