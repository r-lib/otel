function(input, output, session) {

  otel::start_shiny_session(session)

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    otel::start_span("data", session)
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    otel::start_span("kmeans", session)
    Sys.sleep(1)
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    otel::start_span("plot", session)
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

  }
