function(input, output, session) {
  otel::start_shiny_session(session)
  otel::counter_add("sessions")
  otel::log("Start new Shiny session")

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    otel::local_active_span(session$userData$otel_span)
    otel::start_span(
      "data",
      attributes = list(columns = c(input$xcol, input$ycol))
    )
    otel::counter_add("data-changes")
    otel::log(
      "Select new data {input$xcol} & {input$ycol}",
      "info"
    )
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    otel::local_active_span(session$userData$otel_span)
    otel::start_span(
      "kmeans",
      attributes = list(clusters = input$clusters)
    )
    otel::counter_add("kmeans-runs")
    otel::log("Run kmeans with {input$clusters} clusters")
    Sys.sleep(1)
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    otel::local_active_span(session$userData$otel_span)
    otel::start_span("plot")
    otel::counter_add("plots")
    otel::log("Plot")
    palette(c(
      "#E41A1C",
      "#377EB8",
      "#4DAF4A",
      "#984EA3",
      "#FF7F00",
      "#FFFF33",
      "#A65628",
      "#F781BF",
      "#999999"
    ))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(), col = clusters()$cluster, pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}
