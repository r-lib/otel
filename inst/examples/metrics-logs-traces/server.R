function(input, output, session) {
  otel_start_shiny_session(session)
  otel_logger$log("Start new Shiny session")
  otel_cnt_session$add()

  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    otel::local_active_span(session$userData$otel_span)
    otel_tracer$start_as_active_span(
      "data",
      attributes = list(columns = c(input$xcol, input$ycol))
    )
    otel_logger$log(
      "Select new data {input$xcol} & {input$ycol}",
      "info"
    )
    olel_cnt_data_changes$add("data-changes")
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    otel::local_active_span(session$userData$otel_span)
    otel_tracer$start_as_active_span(
      "kmeans",
      attributes = list(clusters = input$clusters)
    )
    otel_logger$log("Run kmeans with {input$clusters} clusters")
    otel_cnt_kmeans_runs$add()
    Sys.sleep(1)
    kmeans(selectedData(), input$clusters)
  })

  output$plot1 <- renderPlot({
    otel::local_active_span(session$userData$otel_span)
    otel_tracer$start_as_active_span("plot")
    otel_logger$log("Plot")
    otel_cnt_plots$add("plots")
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
