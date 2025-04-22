ui = fluidPage(
      selectInput('farm','Bedrijf:',
                  choices = unique(df$farms_id)),
      sliderInput(
        'lactations',"Laktaties:",
        min = 1,max = max(df$lactation),
        step = 1,
        value = c(1,3)
      ),
      echarts4rOutput("plot")
    )

server = function(input, output) {
      output$plot = renderEcharts4r({
        df |> filter(farms_id==input$farm&
                       ureum>0&
                       lactation>=input$lactations[1]&
                       lactation<=input$lactations[2]) |> 
          group_by(farms_id,date_time,dilcat) |> 
          summarise(ureum = sum(production*ureum,na.rm = T)/sum(production,na.rm = T)) |> 
          group_by(dilcat) |> 
          e_charts(date_time) |> 
          e_line(ureum,symbol ="none") |> 
          e_title(input$lactation) |> 
          e_datazoom(y_index = 0, type = "slider") |> 
          e_datazoom(x_index = c(0,1), type = "slider") 
      })
}

shinyApp(ui = ui, server = server)
