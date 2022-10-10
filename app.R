library(shiny)
library(purrr)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Bootstrap samples vs regular samples"),

    sidebarLayout(
          sidebarPanel(
            sliderInput("sample_size",
                        "Sample size:",
                        min = 1,
                        max = 100,
                        value = 30
                        ),
            helpText("Select the size for each of the samples to be taken."),
            sliderInput("num_samples",
                        "Select the number of samples to be created:",
                        min = 1,
                        max = 2000,
                        value = 30
                       ),
            selectInput("statistic",
                        "Select the statistic to be measured",
                        choices = c("mean", "sd", "median")
                        ),
            selectInput("distribution",
                        "Select population distribution:",
                        choices = c("normal", "uniform")
                        ),
            uiOutput("normal.sections"),
            uiOutput("uniform.sections"),
            # hr(),
            # titlePanel("Testing:"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 10,
                        max = 50,
                        value = 20
            )
        ),

        mainPanel(
           plotOutput("distPlot")
        )
    )
)

server <- function(input, output) {
    get_sample <- function(){
      if (input$distribution == "normal") {
        new_sample <- rnorm(input$sample_size, input$mean, input$sd)
      } else if (input$distribution == "uniform") {
        new_sample <- runif(input$sample_size, input$min, input$max)
      }
      return(new_sample)
    }


    output$distPlot <- renderPlot({
        # Generate regular samples
        regular_samples <- data.frame(matrix(ncol = 1, nrow = input$sample_size))
        
        for (i in 1:input$num_samples) {
          regular_samples[[i]] <- get_sample()
        }
        
        #  Generate bootstrap samples
        bootstrap_samples <- data.frame(matrix(ncol = 1, nrow = input$sample_size))
        
        for (i in 1:input$num_samples) {
          bootstrap_samples[[i]] <- sample(regular_samples[[1]], input$sample_size,
                                           replace = TRUE)
        }
        
        names(regular_samples) <- NULL
        regular_statistics_df <- data.frame(mean = map_dbl(regular_samples, mean),
                                               sd= map_dbl(regular_samples, sd),
                                               median= map_dbl(regular_samples, median)
        )
        
        names(bootstrap_samples) <- NULL
        bootstrap_statistics_df <- data.frame(mean = map_dbl(bootstrap_samples, mean),
                                              sd = map_dbl(bootstrap_samples, sd),
                                              median = map_dbl(bootstrap_samples, median)
        )
        
        statistics_df <- bind_rows("regular" = regular_statistics_df,
                                   "bootstrap" = bootstrap_statistics_df, .id = "type")
        
        statistics_df %>%
          rename_with(~ gsub(input$statistic, "target", .x)) %>%
          ggplot(aes(target, color = type, fill=type)) +
          geom_histogram(alpha = 0.2, position = "identity", bins = input$bins) +
          labs(x=input$statistic)
        
    })
    output$normal.sections <- renderUI(if (input$distribution == "normal") {
      tagList(sliderInput(
          inputId = "mean",
          label = "Population mean",
          min = -10,
          max = 10,
          value = 0
        ),
        sliderInput(
          inputId = "sd",
          label = "Population SD",
          min = 1,
          max = 4,
          value = 1
        )
      )
    } else if (input$distribution == "uniform") {
       tagList(
         sliderInput(
           inputId = "min",
           label = "min",
           min = -10,
           max = 10,
           value = 0
         ),
         sliderInput(
           inputId = "max",
           label = "max",
           min = -10,
           max = 10,
           value = 1
         )
       )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
