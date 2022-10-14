library(shiny)
library(purrr)
library(dplyr)
library(ggplot2)
#install.packages("markdown")
library(markdown)

ui <- fluidPage(
    titlePanel("Bootstrap samples vs regular samples"),
    
    sidebarLayout(
      ############
      # Settings #
      ############
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
                    value = 1000
        ),
        helpText("It will create n regular samples and n bootstrap samples."),
        selectInput("statistic",
                    "Select the statistic to be measured",
                    choices = c("mean", "sd", "median")
        ),
        selectInput("distribution",
                    "Select population distribution:",
                    choices = c("normal", "uniform")
        ),
        uiOutput("normal.sections"),
        uiOutput("uniform.sections")
      ),

      ##############
      # Main panel #
      ##############
      mainPanel(
        tabsetPanel(
          tabPanel(
            "CI from bootstrap sample's",
            plotOutput("plotCI"),
            includeMarkdown("Explanation1.md")
          ),
          tabPanel(
            "Regular samples vs Bootstrap samples",
            plotOutput("distPlot"),
            includeMarkdown("Explanation2.md")
          )
        )
      )
    )
)


##########
# Server #
##########
server <- function(input, output) {
  source("data_generation_funcs.R")
  group_colors <- c("regular" = "#0daeff", "bootstrap" = "#ff3262", "population mean" = "black")
  
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
      value = 1,
      step = 0.25
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
  output$plotCI <- renderPlot({
    # Create a dictionary with parameters for each population
    parameter_dic <- list("normal" = c("mean" = input$mean,
                                       "sd" = input$sd,
                                       "median" = input$mean),
                          "uniform" = c("mean" =  (input$max + input$min)/2,
                                        "sd" = sqrt(((input$max - input$min)^2)/12),
                                        "median" = (input$max + input$min)/2))
    
    # Creates defined number of samples from defined distribution and returns
    # a dataframe with statistics from all the created samples.
    statistics_df <- get_statistics_df(input$distribution, input$sample_size,
                                       input$num_samples, input$mean,
                                       input$sd, input$min, input$max)
    
    
    # Plot selected statistic
    statistics_df %>%
      rename_with(~ gsub(input$statistic, "target", .x)) %>%
      filter(type == "bootstrap") %>%
      ggplot(aes(target, color = type, fill = type)) + geom_histogram(alpha = 0.2, position = "identity") +
      geom_vline(aes(xintercept = parameter_dic[[input$distribution]][[input$statistic]],
                     fill = "population mean", color = "population mean"),
                 linetype = "dashed") +
      geom_vline(aes(xintercept = quantile(target, probs=(0.025)),
                                           color = type)) +
      geom_vline(aes(xintercept = quantile(target, probs=(0.975)), 
                                           color = type)) +
      labs(x = input$statistic, y = "Num of samples",
           title = paste0("Bootstrap samples's ", input$statistic,
                          "s vs popupulation's ",
                          input$statistic)) +
      scale_fill_manual(values=group_colors) +
      scale_color_manual(values=group_colors)
    
  })
  output$distPlot <- renderPlot({
    # Create a dictionary with parameters for each population
    parameter_dic <- list("normal" = c("mean" = input$mean,
                                       "sd" = input$sd,
                                       "median" = input$mean),
                          "uniform" = c("mean" =  (input$max + input$min)/2,
                                        "sd" = sqrt(((input$max - input$min)^2)/12),
                                        "median" = (input$max + input$min)/2))
    
    # Creates defined number of samples from defined distribution and returns
    # a dataframe with statistics from all the created samples.
    statistics_df <- get_statistics_df(input$distribution, input$sample_size,
                                       input$num_samples, input$mean,
                                       input$sd, input$min, input$max)
    
    
    # Plot selected statistic
    statistics_df %>%
      rename_with(~ gsub(input$statistic, "target", .x)) %>%
      ggplot(aes(target, color = type, fill=type)) +
      geom_histogram(alpha = 0.2, position = "identity") +
      geom_vline(aes(xintercept = parameter_dic[[input$distribution]][[input$statistic]],
                     fill = "population mean", color = "population mean"),
                 linetype = "dashed") +
      labs(x = input$statistic, y = "Num of samples",
           title = paste("Sample's", input$statistic,
                         "per type of sample vs popupulation's",
                         input$statistic)) +
      scale_fill_manual(values=group_colors) +
      scale_color_manual(values=group_colors)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
