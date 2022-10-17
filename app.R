library(shiny)
library(purrr)
library(dplyr)
library(ggplot2)
library(markdown)

ui <- fluidPage(
    titlePanel("Bootstrap samples vs regular samples"),
    
    sidebarLayout(
      ############
      # Settings #
      ############
      sidebarPanel(
        selectInput("distribution",
                    "Select a population distribution:",
                    choices = c("normal", "uniform")
        ),
        helpText("Multiple samples will be taken from this distribution."),
        uiOutput("normal.sections"),
        uiOutput("uniform.sections"),
        
        hr(),
        
        selectInput("statistic",
                    "Select a statistic to be measured",
                    choices = c("mean", "sd", "median")
        ),
        helpText("This will be calculated for each sample."),
        
        sliderInput("sample_size",
                    "Sample size:",
                    min = 0,
                    max = 100,
                    value = 30
                    ),
        sliderInput("num_samples",
                    "Select the number of samples to be created:",
                    min = 1,
                    max = 2000,
                    value = 1000
        ),
        helpText("Create n regular samples and n bootstrap samples."),
      ),

      ##############
      # Main panel #
      ##############
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Bootstrap samples's boxplot",
            plotOutput("plotbox"),
            includeMarkdown("Explanation1.md")
          ),
          tabPanel(
            "Bootstrap samples vs regular samples",
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
  group_colors <- c("regular" = "#0daeff",
                    "bootstrap" = "#ff3262",
                    "population" = "black")
  
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
  
  output$plotbox <- renderPlot({
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
    
    
    # Plot selected statistic distribution vs population's parameter
    statistics_df %>%
      rename_with(~ gsub(input$statistic, "target", .x)) %>%
      filter(source == "bootstrap") %>%
      ggplot(aes(target, color = source, fill = source)) +
      geom_boxplot(alpha = 0.2) +
      geom_vline(aes(xintercept = parameter_dic[[input$distribution]][[input$statistic]],
                     fill = "population", color = "population"),
                 linetype = "dashed") +
      labs(x = input$statistic, title = paste0("Bootstrap samples's ", input$statistic,
                                               "s vs population's ", input$statistic)) +
      scale_fill_manual(values = group_colors[2:3]) +
      scale_color_manual(values = group_colors[2:3])
    
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
    
    
    # Plot selected statistic distribution
    statistics_df %>%
      rename_with(~ gsub(input$statistic, "target", .x)) %>%
      ggplot(aes(target, color = source, fill=source)) +
      geom_histogram(alpha = 0.2, position = "identity") +
      geom_vline(aes(xintercept = parameter_dic[[input$distribution]][[input$statistic]],
                     fill = "population", color = "population"),
                 linetype = "dashed") +
      labs(x = input$statistic, y = "Num of samples",
           title = paste("Sample's", input$statistic,
                         "per source vs popupulation's", input$statistic)) +
      scale_fill_manual(values=group_colors) +
      scale_color_manual(values=group_colors)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
