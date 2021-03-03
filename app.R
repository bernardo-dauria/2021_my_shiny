#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

list_choices <-  unique(msleep$vore)
list_choices <- list_choices[!is.na(list_choices)]
names(list_choices) <- paste0(list_choices,"vore")

# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
    tabPanel("msleep",
    fluidPage(
    sidebarLayout(sidebarPanel(
        selectInput("select", label = h3("Plot by type of alimentation"), 
                    choices = list_choices,
                    selected = 1)
    ), mainPanel(
        h3("Plots"),
        plotOutput(outputId = "hello")
    )
))),
tabPanel("Random generator",
         sidebarLayout(position = "right",
        sidebarPanel(
            selectInput("dist", label = h3("Select the distribution"), 
                        choices = list(Normal="rnorm", Uniform="runif", Exponential="rexp"),
                        selected = 1),
            sliderInput("n_sample", label = h3("Number of samples"), min = 10, 
                        max = 100, value = 20),
            sliderInput("n_bins", label = h3("Number of bins"), min = 1, 
                        max = 50, value = 30)
        ),
        mainPanel(
            plotOutput(outputId = "pulpo")
        )) # sidebarLayout
         ),
tabPanel("References",
         p(tags$button(class="btn btn-default", 
                       `data-toggle`="collapse", 
                       `data-target`="#hola",
                       "References")),
         div(class="collpase", id="hola",
             div(class="card card-body",
                 includeMarkdown("references.md")
             ))
) #  titlePanel
) # navbarPage

col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$hello <- renderPlot({
        ggplot(msleep %>% filter(vore == input$select)
               , aes(bodywt, sleep_total, colour = vore)) +
            scale_x_log10() +
            col_scale +
            geom_point()
    })
    
    cmd = reactive(eval(parse(text=paste0(input$dist,"(",input$n_sample,")"))));
    output$pulpo <- renderPlot(hist(cmd()))
}

# Run the application 
shinyApp(ui = ui, server = server)
