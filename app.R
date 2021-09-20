#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(ggh4x)
library(bslib)
library(thematic)
library(truncnorm)


proportion_round <- function(p, digits = 2) {
    p1 <- round(p, digits)
    lower_limit <- 0.95 * 10 ^ (-1 * digits)
    upper_limit <- 1 - lower_limit
    p1[p > upper_limit & p <= 1] <- 1 - signif(1 - p[p > upper_limit & p <= 1], digits - 1)
    p1[p < lower_limit & p >= 0] <- signif(p[p < lower_limit & p >= 0], digits - 0)
    p1
}


shinyOptions(plot.autocolor = TRUE)
thematic_shiny(font = "auto")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(
        version = 4,
        bootswatch = "simplex",
        primary = "#A3203699",
        secondary = "#9DA0A5",
        base_font = font_google("Roboto"),
        heading_font = font_google("Faustina"),
        font_scale = NULL,
        `enable-gradients` = TRUE,
        `enable-shadows` = TRUE
    ), 
    useShinyjs(),
    # Application title
    titlePanel("Area Under the Normal Curve"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            splitLayout(
                numericInput("mu",
                             div("Mean (", em(
                                 HTML("&mu;"), .noWS = "outside"
                             ), "):"),
                             value = 100),
                numericInput(
                    "sigma",
                    div("SD (", em(HTML("&sigma;"), .noWS = "outside"), "):"),
                    value = 15,
                    min = 0
                )
            ),
            radioButtons("radioProportions", 
                         "", 
                         choices = c("Less", 
                                     "Greater", 
                                     "Between"),
                         inline = T),
            tabsetPanel
            (
                type = "pills",
                id = "whichpanel",
                tabPanel(
                    "Proportions",
                    value = "proportions",
                        div(id = "divlb", 
                            splitLayout(numericInput("lb",
                                                     "Lower Bound",
                                                     value = 100, 
                                                     step = 0.2),
                                        textOutput("lbp", inline = T))),
                        div(id = "divub", numericInput("ub",
                                     "Upper Bound",
                                     value = 100, step = 0.2)
                    )
                ),
                tabPanel(
                    "Quantiles",
                    value = "quantiles",

                        div(id = "divlq", numericInput(
                            "lq",
                            "Lower Proportion:",
                            value = NA,
                            min = 0,
                            max = min(1, "output.lp", na.rm = T),
                            step = 0.01
                        )),
                    div(id = "divuq", numericInput(
                            "uq",
                            "Upper Proportion:",
                            value = 0.75,
                            max = 1,
                            min = 0,
                            step = 0.01
                        )),
                    splitLayout(p(
                        strong("Lower Quantile: "),
                        textOutput(outputId = "lb")
                    ),
                    p(
                        strong("Lower Quantile: "),
                        textOutput(outputId = "ub")
                    ))
                )
                
            ),
            p(
                "by ",
                a("W. Joel Schneider",
                  href = "https://github.com/wjschne")
            )
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    # output$whichtab <- renderText(input$whichpanel)
    

                     
    
    observe({
        if (input$radioProportions == "Between") {
            shinyjs::show("divlb", anim = T)
            shinyjs::show("divub", anim = T)
            shinyjs::show("divlq", anim = T)
            shinyjs::show("divuq", anim = T)
        } else {

            if (input$radioProportions == "Less") {
                shinyjs::show("divub")
                shinyjs::hide("divlb")
                shinyjs::show("divuq")
                shinyjs::hide("divlq")
            } else {
                shinyjs::hide("divub")
                shinyjs::show("divlb")
                shinyjs::hide("divuq")
                shinyjs::show("divlq")
            }
        }
    })
    
    reactive_lb <- reactive({
        
        
        
        if (input$radioProportions == "Less") {
            -Inf
            
        } else {
            ifelse(
                input$whichpanel == "proportions",
                input$lb,
                qnorm(input$lq, input$mu, input$sigma))
            
        }
        
    })
    
    reactive_ub <- reactive({
        
        if (input$radioProportions == "Greater") {
            Inf
            
        } else {
            ifelse(
                input$whichpanel == "proportions",
                input$ub,
                qnorm(input$uq, input$mu, input$sigma)
            )
        }
    })
    
    output$lb <- renderText({
      
        ifelse(is.na(reactive_lb()), "", 
               round(reactive_lb(), 
                     ifelse(input$sigma <= 1, 4, 2)))
    })
    
    output$ub <- renderText({
        ifelse(is.na(reactive_ub()), "", 
        round(reactive_ub(), ifelse(input$sigma <= 1, 4, 2)))
    })
    
    
    output$lbp <- renderText({
      
      ifelse(is.na(reactive_lb()), "", 
             round(reactive_lb(), 
                   ifelse(input$sigma <= 1, 4, 2)))
    })
    

    

    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        lb <- max(reactive_lb(), -Inf, na.rm = T)
        ub <- min(reactive_ub(), Inf, na.rm = T)
        
        

        
        p <- pnorm(max(lb, ub, na.rm = T),
                   input$mu,
                   input$sigma) - ifelse(is.na(min(lb, ub)), 0, pnorm(min(lb, ub),
                                                                      input$mu,
                                                                      input$sigma))
        m_trunc <- ifelse(lb == ub, 
                          lb, 
                          etruncnorm(a = min(lb,ub), 
                                     b = max(lb, ub), 
                                     mean = input$mu,
                                     sd = input$sigma)) 
        
        
        p_trunc <- pnorm(m_trunc,
                         mean = input$mu,
                         sd = input$sigma)
        p_y <- dnorm(m_trunc,
                     mean = input$mu,
                     sd = input$sigma)
        
        if (p_trunc < 0.05 | p_trunc > 0.95) {
          p_y <-  p_y * 0.05
          color_p <- "gray20"
          
        } else {
          p_y <-  p_y * 1.05
          color_p <- "gray95"
        }
        
        
        neg5 <- input$mu - 5 * input$sigma
        pos5 <- input$mu + 5 * input$sigma
        
        ggp <- ggplot(data.frame(x = c(
            input$mu - 5 * input$sigma,
            input$mu + 5 * input$sigma
        )),
        aes(x)) +
            stat_function(
                geom = "area",
                fun = dnorm,
                args = list(mean = input$mu,
                            sd = input$sigma),
                fill = "#9DA0A5",
                alpha = 0.4,
                color = NA,
                n = 801
            ) +
            stat_function(
                geom = "area",
                fun = dnorm,
                xlim = sort(c(
                    max(lb,
                        neg5,
                        na.rm = T),
                    min(ub,
                        pos5,
                        na.rm = T)
                )),
                args = list(mean = input$mu,
                            sd = input$sigma),
                fill = "#A3203699",
                color = NA,
                n = 801
            ) +
            theme_minimal(base_size = 20, base_family = "Roboto") +
            theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.ticks = element_line(size = 0.2, color = "gray"),
                ggh4x.axis.ticks.length.minor = rel(1)) +
            scale_y_continuous(NULL,
                               breaks = NULL,
                               expand = expansion(mult = c(0,.05))) +
            scale_x_continuous(
                NULL,
                guide = "axis_minor",
                expand = expansion(mult = 0.01),
                breaks = seq(neg5,
                             pos5,
                             input$sigma),
                minor_breaks = seq(
                    neg5,
                    pos5,
                    input$sigma / ifelse(input$sigma %% 3 == 0, 3, 2)
                )
            ) +
          coord_cartesian(xlim = c(neg5, pos5))
            # ggtitle(paste0("Shaded Proportion: ", 
            #                format(proportion_round(p), 
            #                       scientific = FALSE))) +
            
        
        d_text <- tibble(x = round(c(lb, ub), 2),
                         y = dnorm(x, 
                                   input$mu, 
                                   input$sigma),
                         label = x,
                         hjust = 0.5 - sign(x - input$mu) / 2) %>% 
            filter(!is.na(x))
        
        
        
        ggp + geom_text(data = d_text, 
                        aes(label = label, 
                            y = y,
                            hjust = hjust), 
                        vjust = -0.1, family = "Roboto") +
          annotate(
            geom = "text",
            x = m_trunc,
            y = 0,
            size = 4,
            family = "Roboto",
            color = "gray10",
            vjust = -0.30,
            lineheight = 0.85,
            label = paste0("Shaded\nProportion\n", 
                           format(proportion_round(p), 
                                  scientific = FALSE))
          )
        
        
    }, res = 100)
}

# Run the application
shinyApp(ui = ui, server = server)
