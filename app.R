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
library(ggtext)
library(bslib)
library(thematic)
library(truncnorm)
library(ragg)
library(sysfonts)
library(showtextdb)
library(showtext)

options(shiny.useragg = TRUE)


proportion_round <- function(p, digits = 2) {
  p1 <- round(p, digits)
  lower_limit <- 0.95 * 10^(-1 * digits)
  upper_limit <- 1 - lower_limit
  p1[p > upper_limit & p <= 1] <- 1 -
    signif(1 - p[p > upper_limit & p <= 1], digits - 1)
  p1[p < lower_limit & p >= 0] <- signif(
    p[p < lower_limit & p >= 0],
    digits - 0
  )
  p1
}

myfont <- "Roboto"


shinyOptions(plot.autocolor = TRUE)
thematic_shiny(font = "auto")


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "simplex",
    primary = "#A3203699",
    secondary = "#9DA0A5",
    base_font = font_google(myfont),
    heading_font = font_google("Faustina"),
    font_scale = NULL,
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE
  ),
  useShinyjs(),

  # Application title
  titlePanel("Statistical Tools"),
  # Area Under Curve Input ----
  tabsetPanel(
    tabPanel(
      title = "Area Under the Normal Curve",
      sidebarLayout(
        sidebarPanel(
          splitLayout(
            numericInput(
              "mu",
              div(
                "Mean (",
                em(
                  HTML("&mu;"),
                  .noWS = "outside"
                ),
                "):"
              ),
              value = 100
            ),
            numericInput(
              "sigma",
              div("SD (", em(HTML("&sigma;"), .noWS = "outside"), "):"),
              value = 15,
              min = 0
            ),
            numericInput(
              "n",
              span("Samples Size (", em("n", .noWS = "outside"), ")"),
              value = 1L,
              min = 1L,
              step = 1L
            ),
            p(
              HTML("SE (&sigma;<sub>e</sub>)"),
              textOutput("sigmae")
            )
          ),
          radioButtons(
            "radioProportions",
            "",
            choices = c("Less", "Greater", "Between"),
            inline = T
          ),
          tabsetPanel(
            type = "pills",
            id = "whichpanel",
            tabPanel(
              "Proportions",
              value = "proportions",
              div(
                id = "divlb",
                splitLayout(
                  numericInput("lb", "Lower Bound", value = 100, step = 0.2),
                  textOutput("lbp", inline = T)
                )
              ),
              div(
                id = "divub",
                numericInput("ub", "Upper Bound", value = 100, step = 0.2)
              )
            ),
            tabPanel(
              "Quantiles",
              value = "quantiles",

              div(
                id = "divlq",
                numericInput(
                  "lq",
                  "Lower Proportion:",
                  value = NA,
                  min = 0,
                  max = min(1, "output.lp", na.rm = T),
                  step = 0.01
                )
              ),
              div(
                id = "divuq",
                numericInput(
                  "uq",
                  "Upper Proportion:",
                  value = 0.75,
                  max = 1,
                  min = 0,
                  step = 0.01
                )
              ),
              splitLayout(
                p(
                  strong("Lower Quantile: "),
                  textOutput(outputId = "lb")
                ),
                p(
                  strong("Lower Quantile: "),
                  textOutput(outputId = "ub")
                )
              )
            )
          ),
          p(
            "by ",
            a("W. Joel Schneider", href = "https://github.com/wjschne")
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
      )
    ),
    # Power Input ----
    tabPanel(
      title = "Z-Test Power",
      sidebarLayout(
        sidebarPanel(
          h4("Null Distribution"),
          splitLayout(
            numericInput(
              "NullMean",
              label = HTML("Mean (<em>&mu;</em><sub>0</sub>)"),
              value = 90,
              step = 1
            ),
            numericInput(
              "NullSD",
              label = HTML("SD (<em>&sigma;</em><sub>0</sub>)"),
              value = 15,
              min = 0,
              step = 1
            ),
            p(
              HTML("SE (&sigma;<sub>e</sub>)"),
              shiny::textOutput(outputId = "NullSE")
            ),
          ),
          h4("Alternative Distribution"),
          splitLayout(
            numericInput(
              "AltMean",
              label = HTML("Mean (<em>&mu;</em><sub>1</sub>)"),
              value = 100,
              step = 1
            ),
            numericInput(
              "AltSD",
              label = HTML("SD (<em>&sigma;</em><sub>1</sub>)"),
              value = 15,
              min = 0,
              step = 1
            ),
            p(
              HTML("SE (&sigma;<sub>e</sub>)"),
              shiny::textOutput(outputId = "AltSE")
            )
          ),
          splitLayout(
            numericInput(
              "samplesize",
              label = HTML("Sample Size (<em>n</em>)"),
              value = 15,
              min = 1,
              step = 1
            ),
            numericInput(
              "alpha",
              label = HTML("<em>&alpha;</em>"),
              value = .05,
              min = 0,
              max = 1,
              step = .01
            )
          ),
          selectInput(
            inputId = "tails",
            label = "Hypothesis",
            choices = list(
              `Alternative and Null means are unequal (2-tailed test)` = "twotailed",
              `Alternative mean greater than Null mean (1-tailed test)` = "onetailedgreaterthan",
              `Alternative mean less than Null mean (1-tailed test)` = "onetailedlessthan"
            ),
            size = 3,
            selectize = F
          )
        ),
        mainPanel(plotOutput("powerPlot", width = "100%", height = "100%"))
      )
    )
  ),

  # Sidebar with a slider input for number of bins
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
        qnorm(input$lq, input$mu, sigmae)
      )
    }
  })

  reactive_ub <- reactive({
    if (input$radioProportions == "Greater") {
      Inf
    } else {
      ifelse(
        input$whichpanel == "proportions",
        input$ub,
        qnorm(input$uq, input$mu, sigmae)
      )
    }
  })

  output$sigmae <- renderText({
    formatC(input$sigma / sqrt(input$n), digits = 2, format = "f")
  })

  output$lb <- renderText({
    ifelse(
      is.na(reactive_lb()),
      "",
      round(reactive_lb(), ifelse(input$sigma <= 1, 4, 2))
    )
  })

  output$ub <- renderText({
    ifelse(
      is.na(reactive_ub()),
      "",
      round(reactive_ub(), ifelse(input$sigma <= 1, 4, 2))
    )
  })

  output$lbp <- renderText({
    ifelse(
      is.na(reactive_lb()),
      "",
      round(reactive_lb(), ifelse(input$sigma <= 1, 4, 2))
    )
  })

  output$NullSE <- renderText(scales::number(
    input$NullSD / sqrt(input$samplesize),
    .001
  ))
  output$AltSE <- renderText(scales::number(
    input$AltSD / sqrt(input$samplesize),
    .001
  ))

  # Power Plot ----

  output$powerPlot <- renderPlot(
    {
      lb_null <- input$NullMean - input$NullSD * 4
      lb_alt <- input$AltMean - input$AltSD * 4
      lb <- min(lb_null, lb_alt)
      ub_null <- input$NullMean + input$NullSD * 4
      ub_alt <- input$AltMean + input$AltSD * 4
      ub <- max(ub_null, ub_alt)

      null_color <- "royalblue4"
      alt_color <- "firebrick4"

      ntails <- ifelse(input$tails == "twotailed", 2, 1)

      relative_height <- .5

      sd_height = dnorm(1) / dnorm(0)

      se_null <- input$NullSD / sqrt(input$samplesize)
      se_alt <- input$AltSD / sqrt(input$samplesize)

      null_height <- dnorm(input$NullMean, input$NullMean, input$NullSD)
      null_height_se <- dnorm(input$NullMean, input$NullMean, se_null) /
        relative_height

      alt_height <- dnorm(input$AltMean, input$AltMean, input$AltSD)
      alt_height_se <- dnorm(input$AltMean, input$AltMean, se_alt) /
        relative_height

      d_label <- tibble(
        x = c(input$NullMean, input$AltMean),
        y = relative_height,
        l = c(input$NullMean, input$AltMean),
        se_y = relative_height * sd_height,
        sd_y = sd_height + c(0, .01),
        sd_x = x + c(input$NullSD, input$AltSD),
        se_x = x + c(se_alt, se_null),
        dist = c("Null", "Alternative"),
        sd = c(input$NullSD, input$AltSD),
        vjust = c(1.3, -0.3)
      )

      pp <- ggplot(data.frame(x = c(lb, ub)), aes(x)) +
        stat_function(
          fun = \(x) dnorm(x, input$NullMean, input$NullSD) / null_height,
          geom = "area",
          fill = null_color,
          alpha = .1,
          n = 500
        ) +
        stat_function(
          fun = \(x) dnorm(x, input$NullMean, se_null) / null_height_se,
          geom = "area",
          fill = null_color,
          alpha = .2,
          n = 1000
        ) +
        stat_function(
          fun = \(x) dnorm(x, input$AltMean, input$AltSD) / alt_height,
          geom = "area",
          fill = alt_color,
          alpha = .1,
          n = 500
        ) +
        stat_function(
          fun = \(x) dnorm(x, input$AltMean, se_alt) / alt_height_se,
          geom = "area",
          fill = alt_color,
          alpha = .2,
          n = 1000
        ) +
        geom_richtext(
          aes(
            y = 1,
            label = paste0("*&mu;*<sub>", c(0, 1), "</sub> = ", l),
            color = dist,
            vjust = c(1, 0),
            label.color = NA
          ),
          fill = NA,
          data = d_label,
          size = 5
        ) +
        geom_text(
          aes(y = y, label = dist, color = dist, vjust = vjust),
          data = d_label,
          size = 4
        ) +
        geom_text(
          aes(
            y = sd_y,
            label = paste0("SD = ", sd),
            color = dist,
            x = x + sd / 2,
            vjust = vjust
          ),
          data = d_label,
          size = 4
        ) +
        geom_segment(
          data = d_label,
          aes(color = dist, xend = sd_x, yend = sd_y, y = sd_y),
          arrow = arrow(12, type = "closed", length = unit(2.5, "mm"))
        ) +
        geom_segment(
          data = d_label,
          aes(color = dist, xend = se_x, yend = se_y, y = se_y),
          arrow = arrow(12, type = "closed", length = unit(2.5, "mm")),
          size = .25
        ) +
        theme_minimal(base_size = 24, base_family = myfont) +
        scale_x_continuous(NULL) +
        scale_y_continuous(
          NULL,
          breaks = NULL,
          expand = expansion(add = c(0, .1))
        ) +
        scale_color_manual(
          values = c(Alternative = alt_color, Null = null_color)
        ) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_line(linewidth = 0.2, color = "gray"),
          ggh4x.axis.ticks.length.minor = rel(1),
          legend.position = "none"
        )

      z <- qnorm(1 - input$alpha / ntails)

      lb_crit <- input$NullMean - se_null * z
      ub_crit <- input$NullMean + se_null * z

      stat_power_ub <- 1 - pnorm(ub_crit, mean = input$AltMean, sd = se_alt)

      stat_power_lb <- pnorm(lb_crit, mean = input$AltMean, sd = se_alt)

      stat_power <- 0

      if (input$tails != "onetailedgreaterthan") {
        stat_power <- stat_power_lb + stat_power
        pp <- pp +
          stat_function(
            fun = \(x) dnorm(x, input$AltMean, se_alt) / alt_height_se,
            geom = "area",
            fill = alt_color,
            alpha = .6,
            n = 1000,
            xlim = c(lb, lb_crit)
          ) +
          stat_function(
            fun = \(x) dnorm(x, input$NullMean, se_null) / null_height_se,
            geom = "area",
            fill = null_color,
            alpha = .6,
            n = 1000,
            xlim = c(lb, lb_crit)
          )
      }

      if (input$tails != "onetailedlessthan") {
        # lb_crit <- lb
        stat_power <- stat_power_ub + stat_power
        pp <- pp +
          stat_function(
            fun = \(x) dnorm(x, input$AltMean, se_alt) / alt_height_se,
            geom = "area",
            fill = alt_color,
            alpha = .6,
            n = 1000,
            xlim = c(ub_crit, ub)
          ) +
          stat_function(
            fun = \(x) dnorm(x, input$NullMean, se_null) / null_height_se,
            geom = "area",
            fill = null_color,
            alpha = .6,
            n = 1000,
            xlim = c(ub_crit, ub)
          )
      }
      pp +
        ggtitle(paste0("Power: ", scales::number(stat_power, .001)))

      # pp <- pp +
      #   stat_function(fun = \(x) dnorm(x, input$AltMean, se_alt) / alt_height_se, geom = "area", fill = alt_color, alpha = .6, n = 1000, xlim = c(lb,lb_crit)) +
      #   stat_function(fun = \(x) dnorm(x, input$AltMean, se_alt) / alt_height_se, geom = "area", fill = alt_color, alpha = .6, n = 1000, xlim = c(ub_crit, ub)) +
      #   stat_function(fun = \(x) dnorm(x, input$NullMean, se_null) / null_height_se, geom = "area", fill = null_color, alpha = .6, n = 1000, xlim = c(lb,lb_crit)) +
      #   stat_function(fun = \(x) dnorm(x, input$NullMean, se_null) / null_height_se, geom = "area", fill = null_color, alpha = .6, n = 1000, xlim = c(ub_crit, ub))
    },
    res = 100
  )

  # Area under curve plot ----
  output$distPlot <- renderPlot(
    {
      # generate bins based on input$bins from ui.R
      lb <- max(reactive_lb(), -Inf, na.rm = T)
      ub <- min(reactive_ub(), Inf, na.rm = T)

      sigmae <- input$sigma / sqrt(input$n)

      p <- pnorm(max(lb, ub, na.rm = T), input$mu, sigmae) -
        ifelse(is.na(min(lb, ub)), 0, pnorm(min(lb, ub), input$mu, sigmae))
      m_trunc <- ifelse(
        lb == ub,
        lb,
        etruncnorm(
          a = min(lb, ub),
          b = max(lb, ub),
          mean = input$mu,
          sd = input$sigma
        )
      )

      p_trunc <- pnorm(m_trunc, mean = input$mu, sd = sigmae)
      p_y <- dnorm(m_trunc, mean = input$mu, sd = sigmae)

      if (p_trunc < 0.05 | p_trunc > 0.95) {
        p_y <- p_y * 0.05
        color_p <- "gray20"
      } else {
        p_y <- p_y * 1.05
        color_p <- "gray95"
      }

      neg5 <- input$mu - 5 * sigmae
      pos5 <- input$mu + 5 * sigmae

      ggp <- ggplot(
        data.frame(
          x = c(
            input$mu - 5 * sigmae,
            input$mu + 5 * sigmae
          )
        ),
        aes(x)
      ) +
        stat_function(
          geom = "area",
          fun = dnorm,
          args = list(mean = input$mu, sd = sigmae),
          fill = "#9DA0A5",
          alpha = 0.4,
          color = NA,
          n = 801
        ) +
        stat_function(
          geom = "area",
          fun = dnorm,
          xlim = sort(c(
            max(lb, neg5, na.rm = T),
            min(ub, pos5, na.rm = T)
          )),
          args = list(mean = input$mu, sd = sigmae),
          fill = "#A3203699",
          color = NA,
          n = 801
        ) +
        theme_minimal(base_size = 20, base_family = myfont) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_line(linewidth = 0.2, color = "gray"),
          ggh4x.axis.ticks.length.minor = rel(1)
        ) +
        scale_y_continuous(
          NULL,
          breaks = NULL,
          expand = expansion(mult = c(0, .05))
        ) +
        scale_x_continuous(
          NULL,
          guide = "axis_minor",
          expand = expansion(mult = 0.01),
          breaks = seq(neg5, pos5, sigmae),
          minor_breaks = seq(
            neg5,
            pos5,
            sigmae / ifelse(sigmae %% 3 == 0, 3, 2)
          ),
          label = \(x) round(x, 2)
        ) +
        coord_cartesian(xlim = c(neg5, pos5))
      # ggtitle(paste0("Shaded Proportion: ",
      #                format(proportion_round(p),
      #                       scientific = FALSE))) +

      d_text <- tibble(
        x = round(c(lb, ub), 2),
        y = dnorm(x, input$mu, sigmae),
        label = x,
        hjust = 0.5 - sign(x - input$mu) / 2
      ) %>%
        filter(!is.na(x))

      ggp +
        geom_text(
          data = d_text,
          aes(label = label, y = y, hjust = hjust),
          vjust = -0.1,
          size = 4,
          family = myfont
        ) +
        annotate(
          geom = "text",
          x = m_trunc,
          y = 0,
          size = 4,
          family = myfont,
          color = "gray10",
          vjust = -0.30,
          lineheight = 0.85,
          label = paste0(
            "Shaded\nProportion\n",
            format(proportion_round(p), scientific = FALSE)
          )
        )
    },
    res = 100
  )
}

# Run the application
shinyApp(ui = ui, server = server)
