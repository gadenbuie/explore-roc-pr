knitr::opts_chunk$set(echo = FALSE)
library(dplyr)
library(ggplot2)
library(cowplot)
library(PRROC)

theme_set(theme_minimal())

if (!requireNamespace("PRROC", quietly = TRUE)) {
  stop("`PRROC` is required: install.packages('PRROC')")
}

if (!requireNamespace("cowplot", quietly = TRUE)) {
  stop("`cowplot` is required: install.packages('cowplot')")
}

gen_data <- function(n = 5) {
  data_frame(
    score = c(
      runif(n, 0.25, 1),
      runif(n, 0, 0.75)
    ),
    class = factor(
      c(rep("Positive", n), rep("Negative", n)),
      levels = c("Negative", "Positive")
    )
  )
}

data <- gen_data(5)

ui <- basicPage(
  tags$h1("Manual ROC/PR Explorer"),
  fluidRow(
    column(
      width = 4,
      tags$p(
        tags$strong("Click"), "to add points, or",
        tags$strong("double click"), "to remove."
      )
    ),
    column(
      width = 3,
      actionButton("reset", "Random Data")
    )
  ),
  uiOutput("helptext"),
  plotOutput(
    "line_plot",
    height = 200,
    click = "line_plot_click",
    dblclick = "line_plot_dblclick"
  ),
  plotOutput("roc_pr_plot"),
  tags$div(
    style = "position: absolute; top: 0; right: 0;",
    # https://github.blog/2008-12-19-github-ribbons/
    HTML(
      '<a href="https://github.com/gadenbuie/explore-roc-pr">
      <img width="149" height="149" src="fork-on-gh.png" class="attachment-full size-full" alt="Fork me on GitHub" data-recalc-dims="1">
      </a>'
    )
  )
)

server <- function(
  input,
  output,
  session
) {
  observeEvent(input$reset, {
    req(input$reset > 0)
    data(gen_data(5))
  })

  data <- reactiveVal(gen_data(5))
  redraw <- reactiveVal(value = 0L)

  observeEvent(input$line_plot_click, priority = 100, {
    new_data <-
      bind_rows(
        data(),
        data_frame(
          score = input$line_plot_click$x,
          class = factor(
            c("Negative", "Positive")[round(input$line_plot_click$y, 0)],
            levels = c("Negative", "Positive")
          )
        )
      )
    data(new_data)
  })

  observeEvent(input$line_plot_dblclick, priority = 100, {
    new_data <-
      data() %>%
      nearPoints(input$line_plot_dblclick, allRows = TRUE) %>%
      filter(!selected_) %>%
      select(-selected_)
    data(new_data)
  })

  output$line_plot <- renderPlot({
    ggplot(data()) +
      aes(score, class, color = class) +
      geom_point(size = 5, alpha = 0.75) +
      # geom_hline(yintercept = 1.5, color = "grey75") +
      annotate(
        "label",
        x = seq(0, 1, by = 0.25),
        y = 1.5,
        label = seq(0, 1, 0.25)
      ) +
      xlim(0, 1) +
      scale_y_discrete(labels = c("Negative", "Positive")) +
      guides(color = FALSE) +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 14)
      )
  })


  output$roc_pr_plot <- renderPlot({
    df <- data()
    pr <- PRROC::pr.curve(df[df$class == "Positive", ]$score, df[df$class == "Negative", ]$score, curve = TRUE)
    roc <- PRROC::roc.curve(df[df$class == "Positive", ]$score, df[df$class == "Negative", ]$score, curve = TRUE)

    prroc <- list(
      "ROC" = as_tibble(roc$curve),
      "PR" = as_tibble(pr$curve)
    )

    g_roc <- ggplot(prroc$ROC) +
      aes(V1, V2, color = V3) +
      geom_line(alpha = 0.75, size = 0.5) +
      geom_point(size = 1.5) +
      guides(color = FALSE) +
      coord_equal() +
      annotate(
        "text",
        x = 0.9,
        y = 0.05,
        label = round(roc$auc, 3)
      ) +
      labs(
        x = "FPR (1 - Specificity)",
        y = "TPR (Recall, Sensitivity)",
        title = "ROC"
      ) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_color_viridis_c() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.border = element_rect(fill = NA, color = "grey40")
      )

    g_pr <- ggplot(prroc$PR) +
      aes(V1, V2, color = V3) +
      geom_line(alpha = 0.75, size = 0.5) +
      geom_point(size = 1.5) +
      annotate(
        "text",
        x = 0.9,
        y = 0.05,
        label = round(pr$auc.davis.goadrich, 3)
      ) +
      coord_equal() +
      labs(
        x = "Recall (TPR, Sensitivity)",
        y = "Precision",
        color = "Threshold",
        title = "PR"
      ) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
      scale_color_viridis_c() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.border = element_rect(fill = NA, color = "grey40")
      )

    cowplot::plot_grid(g_roc, g_pr, ncol = 2, rel_widths = c(1, 1.25))
  })
}

shinyApp(ui, server)
