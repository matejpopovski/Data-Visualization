# STAT 436
# HW 2
# Henry Burke

library(shiny)
library(tidyverse)
library(shinythemes)
library(reactable)

# setup & data cleaning

# standalone nfl combine data
combine = read.csv("https://uwmadison.box.com/shared/static/jbmplh2j3nxfylzb3c74mkibazoves64.csv") |>
  mutate(player_name = str_extract(Player, ".*\\\\")) |>
  mutate(player_name = substr(player_name, 1, nchar(player_name) - 1))

# standalone nfl draft data
draft = read.csv("https://uwmadison.box.com/shared/static/7kyngc28p6fizq36v5rksa9z5ayrb2cg.csv") |>
  filter(draft_year > 2008, draft_year < 2020)

# joined full nfl data
nfl = inner_join(combine, draft, by="player_name", relationship="many-to-many") |> 
  # selected multiple variables with potential for comparison
  select(player_name, Year, position, overall, round, team, pos_rk, ovr_rk, 
         grade, Age, School, height, weight, Sprint_40yd, Vertical_Jump, 
         Bench_Press_Reps, Broad_Jump, Agility_3cone, Shuttle, BMI) |>
  drop_na(overall, height, grade, position)

# plot scatter plot based on selected points & positions
scatterplot = function(df, selected_, positions_) {
  df |>
    mutate(selected = selected_) |>
    filter(position %in% positions_) |>
    ggplot() +
        geom_point(
          aes(
            overall, grade, col=position,
            alpha = as.numeric(selected)
          ),
          size = 4
        ) +
    theme(legend.position="none") +
    ggtitle("Overall Draft Pick vs. Combine Pre-Draft Grade") +
    xlab("Overall Draft Pick") +
    ylab("Pre-Draft Grade / 100") +
    scale_color_manual( 
      values = c("dodgerblue2", "darkturquoise",
      "green4", "#6A3D9A", "#FF7F00",
      "steelblue4", "gold1","skyblue2", "#FB9A99",
      "palegreen2","#CAB2D6","#FDBF6F",
      "gray70", "khaki2", "maroon", 
      "orchid1", "deeppink1", "blue1"
    ) )
}

# plot histogram based on selected points & positions
overlay_histogram = function(df, selected_, positions_) {
  sub_df = filter(df, selected_) |>
    filter(position %in% positions_)
  
  ggplot(df, aes(height, fill = position)) +
    geom_histogram(alpha = 0.3, binwidth = 1) +
    geom_histogram(data = sub_df, binwidth = 1) +
    scale_y_continuous(expand = c(0, 0, 0.1, 0)) +
    xlab("Height (inches)") +
    ylab("# of Players") +
    labs(title="Height Across NFL Players", fill="Position") +
    scale_fill_manual( 
      values = c("dodgerblue2", "darkturquoise",
        "green4", "#6A3D9A", "#FF7F00",
        "steelblue4", "gold1","skyblue2", "#FB9A99",
        "palegreen2","#CAB2D6","#FDBF6F",
        "gray70", "khaki2", "maroon", 
        "orchid1", "deeppink1", "blue1"
      ) )
}

# filter df based on selected points & positions
filter_df = function(df, selected_, positions_) {
  df |>
  filter(selected_, position %in% positions_) |>
    select("Player Name" = player_name, "Position" = position,
            Year, "Team" = team, "Overall Draft Pick" = overall,
           "Pre-Draft Grade" = grade, "Height" = height)
}


ui = fluidPage(
  theme=shinytheme("simplex"),
  titlePanel("NFL Combine Data Compared to Draft Position"),
  verbatimTextOutput("helpText"),
  
  # position selector
  column(2, selectInput(inputId="positions", label="Choose Positions to Compare", 
                        choices=sort(unique(nfl$position)), 
                        selected=sort(
                          unique(nfl$position)
                          # c("Quarterback", "Running Back", "Safety",
                          #   "Tight End", "Wide Receiver")
                          ), multiple=TRUE)),
  # histogram
  column(5,
    plotOutput("histogram", brush = brushOpts("plot_brush", direction = "x"), height = 500)
  ),
  # scatter plot
  column(5, plotOutput("scatter", brush = "plot_brush", height = 500)),
  # data table
  column(12, reactableOutput("table"))
)


server = function(input, output) {
  selected = reactiveVal(rep(TRUE, nrow(nfl)))
  
  # brush observer for scatter plot & histogram
  observeEvent(
    input$plot_brush,
        {
      bp = brushedPoints(nfl, input$plot_brush, allRows = TRUE)
      selected(bp$selected_)
    })

  output$histogram = renderPlot(overlay_histogram(nfl, selected(), input$positions))
  output$scatter = renderPlot(scatterplot(nfl, selected(), input$positions))
  output$table =  renderReactable({
    reactable(filter_df(nfl, selected(), input$positions))
      })
  output$helpText = renderText({
    paste("* Select height ranges on histogram and players on scatter plot to draw comparisons.")
  })
}

shinyApp(ui = ui, server = server)
