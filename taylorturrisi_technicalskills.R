library(base64enc)
library(dplyr)
library(plotly)
library(shiny)
library(png)
library(purrr)
library(httr)

options(download.file.method = "wininet")
options(shiny.launch.browser = TRUE)

df <- data.frame(
  Software = c("R", "SAS", "Python", "SPSS", "Excel", "PowerBI", "ArcGIS", "Tableau",
               "Sawtooth", "MarketSight", "Qualtrics", "REDCap", "MySQL"),
  Courses = c(9, 3, 3, 3, 3, 1, 2, 1, 0, 0, 0, 0, 1),
  Professional_Years = c(6, 0, 1, 1, 8, 0, 0, 0, 1, 1, 4, 3, 0),
  Image = c(
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/r.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/sas.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/python.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/spss.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/excel.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/powerbi.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/arcgis.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/tableau.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/sawtooth.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/marketsight.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/qualtrics.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/redcap.png",
    "https://raw.githubusercontent.com/taylorturrisi0220/resume_logos/main/mysql.png"
  ),
  Size_Adjustment = c(0.85, 0.667, 0.9, 1.15, 0.75, 0.85, 1.15, 1.075, 0.85, 0.95, 0.825, 0.9, 1)
)

df$tooltip <- paste0(
  "<b>", df$Software, "</b><br>Courses: ", df$Courses,
  "<br>Professional Use: ", df$Professional_Years, " yrs"
)

df <- df %>%
  group_by(Courses, Professional_Years) %>%
  mutate(
    group_size = n(),
    group_index = row_number(),
    Courses_jitter = ifelse(group_size == 2,
                            Courses + (group_index - 1.5) * 0.475,
                            Courses),
    Professional_Years_jitter = ifelse(group_size >= 3,
                                       Professional_Years + (group_index - (group_size + 1) / 2) * 0.725,
                                       Professional_Years)
  ) %>%
  ungroup()

ui <- fluidPage(
  tags$head(
    tags$title("Taylor Turrisi's Experience with Statistical Software Programs"),
    tags$style(HTML("
      body, .container-fluid {
        background-color: #D5DCE4 !important;
      }
    "))
  ),
  div(
    style = "display: flex; align-items: center; justify-content: center; height: 100vh;",
    plotlyOutput("logoPlot", height = "700px")
  )
)


server <- function(input, output, session) {
  output$logoPlot <- renderPlotly({

    df_groups <- df %>%
      group_by(Courses, Professional_Years) %>%
      mutate(outline = n() > 1) %>%
      ungroup()

    shapes_list <- df_groups %>%
      filter(outline) %>%
      group_by(Courses, Professional_Years) %>%
      mutate(group_size = n()) %>%   # <--- ADD THIS LINE
      summarise(
        xmin = min(Courses_jitter) - ifelse(first(group_size) == 2, 0.25, 0.47),  # <--- MODIFY HERE
        xmax = max(Courses_jitter) + ifelse(first(group_size) == 2, 0.25, 0.47),  # <--- AND HERE
        ymin = min(Professional_Years_jitter) - ifelse(first(group_size) == 2, 0.485, 0.475),  # <--- AND HERE
        ymax = max(Professional_Years_jitter) + ifelse(first(group_size) == 2, 0.485, 0.475),  # <--- AND HERE
        .groups = 'drop'
      ) %>%
      pmap(function(xmin, xmax, ymin, ymax, ...) {
        list(
          type = "rect",
          xref = "x",
          yref = "y",
          x0 = xmin,
          x1 = xmax,
          y0 = ymin,
          y1 = ymax,
          line = list(color = "gray", width = 2),
          fillcolor = "rgba(0,0,0,0)"
        )
      })

    p <- plot_ly(
      data = df_groups,
      x = ~Courses_jitter,
      y = ~Professional_Years_jitter,
      type = 'scatter',
      mode = 'markers',
      text = ~tooltip,
      hoverinfo = 'text',
      marker = list(
        size = 20,
        color = 'rgba(0,0,0,0.01)'
      )
    )

    p <- layout(
      p,
      images = lapply(1:nrow(df_groups), function(i) {
        list(
          source = df_groups$Image[i],
          x = df_groups$Courses_jitter[i],
          y = df_groups$Professional_Years_jitter[i],
          xref = "x",
          yref = "y",
          sizex = df_groups$Size_Adjustment[i],
          sizey = df_groups$Size_Adjustment[i],
          xanchor = "center",
          yanchor = "middle",
          layer = "above"
        )
      }),
      shapes = shapes_list,
      xaxis = list(
        title = "Semesters of Academic Exposure",
        range = c(-0.5, 10),
        dtick = 1,
        showgrid = TRUE,
        autorange = TRUE,
        gridcolor = 'gray33',
        titlefont = list(family = 'Bookman, sans-serif', size = 24, color = '#0E2841'),
        tickfont = list(family = 'Bookman, sans-serif', size = 18, color = '#0E2841')),
      yaxis = list(
        title = "Years of Professional Experience",
        range = c(-0.5, 10),
        dtick = 1,
        showgrid = TRUE,
        autorange = TRUE,
        gridcolor = 'gray33',
        titlefont = list(family = 'Bookman, sans-serif', size = 24, color = '#0E2841'),
        tickfont = list(family = 'Bookman, sans-serif', size = 18, color = '#0E2841')),
      title = list(
        text = "Taylor Turrisi's Experience with Statistical Software Programs",
        x = 0.5,
        y = 0.975,   # default is 1.0, lower = lower
        xanchor = 'center',
        yanchor = 'top',
        font = list(family = 'Bookman, sans-serif', size = 32, color = '#0E2841')
      ),
      margin = list(l = 50, r = 50, t = 50, b = 50)
    )

    # Add anchor points and lines
    anchors <- data.frame(
      Courses = c(1, 0, 3),
      Professional_Years = c(0, 1, 1)
    )

    p <- add_trace(
      p,
      data = anchors,
      x = ~Courses,
      y = ~Professional_Years,
      type = 'scatter',
      mode = 'markers',
      marker = list(color = 'black', size = 8),
      inherit = FALSE,
      showlegend = FALSE,
      hoverinfo = 'none'
    )

    for (i in 1:nrow(anchors)) {
      anchor_x <- anchors$Courses[i]
      anchor_y <- anchors$Professional_Years[i]
      group_points <- df_groups %>%
        filter(Courses == anchor_x, Professional_Years == anchor_y)

      for (j in 1:nrow(group_points)) {
        p <- add_trace(
          p,
          x = c(anchor_x, group_points$Courses_jitter[j]),
          y = c(anchor_y, group_points$Professional_Years_jitter[j]),
          type = 'scatter',
          mode = 'lines',
          line = list(color = 'black', width = 0.5),
          showlegend = FALSE,
          inherit = FALSE,
          hoverinfo = 'none'
        )
      }
    }

    p
  })
}

shinyApp(ui, server)
