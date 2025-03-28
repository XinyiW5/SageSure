library(shiny)
library(shinythemes)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyr)
library(tidygeocoder)
library(purrr)

gs4_auth()
# CEDEC dashboards
# Define UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(h1("Summary of CEDEC Outcomes", align = "center", style = "color:white;")),  # Centered Title
  
  # First Row: Filter and Bar Plot
  fluidRow(
    column(
      width = 3,  # Sidebar for filter
      sidebarPanel(
        checkboxGroupInput(
          inputId = "status", 
          label = "Select Status for Text Table:", 
          choices = c("Completed", "In Progress", "In Development", "Scheduled"),
          selected = c("Completed", "In Progress", "In Development", "Scheduled")  # Default selection
        ),
        width = 12  # Full width of the sidebar content
      )
    ),
    column(
      width = 9,  # Bar plot occupies the remaining space
      plotlyOutput("text_plot", height = "400px")  # Bar plot
    )
  ),
  
  # Second Row: Stacked Plots
  fluidRow(
    column(
      width = 6,
      plotlyOutput("cumulative", height = "300px") # Cumulative Projects Over Time
    ),
    column(
      width = 6,
      plotlyOutput("cumulative_histogram", height = "300px")  # Cumulative Projects Over Time - Histogram
    )
  ),
  
  # Third Row: Donut Chart and Heatmap with Additional Spacing
  fluidRow(
    style = "margin-top: 20px;",  # Add margin at the top of the third row
    column(
      width = 6,
      plotlyOutput("fig_donut", height = "400px")  # Donut chart
    ),
    column(
      width = 6,
      plotlyOutput("fig_location", height = "400px")  # Heat Map
    )
  ),
  
  # Fourth Row: Histogram of Projects per Community Partner
  # fluidRow(
  #   column(
  #     width = 6,
  #     plotlyOutput("partner_project_barplot", height = "400px")  # Correct ID and height
  #   )
  # )
  # Fourth Row: Histogram of Projects per Community Partner
  fluidRow(
    column(
      width = 12,
      plotlyOutput("partner_project_barplot", height = "400px")  # Use full width
    )
  ),
  fluidRow(
    column(
      width = 12,
      downloadButton("download_plots", "Download All Plots", style = "margin: 20px;")
    )
  )
  
)



# Define server logic
server <- function(input, output) {
  
  # ----------------- read in data from Google Sheets -----------------
  sheet_url <- "https://docs.google.com/spreadsheets/d/1WAhVtp0bNP08dmomx9xSCJuuuSf52GbAQJWliO2FWPk/edit?gid=1195950716#gid=1195950716"
  
  # Read the data from Google Sheets
  projects <- read_sheet(sheet_url, sheet = 'projects')
  students <- read_sheet(sheet_url, sheet = 'students')
  faculty_staff <- read_sheet(sheet_url, sheet = 'faculty_staff')
  address <- read_sheet(sheet_url, sheet = 'Addresses')
  
  # ----------------- Counts of Metrics -----------------
  commu_partner_num <- projects %>%
    filter(status == "Completed" | status == "In Progress") %>%
    pull(community_partner) %>% 
    n_distinct() #33
  
  commu_partner_completed_num <- projects %>%
    filter(status == "Completed") %>%
    pull(community_partner) %>% 
    n_distinct() #27
  
  # create full name of students to get distinct student counts
  students <- students %>%
    mutate(student_full_name = paste(student_first_name, student_last_name))
  student_num <- n_distinct(students$student_full_name) #68
  
  # create full name of faculty to get distinct faculty counts
  faculty_staff <- faculty_staff %>%
    mutate(full_name = paste(first_name, last_name))
  faculty_num <- faculty_staff %>%
    filter(position == "faculty") %>%
    pull(full_name) %>% 
    n_distinct() #13
  
  # staff counts
  staff_num <- faculty_staff %>%
    filter(position == "staff") %>%
    pull(full_name) %>% 
    n_distinct() #2
  
  # project counts
  proj_completed_num <- projects %>%
    filter(status == "Completed") %>%
    pull(id) %>% 
    n_distinct() #40
  proj_inprogress_num <- projects %>%
    filter(status == "In Progress") %>%
    pull(id) %>% 
    n_distinct() #38
  proj_indevelop_num <- projects %>%
    filter(status == "In Development") %>%
    pull(id) %>% 
    n_distinct() #18
  proj_scheduled_num <- projects %>%
    filter(status == "Scheduled") %>%
    pull(id) %>% 
    n_distinct() #13
  
  # ----------------- Donut Chart of "# of Projects" -----------------
  count_data <- data.frame(
    Category = c("# of Community Partner", "# of Students", "# of Faculty","# of Staff","# of Projects","# of Projects","# of Projects","# of Projects"),
    Subcategory = c("Total", "Total", "Total", "Total", "Completed", "In Progress", "In Development", "Scheduled"),
    Count = c(commu_partner_num, student_num, faculty_num, staff_num, proj_completed_num, proj_inprogress_num, proj_indevelop_num, proj_scheduled_num)
  )
  
  # Data for project counts by status
  project_counts <- data.frame(
    Status = c("Completed", "In Progress", "In Development", "Scheduled"),
    Count = c(proj_completed_num, proj_inprogress_num, proj_indevelop_num, proj_scheduled_num)
  )
  
  # Create an interactive donut chart
  output$fig_donut <- renderPlotly({
    plot_ly(
      project_counts, 
      labels = ~Status, 
      values = ~Count, 
      type = 'pie',
      hole = 0.4,
      textinfo = 'label+percent',
      hoverinfo = 'label+percent+value',
      marker = list(colors = c("#6baed6", "#3182bd", "#9ecae1", "#deebf7"))
    ) %>% layout(
      title = list(
        text = "<b>Counts of Projects by Status</b>",
        x = 0.5,
        font = list(color = "white", size = 16)  # Set title color to white
      ),
      paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
      plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot area
      showlegend = FALSE
    )
  })
  
  # ----------------- Reactive Expression for Filtered Text Table -----------------
  filtered_counts <- reactive({
    # Filter projects based on selected statuses
    filtered_projects <- projects %>% filter(status %in% input$status)
    commu_partner_num_filtered <- filtered_projects %>%
      pull(community_partner) %>%
      n_distinct()
    # Student
    student_num_filtered <- students %>%
      inner_join(projects, by = "community_partner") %>%  # Join to bring in status
      filter(status %in% input$status) %>%  # Filter by status
      pull(student_full_name) %>%
      n_distinct()
    #Faculty
    faculty_num_filtered <- faculty_staff %>%
      inner_join(projects, by = "community_partner") %>%  # Join to bring in status
      filter(position == "faculty", status %in% input$status) %>%  # Filter by position and status
      pull(full_name) %>%
      n_distinct()
    #Staff
    staff_num_filtered <- faculty_staff %>%
      inner_join(projects, by = "community_partner") %>%  # Join to bring in status
      filter(position == "staff", status %in% input$status) %>%  # Filter by position and status
      pull(full_name) %>%
      n_distinct()
    
    # Create data frame for the filtered text table
    data.frame(
      Category = c("Community Partners", "Students", "Faculty", "Staff"),
      Count = c(commu_partner_num_filtered, student_num_filtered, faculty_num_filtered, staff_num_filtered)
    )
  })

  # Render the text-based table plot with filtered data
  output$text_plot <- renderPlotly({
    # Debugging: Ensure the data is properly loaded
    table_data <- filtered_counts()
    
    # Check if table_data has rows
    if (nrow(table_data) == 0) {
      return(NULL)  # Avoid rendering if there's no data
    }
    
    # Create a Plotly horizontal bar chart
    plot_ly(
      data = table_data,
      x = ~Count,
      y = ~Category,
      type = 'bar',
      orientation = 'h',  # Horizontal bar chart
      text = ~Count,
      textposition = 'outside',
      textfont = list(color = "white"), 
      marker = list(color = 'rgba(255, 255, 255, 0.8)')  # Light transparent white bars
    ) %>%
      layout(
        title = list(
          text = "Summary of Counts by Category (Filtered)",
          font = list(color = "white", size = 16),
          x = 0.5
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE,
          tickfont = list(color = "white"),
          automargin = TRUE 
        ),
      yaxis = list(
          title = "",
          tickfont = list(color = "white"), automargin = TRUE
        ),
        paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
        plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot area
        margin = list(l = 150, r = 50, t = 50, b = 50)  # Adjust margins for better layout
      )
  })
  # ----------------- Cumulative Projects Over Time Chart -----------------
  projects <- read_sheet(sheet_url, sheet = 'projects')
  
  # Convert start_month and end_month to Date format
  projects$start_month <- as.Date(as.POSIXct(as.numeric(unlist(projects$start_month)), origin = "1970-01-01", tz = "UTC"))
  projects$end_month <- as.Date(as.POSIXct(as.numeric(unlist(projects$end_month)), origin = "1970-01-01", tz = "UTC"))
  
  # Generate a timeline based on the range of start and end dates
  projects_timeline <- data.frame(
    month = seq(
      from = min(projects$start_month, na.rm = TRUE),
      to = max(projects$end_month, na.rm = TRUE),
      by = "month"
    )
  )
  
  # Calculate cumulative projects with the specific status handling rules
  cumulative_data <- projects_timeline %>%
    rowwise() %>%
    mutate(
      cumulative_projects_by_status = list(
        projects %>%
          mutate(
            # Adjust status based on rules
            status_adjusted = case_when(
              # Projects that are still in progress (no end date or TBD) remain "In Progress"
              is.na(end_month) & status == "In Progress" ~ "In Progress",
              # Projects that were "In Progress" but have reached their end date become "Completed"
              status == "In Progress" & !is.na(end_month) & month >= end_month ~ "Completed",
              # Projects that are "In Progress" and within their active time range
              status == "In Progress" & month >= start_month & month < end_month ~ "In Progress",
              # Completed projects remain "Completed" and continue to be counted after their end date
              status == "Completed" & month >= start_month ~ "Completed",
              TRUE ~ status
            )
          ) %>%
          filter(
            start_month <= month & # Project has started
              (is.na(end_month) | end_month >= month | status_adjusted == "Completed") # Project is active or marked as completed
            &
              !status %in% c("On Hold", "Closed", "In Development")) %>%
          group_by(status_adjusted) %>%
          summarise(count = n_distinct(id))
      )
    ) %>%
    unnest(cumulative_projects_by_status)
  
  # Rename for plot clarity
  cumulative_data <- cumulative_data %>%
    rename(status = status_adjusted)
  
  # Plot the cumulative projects over time
  custom_colors <- c(
    "Completed" = "#FFD700",  # Customize this color for "Completed"
    "In Progress" = "#4DB6AC",
    "Scheduled" =  "#4169E1" # Customize this color for "In Progress"
  )

  # Plot the cumulative projects over time as a stacked area plot
  p <- ggplot(cumulative_data, aes(x = month, y = count, fill = status)) +
    geom_area(position = "stack") +
    labs(title = "Cumulative Projects Over Time by Status", x = "Month", y = "Cumulative Projects") +
    theme_minimal() +
    scale_fill_manual(values = custom_colors)
  
  
  p_interactive <- ggplotly(p)
  output$cumulative <- renderPlotly({
    p_interactive %>%
      layout(
        title = list(
          text = "<b>Cumulative Projects Over Time by Status</b>",
          x = 0.5,
          font = list(color = "white", size = 16)  # Set title color to white
        ),
        xaxis = list(
          title = list(
            text = "Date",
            font = list(color = "white", size = 12)  # X-axis title in white
          ),
          tickfont = list(color = "white")  # X-axis tick labels in white
        ),
        yaxis = list(
          title = list(
            text = "Cumulative Projects",
            font = list(color = "white", size = 12)  # Y-axis title in white
          ),
          tickfont = list(color = "white")  # Y-axis tick labels in white
        ),
        legend = list(title = list(
          text = "<b>Status</b>",  # Legend title text
          font = list(color = "white", size = 12)  # Make "status" white
        ),
          font = list(color = "white", size = 12)  # Legend text color set to white
        ),
        paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
        plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot area
        showlegend = TRUE  # Ensure the legend is visible
      )
  })
  
  # ----------------- Cumulative Projects Over Time HISTOGRAM -----------------
  # Plot the cumulative projects over time as a stacked histogram
  p_histogram <- ggplot(cumulative_data, aes(x = month, y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack") +  # Use geom_bar for stacked histogram
    labs(
      title = "Cumulative Projects Over Time by Status",
      x = "Month",
      y = "Cumulative Projects"
    ) +
    theme_minimal() +
    scale_fill_manual(values = custom_colors)  # Use the custom colors defined earlier
  
  # Convert to interactive Plotly plot
  p_histogram_interactive <- ggplotly(p_histogram)
  
  # Render the histogram version
  output$cumulative_histogram <- renderPlotly({
    p_histogram_interactive %>%
      layout(
        title = list(
          text = "<b>Cumulative Projects Over Time by Status</b>",
          x = 0.5,
          font = list(color = "white", size = 16)  # Set title color to white
        ),
        xaxis = list(
          title = list(
            text = "Date",
            font = list(color = "white", size = 12)  # X-axis title in white
          ),
          tickfont = list(color = "white")  # X-axis tick labels in white
        ),
        yaxis = list(
          title = list(
            text = "Cumulative Projects",
            font = list(color = "white", size = 12)  # Y-axis title in white
          ),
          tickfont = list(color = "white")  # Y-axis tick labels in white
        ),
        legend = list(title = list(
          text = "<b>Status</b>",  # Legend title text
          font = list(color = "white", size = 12)  # Make "status" white
        ),
        font = list(color = "white", size = 12)  # Legend text color set to white
        ),
        paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
        plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot area
        showlegend = TRUE  # Ensure the legend is visible
      )
  })
  # ----------------- # of projects each community partner has -----------------
  # Summarize the number of projects per community partner
  # partner_project_counts <- projects %>%
  #   group_by(community_partner) %>%
  #   summarise(project_count = n_distinct(id))  # Count distinct project IDs per partner
  partner_project_counts <- projects %>%
    distinct(community_partner, id, .keep_all = TRUE) %>%  # Remove duplicate rows
    group_by(community_partner) %>%
    summarise(project_count = n_distinct(id))
  # Create a histogram of project counts
  # Create a bar plot with hidden x-axis labels and tooltips for partner names
  p_partner_barplot <- ggplot(partner_project_counts, aes(x = community_partner, y = project_count, text = paste("Community Partner:", community_partner, 
                                                                                                                 "<br>Number of Projects:", project_count))) +
    geom_bar(stat = "identity", fill = "#5DADE2", color = "white") +  # Bar plot #5DADE2
    labs(
      title = "Number of Projects per Community Partner",
      x = NULL,  # Remove x-axis label
      y = "Number of Projects"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),  # Hide x-axis text
      axis.ticks.x = element_blank(),  # Hide x-axis ticks
      plot.title = element_text(size = 16, hjust = 0.5)  # Center title
    )
  
  # Convert the bar plot to an interactive Plotly plot
  p_partner_barplot_interactive <- ggplotly(p_partner_barplot, tooltip = "text")
  
  # Render the interactive bar plot
  output$partner_project_barplot <- renderPlotly({
    p_partner_barplot_interactive %>%
      layout(
        title = list(
          text = "<b>Number of Projects per Community Partner</b>",
          x = 0.5,
          font = list(color = "white", size = 16)
        ),
        xaxis = list(
          title = "",
          showticklabels = FALSE  # Hide x-axis tick labels
        ),
        yaxis = list(
          title = list(
            text = "Number of Projects",
            font = list(color = "white", size = 12)
          ),
          tickfont = list(color = "white")
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        showlegend = FALSE
      )
  })
  
  
  # ----------------- Cumulative Projects ("Scheduled" Only) Over Time Chart -----------------
  # projects <- read_sheet(sheet_url, sheet = 'projects')
  # # Mutate GoogleSheet Date Format to R Date: to numeric, then to POSIXct, and finally to Date
  # projects$start_month <- as.Date(as.POSIXct(as.numeric(unlist(projects$start_month)), origin = "1970-01-01", tz = "UTC"))
  # 
  # projects$end_month <- as.Date(as.POSIXct(as.numeric(unlist(projects$end_month)), origin = "1970-01-01", tz = "UTC"))
  # 
  # # Step 2: Generate timeline
  # projects_timeline <- data.frame(
  #   month = seq(
  #     from = min(projects$start_month, na.rm = TRUE),
  #     to = max(projects$end_month, na.rm = TRUE),
  #     by = "month"
  #   )
  # )
  # 
  # # Step 3: Calculate cumulative projects
  # cumulative_data_scheduled <- projects_timeline %>%
  #   rowwise() %>%
  #   mutate(
  #     cumulative_projects_by_status = list(
  #       projects %>%
  #         filter(
  #           start_month <= month &
  #             (is.na(end_month) | end_month >= month) &
  #             status == "Scheduled"
  #         ) %>%
  #         group_by(status) %>%
  #         summarise(count = n_distinct(id))
  #     )
  #   ) %>%
  #   unnest(cumulative_projects_by_status)
  # 
  # # Step 4: Plot the cumulative projects over time
  # p_scheduled <- ggplot(cumulative_data_scheduled, aes(x = month, y = count, fill = status)) +
  #   geom_area(position = "stack") +
  #   labs(title = "Cumulative Scheduled Projects Over Time", x = "Month", y = "Cumulative Projects") +
  #   theme_minimal() +
  #   scale_y_continuous(breaks = scales::pretty_breaks(), limits = c(0, NA))
  # 
  # p_interactive_scheduled <- ggplotly(p_scheduled)
  # 
  # output$cumulative_scheduled <- renderPlotly({
  #   p_interactive_scheduled %>%
  #     layout(
  #       title = list(
  #         text = "<b>Cumulative Scheduled Projects Over Time</b>",
  #         x = 0.5,
  #         font = list(color = "white", size = 16)  # Set title color to white
  #       ),
  #       xaxis = list(
  #         title = list(
  #           text = "Month",
  #           font = list(color = "white", size = 12)  # X-axis title in white
  #         ),
  #         tickfont = list(color = "white")  # X-axis tick labels in white
  #       ),
  #       yaxis = list(
  #         title = list(
  #           text = "Cumulative Projects",
  #           font = list(color = "white", size = 12)  # Y-axis title in white
  #         ),
  #         tickfont = list(color = "white")  # Y-axis tick labels in white
  #       ),
  #       legend = list(
  #         title = list(
  #           text = "<b>Status</b>",  # Legend title text
  #           font = list(color = "white", size = 12)  # Make "status" white
  #         ),
  #         font = list(color = "white", size = 12),  # Legend item text in white
  #         bgcolor = "rgba(0,0,0,0)",  # Transparent background for legend
  #         bordercolor = "rgba(0,0,0,0)"  # Transparent border for legend
  #       ),
  #       paper_bgcolor = "rgba(0,0,0,0)",  # Transparent background
  #       plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot area
  #       showlegend = TRUE
  #     )
  # })
  # 
  # 
 
  #----------------- Heat Map of Community Partners -----------------
  # Already got longtidue/latitude and saved them in the Google Sheets
  # Create a scatter plot on a map
  output$fig_location <- renderPlotly({
    plot_ly(
      data = address,
      type = 'scattermapbox',
      mode = 'markers',
      lon = ~Longitude,
      lat = ~Latitude,
      text = ~community_partner,  # Assuming you want to show community partner names on hover
      marker = list(size = 10, color = "red")
    ) %>%
      layout(title = list(
        text = "<b>Counts of Projects by Status</b>",
        x = 0.5,
        font = list(color = "white", size = 16)  # Set title color to white
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
        mapbox = list(
          style = "open-street-map",
          zoom = 12,
          center = list(
            lon = mean(address$Longitude, na.rm = TRUE),  # Corrected
            lat = mean(address$Latitude, na.rm = TRUE)    # Corrected
          )
        ),
        title = "Heat Map of Community Partners"
      )
  })
  
  # ----------------------------------- Download Plots to Save ----------------------------------------------
  #create a static version of Donut Chart
  library(ggplot2)
  
  # Static donut chart
  p_static_donut <- ggplot(project_counts, aes(x = 2, y = Count, fill = Status)) +
    geom_bar(stat = "identity", width = 1, color = "white") +  # Create the donut segments
    coord_polar(theta = "y") +  # Convert to polar coordinates for the circular shape
    scale_fill_manual(values = c(
      "Completed" = "#6baed6",
      "In Progress" = "#3182bd",
      "In Development" = "#9ecae1",
      "Scheduled" = "#deebf7"
    )) +  # Set the colors to match the interactive version
    geom_text(aes(label = paste0(round(Count / sum(Count) * 100, 1), "%")),
              position = position_stack(vjust = 0.5), color = "white", size = 5) +  # Add percentages
    theme_void() +  # Remove gridlines and background
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),  # Center the title
      legend.position = "none" # Match the black background
    ) +
    labs(title = "Counts of Projects by Status")
  
  
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste("plots_summary", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)  # Open a PDF device
      print(p_partner_barplot) # count of project for each community partner
      print(p) # stack plot (cumulative projects over time)
      print(p_histogram) # histogram (cumulative projects over time)
      print(p_static_donut)
      dev.off()  # Close the PDF device
    }
  )
  
  }

# Run the application 
shinyApp(ui = ui, server = server)
