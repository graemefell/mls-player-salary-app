library(shiny)
library(shinythemes)
library(DT)
library(openxlsx)
library(ggplot2)
library(ggiraph)
library(dplyr)
library(shinyWidgets)

# charts/styles
source("code/plots.R")

# data
app_start_time <- Sys.time()
data_table <- readRDS("data/salaries_table.rds") 
data_chart <- readRDS("data/salaries_charts.rds") 

# Custom CSS for buttons and selection
custom_css <- "
button.small-btn {
  font-size: 12px;
  padding: 2px 5px;
}

table.dataTable tbody td.selected, 
table.dataTable tbody tr.selected {
  background-color: #d3d3d3 !important;
  color: black !important;
}

table.dataTable tbody tr:hover {
  background-color: inherit !important;
}
"

ui <- navbarPage(
  title = "MLS Player Salaries",
  theme = shinytheme("flatly"),  # Choose a theme
  tabPanel(
    "Time-Series",
    sidebarLayout(
      sidebarPanel(
        h4(tags$b("Select Filters")),
        
        # Club Filter
        pickerInput(
          inputId = "club_filter_plot",
          label = "Club",
          choices = sort(unique(data_chart$Club)), # Alphabetical order
          selected = sort(unique(data_chart$Club)), # Default: All selected
          options = list(
            `actions-box` = TRUE,   # Select all/deselect all buttons
            `live-search` = TRUE,   # Search bar
            `size` = 10,             # Dropdown size with scroll
            `selectedTextFormat` = "static"
          ),
          multiple = TRUE
        ),
        tags$script(HTML("
    $(document).on('shiny:inputchanged', function(event) {
      var element, selected;

      // Handle club_filter_plot
      if (event.name === 'club_filter_plot') {
        selected = event.value;
         element = $('#club_filter_plot');
        if (selected && selected.length > 30) {
          element.next().find('.filter-option-inner-inner').text('All');
        } else if (selected && selected.length > 3) {
          element.next().find('.filter-option-inner-inner').text(selected.length + ' clubs selected');
        } else if (selected && selected.length < 1) {
          element.next().find('.filter-option-inner-inner').text('None selected');
        } else {
          element.next().find('.filter-option-inner-inner').text(selected.join(', '));
        }
      }

      // Handle position_filter_plot
      if (event.name === 'position_filter_plot') {
        selected = event.value;
        element = $('#position_filter_plot');
         if (selected && selected.length < 1) {
          element.next().find('.filter-option-inner-inner').text('None selected');
        } else if (selected && selected.length < 9)  {
          element.next().find('.filter-option-inner-inner').text(selected.join(', '));
        } else {
          element.next().find('.filter-option-inner-inner').text('All');
        }
      }
    });
")),
        
        # Position Filter
        pickerInput(
          inputId = "position_filter_plot",
          label = "Position",
          choices = c("GK", "D", "D-M", "M-D", "M", "M-F", "F-M", "F", "UNK"), # Custom order
          selected = c("GK", "D", "D-M", "M-D", "M", "M-F", "F-M", "F", "UNK"), # Default: All selected
          options = list(
            `actions-box` = TRUE,   # Select all/deselect all buttons
            `live-search` = FALSE,  # No search bar for positions
            `size` = 7              # Dropdown size with scroll
          ),
          multiple = TRUE
        ),
        
        
        # Year Range Slicer
        sliderInput(
          inputId = "year_range_plot",
          label = "Year Range",
          min = min(data_chart$Year, na.rm = TRUE),
          max = max(data_chart$Year, na.rm = TRUE),
          value = c(min(data_chart$Year, na.rm = TRUE), max(data_chart$Year, na.rm = TRUE)),
          step = 1,
          sep = ""
        )
      ),
      mainPanel(
        girafeOutput("time_series_plot")
      )
    )
  ),
  tabPanel(
    "Data",
    sidebarLayout(
      sidebarPanel(
        h4(tags$b("Select Filters")),
        
        # Club Filter as Dropdown
        h5(tags$b("Club")),
        pickerInput(
          inputId = "club_filter",
          #label = "Select Club(s):",
          choices = sort(unique(data_table$Club)),  # Alphabetical order
          selected = sort(unique(data_table$Club)), # Default: All selected
          options = list(
            `actions-box` = TRUE,   # Select all/deselect all buttons
            `live-search` = TRUE,   # Search bar
            `size` = 10              # Dropdown size with scroll
          ),
          multiple = TRUE
        ),
        
        tags$script(HTML("
    $(document).on('shiny:inputchanged', function(event) {
      var element, selected;

      // Handle club_filter
      if (event.name === 'club_filter') {
        selected = event.value;
        element = $('#club_filter');
        if (selected && selected.length > 30) {
          element.next().find('.filter-option-inner-inner').text('All');
        } else if (selected && selected.length > 3) {
          element.next().find('.filter-option-inner-inner').text(selected.length + ' clubs selected');
        } else if (selected && selected.length < 1) {
          element.next().find('.filter-option-inner-inner').text('None selected');
        } else {
          element.next().find('.filter-option-inner-inner').text(selected.join(', '));
        }
      }

      // Handle position_filter
      if (event.name === 'position_filter') {
        selected = event.value;
        element = $('#position_filter');
        if (selected && selected.length < 1) {
          element.next().find('.filter-option-inner-inner').text('None selected');
        } else if (selected && selected.length < 9)  {
          element.next().find('.filter-option-inner-inner').text(selected.join(', '));
        } else {
          element.next().find('.filter-option-inner-inner').text('All');
        }
      }
    });
")),
        
        # Position Filter as Dropdown
        h5(tags$b("Position")),
        pickerInput(
          inputId = "position_filter",
          #label = "Select Position(s):",
          choices = c("GK", "D", "D-M", "M-D", "M", "M-F", "F-M", "F", "UNK"), # Custom order
          selected = c("GK", "D", "D-M", "M-D", "M", "M-F", "F-M", "F", "UNK"), # Default: All selected
          options = list(
            `actions-box` = TRUE,   # Select all/deselect all buttons
            `live-search` = FALSE,  # No search bar for positions
            `size` = 7              # Dropdown size with scroll
          ),
          multiple = TRUE
        ),
        
        # Year Filter as Dropdown
        h5(tags$b("Year")),
        pickerInput(
          inputId = "year_filter",
         # label = "Select Year(s):",
          choices = sort(as.numeric(unique(data_table$Year)), decreasing = TRUE),  # Descending order
          selected = sort(as.numeric(unique(data_table$Year)), decreasing = TRUE), 
          options = list(
            `actions-box` = TRUE,   # Select all/deselect all buttons
            `live-search` = TRUE,   # Search bar
            `size` = 10             # Dropdown size with scroll
          ),
          multiple = TRUE
        ),
        
        actionButton("generate", "Generate Table"),
        br(), br(),
        downloadButton("export_csv", "Export CSV"),
        downloadButton("export_xlsx", "Export XLSX")
      ),
      mainPanel(
        DTOutput("table")
      )
    )
  ),
  tabPanel(
    "Notes",
    fluidPage(
      h4(tags$b("Data Sources")),
      p("This application uses data sourced from the latest available MLS player salary datasets at 
        https://mlsplayers.org."),
      br(),
      h4(tags$b("Pages")),
      p("To visualize total guaranteed compensation by club by year, use the Time-Series page. To 
        view and export tabular data of player salaries, use the Data page."),
      br(),
      h4(tags$b("Latest Refresh")),
      p(paste("The data was last refreshed on:", format(app_start_time, "%Y-%m-%d %H:%M:%S")))
    )
  )
)


# Server
server <- function(input, output, session) {
  # Reactive values for filters
  selected_filters <- reactiveValues(
    club = NULL,
    position = NULL,
    year = NULL,
    team = NULL
  )
  
  # Update selected filters based on input
  observe({
    selected_filters$club <- input$club_filter
    selected_filters$position <- input$position_filter
    selected_filters$year <- input$year_filter
  })
  
  # Filter Data
  filtered_data <- reactive({
    if (is.null(input$generate) || input$generate == 0) {
      # If 'Generate Table' has not been clicked, show the entire dataset
      data_table %>%
        arrange(Club, desc(Year), Name)
    } else {
      # Apply the filters once 'Generate Table' is clicked
      isolate({
        data_table %>%
          filter(
            Club %in% selected_filters$club,
            Position %in% selected_filters$position,
            Year %in% selected_filters$year
          ) %>%
          arrange(Club, desc(Year), Name)
      })
    }
  })
  
  # Render Data Table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # Render Time-Series Plot
  output$time_series_plot <- renderGirafe({
    req(input$club_filter_plot, input$position_filter_plot, input$year_range_plot)
    
    # Generate the dynamic title inline
    all_clubs_selected <- setequal(input$club_filter_plot, unique(data_chart$Club))
    all_positions_selected <- setequal(input$position_filter_plot, unique(data_chart$Position))
    year_range <- input$year_range_plot
    title <- paste0("Guaranteed Compensation by Club, ", year_range[1], "-", year_range[2])
    if (!all_clubs_selected || !all_positions_selected) {
      title <- paste0("Guaranteed Compensation by Club, with selected filters, ", year_range[1], "-", year_range[2])
    }
    
    filtered_chart_data <- data_chart %>%
      filter(
        Club %in% input$club_filter_plot,
        Position %in% input$position_filter_plot,
        Year >= input$year_range_plot[1],
        Year <= input$year_range_plot[2]
      ) %>%
      group_by(Club, Year) %>%
      summarize(SumSalary = sum(`Guaranteed Comp`, na.rm = TRUE), .groups = "drop")
    
    y_max <- max(filtered_chart_data$SumSalary, na.rm = TRUE) * 1.1
    
    plot <- ggplot(filtered_chart_data, aes(x = Year, y = SumSalary / 1e6, group = Club, color = Club)) +
      geom_line_interactive(
        aes(
          tooltip = paste0(Club, "<br>"),
          data_id = Club
        ),
        size = 0.7
      ) +
      geom_point_interactive(
        aes(
          tooltip = paste0(Year, "<br>",
                          Club, "<br>", 
                          scales::dollar(SumSalary))
        ),
        size = 3
      ) +
      labs(
        title = title,
        x = "Year",
        y = "Guaranteed Comp (millions USD)"
      ) +
      scale_color_manual(values = club_colors) +
      scale_y_continuous(
        limits = c(0, y_max / 1e6),
        breaks = seq(0, y_max / 1e6, by = ifelse(y_max / 1e6 < 10, 2, 5)),
        expand = c(0,0)
      ) +
      scale_x_continuous(
        limits = c(input$year_range_plot[1], input$year_range_plot[2]),
        breaks = seq(input$year_range_plot[1], input$year_range_plot[2], by = 1)
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face="bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", size = 0.5),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
      )
    
    girafe(
      ggobj = plot,
      options = list(
        opts_hover(css = "stroke-width: 3px; opacity: 1;"),
        opts_selection(type = "none")
      ),
    width_svg = 10, height_svg = 7.5)
  })
  
  # Export CSV
  output$export_csv <- downloadHandler(
    filename = function() { "data_export.csv" },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Export XLSX
  output$export_xlsx <- downloadHandler(
    filename = function() { "data_export.xlsx" },
    content = function(file) {
      write.xlsx(filtered_data(), file)
    }
  )
}

# Run the App
shinyApp(ui = ui, server = server)