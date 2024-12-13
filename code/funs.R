#helper functions

# scrape main page for latest data----------------------------------------------

load_main_data <- function(){
  url <- "https://mlsplayers.org/resources/salary-guide"
  webpage <- read_html(url)
  table <- webpage %>%
    html_node("table") %>%  
    html_table(fill = TRUE)
  
  # Remove any rows with missing data
  table <- na.omit(table)
  
  # Convert relevant columns to appropriate data types
  latest_salaries <- table %>%
    mutate(
      `Base Salary` = as.numeric(gsub("[\\$,]", "", `Base Salary`)),
      `Guaranteed Compensation` = as.numeric(gsub("[\\$,]", "", `Guaranteed Compensation`))
    )
  return(latest_salaries)
  
}

# scrape all pdf links from mlsplayers site ------------------------------------

get_links <- function(){
  url <- "https://mlsplayers.org/resources/salary-guide"
  # Read the webpage
  webpage <- read_html(url)
  # Extract all links
  links <- webpage %>%
    html_nodes("a") %>%  # Select all <a> (anchor) tags
    html_attr("href")    # Extract the 'href' attribute (the link)
  # Remove any NA values or duplicates
  links <- links[!is.na(links)]
  links <- unique(links)
  #salary pages
  filtered_links <- links[grepl("^http://s3.amazonaws.com/mlspa", links)]
  #pdfs only
  filtered_links <- filtered_links[!grepl("\\.csv\\?", filtered_links)]
  return(filtered_links)
}


# scrape a pdf from mlsplayers site and convert to tidy table ------------------

#helper for line
parse_line <- function(line) {
  sapply(seq_along(col_positions), function(i) {
    start <- col_positions[i]
    end <- if (i < length(col_positions)) col_positions[i + 1] - 1 else nchar(line)
    substring(line, start, end) %>% trimws() # Extract and trim whitespace
  })
}

#helper for page
process_page <- function(page_text) {
  # Pre-parse: Split into lines and filter out irrelevant lines
  lines <- strsplit(page_text, "\n")[[1]]
  lines <- lines[!sapply(lines, function(line){ nchar(trimws(line)) < 5 || grepl("2022 Fall Salary Guide", line, ignore.case = TRUE)
  })]
  first_row <- lines.f[1]
  col_positions <- sapply(column_titles, function(title) {
    gregexpr(title, first_row)[[1]][1]
  })
  lines <- lines[-1]
  # Parse: Extract data into columns (you should define the `parse_line` function separately)
  parsed_lines <- do.call(rbind, lapply(lines, parse_line))
  table_df <- as.data.frame(parsed_lines, stringsAsFactors = FALSE)
  
  # Set column names
  colnames(table_df) <- c("Team", "First Name ", "Last Name", "Base Salary", "Guaranteed Comp", "Position")
  table_df <- table_df %>% mutate(Year = "2023")
  
  return(table_df)
}
process_url <- function(url){
  url <- "http://s3.amazonaws.com/mlspa/2023-Salary-Report-as-of-Sept-15-2023.pdf?mtime=20231018173909"
  # Extract year
  base_pattern <- "http://s3.amazonaws.com/mlspa/"
  match <- sub(base_pattern, "", url, fixed=TRUE) # Remove the base URL
  year <- substr(match, 1, 4) # Take the first 4 characters
  if(year=="Sala"){
    year = "2024"
  }

  # Get pdf file
  pdf_file <- year
  download.file(url, pdf_file, mode = "wb")
  
  # Extract text from the PDF
  pdf_text_data <- pdf_text(pdf_file)
  
  # Column names
  column_titles <- case_when(
    year == "2022" ~ c("Club", "Last Name", "First Name", "Position", "2022 Base Salary", "2022 Guar. Comp."),
    year == "2023" ~ c("Team", "First Name ", "Last Name", "2023 Base Salary", "2023 Guaranteed Comp", "Position"),
    year == "2024" ~ c("First Name", "Last Name", "Team Contract", "Main Position", "Base Salary", "Guaranteed Comp"),
    TRUE ~ c("Club", "Last Name", "First Name", "Position", "2022 Base Salary", "2022 Guar. Comp.")
  )
  
  #following exampel is for page [1]
  all_data <- map(pdf_text_data, process_page) %>% 
    bind_rows() 
  
return(all_data)
}

#
# 
# lines <- strsplit(page_text, "\n")[[3]]
# lines
# lines <- lines[!sapply(lines, function(line){ nchar(trimws(line)) < 5 || grepl("2022 Fall Salary Guide", line, ignore.case = TRUE)
# })]
# first_row <- lines.f[1]
# col_positions <- sapply(column_titles, function(title) {
#   gregexpr(title, first_row)[[1]][1]
# })

