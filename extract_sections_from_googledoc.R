#!/usr/bin/env Rscript
# HTML Section Extractor for Dissertation (R Version)
# Extracts specific sections from dissertation.html based on heading IDs or section names.

# Load required libraries
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

#' Extract a section from HTML file by section name
#' 
#' @param html_file Path to the HTML file
#' @param section_name The name of the section (e.g., 'Abstract', 'Introduction')
#' @param output_file Output file path (optional). If NULL, prints to console
#' @param clean_html Logical. If TRUE (default), creates clean HTML with styling. If FALSE, returns raw HTML as-is from the file.
#' @param verbose Logical. If TRUE (default), prints status messages. If FALSE, suppresses output.
#' @return The extracted HTML content as a character string
extract_section_by_name <- function(html_file, section_name, output_file = NULL, clean_html = TRUE, verbose = TRUE) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # Find the heading with the specified text (case insensitive)
  headings <- html_content %>% html_nodes("h1")
  
  heading <- NULL
  for (h in headings) {
    text <- h %>% html_text() %>% str_trim()
    if (str_detect(tolower(text), tolower(section_name))) {
      heading <- h
      break
    }
  }
  
  if (is.null(heading)) {
    stop(paste("Error: Section '", section_name, "' not found.", sep = ""))
  }
  
  # Get the section title
  section_title <- heading %>% html_text() %>% str_trim()
  
  # Find all content until the next h1 heading
  extracted_content <- extract_content_until_next_heading(html_content, heading)
  
  # Create output HTML based on clean_html parameter
  if (clean_html) {
    final_html <- create_clean_html(section_title, extracted_content)
  } else {
    # Return raw HTML as-is
    final_html <- extracted_content
  }
  
  # Save or print
  if (!is.null(output_file)) {
    writeLines(final_html, output_file, useBytes = TRUE)
    if (verbose) {
      cat("Section '", section_title, "' extracted to '", output_file, "'\n", sep = "")
    }
  } else {
    if (verbose) {
      cat(final_html)
    }
  }
  
  return(final_html)
}

#' List all available sections in the HTML file
#' 
#' @param html_file Path to the HTML file
list_available_sections <- function(html_file) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # Find all h1 headings
  headings <- html_content %>% html_nodes("h1")
  
  cat("Available sections:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (i in seq_along(headings)) {
    title <- headings[i] %>% html_text() %>% str_trim()
    heading_id <- headings[i] %>% html_attr("id")
    if (is.na(heading_id)) heading_id <- "No ID"
    
    cat(sprintf("%2d. %s\n", i, title))
    cat(sprintf("    ID: %s\n", heading_id))
    cat("\n")
  }
}

#' Extract content from a heading until the next h1 heading
#' 
#' @param html_content Parsed HTML content
#' @param start_heading The starting heading element
#' @return HTML content as a character string
extract_content_until_next_heading <- function(html_content, start_heading) {
  # Get all elements after the starting heading
  all_elements <- html_content %>% html_nodes("*")
  
  # Find the index of the starting heading
  start_index <- which(sapply(all_elements, function(x) identical(x, start_heading)))
  
  if (length(start_index) == 0) {
    return(as.character(start_heading))
  }
  
  # Find the next h1 heading
  end_index <- length(all_elements)
  for (i in (start_index + 1):length(all_elements)) {
    if (html_name(all_elements[i]) == "h1") {
      end_index <- i - 1
      break
    }
  }
  
  # Extract the content
  extracted_elements <- all_elements[start_index:end_index]
  extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
  
  return(extracted_content)
}

#' Create clean HTML with proper structure and styling
#' 
#' @param title Section title
#' @param content HTML content
#' @return Clean HTML as a character string
create_clean_html <- function(title, content) {
  # Parse the content to clean it up
  content_soup <- read_html(content)
  
  # Remove any existing style tags
  style_tags <- content_soup %>% html_nodes("style")
  for (style in style_tags) {
    xml_remove(style)
  }
  
  # Clean up the content
  clean_content <- as.character(content_soup)
  
  # Create the final HTML
  html_template <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s - The Life of Saint Mary the Egyptian</title>
    <style>
        body {
            font-family: "Times New Roman", serif;
            line-height: 1.6;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            background-color: #ffffff;
        }
        h1 {
            text-align: center;
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 30px;
            color: #000000;
        }
        h2 {
            font-size: 18px;
            font-weight: bold;
            margin-top: 25px;
            margin-bottom: 15px;
            color: #000000;
        }
        p {
            text-indent: 36pt;
            margin-bottom: 15px;
            font-size: 12pt;
            text-align: left;
        }
        .keywords {
            font-weight: bold;
            margin-top: 20px;
        }
        .keywords span {
            font-weight: normal;
        }
        em {
            font-style: italic;
        }
        sup {
            vertical-align: super;
            font-size: smaller;
        }
        table {
            border-collapse: collapse;
            margin: 20px 0;
            width: 100%%;
        }
        td {
            border: 1px solid #000;
            padding: 10px;
            vertical-align: top;
        }
    </style>
</head>
<body>
%s
</body>
</html>', title, clean_content)
  
  return(html_template)
}
