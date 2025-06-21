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

#' Extract content from a heading until the next h1 heading, including footnotes
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
  
  # Extract the main content
  extracted_elements <- all_elements[start_index:end_index]
  
  # Deduplicate content to prevent the same text from appearing multiple times
  seen_content <- character(0)
  unique_elements <- list()
  
  for (element in extracted_elements) {
    element_text <- xml_text(element)
    element_text_clean <- str_squish(element_text)
    
    # Skip empty elements
    if (element_text_clean == "") {
      next
    }
    
    # Normalize the text for better comparison (remove extra spaces, normalize quotes, etc.)
    element_text_normalized <- str_replace_all(element_text_clean, "\\s+", " ")
    element_text_normalized <- str_trim(element_text_normalized)
    
    # Check if we've seen this content before (using normalized text)
    if (!(element_text_normalized %in% seen_content)) {
      seen_content <- c(seen_content, element_text_normalized)
      unique_elements <- c(unique_elements, list(element))
    }
  }
  
  extracted_content <- paste(sapply(unique_elements, as.character), collapse = "\n")
  
  # Find footnote references in the extracted content
  footnote_refs <- character(0)
  for (element in extracted_elements) {
    # Look for sup elements with footnote links
    footnote_links <- element %>% html_nodes("sup a[href*='#ftnt']")
    for (link in footnote_links) {
      href <- html_attr(link, "href")
      footnote_num <- stringr::str_extract(href, "\\d+")
      if (!is.na(footnote_num)) {
        footnote_refs <- c(footnote_refs, footnote_num)
      }
    }
  }
  
  # Remove duplicates and sort
  footnote_refs <- unique(sort(as.numeric(footnote_refs)))
  
  # Extract footnote definitions for the referenced footnotes
  if (length(footnote_refs) > 0) {
    footnote_definitions <- character(0)
    
    for (footnote_num in footnote_refs) {
      # Look for footnote definition in the full document
      footnote_anchor <- html_content %>% html_nodes(paste0("a[id='ftnt", footnote_num, "']"))
      
      if (length(footnote_anchor) > 0) {
        # Get the footnote content - it's in the parent div after the anchor
        footnote_element <- footnote_anchor[[1]]
        
        # Get the parent div that contains the footnote
        parent_div <- xml_parent(footnote_element)
        
        if (!is.na(parent_div)) {
          # Get all children of the parent div
          children <- xml_children(parent_div)
          
          # Find the anchor's position
          anchor_pos <- which(sapply(children, function(x) identical(x, footnote_element)))
          
          if (length(anchor_pos) > 0 && anchor_pos[1] < length(children)) {
            # Get ALL elements after the anchor (not just the first one)
            footnote_content_elements <- children[(anchor_pos[1] + 1):length(children)]
            
            # Convert footnote content to HTML with preserved formatting
            footnote_html_content <- paste(sapply(footnote_content_elements, as.character), collapse = "")
            
            # Remove Google Docs comment references like [as], [at], [e], etc. (robust)
            footnote_html_content <- str_replace_all(footnote_html_content, "\\[[a-zA-Z]{1,3}\\]", "")
            
            # Create a simple footnote definition with preserved HTML formatting
            footnote_def <- paste0('<div class="footnote" id="ftnt', footnote_num, '_def">',
                                 '<sup>[', footnote_num, ']</sup> ', footnote_html_content, '</div>')
            footnote_definitions <- c(footnote_definitions, footnote_def)
          }
        }
      }
    }
    
    # Add footnote definitions to the extracted content
    if (length(footnote_definitions) > 0) {
      extracted_content <- paste0(extracted_content, "\n\n<div class='footnotes'>\n",
                                 paste(footnote_definitions, collapse = "\n"), "\n</div>")
    }
  }
  
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
