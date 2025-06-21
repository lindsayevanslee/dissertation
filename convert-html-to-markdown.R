# Function to convert HTML to clean markdown with formatting using XML parsing
convert_html_to_markdown <- function(html_file) {
  if (!file.exists(html_file)) {
    stop(paste("HTML file '", html_file, "' not found.", sep = ""))
  }
  
  library(xml2)
  library(stringr)
  
  # Read HTML file with UTF-8 encoding
  html_content <- read_html(html_file, encoding = "UTF-8")
  
  # Helper: is italic style?
  is_italic <- function(node) {
    style <- xml_attr(node, "style")
    !is.na(style) && str_detect(style, "font-style:italic")
  }
  # Helper: is superscript style?
  is_super <- function(node) {
    style <- xml_attr(node, "style")
    !is.na(style) && str_detect(style, "vertical-align:super")
  }
  # Convert a node to markdown, but don't wrap italics here
  to_md_leaf <- function(node) {
    style <- xml_attr(node, "style")
    text <- xml_text(node)
    if (!is.na(style)) {
      if (str_detect(style, "vertical-align:super")) {
        return(paste0("^", text, "^"))
      }
    }
    return(text)
  }
  # Main function: group adjacent italics, keep superscript inside
  convert_paragraph <- function(p) {
    children <- xml_children(p)
    out <- character(0)
    i <- 1
    while (i <= length(children)) {
      node <- children[[i]]
      
      # Check if this is a sup element with footnote
      if (xml_name(node) == "sup") {
        # Look for footnote link inside sup
        footnote_link <- xml_find_first(node, ".//a[contains(@href, '#ftnt')]")
        if (!is.na(footnote_link)) {
          # Extract footnote number from href or text
          href <- xml_attr(footnote_link, "href")
          footnote_num <- str_extract(href, "\\d+")
          if (is.na(footnote_num)) {
            # Fallback to text content
            footnote_text <- xml_text(footnote_link)
            footnote_num <- str_extract(footnote_text, "\\d+")
          }
          if (!is.na(footnote_num)) {
            out <- c(out, paste0("[^", footnote_num, "]"))
            i <- i + 1
            next
          }
        }
      }
      
      if (is_italic(node)) {
        # Start italic group
        italic_group <- character(0)
        while (i <= length(children) && is_italic(children[[i]])) {
          n <- children[[i]]
          # If also superscript, convert to ^text^
          if (is_super(n)) {
            italic_group <- c(italic_group, paste0("^", xml_text(n), "^"))
          } else {
            italic_group <- c(italic_group, xml_text(n))
          }
          i <- i + 1
        }
        out <- c(out, paste0("*", paste0(italic_group, collapse=""), "*"))
      } else {
        # Not italic
        if (is_super(node)) {
          out <- c(out, paste0("^", xml_text(node), "^"))
        } else {
          out <- c(out, xml_text(node))
        }
        i <- i + 1
      }
    }
    paste0(out, collapse = "")
  }
  
  # Function to convert heading to markdown
  convert_heading <- function(heading) {
    level <- as.numeric(str_extract(xml_name(heading), "\\d+"))
    if (is.na(level)) level <- 1
    
    # Use the same logic as convert_paragraph to extract heading text
    children <- xml_children(heading)
    if (length(children) == 0) {
      heading_text <- xml_text(heading)
    } else {
      heading_text <- convert_paragraph(heading)
    }
    heading_text <- str_squish(heading_text)
    
    # Create markdown heading with appropriate number of #
    paste0(paste(rep("#", level), collapse = ""), " ", heading_text)
  }
  
  # Process all elements in document order
  text_content <- character(0)
  footnote_definitions <- character(0)
  
  # Get all elements in document order
  all_elements <- xml_find_all(html_content, ".//p | .//h1 | .//h2 | .//h3 | .//h4 | .//h5 | .//h6")
  
  # Track if we've seen the first h1
  first_h1_skipped <- FALSE
  
  for (element in all_elements) {
    element_name <- xml_name(element)
    
    if (element_name == "p") {
      # Process paragraph
      md <- convert_paragraph(element)
      md <- str_squish(md)
      if (md != "") {
        text_content <- c(text_content, md)
      }
    } else if (str_detect(element_name, "^h\\d+$")) {
      # Process heading
      if (element_name == "h1" && !first_h1_skipped) {
        # Skip the first h1
        first_h1_skipped <- TRUE
        next
      }
      md <- convert_heading(element)
      if (md != "") {
        text_content <- c(text_content, md)
      }
    }
  }
  
  # Look for footnote definitions in the HTML
  # Only look for footnote definitions that were extracted by the section extraction function
  footnote_def_divs <- xml_find_all(html_content, ".//div[contains(@id, '_def')]")
  for (def_div in footnote_def_divs) {
    footnote_id <- xml_attr(def_div, "id")
    # Only process if it's a footnote definition (starts with ftnt and ends with _def)
    if (str_detect(footnote_id, "^ftnt\\d+_def$")) {
      footnote_num <- str_extract(footnote_id, "\\d+")
      if (!is.na(footnote_num)) {
        # Extract content from the footnote definition div
        footnote_content <- xml_text(def_div)
        # Remove the footnote number prefix if present
        footnote_content <- str_replace(footnote_content, paste0("^\\[", footnote_num, "\\]\\s*"), "")
        footnote_content <- str_squish(footnote_content)
        
        # Remove Google Docs comment references like [as], [at], [e], etc. (robust)
        footnote_content <- str_replace_all(footnote_content, "\\[[a-zA-Z]{1,3}\\]", "")
        footnote_content <- str_squish(footnote_content)
        
        footnote_definitions <- c(footnote_definitions, paste0("[^", footnote_num, "]: ", footnote_content))
      }
    }
  }
  
  # Deduplicate footnote definitions before appending
  footnote_definitions <- unique(footnote_definitions)
  # Combine into markdown
  markdown_content <- paste(text_content, collapse = "\n\n")
  # Add footnote definitions at the end
  if (length(footnote_definitions) > 0) {
    markdown_content <- paste0(markdown_content, "\n\n", paste(footnote_definitions, collapse = "\n"))
  }
  
  return(markdown_content)
}