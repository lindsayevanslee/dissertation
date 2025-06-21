# Function to convert HTML to clean markdown
convert_html_to_markdown <- function(html_file) {
  if (!file.exists(html_file)) {
    stop(paste("HTML file '", html_file, "' not found.", sep = ""))
  }
  
  # Read HTML file with UTF-8 encoding
  html_content <- read_html(html_file, encoding = "UTF-8")
  
  # Remove sup tags (which contain comment references) before extracting text
  sup_tags <- html_content %>% html_nodes("sup")
  for (tag in sup_tags) {
    xml_remove(tag)
  }
  
  # Extract all text content from paragraphs
  paragraphs <- html_content %>% html_nodes("p")
  
  # Get text from each paragraph
  text_content <- character(0)
  for (p in paragraphs) {
    text <- p %>% html_text() %>% str_trim()
    if (text != "") {
      # Clean up any extra spaces
      text <- str_replace_all(text, "\\s+", " ")
      text <- str_trim(text)
      
      if (text != "") {
        text_content <- c(text_content, text)
      }
    }
  }
  
  # Combine into markdown
  markdown_content <- paste(text_content, collapse = "\n\n")
  
  # Clean up final content
  markdown_content <- str_replace_all(markdown_content, "\n{3,}", "\n\n")
  markdown_content <- str_trim(markdown_content)
  
  return(markdown_content)
}