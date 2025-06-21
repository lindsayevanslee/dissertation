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
  
  # Extract all paragraphs
  paragraphs <- xml_find_all(html_content, ".//p")
  text_content <- character(0)
  for (p in paragraphs) {
    md <- convert_paragraph(p)
    md <- str_squish(md)
    if (md != "") {
      text_content <- c(text_content, md)
    }
  }
  
  # Combine into markdown
  markdown_content <- paste(text_content, collapse = "\n\n")
  
  # Add simple footnote definitions at the end (if needed)
  footnote_refs <- unique(str_extract_all(markdown_content, "\\[\\^([a-z])\\]")[[1]])
  if (length(footnote_refs) > 0) {
    footnote_definitions <- character(0)
    for (ref in footnote_refs) {
      ref_letter <- str_extract(ref, "[a-z]")
      footnote_definitions <- c(footnote_definitions, paste0("[^", ref_letter, "]: ", "Footnote ", ref_letter))
    }
    markdown_content <- paste0(markdown_content, "\n\n", paste(footnote_definitions, collapse = "\n"))
  }
  
  return(markdown_content)
}