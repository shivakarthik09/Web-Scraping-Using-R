##########################################################################################################################################################
## Preliminary: Please select and run each subtask for best results.
##########################################################################################################################################################


install.packages("rvest")
install.packages("httr")
install.packages("xml2")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("forcats")

##########################################################################################################################################################
## TASK - 1 : Loading the libraries
##########################################################################################################################################################


# Load libraries
library(rvest)
library(xml2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)

##########################################################################################################################################################
## TASK - 2 : Scraping Article Data
##########################################################################################################################################################



##################################################################
# Subtask - 1 : Extracting Article data from the article webpage.
# Logic: Extract data from article using css selectors with html_nodes` and `html_text` functions. 
#        Each required column is structured differently in the website, hence the different paths for different columns.
#.       For example the css selector path for title is different than the path for Author or PublishDate.
##################################################################
extract_article_info <- function(url) {

  # Read the HTML content of the article
  webpage <- read_html(url)
  
  # Extract the article title
  Title <- webpage %>% html_nodes("#main-content > main > article > div.c-article-header > h1") %>% html_text()

  
  # Extract the authors
  authxpath <- "#Aff1 > p.c-article-author-affiliation__authors-list"
  authors <- webpage %>% html_nodes(authxpath) %>% html_text()
  
  # Extract the Corresponding authors
  author_paragraph <- webpage %>% html_node("#corresponding-author-list")
  
  if (!is.null(author_paragraph)) {
    # Extract the <a> elements within the <p> element
    author_links <- author_paragraph %>% html_nodes("a")
    
    # Extract the names of the corresponding authors
    author_names <- html_text(author_links)
    
    author_emails <- author_links %>% html_attr("href")
    author_emails <- gsub("mailto:", "", author_emails)
    
    # Print the extracted author names
    corresp_authors <- author_names
    corresp_emails <- author_emails
  } else {
    cat("Corresponding author element not found in the HTML file.\n")
    corresp_authors <- NA
    corresp_emails <- NA
  }
  
  # Extract the publish date
  publish_Date <- webpage %>% html_nodes("#main-content > main > article > div.c-article-header > ul.c-article-identifiers > li:nth-child(3) > a > time") %>% html_text()
  
  # Extract the Abstract
  Abstract <- webpage %>% html_nodes("#Abs1-content > p") %>% html_text()
  
  # Extract keywords
  css_selector_keywords <- "#article-info-content > div > div > ul.c-article-subject-list"
  keywords_element <- webpage %>% html_node(css_selector_keywords)
  keywords_vector <- character(0)
  if (!is.null(keywords_element)) {
    # Extract the <span> elements within the <ul> element
    keyword_spans <- keywords_element %>% html_nodes("span")
    # Print the extracted keywords
    keywords_vector <- html_text(keyword_spans)
  } else {
    cat("Keywords element not found in the HTML file.\n")
  }
  
  
  return(list(
    Title = Title,
    Authors = authors,
    CorrespondingAuthors = paste(corresp_authors, collapse = ", "),
    CorrespondingAuthorsEmails = paste(corresp_emails, collapse =", "),
    PublishDate = publish_Date,
    Abstract = Abstract,
    Keywords = paste(keywords_vector, collapse = ", ")
  ))

}

##################################################################
# Subtask - 2 : Scrape articles for a given volume (year)
# Logic: Run the extract_articke_info function accross all volumes and all articles. 
#.       Basically the extract_article_info function runs for any one singular article.
#.       This scrape_articles_by_volume function will iterate the extract_article_info function over a whole volume of articles.
##################################################################

scrape_articles_by_volume <- function(volume) {
  
  # Specify the base URL for articles
  base_url <- "https://translationalneurodegeneration.biomedcentral.com"
  
  # Initialize a list to store article information
  article_info_list <- list()
  
  # Construct the URL for the page with articles for the specified volume (year)
  url <- paste0(base_url, "/articles?query=&volume=", volume, "&searchType=&tab=keyword")
  
  # Read the HTML content of the page
  webpage <- read_html(url)
  
  # Check if there is pagination, i.e., multiple pages
  has_pagination <- length(webpage %>% html_nodes("#main-content > div > main > div:nth-child(3) > nav > ul > li")) > 3
  
  # If there is pagination, extract the total number of pages
  if (has_pagination) {
    total_pages <- length(webpage %>% html_nodes("#main-content > div > main > div:nth-child(3) > nav > ul > li") %>% html_text()) -2
  } else {
    total_pages <- 1
  }
  
  
  # Loop through pages of articles for the specified volume (year)
  for (page in 1:total_pages) {
    # Construct the URL for the current page
    url <- paste0(base_url, "/articles?query=&volume=", volume, "&searchType=&tab=keyword&page=",page)
    
    # Read the HTML content of the page
    webpage <- read_html(url)
    
    # Extract the URLs of individual articles on the page
    article_links <- webpage %>% html_nodes("#main-content > div > main > div:nth-child(3) > ol > li article h3 a") %>% html_attr("href")
    
    
    # Extract article information for each article URL and store it in the list
    article_info <- lapply(article_links, function(article_link) {
      full_article_url <- paste0(base_url, article_link)
      return(extract_article_info(full_article_url))
    })
    
    # Add the article information to the list
    article_info_list <- c(article_info_list, article_info)
  }
  
  return(article_info_list)
}

##################################################################
# Subtask - 3 : Extract article data i.e., when year is given as an input, output will be the extracted article data of that year.
# Logic: Given an input year, the start_proj function outputs the data of all the articles in that year. Calls scrape_articles_by_volume on a particular year.
##################################################################

start_proj <- function() {
  volume <- as.character(readline("Enter the year (between 2012-2023, both inclusive) to extract articles from: "))
  
  if (volume < 2012 || volume > 2023) {
    print("Enter again")
    start_proj()
  } else {
    volume <- as.integer(volume)
    volume <- volume - 2011
    article_info <- scrape_articles_by_volume(as.character(volume))
    
    # Print the extracted details for each article
    for (info in article_info) {
      cat("Title:", info$Title, "\n",
          "Authors:", info$Authors, "\n",
          "Corresponding Authors:", info$CorrespondingAuthors, "\n",
          "Corresponding Authors emails:", info$CorrespondingAuthorsEmails, "\n",
          "Publish date:", info$PublishDate, "\n",
          "Abstract:", info$Abstract, "\n",
          "Keywords:", info$Keywords, "\n\n")
    }
  }
}

start_proj()


##################################################################
# Subtask - 4 : Verify if the number of articles scraped is correct. Return the total number of articles.
# Logic: The dataset builder function that goes through all volumes, and in each volume, goes through all articles. 
#        Also prints the cumulative number of articles scraped each year.
##################################################################

coun <- 0
for (i in 2012:2023) {
  volume <- i
  volume <- volume - 2011
  article_info <- scrape_articles_by_volume(as.character(volume))
  
  for (info in article_info) {
    cat("Title:", info$Title, "\n",
        "Authors:", info$Authors, "\n",
        "Corresponding Authors:", info$CorrespondingAuthors, "\n",
        "Corresponding Authors emails:", info$CorrespondingAuthorsEmails, "\n",
        "Publish date:", info$PublishDate, "\n",
        "Abstract:", info$Abstract, "\n",
        "Keywords:", info$Keywords, "\n\n")
  }
  
  print(i)
  print(length(article_info))
  coun <- coun + length(article_info)
  print(coun)
}


##########################################################################################################################################################
## TASK - 3 : Data Cleaning and Preprocessing
##########################################################################################################################################################

##################################################################
#Subtask - 1 : Building the dataset - Create an empty dataframe to store the article information
##################################################################

articles_df <- data.frame(
  Title = character(),
  Authors = character(),
  CorrespondingAuthors = character(),
  CorrespondingAuthorsEmails = character(),
  PublishDate = character(),
  Abstract = character(),
  Keywords = character(),
  stringsAsFactors = FALSE
)

##################################################################
# Subtask - 2 : Filling the empty dataframe with article data, year by year.
# Logic: If a particluar field is empty, do not leave it empty, rather fill it with "NA" value.
#.       Else add the field to the appropriate column.
#        Had to do some error handling to identify the reason for empty values, hence the try-catch block.
##################################################################


for (i in 2012:2023) {
  volume <- i
  volume <- volume - 2011
  
  tryCatch({
    article_info <- scrape_articles_by_volume(as.character(volume))
    
    for (info in article_info) {
      if (length(info$Title) > 0) {  # Check if the article info is not empty
        # Replace empty fields with "NA" or NA
        if (length(info$Title) == 0) info$Title <- "NA"
        if (length(info$Authors) == 0) info$Authors <- "NA"
        if (length(info$CorrespondingAuthors) == 0) info$CorrespondingAuthors <- "NA"
        if (length(info$CorrespondingAuthorsEmails) == 0) info$CorrespondingAuthorsEmails <- "NA"
        if (length(info$PublishDate) == 0) info$PublishDate <- "NA"
        if (length(info$Abstract) == 0) info$Abstract <- "NA"
        if (length(info$Keywords) == 0) info$Keywords <- "NA"
        
        temp_df <- data.frame(
          Title = info$Title,
          Authors = info$Authors,
          CorrespondingAuthors = info$CorrespondingAuthors,
          CorrespondingAuthorsEmails = info$CorrespondingAuthorsEmails,
          PublishDate = info$PublishDate,
          Abstract = info$Abstract,
          Keywords = info$Keywords,
          stringsAsFactors = FALSE
        )
        
        articles_df <- rbind(articles_df, temp_df)
      }
    }
  }, error = function(e) {
    cat("Error in processing volume", i, ": ", e$message, "\n")
  })
}


print(articles_df)

##################################################################
#Subtask - 3 : Cleaning the dataset to remove duplicates.
##################################################################

# Check for duplicates based on Title
duplicate_titles <- duplicated(articles_df$Title)

# Identify rows to keep (first occurrences of duplicates)
rows_to_keep <- !duplicate_titles

# Filter the dataframe to keep rows where Title is not a duplicate
nonduplicate_articles_df <- articles_df[rows_to_keep, ]

# Show the nonduplicate dataframe
head(nonduplicate_articles_df)

# write the dataframe to csv file
write.csv(nonduplicate_articles_df, "Non_DuplicateRecords.csv", row.names = FALSE)
##################################################################
#Subtask - 5 : Cleaning the dataset to remove records with NA values.
# Logic: If there are any NA values, print where are they located. If NA values present, remove the whole record.
##################################################################

# Convert string "NA" to actual NA (missing value)
nonduplicate_articles_df[nonduplicate_articles_df == "NA"] <- NA

# Check for NA values
any_na <- any(is.na(nonduplicate_articles_df))

if (any_na) {
  # If NA values exist, identify which columns contain NA values
  na_columns <- colnames(nonduplicate_articles_df)[colSums(is.na(nonduplicate_articles_df)) > 0]
  
  # Display the columns with NA values
  print("Columns with NA values:")
  print(na_columns)
  
  # Show rows and columns with NA values
  print("Rows and columns with NA values:")
  print(which(is.na(nonduplicate_articles_df), arr.ind = TRUE))
} else {
  print("No NA values in the dataframe.")
}

# Remove rows with NA values
cleaned_project_dataframe <- nonduplicate_articles_df[complete.cases(nonduplicate_articles_df), ]

# write the dataframe to csv file
write.csv(cleaned_project_dataframe, "CleanedRecords.csv", row.names = FALSE)

##################################################################
#Subtask - 6 : Pre-processing the dataset to modify date format to YYYY-MM-DD.
##################################################################

# Convert 'Publish_Date' to 'YYYY-MM-DD' format
cleaned_project_dataframe$PublishDate <- as.Date(cleaned_project_dataframe$PublishDate, format = "%d %B %Y")

cleaned_project_dataframe$PublishDate <- format(cleaned_project_dataframe$PublishDate, "%Y-%m-%d")

# Show the cleaned dataframe
head(cleaned_project_dataframe)


# load the dataframe into a new dataframe for convenience
preprocessed_articles_df <- cleaned_project_dataframe

# write the dataframe to csv file
write.csv(preprocessed_articles_df, "PreprocessedRecords.csv", row.names = FALSE)

##########################################################################################################################################################
## TASK - 4 : Data Analysis and Visualization
#Logic: Separate the keywords based on "," separator. If there's any whitespace, remove it too.
#.      Build a list of keywords, group the whole column by keywords and count the number of times each keyword occured.
#.      Count the top occurences using top_n function
#       Re-order the keywords in ascending order.
#       Create a barchart showing the frequency of top keywords.
##########################################################################################################################################################


# Split the 'Keywords' column into a list of keywords and clean up whitespace
viz_project_dataframe <- preprocessed_articles_df %>% mutate(Keywords = str_squish(Keywords)) %>% tidyr::separate_rows(Keywords, sep = ",")
viz_project_dataframe <- viz_project_dataframe %>% mutate(Keywords = str_trim(Keywords, side = "left"))

# Count occurrences of each keyword  
keyword_counts <- viz_project_dataframe %>% group_by(Keywords) %>% summarize(n = n())

# Filter for top keywords
top_10 <- keyword_counts %>% top_n(10, n)

# Generate a palette of colors based on the number of unique keywords
top_10_colors <- scales::hue_pal()(length(unique(top_10$Keywords)))


# Reorder the Keywords based on their total counts
top_10$Keywords <- factor(top_10$Keywords, levels = top_10$Keywords[order(top_10$n)])

# Plot bar chart with numbers on top of bars and dynamic colors, with swapped axes
ggplot(top_10, aes(x = n, y = Keywords, fill = Keywords)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.5, size = 3) +  # Add numbers on the bars
  labs(x = "Count") +
  ggtitle("Top 11 Article Keywords") +  # Add the chart title
  scale_fill_manual(values = top_10_colors) +  # Assign dynamic colors
  theme_minimal()  # Optional: Use a minimal theme for better readability




