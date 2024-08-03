# Restart R session to ensure no old versions are in use
.rs.restartR()

# Ensure the latest version of htmltools is installed
install.packages("htmltools")

# Check and unload old version if necessary
if ("htmltools" %in% loadedNamespaces()) {
  detach("package:htmltools", unload = TRUE)
}

# Load the updated htmltools package
library(htmltools)

# Verify the version
packageVersion("htmltools")

# Attempt to install the RedditExtractor package again
devtools::install_github('ivan-rivera/RedditExtractor')

install.packages("radarchart")
install.packages("syuzhet")
install.packages("tm")

require("RedditExtractoR");require("radarchart");require("tm");require("syuzhet")



# Fetch Reddit data
links <- find_thread_urls(keywords = "NVDA", subreddit = "wallstreetbets", sort_by = "new", period = "week")

# Sample a subset of links
set.seed(123) # For reproducibility
sampled_links <- links[sample(nrow(links), size = 20), ] # Adjust the sample size as needed

# Fetch thread content for the sampled links
content_list <- lapply(sampled_links$url, get_thread_content)
print(content_list)

print(content_list[[1]])  # Print the first element for inspection


# Extract data from the content_list
extract_data <- function(content) {
  # Check if 'threads' and 'comments' are available
  if (is.null(content$threads) || is.null(content$comments)) {
    return(NULL)
  }
  
  # Extract thread data
  threads_df <- data.frame(
    url = content$threads$url,
    author = content$threads$author,
    date_utc = as.Date(content$threads$date, format = "%Y-%m-%d"),
    timestamp = content$threads$timestamp,
    title = content$threads$title,
    text = content$threads$text,
    subreddit = content$threads$subreddit,
    score = content$threads$score,
    upvotes = content$threads$upvotes,
    downvotes = content$threads$downvotes,
    up_ratio = content$threads$up_ratio,
    total_awards_received = content$threads$total_awards_received,
    golds = content$threads$golds,
    cross_posts = content$threads$cross_posts,
    comments_count = content$threads$comments,
    stringsAsFactors = FALSE
  )
  
  # Extract comment data
  comments_df <- data.frame(
    url = content$comments$url,
    author = content$comments$author,
    date_utc = as.Date(content$comments$date, format = "%Y-%m-%d"),
    timestamp = content$comments$timestamp,
    score = content$comments$score,
    upvotes = content$comments$upvotes,
    downvotes = content$comments$downvotes,
    golds = content$comments$golds,
    comment = content$comments$comment,
    stringsAsFactors = FALSE
  )
  
  # Return combined data
  list(threads_df = threads_df, comments_df = comments_df)
}

# Apply the extraction function to each item in content_list
extracted_data_list <- lapply(content_list, extract_data)

# Combine all thread data into a single data frame
all_threads <- do.call(rbind, lapply(extracted_data_list, function(x) x$threads_df))

# Combine all comment data into a single data frame
all_comments <- do.call(rbind, lapply(extracted_data_list, function(x) x$comments_df))

# Optionally combine threads and comments if needed
# For instance, you might want to keep them separate or merge them based on specific requirements.

# Print results
print(head(all_threads))
print(head(all_comments))

reddit_NVDA <- all_comments
reddit_NVDA_threats <- all_threads
