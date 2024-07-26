library(rvest)
library(tidyr)
library(tidyverse)

# Script for Getting IMDB Top 250 Links 

# Get the URL for the Top 250 and the Base URL 
movie_base <- "https://www.imdb.com"
url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"

# Get the Top 250 Page
page = read_html(url)

# Get the The Link Tags 
link_tags <- page |>
  html_elements(".ipc-title-link-wrapper") |>
  html_attr("href")

# Create Empty DF
empty_matrix = matrix(nrow=0, ncol=1)
link_df = data.frame(empty_matrix)
colnames(link_df) = c("Links")

# Create Full Link Data Frame 
i = 1 # Iterator 
for(link_tag in link_tags){
  curr_link = paste0(movie_base, link_tag, sep="")
  link_df[i,] = curr_link 
  i = i + 1
}

# Create Final Dataframe
final_empty = matrix(nrow=0, ncol=9)
final_df = data.frame(final_empty)
colnames(final_df) = c("Movie_Name", "Description", "Year", "Star_Rating", "Metascore_Rating", "Age_Rating", "Duration", "Genre", "Image")

# Check if UpperCase Function - Helper for Below
uppercase <- function(inp_letter){
  result = (inp_letter == toupper(inp_letter))
  return(result)
}

# Iterate Through Data Frame and Collect Information 
i = 1 # Link Iterator 
ind_max = 250
curr_row = 1 # Row Iterator
while(i <= 250){
  
  # Get the Page for Current Movie 
  curr_page <- read_html(link_df[i, 1])
  cat("Reading", link_df[i, 1], "\n")
  
  # Get the Title from the Current Movie
  title <- curr_page |>
    html_elements(".hero__primary-text") |>
    html_text()
  final_df[curr_row, 1] = title[1]
  
  
  
  
  # Get the Year, Duration and Rating from the Current Movie 
  year_age <- curr_page |>
    html_elements(".ipc-inline-list.ipc-inline-list--show-dividers.sc-d8941411-2.cdJsTz.baseAlt") |>
    html_elements("li")
  
  # Assign the year 
  final_df[curr_row, 3] = year_age[1] |> html_text()
  
  # If Rating is Available 
  if(length(year_age) == 3){
    final_df[curr_row, 6] = year_age[2] |> html_text() # Assign Rating
    duration = year_age[3] |> html_text() # Get Duration
  }
  # Rating is Not Available
  else{
    final_df[curr_row, 6] = "Not Rated"
    duration = year_age[2] |> html_text()
  }
  
  
  # Split the Duration by the Space (ex. 2h 22m)
  hour_min <- strsplit(duration, " ")[[1]] # See Citation [8]
  
  # The Hours is the First (Cut out the h)
  hours = as.integer(substr(hour_min[1], 1, length(hour_min[1])))
  
  # Minutes is 0 Unless Hour_Min is longer than 1
  minutes = 0
  
  # If There are minutes, cut out the m and get minutes
  if(length(hour_min) == 2){
    minutes = as.integer(substr(hour_min[2], 1, length(hour_min[2])))
  }
  
  # Hours * 60 + minutes = total minute duration
  final_df[curr_row, 7] = (hours * 60) + minutes
  
  # Get HREF for Photo of the movie
  photo_href = curr_page |>
    html_node("div[data-testid='photos-title'] a") |>
    html_attr("href")
  
  # Create and Retrieve the URL for the Photo
  photos_url = paste(movie_base, photo_href, sep="")
  photo_url = read_html(photos_url) |>
    html_node("img[data-image-id]") |>
    html_attr("src")
  final_df[curr_row, 9] = photo_url # Add URL To Data frame
    
  
  # Get the Description
  description <- curr_page |>
    html_node("p.sc-7193fc79-3.gVZVor span") |>
    html_text(trim = TRUE)
  final_df[curr_row, 2] = description[1]
  
  
  
  # Get the Star Rating
  star_rating <- curr_page |>
    html_elements(".cMEQkK") |>
    html_text() 
  final_df[curr_row, 4] = star_rating[1]
  

  
  # Get the Metascore Rating
  metascore_rating <- curr_page |>
    html_elements(".metacritic-score-box") |>
    html_text()
  final_df[curr_row, 5] = metascore_rating[1]
  
  
  # Get the Genre(s) as One Word 
  genre <- curr_page |>
    html_elements(".ipc-chip-list__scroller") |>
    html_text()
  
  ## Solve Issue with Multiple Genres
  
  # Put a Comma Before Each Capital Letter 
  dash_b4_upper <- gsub("([[:upper:]])", ",\\1", genre[1], perl = TRUE) # See Citation [7]
  
  # Split the Words by The Comma 
  diff_genres <- strsplit(dash_b4_upper, ",")[[1]] 
  
  # Get the First Genre
  
  # Sci-Fi and Film-Noir First Genre 
  if(diff_genres[2] == "Sci-" | diff_genres[2] == "Film-"){
    final_df[curr_row, 8] = paste(diff_genres[2], diff_genres[3])
    k = 4
  }
  
  # Sci-Fi and Film-Noir Is Not First Genre
  else{
    final_df[curr_row, 8] = diff_genres[2]
    k = 3
  }
  
  # Iterate Through the Rest of Genres if They are There
  
  while(k <= length(diff_genres)){
    curr_row = curr_row + 1  # New Row
    final_df[curr_row,] = final_df[curr_row-1,] # Copy Previous Row to New Row
    
    # If the Genre is Sci-Fi or Film-Noir 
    if(diff_genres[k] == "Sci-" | diff_genres[k] == "Film-"){
      final_df[curr_row, 8] = paste(diff_genres[k], diff_genres[k+1]) # Get the Next Genre
      k = k + 1 # Iterate an Additional time 
    }
    else{
      final_df[curr_row, 8] = diff_genres[k] # Get the Next Genre
    }
    
    k = k + 1 # Increment
  }
  
  i = i + 1 # Movie Iterator
  curr_row = curr_row + 1  # Row in Final_Df Iterator 
}


write.csv(final_df, "data/imdb.csv")
