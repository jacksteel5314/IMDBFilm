library(RSQLite)

get_user = function(username) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  userQuery = "
      SELECT *
      FROM users
      WHERE username = :username
    "
  results = dbGetQuery(db, userQuery, params = list(username = username))
  dbDisconnect(db)
  if (length(results$username) != 1) {
    return(NULL)
  }
  return(list(results))
}

login = function(username, password) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  userQuery = "
      SELECT *
      FROM users
      WHERE username = :username
    "
  results = dbGetQuery(db, userQuery, params = list(username = username))
  dbDisconnect(db)
  
  # Hashing the Passwords (Cannot See the Passwords)
  if (length(results$username) != 1 || !(sodium::password_verify(results$password, password))) {
    return(NULL)
  }
  return(list(results))
}

sign_up = function(username, password) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  hashed_password = sodium::password_store(password)
  rs = dbSendStatement(db, 'INSERT INTO users (username, password) VALUES (:username, :password)')
  params = list(username = username, password = hashed_password)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}


add_watchlist = function(user_id, movie_id) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  rs = dbSendStatement(db, "INSERT INTO watchlist (user_id, movie_id) VALUES (:user_id, :movie_id)")
  params = list(user_id = user_id, movie_id = movie_id)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}

delete_watchlist = function(user_id, movie_id) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  rs = dbSendStatement(db, "DELETE FROM watchlist WHERE user_id = :user_id AND movie_id = :movie_id")
  params = list(user_id = user_id, movie_id = movie_id)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}

get_movie = function(title) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  movieQuery = "
      SELECT *
      FROM movies
      WHERE title = :title
    "
  results = dbGetQuery(db, movieQuery, params = list(title = title))
  dbDisconnect(db)
  if (length(results$title) != 1) {
    return(NULL)
  }
  return(list(results))
}

add_movie = function(title, genre = '', duration = NA, description = '', image = '', imdb_url = '') {
  existing_movie = get_movie(title)
  if (!is.null(existing_movie)) {
    return()
  }
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  rs = dbSendStatement(db, "INSERT INTO movies (title, genre, duration, description, image, imdb_url) VALUES (:title, :genre, :duration, :description, :image, :imdb_url)")
  params = list(title = title, genre = genre, duration = duration, description = description, image = image, imdb_url = imdb_url)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}

get_watchlists <- function(user_id) {
  db <- dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  # SQL to join watchlist with movies table
  watchlistQuery <- "
    SELECT m.*
    FROM watchlist w
    JOIN movies m ON w.movie_id = m.movie_id
    WHERE w.user_id = :user_id
    "
  # Execute the query using the provided user_id
  results <- dbGetQuery(db, watchlistQuery, params = list(user_id = user_id))
  # Disconnect from the database
  dbDisconnect(db)
  # Return the results
  if (nrow(results) == 0) {
    return(NULL)
  }
  return(results)
}

get_watchlist <- function(user_id, movie_id) {
  db <- dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  # SQL to join watchlist with movies table
  watchlistQuery <- "
    SELECT *
    FROM watchlist
    WHERE user_id = :user_id
    AND movie_id = :movie_id
    "
  # Execute the query using the provided user_id
  results <- dbGetQuery(db, watchlistQuery, params = list(user_id = user_id, movie_id = movie_id))
  # Disconnect from the database
  dbDisconnect(db)
  # Return the results
  if (length(results$watchlist_id) != 1) {
    return(NULL)
  }
  return(list(results))
}

add_review <- function(user_id, movie_id, review_text) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  rs = dbSendStatement(db, "INSERT INTO reviews (user_id, movie_id, review_text) VALUES (:user_id, :movie_id, :review_text)")
  params = list(user_id = user_id, movie_id = movie_id, review_text = review_text)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}

add_rating <- function(user_id, movie_id, rating) {
  db = dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  rs = dbSendStatement(db, "INSERT INTO ratings (user_id, movie_id, rating) VALUES (:user_id, :movie_id, :rating)")
  params = list(user_id = user_id, movie_id = movie_id, rating = rating)
  dbBind(rs, params)
  dbClearResult(rs)
  dbDisconnect(db)
}

get_reviews <- function(user_id) {
  db <- dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  # SQL to join watchlist with movies table
  reviewsQuery <- "
    SELECT m.*, r.review_text, r.review_date
    FROM reviews r
    JOIN movies m ON r.movie_id = m.movie_id
    WHERE r.user_id = :user_id
    "
  # Execute the query using the provided user_id
  results <- dbGetQuery(db, reviewsQuery, params = list(user_id = user_id))
  # Disconnect from the database
  dbDisconnect(db)
  # Return the results
  if (nrow(results) == 0) {
    return(NULL)
  }
  return(results)
}
get_ratings <- function(user_id) {
  db <- dbConnect(RSQLite::SQLite(), dbname = "data/movies.db")
  # SQL to join watchlist with movies table
  ratingsQuery <- "
    SELECT m.*, r.rating
    FROM ratings r
    JOIN movies m ON r.movie_id = m.movie_id
    WHERE r.user_id = :user_id
    "
  # Execute the query using the provided user_id
  results <- dbGetQuery(db, ratingsQuery, params = list(user_id = user_id))
  # Disconnect from the database
  dbDisconnect(db)
  # Return the results
  if (nrow(results) == 0) {
    return(NULL)
  }
  return(results)
}