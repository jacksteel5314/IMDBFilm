library(shiny)
library(bslib)
library(tidyverse)   # Transform and Show Data
library(purrr)   # To Create Accessible Links 
library(RSQLite)
library(shinydashboard)
library(DT)
library(slickR)
library(svglite)
library(gdtools)
library(waiter)
library(shinyjs)

# Retrieve the CRUD script
source("crud.R")

waiting_screen <- tagList(
  spin_3(),
  h4("Fetching data...", style = "color: black;")
) 
waiter_set_theme(color = "white")

# Retrieve the data_table CSV file (don't need to keep scraping)
data_table = read.csv("data/imdb.csv")

# From bslib library - different parts of the app 
cards = list(
  # If the User is Logging In
  card(
    full_screen = TRUE,
    card_header("Login"),
    textInput("username", "Username"),
    textInput("password", "Password"),
    actionButton("login_button", "Login")
  ),
  # If the User is Making an Account 
  card(
    full_screen = TRUE,
    card_header("Sign-up"),
    textInput("new_username", "Username"),
    textInput("new_password", "Password"),
    actionButton("sign_up", "Sign up")
  ),
  # If the User (Logged in or Not) Searches for a Movie 
  card(
    full_screen = TRUE,
    card_header("Search Movies"),
    # Sidebar with Filters
    sidebarLayout(
      sidebarPanel(
        h3("Directions"),
        h5("Use any combination of the following filters to receive a movie recommendation"),
        h3("Filters"),
        
        
        # Genre Filter - No Effect if Untouched
        checkboxGroupInput("genreFilter", label = h4("Select Genre(s)"), 
                           choices = unique(data_table$Genre),
                           selected = "Drama"),
        
        
        # Year Filter - No Effect if Untouched 
        sliderInput("yearFilter", label = h4("Select Year Range"), min = min(as.numeric(data_table$Year)), 
                    max = max(as.numeric(data_table$Year)), value = c(min(as.numeric(data_table$Year)), max(as.numeric(data_table$Year))), sep="", pre = "Year ", step=1),
        
        # Star Filter - No Effect if Untouched
        sliderInput("starFilter", label = h4("Select Star Range"), min = 0, max = 10, value = c(0, 10), post = "/10"),
        
        # Rotten Tomato - No Effect if Untouched
        sliderInput("metaFilter", label = h4("Select Metascore Range"), min = 0, max = 100, value = c(0, 100), post = "/100"),
        
        # Duration Range in Minutes - No Effect if Untouched 
        sliderInput("durationFilter", label = h4("Select Duration Range"), min = min(as.numeric(data_table$Duration)), 
                    max = max(as.numeric(data_table$Duration)), value = c(min(as.numeric(data_table$Duration)), max(as.numeric(data_table$Duration))), sep="", post = " Minutes", step=1),
        
        # Viewer Rating - No Effect if Untouched
        checkboxGroupInput("viewerRating", label = h4("Select Viewer Rating(s)"), 
                           choices = unique(data_table$Age_Rating),
                           selected = unique(data_table$Age_Rating)),
        
        # Sort the Results by Rating, Metascore, etc. 
        radioButtons("arrangeBy", label = h4("Arrange Results By"), 
                     choices = list("Stars" = 1, "Metascore" = 2, "Duration" = 3, "Years" = 4),
                     selected = 1),
        
        # Action Button
        actionButton("doFilter", label="Find a Movie!")
        
      ),
      
      # Main Panel
      mainPanel(
        
        # Title Main Panel
        h4("Filtered Movies Recommendations"),
        
        # Connects to Movie Output (Below)
        tableOutput("movies"),
      )
    )
  ),
  # Card to View Profile
  card(
    full_screen = TRUE,
    card_header("Profile"),
    # Creating the Dashboard Page for Profile
    dashboardPage(
      dashboardHeader(title = ""),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        # Different Parts
        fluidRow(
          # Information on the User (username)
          box(title = "User Info", status = "primary", solidHeader = TRUE, width = 12,
              textOutput("user_info")),
          # Movies the User Added to Watchlist
          box(title = "Watchlist", status = "warning", solidHeader = TRUE, width = 6,
              DTOutput("user_watchlist")),
          # Ratings the User Created on Listed Movies 
          box(title = "Ratings", status = "info", solidHeader = TRUE, width = 6,
              DTOutput("user_ratings")),
          # Reviews the User Created on Listed Movies
          box(title = "Reviews", status = "success", solidHeader = TRUE, width = 12,
              slickROutput("user_reviews",width='100%',height='100%'))
        )
      )
    )
  )
)

# User Interface Side - Creating the Navigation Panel for the App 
ui = page_navbar(
  id = "navbar",
  title = "IMDB Movie dashboard",
  nav_spacer(),
  # Different Cards of the App (See Above) 
  nav_panel("Login", uiOutput("login")),
  nav_panel("Sign-up", uiOutput("signup")),
  nav_panel("Search Movies", cards[[3]]),
  nav_panel("Profile", uiOutput("profile")),
  nav_panel("Logout", uiOutput("logout")),
  tags$head(
    useShinyjs(),  # Initialize shinyjs
    useWaiter()    # Initialize waiter
  )
)

# Server Side of the App 
server = function(input, output, session) {
  # Reactive value to store user state
  user_data = reactiveVal(NULL)
  # Reactive value to store click number (using to guarantee unique link ids)
  clicks = reactiveVal(0)
  reviews <- reactiveVal(NULL)
  # Initialize observers
  observers <- reactiveValues(handles = list())
  # Hide the profile navbar initially (only if logged in should we show)
  observe({
    nav_hide("navbar", target = "Profile")
    nav_hide("navbar", target = "Logout")
  })
  
  # If The Profile Tab Was Clicked, Show Username
  output$user_info <- renderText({
    paste("Logged in as:", user_data()$username)
  })
  
  # Show the Watchlist of the User (getting info from crud function)
  output$user_watchlist <- renderDT({
    # Get the Watchlist from Database matching current userid
    watchlists = get_watchlists(user_data()$user_id)
    # If the Watchlist is not empty, create a table with title and duration of movie
    if (!is.null(watchlists)) {
      watchlists = tibble(watchlists)
      watchlists = watchlists |>
        select(title, duration)
    }
    # display it, paginated by 5 to keep it neat
    datatable(watchlists, options = list(pageLength = 5))
  })
  # Get the User Ratings 
  output$user_ratings <- renderDT({
    # Retrieve the User Ratings from Database
    ratings = get_ratings(user_data()$user_id)
    # If the User Ratings list is not empty, create a table with title and the rating 
    if (!is.null(ratings)) {
      ratings = tibble(ratings)
      ratings = ratings |>
        select(title, rating)
    }
    datatable(ratings, options = list(pageLength = 5))
  })
  
  # Get the User Reviews (Written Reviews on App) - Make it a Sliding Carousel
  output$user_reviews <- renderSlickR({
    # Get the Reviews from the Database 
    reviews <- get_reviews(user_data()$user_id)
    # If the Reviews are not Null and there are Titles 
    if (!is.null(reviews) && length(reviews$title) > 0) {
      # Display the image of movie, review, and date of the review
      # Using css to make the appearance nice (image to left of other stuff)
      img_list <- lapply(1:length(reviews$title), function(i) {
        tags$div(
          style = "display: flex; justify-content: start; margin-bottom: 20px;",
          tags$div(
            style = "flex: 1; padding-right: 20px;",
            img(src = reviews[i, ]$image, style = "width: 200px; height: auto; max-height: 250px; display: block;")
          ),
          tags$div(
            style = "flex: 3;",
            tags$h3(reviews[i, ]$title, style = "margin: 0; text-align: left;"),
            tags$p(paste("Review: ", reviews[i, "review_text"], sep=""), style = "text-align: left; color: black;"),
            tags$p(paste("Date: ", format(as.Date(reviews[i, "review_date"]), "%Y-%m-%d"), sep=""), style = "text-align: left; color: grey")
          )
        )
      })
      # Create a slickR object with the list of review elements
      slickR(obj = img_list, slideId = "slick-reviews", width = "90%", height = "auto") +
        settings(slidesToShow = 1, centerMode = FALSE, dots = TRUE)
    } else {
      tags$div("No reviews available")
    }
  })
  
  # If the User Clicks the Login Button 
  observeEvent(input$login_button, {
    
    # Retrieve the inputted username and password and verify that they are correct 
    username = input$username
    password = input$password
    user = login(username, password)
    # Incorrect username or password, tell them
    if (is.null(user)) {
      showNotification("Incorrect username or password.")
      return()
    }
    # If Correct, Remove the Login/Signup Tabs and Add Profile tab (alongside search movies)
    nav_hide("navbar", target = "Login")
    nav_hide("navbar", target = "Sign-up")
    nav_show("navbar", target = "Profile")
    nav_show("navbar", target = "Logout")
    # Set our user state
    user_data(user[[1]])
    # Now include profile tab
    updateNavbarPage(session, "navbar", selected = "Profile")
  })
  
  # Upon clicking sign up button
  observeEvent(input$sign_up, {
    # Get user and pass
    username = input$new_username
    password = input$new_password
    
    # Check that this username is not already taken
    existing_user = get_user(username)
    if (!is.null(existing_user)) {
      showNotification("That username is taken.")
      return()
    }
    # Sign them up
    sign_up(username, password)
    showNotification("Successfully signed up.")
    # Take them to the login page
    updateNavbarPage(session, "navbar", selected = "Login")
    
  })
  # Safeguarding to ensure that UIs only show up if user state is correct and 
  # matches nav bar state
  output$login = renderUI({
    if (is.null(user_data())) {
      cards[[1]]
    }
  })
  
  output$signup = renderUI({
    if (is.null(user_data())) {
      cards[[2]]
    }
  })
  
  output$profile = renderUI({
    if (!is.null(user_data())) {
      cards[[4]]
    }
  })
  
  output$logout = renderUI({
    if (!is.null(user_data())) {
      actionButton("logoutButton", "Log Out", class = "btn-primary")
    }
  })
  
  
  observeEvent(input$logoutButton, {
    user_data(NULL)
    nav_show("navbar", target = "Login")
    nav_show("navbar", target = "Sign-up")
    nav_hide("navbar", target = "Profile")
    nav_hide("navbar", target = "Logout")
    updateNavbarPage(session, "navbar", selected = "Login")
  })
  observeEvent(input$doFilter, {
    
    # Clear existing observers
    lapply(observers$handles, function(observer) {
      try(suppressWarnings(observer$destroy()))
    })
    observers$handles <- list()
    output$movies <- renderUI({
      NULL
    })
    
    waiter_show(html = waiting_screen, color="white")
    
    # Get the Inputted Values
    genres = input$genreFilter
    years = input$yearFilter
    stars = input$starFilter
    metascore = input$metaFilter
    durations = input$durationFilter
    viewers = input$viewerRating
    ordering = input$arrangeBy
    
    # Establish ordering Array and Get Column
    ordering_arr = c("Star_Rating", "Metascore_Rating", "Duration", "Year")
    order_col = ordering_arr[as.integer(ordering)]
    
    # Filter the Data Table Based on the Filter Criteria
    data_table = data_table |>
      filter(
        Genre %in% genres &  # See Citation [2]
          Year >= years[1] &  # In Year Range
          Year <= years[2] &
          as.double(Star_Rating) >= stars[1] &  # These take care of comparing int to double
          as.double(Star_Rating) <= stars[2] &
          as.double(Metascore_Rating) >= metascore[1] &  # In Tomato Range
          as.double(Metascore_Rating) <= metascore[2] &
          as.double(Duration) >= durations[1] &  # In Duration Range
          as.double(Duration) <= durations[2] &
          Age_Rating %in% viewers  # See Citation [2]
      )
    # Remove the Duplicates and Combine the Genres
    no_dup = data_table |>
      group_by(Movie_Name, Description, Year, Star_Rating, Metascore_Rating, Age_Rating, Duration, Image) |>
      summarize(`Genre(s)` = paste(Genre, collapse = ", "), # See Citation [1]
                
                # Ensure that all of the Variables are Numerical
                Year = as.integer(Year),
                Star_Rating = as.double(Star_Rating),
                Metascore_Rating = as.integer(Metascore_Rating),
                Duration = as.integer(Duration)) |>
      
      unique() |> # Remove Repeat Rows
      
      # Arrange them According to Specified Input
      arrange(!!sym(order_col)) # See Citation [3]
    # Filtering logic and other operations...
    movie_names <- no_dup$Movie_Name
    # Dynamically create action links and corresponding observers
    clickNo = clicks()
    output$movies <- renderUI({
      ui_elems <- lapply(seq_along(movie_names), function(i) {
        idLabel = paste0("movie", clickNo, gsub("[[:punct:] ]", "", no_dup[i, ]$Movie_Name), i)
        fluidRow(actionLink(idLabel, movie_names[i]))
      })
      do.call(tagList, ui_elems)
    })
    # Create new observers for each link
    observers$handles <- lapply(seq_along(movie_names), function(j) {
      idLabel = paste0("movie", clickNo, gsub("[[:punct:] ]", "", no_dup[j, ]$Movie_Name), j)
      reviewTextInputId <- paste0("reviewtext", clickNo, gsub("[[:punct:] ]", "", no_dup[j, ]$Movie_Name), j)
      ratingInputId <- paste0("rating", clickNo, gsub("[[:punct:] ]", "", no_dup[j, ]$Movie_Name), j)
      submitReviewButtonId <- paste0("submitreview", clickNo, gsub("[[:punct:] ]", "", no_dup[j, ]$Movie_Name), j)
      watchlaterId = paste0("watchlater", clickNo, gsub("[[:punct:] ]", "", no_dup[j, ]$Movie_Name), j)
      # If the User Clicks the Link for a Movie
      # See Citation [5]
      observeEvent(input[[idLabel]], {
        on_watchlist = FALSE
        if (!is.null(user_data())) {
          user_id = user_data()$user_id
          movie = get_movie(no_dup[j, ]$Movie_Name)
          movie = movie[[1]]
          if(!is.null(movie)) {
            on_watchlist = !is.null(get_watchlist(user_id, movie$movie_id))
          }
        }
        # ModalDialog Output of Title and Movie Description
        showModal(
          modalDialog(
            
            # Output Movie Title
            title = movie_names[j],
            # EDIT - TEMPORARY PHOTO
            img(src = no_dup[j, ]$Image, style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"),
            
            # OUTPUT THE DESCRIPTION
            tags$p(no_dup[j, 2]),
            tags$p(no_dup[j, 3], "   •   ", no_dup[j, 6], "   •   ", no_dup[j, 7], " Minutes"),  # Output Information 
            tags$p(no_dup[j, 4], " Stars   •   ", no_dup[j, 5], " Metascore"),
            actionButton(watchlaterId,
                         label = if (on_watchlist) "Remove from Watchlist" else "Add to Watchlist"),
            textInput(reviewTextInputId, "Write your review", ""),
            numericInput(ratingInputId, "Rating (1-5)", min = 1, max = 5, value = NA),
            actionButton(submitReviewButtonId, "Submit Review", class = "btn-success"),
            tags$script(HTML(paste0("
                $(document).on('shiny:inputchanged', function(event) {
                    if (event.name === '", reviewTextInputId, "' || event.name === '", ratingInputId, "') {
                        if ($('#", reviewTextInputId, "').val() !== '' && $('#", ratingInputId, "').val()!== '') {
                            $('#", submitReviewButtonId, "').prop('disabled', false);
                        } else {
                            $('#", submitReviewButtonId, "').prop('disabled', true);
                        }
                    }
                });
            "))),
            footer = tagList(
              modalButton("Close")
            )
          )
        )
        
      }, ignoreInit=TRUE)
      
      observeEvent(input[[watchlaterId]], {
        if (is.null(user_data())) {
          showNotification("You must be logged in to use that feature.")
          return()
        }
        user_id = user_data()$user_id
        movie_data = no_dup[j, ]
        movie = get_movie(movie_data$Movie_Name)
        if (is.null(movie)) {
          add_movie(
            title = movie_data$Movie_Name,
            genre = movie_data$`Genre(s)`,
            duration = movie_data$Duration,
            description = movie_data$Description,
            image = movie_data$Image
          )
          movie = get_movie(movie_data$Movie_Name)
          
        }
        movie = movie[[1]]
        existing_watchlist_movie = get_watchlist(user_id, movie$movie_id)[[1]]
        if (is.null(existing_watchlist_movie)) {
          # Add to watchlist
          add_watchlist(user_id, movie$movie_id)
          showNotification("Successfully added movie to watchlist.")
          updateActionButton(session, watchlaterId, label = "Remove from Watchlist")
        } else {
          # Remove from watchlist
          delete_watchlist(user_id, movie$movie_id)
          showNotification("Removed movie from watchlist.")
          updateActionButton(session, inputId = watchlaterId, label = "Add to Watchlist")
        }
        
        # Update watchlist UI after remove/add
        # Show the Watchlist of the User (getting info from crud function)
        output$user_watchlist <- renderDT({
          # Get the Watchlist from Database matching current userid
          watchlists = get_watchlists(user_data()$user_id)
          # If the Watchlist is not empty, create a table with title and duration of movie
          if (!is.null(watchlists)) {
            watchlists = tibble(watchlists)
            watchlists = watchlists |>
              select(title, duration)
          }
          # display it, paginated by 5 to keep it neat
          datatable(watchlists, options = list(pageLength = 5))
        })
        
      }, ignoreInit = TRUE)
      
      observeEvent(input[[submitReviewButtonId]], {
        if (is.null(user_data())) {
          showNotification("You must be logged in to use this feature.")
          return()
        }
        if (input[[reviewTextInputId]] != "" && !is.na(input[[ratingInputId]])) {
          user_id = user_data()$user_id
          movie_data = no_dup[j, ]
          movie = get_movie(movie_data$Movie_Name)
          if (!is.null(movie)) {
            movie = movie[[1]]
            add_review(user_id, movie$movie_id, input[[reviewTextInputId]])
            add_rating(user_id, movie$movie_id, input[[ratingInputId]])
            showNotification("Review added successfully.")
          } else {
            add_movie(
              title = movie_data$Movie_Name,
              genre = movie_data$`Genre(s)`,
              duration = movie_data$Duration,
              description = movie_data$Description,
              image = movie_data$Image
            )
            movie = get_movie(movie_data$Movie_Name)[[1]]
            add_review(user_id, movie$movie_id, input[[reviewTextInputId]])
            add_rating(user_id, movie$movie_id, input[[ratingInputId]])
            showNotification("Review added successfully.")
          }
        } else {
          showNotification("Please fill in both the review and rating.")
        }
        
        # Update user ratings
        # Get the User Ratings 
        output$user_ratings <- renderDT({
          # Retrieve the User Ratings from Database
          ratings = get_ratings(user_data()$user_id)
          # If the User Ratings list is not empty, create a table with title and the rating 
          if (!is.null(ratings)) {
            ratings = tibble(ratings)
            ratings = ratings |>
              select(title, rating)
          }
          datatable(ratings, options = list(pageLength = 5))
        })
        # Commented out because was failing and duplicating SlickR
        # # Update reviews after submitting one 
        # # Get the User Reviews (Written Reviews on App) - Make it a Sliding Carousel
        # output$user_reviews <- renderSlickR({
        #   # Get the Reviews from the Database 
        #   reviews <- get_reviews(user_data()$user_id)
        #   # If the Reviews are not Null and there are Titles 
        #   if (!is.null(reviews) && length(reviews$title) > 0) {
        #     # Display the image of movie, review, and date of the review
        #     # Using css to make the appearance nice (image to left of other stuff)
        #     img_list <- lapply(1:length(reviews$title), function(i) {
        #       tags$div(
        #         style = "display: flex; justify-content: start; margin-bottom: 20px;",
        #         tags$div(
        #           style = "flex: 1; padding-right: 20px;",
        #           img(src = reviews[i, ]$image, style = "width: 200px; height: auto; max-height: 250px; display: block;")
        #         ),
        #         tags$div(
        #           style = "flex: 3;",
        #           tags$h3(reviews[i, ]$title, style = "margin: 0; text-align: left;"),
        #           tags$p(paste("Review: ", reviews[i, "review_text"], sep=""), style = "text-align: left; color: black;"),
        #           tags$p(paste("Date: ", format(as.Date(reviews[i, "review_date"]), "%Y-%m-%d"), sep=""), style = "text-align: left; color: grey")
        #         )
        #       )
        #     })
        #     # Create a slickR object with the list of review elements
        #     slickR(obj = img_list, slideId = "slick-reviews", width = "90%", height = "auto") +
        #       settings(slidesToShow = 1, centerMode = FALSE, dots = TRUE)
        #   } else {
        #     tags$div("No reviews available")
        #   }
        # })
      }, ignoreInit = TRUE)
    })
    waiter_hide()
    current_value <- clicks()  # Get the current value
    clicks(current_value + 1) 
  }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)
