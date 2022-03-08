
# Function that returns value similar to _merge in Stata (i.e tells you
# how many rows did or did not merge)
# Source: https://stackoverflow.com/questions/40110644/is-there-a-merge-indicator-available-after-a-merge?noredirect=1&lq=1
full_join_track <- function(x, y, by = NULL, suffix = c(".x", ".y"),
                            .merge = FALSE, ...){
  
  # Checking to make sure used variable names are not already in use
  if(".x_tracker" %in% names(x)){
    message("Warning: variable .x_tracker in left data was dropped")
  }
  if(".y_tracker" %in% names(y)){
    message("Warning: variable .y_tracker in right data was dropped")
  }
  if(.merge & (".merge" %in% names(x) | ".merge" %in% names(y))){
    stop("Variable .merge already exists; change name before proceeding")
  }
  
  # Adding simple merge tracker variables to data frames
  x[, ".x_tracker"] <- 1
  y[, ".y_tracker"] <- 1
  
  # Doing full join
  joined <- full_join(x, y, by = by, suffix = suffix,  ...)
  
  # Calculating merge diagnoses 
  matched <- joined %>%
    filter(!is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    NROW()
  unmatched_x <- joined %>%
    filter(!is.na(.x_tracker) & is.na(.y_tracker)) %>%
    NROW()
  unmatched_y <- joined %>%
    filter(is.na(.x_tracker) & !is.na(.y_tracker)) %>%
    NROW()
  
  # Print merge diagnoses
  message(
    unmatched_x, " Rows ONLY from left data frame", "\n",
    unmatched_y, " Rows ONLY from right data frame", "\n",
    matched, " Rows matched"
  )
  
  # Create .merge variable if specified
  if(.merge){
    joined <- joined %>%
      mutate(.merge = 
               case_when(
                 !is.na(.$.x_tracker) & is.na(.$.y_tracker) ~ "left_only",
                 is.na(.$.x_tracker) & !is.na(.$.y_tracker) ~ "right_only",
                 TRUE ~ "matched"
               )
      )
  }
  
  # Dropping tracker variables and returning data frame
  joined <- joined %>%
    dplyr::select(-.x_tracker, -.y_tracker)
  return(joined)
} 
