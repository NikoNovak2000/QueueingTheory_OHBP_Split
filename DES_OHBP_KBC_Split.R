install.packages(c("simmer", "simmer.plot"))

library(simmer)
library(simmer.plot)

# Set seed to make the results reproducible
set.seed(2)

# Create a patient every 3 minutes
P_INTER <- function() round(rnorm(1, mean = 3, sd = 1.5), 1)

# Number of workers in triage
NUM_TRIAGE_WORKER <- 3

# Time(MINUTES) required to examine patients in triage
TRIAGE_TIME <- function() round(rnorm(1, mean = 5, sd = 3), 1)

# Number of doctors in OHBP
NUM_DOCTORS <- 32

# Patient categories
PATIENT_CATEGORY <- c("Red", "Yellow", "Green", "Blue")
# Blue are non-urgent, can be sent to their primary care
# Green are less urgent, but require medical attention
# Yellow are serious cases, but stable
# Red are critical cases, required immediate attention

# Probability distribution for each category
PROBABILITY_CATEGORY <- c(0.05, 0.20, 0.50, 0.25)

# Function to randomly assign a category
ASSIGN_CATEGORY <- function() { 
  sample(PATIENT_CATEGORY, size = 1, prob = PROBABILITY_CATEGORY)
}

# Help time or examination by doctors time is calculated based on the category of the patients cases
HELP_TIME <- function(category) {
  if (category == "Blue") {
    return(0)  # No medical attention, sent to primary care
  } else if (category == "Green") {
    return(round(rnorm(1, mean = 60, sd = 20), 1))  # Less urgent cases
  } else if (category == "Yellow") {
    return(round(rnorm(1, mean = 160, sd = 25), 1))  # Serious cases
  } else if (category == "Red") {
    return(round(rnorm(1, mean = 240, sd = 30), 1))  # Critical cases
  } else {
    return(NA)  # Fallback in case of an error
  }
}