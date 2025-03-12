library(simmer)
library(simmer.plot)

set.seed(12)

# Create a patient every 3 minutes
P_INTER <- function() rexp(1, rate = 1/3)

# Number of workers in triage
NUM_TRIAGE_WORKER <- 2

# Time(MINUTES) required to examine patients in triage
TRIAGE_TIME <- function() round(rnorm(1, mean = 5, sd = 2), 1)

# Number of doctors
NUM_DOCTORS <- 32

# Define probability distribution for priority levels (1 = lowest, 4 = highest)
# Blue--> 1
# Green--> 2
# Yellow--> 3
# Red--> 4
PROBABILITY_PRIORITY <- c(0.25, 0.50, 0.20, 0.05)

# Function to randomly assign a priority based on probability
ASSIGN_PRIORITY <- function() {
  sample(1:4, size = 1, prob = PROBABILITY_PRIORITY)
}

# Blue are non-urgent, can be sent to their primary care --> 1
# Green are less urgent, but require medical attention --> 2
# Yellow are serious cases, but stable --> 3
# Red are critical cases, required immediate attention --> 4

# Function to determine doctor consultation time based on priority
DOCTOR_TIME <- function(priority) {
  if (priority == 1) {
    time <- round(rnorm(1, mean = 20, sd = 5), 1)
    return(max(time, 10))  # Ensures the time is at least 10 minute
  } else if (priority == 2) {
    time <- round(rnorm(1, mean = 120, sd = 20), 1)
    return(max(time, 100))  # Ensures the time is at least 100 minutes
  } else if (priority == 3) {
    time <- round(rnorm(1, mean = 240, sd = 25), 1)
    return(max(time, 200))  # Ensures the time is at least 200 minutes
  } else if (priority == 4) {
    time <- round(rnorm(1, mean = 320, sd = 30), 1)
    return(max(time, 250))  # Ensures the time is at least 250 minutes
  }
}

# Patient doctor trajectory
doctor_trajectory <- trajectory() %>%
  log_("Entering doctor queue") %>%
  set_attribute("wait_start", function() now(env)) %>%  # Record queue entry time
  set_prioritization(function() {
    priority <- get_attribute(env, "priority")
    return(c(priority, NA, NA))  # Patients with higher priority go first
  }) %>%
  seize("doctor") %>%
  set_attribute("wait_time", function() (round(now(env) - get_attribute(env, "wait_start"), 2))) %>%  # Calculate wait time
  log_(function() paste("Waited for doctor:", get_attribute(env, "wait_time"), "minutes")) %>%
  log_("Seeing doctor") %>%
  timeout(function() round(DOCTOR_TIME(get_attribute(env, "priority")), 2)) %>%  # Doctor consultation time based on priority
  release("doctor") %>%
  log_("Leaving hospital")

# Patient triage trajectory
triage_trajectory <- trajectory() %>%
  log_("Patient arrives") %>%
  seize("triage_worker", amount = 1) %>%
  timeout(TRIAGE_TIME) %>%  # Triage duration (5 mins +/- 3min)
  set_attribute("priority", function() ASSIGN_PRIORITY()) %>%  # Assigning priority (1 (Blue) is lowest priority, 4 (Red) is highest)
  log_(function() paste("Assigned priority:", get_attribute(env, "priority"))) %>%
  release("triage_worker") %>%
  join(doctor_trajectory)

plot(doctor_trajectory, verbose = TRUE)

env <- simmer("hospital")

# Add resources
env %>%
  add_resource("triage_worker", capacity = NUM_TRIAGE_WORKER) %>%  # Two triage workers
  add_resource("doctor", capacity = NUM_DOCTORS, queue_size = Inf, preemptive = FALSE) %>%  # 32 doctors,FIFO for same priority
  add_generator("patient", triage_trajectory, P_INTER) %>%  # Patients arrive every 3 min
  run(until = 1440)

# Gather information about patient arrivals in simulation
patient_arr <- get_mon_arrivals(env)

# Gather information about resources in the simulation (doctors and triage_workers)
res <- get_mon_resources(env)

# Gather information about patient attributes
patient_attrs <- get_mon_attributes(env)

# Amount of time spent in active state
plot(patient_arr, metric="activity_time")

# Amount of time spent waiting
plot(patient_arr, metric = "waiting_time")

# Amount of time spent in the system
plot(patient_arr, metric = "flow_time")

# Average resource utilization (total time in use divided by the total simulation time)
plot(res, metric="utilization")

# Resource usage throughout the simulation
plot(res, metric="usage", items=c("queue", "server"), steps=TRUE)



