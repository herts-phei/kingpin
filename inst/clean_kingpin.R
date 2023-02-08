
library(kingpin)
library(tidyverse)
board <- board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))

kingpin <- pin_return(board, "kingpin")

record_level <- kingpin$records
summary_level <- kingpin$summary


# Pins --------------------------------------------------------------------

pins <- record_level %>%
  mutate(reader_count = ifelse(is.na(reader), 0, 1),
         writer_count = ifelse(is.na(writer), 0, 1)) %>%
  group_by(pin_name) %>%
  summarise(read_instances = sum(reader_count),
            last_read = max(as.Date(read_date), na.rm = TRUE),
            write_instances = sum(writer_count),
            last_write = max(as.Date(write_date), na.rm = TRUE),
            comment = comment,
            .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(last_read = as.Date(ifelse(last_read == -Inf, NA, last_read),
                                    origin = "1970-01-01"),
                last_write = as.Date(ifelse(last_write == -Inf, NA, last_write),
                                     origin = "1970-01-01"))

project_read <- record_level %>%
  filter(!is.na(reader)) %>%
  select(pin_name, project_name) %>%
  distinct() %>%
  group_by(pin_name) %>%
  mutate(projects_read_conc = paste(project_name, collapse = "|")) %>%
  ungroup() %>%
  select(-project_name) %>%
  distinct()

user_read <- record_level %>%
  filter(!is.na(reader)) %>%
  select(pin_name, reader) %>%
  distinct() %>%
  group_by(pin_name) %>%
  mutate(users_read_conc = paste(reader, collapse = "|")) %>%
  ungroup() %>%
  select(-reader) %>%
  distinct()

project_write <- record_level %>%
  filter(!is.na(writer)) %>%
  select(pin_name, project_name) %>%
  distinct() %>%
  group_by(pin_name) %>%
  mutate(projects_write_conc = paste(project_name, collapse = "|")) %>%
  ungroup() %>%
  select(-project_name) %>%
  distinct()

user_write <- record_level %>%
  filter(!is.na(writer)) %>%
  select(pin_name, writer) %>%
  distinct() %>%
  group_by(pin_name) %>%
  mutate(users_write_conc = paste(writer, collapse = "|")) %>%
  ungroup() %>%
  select(-writer) %>%
  distinct()

read_interval <- record_level %>%
  filter(!is.na(reader)) %>%
  group_by(pin_name) %>%
  mutate(read_interval = lead(as.Date(read_date)) - as.Date(read_date)) %>%
  filter(read_interval != 0) %>%
  summarise(median_read_interval = as.numeric(median(read_interval)),
            mean_read_interval = as.numeric(mean(read_interval))) %>%
  ungroup()

write_interval <- record_level %>%
  filter(!is.na(writer)) %>%
  group_by(pin_name) %>%
  mutate(write_interval = lead(as.Date(write_date)) - as.Date(write_date)) %>%
  filter(write_interval != 0) %>%
  summarise(median_write_interval = as.numeric(median(write_interval)),
            mean_write_interval = as.numeric(mean(write_interval))) %>%
  ungroup()

pins <- pins %>%
  left_join(read_interval, by = c("pin_name" = "pin_name")) %>%
  left_join(write_interval, by = c("pin_name" = "pin_name")) %>%
  left_join(project_read, by = c("pin_name" = "pin_name")) %>%
  left_join(user_read, by = c("pin_name" = "pin_name")) %>%
  left_join(project_write, by = c("pin_name" = "pin_name")) %>%
  left_join(user_write, by = c("pin_name" = "pin_name"))


# Users -------------------------------------------------------------------

users_read <- record_level %>%
  filter(!is.na(reader)) %>%
  mutate(reader_count = ifelse(is.na(reader), 0, 1)) %>%
  group_by(reader) %>%
  summarise(read_instances = sum(reader_count),
            last_read = max(as.Date(read_date), na.rm = TRUE),
            .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(last_read = as.Date(ifelse(last_read == -Inf, NA, last_read),
                                    origin = "1970-01-01"))

users_write <- record_level %>%
  filter(!is.na(writer)) %>%
  mutate(writer_count = ifelse(is.na(writer), 0, 1)) %>%
  group_by(writer) %>%
  summarise(write_instances = sum(writer_count),
            last_write = max(as.Date(write_date), na.rm = TRUE),
            .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(last_write = as.Date(ifelse(last_write == -Inf, NA, last_write),
                                    origin = "1970-01-01"))

users_project_read <- record_level %>%
  filter(!is.na(reader)) %>%
  select(reader, project_name) %>%
  distinct() %>%
  group_by(reader) %>%
  mutate(users_read_conc = paste(project_name, collapse = "|")) %>%
  ungroup() %>%
  select(-project_name) %>%
  distinct()

users_project_write <- record_level %>%
  filter(!is.na(writer)) %>%
  select(writer, project_name) %>%
  distinct() %>%
  group_by(writer) %>%
  mutate(users_write_conc = paste(project_name, collapse = "|")) %>%
  ungroup() %>%
  select(-project_name) %>%
  distinct()

if(nrow(users_read) > nrow(users_write)) {
  users <- rename(users_read, user = reader) %>%
    left_join(users_write, by = c("user" = "writer"))
} else {
  users <- rename(users_write , user = writer) %>%
    left_join(users_read, by = c("user" = "reader"))
}

users <- users %>%
  left_join(users_project_read, by = c("user" = "reader")) %>%
  left_join(users_project_write, by = c("user" = "writer"))


# Projects ----------------------------------------------------------------

projects_read <- record_level %>%
  filter(!is.na(reader)) %>%
  mutate(reader_count = ifelse(is.na(reader), 0, 1)) %>%
  group_by(project_name) %>%
  summarise(read_instances = sum(reader_count),
            last_read = max(as.Date(read_date), na.rm = TRUE),
            .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(last_read = as.Date(ifelse(last_read == -Inf, NA, last_read),
                                    origin = "1970-01-01"))

projects_write <- record_level %>%
  filter(!is.na(writer)) %>%
  mutate(writer_count = ifelse(is.na(writer), 0, 1)) %>%
  group_by(project_name) %>%
  summarise(write_instances = sum(writer_count),
            last_write = max(as.Date(write_date), na.rm = TRUE),
            .groups = "drop") %>%
  distinct() %>%
  dplyr::mutate(last_write = as.Date(ifelse(last_write == -Inf, NA, last_write),
                                     origin = "1970-01-01"))

project_pins_read <- record_level %>%
  filter(!is.na(reader)) %>%
  select(project_name, pin_name) %>%
  distinct() %>%
  group_by(project_name) %>%
  mutate(project_read_conc = paste(pin_name, collapse = "|")) %>%
  ungroup() %>%
  select(-pin_name) %>%
  distinct()

project_pins_write <- record_level %>%
  filter(!is.na(writer)) %>%
  select(project_name, pin_name) %>%
  distinct() %>%
  group_by(project_name) %>%
  mutate(project_write_conc = paste(pin_name, collapse = "|")) %>%
  ungroup() %>%
  select(-pin_name) %>%
  distinct()

if(nrow(projects_read) > nrow(projects_write)) {
  projects <- projects_read %>%
    left_join(projects_write, by = c("project_name" = "project_name"))
} else {
  projects <- projects_write %>%
    left_join(projects_read, by = c("project_name" = "project_name"))
}

projects <- projects %>%
  left_join(project_pins_read, by = c("project_name" = "project_name")) %>%
  left_join(project_pins_write, by = c("project_name" = "project_name"))

# Pinning -----------------------------------------------------------------
l <- list(pin_summary = pins,
          user_summary = users,
          project_summary = projects)

kingpin::pin_throw(board, l, name = "kingpin_summary",
                   comment = "Kingpin summary data: Pins, users, projects")

# Clean kingpin data
kingpin <- list(
  records = data.frame(pin_name = "kingpin", # pin name
                       project_name = "none", # name of project associated with pin, if applicable
                       writer = Sys.info()["user"], # username of pin_write instance
                       write_date = Sys.time(), # date of pin_write instance
                       reader = NA, # username of pin_read instance
                       read_date = NA, # date of pin_read instance
                       comment = "Kingpin holding pin usage data"
  ))

kingpin::pin_throw(board, kingpin, name = "kingpin",
                   comment = "Kingpin holding pin usage data")
