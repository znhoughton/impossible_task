library(dplyr)
library(tidyr)
library(purrr)
library(clue)
library(irr)
library(ggplot2)
library(writexl)



ethogram <- c("alternating gaze", "looking at owner", "positioning in front of owner", 
              "bark/whine vocalization", "interacting with tupperware", "nudging/scratching/paw owner")
df_all=read.csv("impossible_task_all_behaviors.csv") %>% filter(Behavior %in% ethogram)%>%
  filter(Behavior != "alternating gaze")

df2 <- df_all %>%
  rename(
    obs     = `Observation.id`,
    behavior= Behavior,
    btype   = `Behavior.type`,
    t_start = `Start..s.`,
    t_stop  = `Stop..s.`
  ) %>%
  mutate(
    behavior = factor(behavior),
    # factorise behaviors
    ybase    = as.numeric(behavior),
    # offset for coders
    y        = ybase + ifelse(coder == "Amanda", -0.2, +0.2)
  )
# Plotting behaviors
ggplot(df2, aes(color = coder)) +
  geom_segment(
    data     = filter(df2, btype == "STATE"),
    aes(x = t_start, xend = t_stop, y = behavior, yend = behavior),
    size     = 2,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(
    data     = filter(df2, btype == "POINT"),
    aes(x = t_start, y = behavior),
    size     = 3,
    position = position_dodge(width = 0.6)
  ) +
  facet_wrap(~ obs, scales = "free_x") +
  scale_y_discrete(drop = FALSE) +   # <-- aquí
  labs(x = "Tiempo (s)", y = "Comportamiento", color = "Coder") +
  theme_bw()

# AGREEMENT SUMMARY 
#  Count how many times each coder annotated each behavior within each trial
counts_per_trial <- df_all %>%
  rename(
    trial    = `Observation.id`,
    behavior = Behavior
  ) %>%
  group_by( behavior, coder) %>%
  summarise(
    n_events = n(),
    .groups  = "drop"
  )

# Pivot to wide form so each row is one (trial, behavior) combination
# and we get one column per coder
counts_wide <- counts_per_trial %>%
  pivot_wider(
    names_from  = coder,
    values_from = n_events,
    values_fill = 0
  )

# For each (trial, behavior), compute the maximum possible overlap
# i.e. you cannot match more events than the smaller of the two counts
counts_wide <- counts_wide %>%
  mutate(
    max_possible_overlap   = pmin(Amanda, Chaewon),
    ceiling_agreement_rate = max_possible_overlap / pmax(Amanda, Chaewon)
  )

summary_row <- counts_wide %>%
  summarise(
    trial                   = "Overall",
    behavior                = "Max Possible Overlap Overall Behaviors",
    Amanda                  = sum(Amanda),
    Chaewon                 = sum(Chaewon),
    max_possible_overlap    = sum(max_possible_overlap),
    ceiling_agreement_rate  = sum(max_possible_overlap) / pmax(sum(Amanda), sum(Chaewon))
  )

counts_final <- bind_rows(counts_wide, summary_row)

# Display the final table
print(counts_final)

# Prepare a flat event table using start‐times as the anchoring point
events <- df_all %>%
  rename(
    trial    = `Observation.id`,
    behavior = Behavior,
    time     = `Start..s.`
  ) %>%
  select(trial, behavior, coder, time)

# Matching function per trial × behavior
match_within_tol <- function(df_sub, tol = 8) {
  # Split Amanda vs Chaewon times
  A_times <- df_sub %>% filter(coder == "Amanda") %>% pull(time)
  B_times <- df_sub %>% filter(coder == "Chaewon") %>% pull(time)
  nA <- length(A_times); nB <- length(B_times); n <- max(nA, nB)
  
# Build cost matrix: 0 if |dt| <= tol, 1 otherwise
  cost <- matrix(1, n, n)
  if (nA > 0 && nB > 0) {
    dt <- abs(outer(A_times, B_times, "-"))
    cost[1:nA, 1:nB] <- ifelse(dt <= tol, 0, 1)
  }
  
# Solve assignment to minimize sum(cost) ⇒ maximize #zeros ⇒ max matches
  sol <- solve_LSAP(cost)
  
# Build matched/unmatched rows for Amanda
  dfA <- tibble(
    trial     = df_sub$trial[1],
    behavior  = df_sub$behavior[1],
    coder     = "Amanda",
    time      = A_times,
    partner   = as.integer(sol[1:nA])
  ) %>%
    mutate(
      matched   = (partner <= nB) & (cost[cbind(row_number(), partner)] == 0),
      time_other = ifelse(matched, B_times[partner], NA_real_)
    ) %>%
    select(-partner)
  
# And for Chaewon: any B_times that weren’t matched to an A
  matched_B_idx <- sol[1:nA]
  unmatched_B <- setdiff(seq_len(nB),
                         matched_B_idx[which(matched_B_idx <= nB &
                                               cost[cbind(which(matched_B_idx <= nB),
                                                          matched_B_idx[matched_B_idx <= nB])] == 0)])
  dfB <- tibble(
    trial      = df_sub$trial[1],
    behavior   = df_sub$behavior[1],
    coder      = "Chaewon",
    time       = B_times,
    matched    = seq_len(nB) %in% (matched_B_idx[matched_B_idx <= nB &
                                                   cost[cbind(which(matched_B_idx <= nB),
                                                              matched_B_idx[matched_B_idx <= nB])] == 0]),
    time_other = map_dbl(seq_len(nB), ~{
      ai <- which(matched_B_idx == .x)
      if (length(ai)==1 && cost[ai, .x] == 0) A_times[ai] else NA_real_
    })
  )
  
  bind_rows(dfA, dfB)
}

#  Apply matching over every trial × behavior
match_table <- events %>%
  group_by(trial, behavior) %>%
  nest() %>%
  mutate(matched = map(data, ~match_within_tol(.x, tol = 8))) %>%
  select(trial, behavior, matched) %>%
  unnest(matched)

# Summarise agreement per behavior (overall across trials)
agreement_by_behavior <- match_table %>%
  group_by(behavior, coder) %>%
  tally(name = "n_events") %>%
  pivot_wider(names_from = coder, values_from = n_events, values_fill = 0) %>%
  left_join(
    match_table %>%
      filter(matched) %>%
      count(behavior, name = "matches"),
    by = "behavior"
  ) %>%
  mutate(
    matches = replace_na(matches, 0),
    ceiling_agreement = matches / pmax(Amanda, Chaewon)
  )

# Overall summary row
overall <- agreement_by_behavior %>%
  summarise(
    behavior            = "Overall",
    Amanda              = sum(Amanda),
    Chaewon             = sum(Chaewon),
    matches             = sum(matches),
    ceiling_agreement   = matches / pmax(Amanda, Chaewon)
  )

agreement_summary <- bind_rows(agreement_by_behavior, overall)

# Inspect your final tables:
print(match_table)        # one row per event, with match flag + partner time
print(agreement_summary)  # per‐behavior + overall ceiling agreement


# Visualize

# Join your duration data with the match results
df_vis <- df_all %>%
  rename(
    trial    = `Observation.id`,
    behavior = Behavior,
    btype    = `Behavior.type`,
    t_start  = `Start..s.`,
    t_stop   = `Stop..s.`
  ) %>%
  # bring in the matched flag from match_table
  left_join(
    match_table %>% select(trial, behavior, coder, time, matched),
    by = c("trial", "behavior", "coder", "t_start" = "time")
  ) %>%
  # anything not joined is unmatched
  mutate(
    matched      = ifelse(is.na(matched), FALSE, matched),
    match_status = ifelse(matched, "matched", "unmatched"),
    behavior     = factor(behavior),
    coder        = factor(coder)
  )

# Define a dodge so Amanda/ChaeWon sit side by side
dodge <- position_dodge(width = 0.6)

# Plot: STATE as segments, POINT as dots, colored by match_status
ggplot(df_vis, aes(group = coder, color = match_status)) +
  # draw state durations
  geom_segment(
    data     = filter(df_vis, toupper(btype) == "STATE"),
    aes(
      x    = t_start,
      xend = t_stop,
      y    = behavior,
      yend = behavior
    ),
    size     = 2,
    position = dodge
  ) +
  # draw point events
  geom_point(
    data     = filter(df_vis, toupper(btype) == "POINT"),
    aes(x = t_start, y = behavior),
    size     = 3,
    shape=4,
    alpha=0.8,
    position = dodge
  ) +
  # pick your colors
  scale_color_manual(
    values = c(matched = "forestgreen", unmatched = "firebrick")
  ) +
  facet_wrap(~ trial, scales = "free_x") +
  labs(
    x     = "Time (s)",
    y     = "Behavior",
    color = "Match Status"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text  = element_text(face = "bold")
  )


# Drop "alternating gaze"
mt2 <- match_table %>%
  filter(behavior != "alternating gaze")%>%
  filter(behavior != "bark/whine vocalization")

# Give each event a slot index within its trial+behavior
mt2 <- mt2 %>%
  group_by(trial, behavior, coder) %>%
  mutate(slot = row_number()) %>%
  ungroup()

# Pivot so each slot has one Amanda & one Chaewon row, filling FALSE if missing
wide_match <- mt2 %>%
  select(trial, behavior, slot, coder, matched) %>%
  pivot_wider(
    names_from  = coder,
    values_from = matched,
    values_fill = FALSE)


# Compute Cohen’s κ on those two logical columns
# (they’ll be coerced to factors internally)
kappa_res <- kappa2(
  wide_match %>% select(Amanda, Chaewon),
  weight = "unweighted")


actual_matches <- match_table %>%ungroup() %>% select(-trial) %>% 
  # count each matched pair once by looking at Amanda’s side
  filter(matched, coder == "Amanda") %>%
  count(behavior, name = "actual_matches")

# Compute Cohen’s κ for each behavior
kappa_by_behavior <- wide_match %>%
  group_by(behavior) %>%
  summarise(
    # run unweighted κ on the two logical columns
    kappa = kappa2(
      select(cur_data(), Amanda, Chaewon),
      weight = "unweighted"
    )$value,
    .groups = "drop"
  )

# Add percent‐agreement to your counts_final table
counts_final_augmented <- counts_final %>%
  left_join(actual_matches, by = "behavior") %>%
  mutate(
    actual_matches      = replace_na(actual_matches, 0),
    # percent agreement = matched pairs / max events by either coder
    percent_agreement   = actual_matches / pmax(Amanda, Chaewon)
  ) %>% select(-trial)


# Drop the existing “Max Possible Overlap Overall Behaviors” row
counts_base <- counts_final_augmented %>%
  filter(behavior != "Max Possible Overlap Overall Behaviors")

# Recompute the overall summary
overall_summary <- counts_base %>%
  summarise(
    behavior               = "Overall",
    Amanda                 = sum(Amanda),
    Chaewon                = sum(Chaewon),
    max_possible_overlap   = sum(max_possible_overlap),
    ceiling_agreement_rate = sum(max_possible_overlap) / pmax(sum(Amanda), sum(Chaewon)),
    actual_matches         = sum(actual_matches),
    percent_agreement      = actual_matches / pmax(Amanda, Chaewon)
  )

# Append the new Overall row
counts_final_with_overall <- bind_rows(counts_base, overall_summary)

# Write the table to an Excel workbook
write_xlsx(
  counts_final_with_overall,
  path = "agreement_summary.xlsx"
)

# Define 3-second windows for each trial
windows <- df_all %>%
  rename(trial = `Observation.id`, t_start = `Start..s.`, t_stop = `Stop..s.`) %>%
  group_by(trial) %>%
  summarise(max_time = max(t_stop), .groups = "drop") %>%
  mutate(window_start = map(max_time, ~ seq(0, .x, by = 3))) %>%
  unnest(window_start) %>%
  mutate(window_end = pmin(window_start + 3, max_time)) %>%
  select(trial, window_start, window_end)

# Expand to every (trial, window, behavior, coder) slot
ethogram <- c("looking at owner", "positioning in front of owner",
              "bark/whine vocalization", "interacting with tupperware")
coders   <- c("Amanda", "Chaewon")

slots <- windows %>%
  expand(trial, window_start, window_end,
         behavior = ethogram, coder = coders)

# Flag presence if any event of that behavior/coder overlaps the window
presence <- slots %>%
  left_join(df_all %>%
              rename(trial = `Observation.id`, t_start = `Start..s.`, t_stop = `Stop..s.`),
            by = "trial") %>%
  filter(behavior == Behavior, coder == coder) %>%
  mutate(overlap = (t_start < window_end) & (t_stop > window_start)) %>%
  group_by(trial, window_start, window_end, behavior, coder) %>%
  summarise(present = any(overlap, na.rm = TRUE), .groups = "drop")

# Pivot to wide for Cohen’s κ
presence_wide <- presence %>%
  pivot_wider(names_from = coder, values_from = present, values_fill = list(present = FALSE))

# Compute κ per behavior across all windows & trials
kappa_by_behavior <- presence_wide %>%
  group_by(behavior) %>%
  summarise(
    kappa = kappa2(select(cur_data(), Amanda, Chaewon),
                   weight = "unweighted")$value,
    .groups = "drop"
  )
