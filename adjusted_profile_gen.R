## generate profiles for conjoint experiment

## underlying experimental design
experiment <- data.frame(
  party = sample(c("Conservative", "Labour", "Liberal Democrat", "Green Party"), 4),
  polls = sample(c(5, 10, 20, 30, 40), 4),
  constituency_margin = rep(sample(c("safe", "marginal"), 1), times = 4),
  change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4),
  constituency_change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4),
  sn_emphasised =
    as.integer(rbernoulli(4, p = 0.2)),
  dn_emphasised = 
    as.integer(rbernoulli(4, p = 0.2)),
  sl_emphasised = 
    as.integer(rbernoulli(4, p = 0.2)),
  dl_emphasised = 
    as.integer(rbernoulli(4, p = 0.2)),
  mediate = rep(
    as.integer(
      rbernoulli(1,
                 p = 0.5)), 
    times = 4)) %>%
  mutate(
    constituency = case_when(
      constituency_margin == "marginal" ~ sample(c(5, 10, 20, 30, 40), 4),
      constituency_margin == "safe" ~ sample(c(5, 10, 20, 60), 4, replace = F),
      TRUE ~ 0
    ),
    change = case_when(
      polls < 5 ~ sample(
      c(-10, -7, -5, -2, 0, 2), 4),
      polls < 7 ~ sample(
      c(-10, -7, -5, -2, 0, 2, 5), 4),
      polls < 10 ~ sample(
      c(-10, -7, -5, -2, 0, 2, 5, 7), 4),
      TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4)),
    constituency_change = case_when(
      constituency < 5 ~ sample(
        c(-10, -7, -5, -2, 0, 2), 4),
      constituency < 7 ~ sample(
        c(-10, -7, -5, -2, 0, 2, 5), 4),
      constituency < 10 ~ sample(
        c(-10, -7, -5, -2, 0, 2, 5, 7), 4),
      TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4))
  )

for(i in 1:1000) {
  experiment <- rbind(experiment,
                      data.frame(
                        party = sample(c("Conservative", "Labour", "Liberal Democrat", "Green Party"), 4),
                        polls = sample(c(5, 10, 20, 30, 40), 4),
                        constituency_margin = rep(sample(c("safe", "marginal"), 1), times = 4),
                        change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4),
                        constituency_change = sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4),
                        sn_emphasised =
                          as.integer(rbernoulli(4, p = 0.2)),
                        dn_emphasised = 
                          as.integer(rbernoulli(4, p = 0.2)),
                        sl_emphasised = 
                          as.integer(rbernoulli(4, p = 0.2)),
                        dl_emphasised = 
                          as.integer(rbernoulli(4, p = 0.2)),
                        mediate = rep(
                          as.integer(
                            rbernoulli(1,
                                       p = 0.5)), 
                          times = 4)) %>%
                        mutate(
                          constituency = case_when(
                            constituency_margin == "marginal" ~ sample(c(5, 10, 20, 30, 40), 4),
                            constituency_margin == "safe" ~ sample(c(5, 10, 20, 60), 4, replace = F),
                            TRUE ~ 0
                          ),
                          change = case_when(
                            polls < 5 ~ sample(
                              c(-10, -7, -5, -2, 0, 2), 4),
                            polls < 7 ~ sample(
                              c(-10, -7, -5, -2, 0, 2, 5), 4),
                            polls < 10 ~ sample(
                              c(-10, -7, -5, -2, 0, 2, 5, 7), 4),
                            TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4)),
                          constituency_change = case_when(
                            constituency < 5 ~ sample(
                              c(-10, -7, -5, -2, 0, 2), 4),
                            constituency < 7 ~ sample(
                              c(-10, -7, -5, -2, 0, 2, 5), 4),
                            constituency < 10 ~ sample(
                              c(-10, -7, -5, -2, 0, 2, 5, 7), 4),
                            TRUE ~ sample(c(-10, -7, -5, -2, 0, 2, 5, 7, 10), 4))
                        ))
}


## profiles displayed to respondents 

profiles <- experiment %>%
  mutate(
    sn_emphasis = case_when(
      sn_emphasised == 1 & polls == 40 ~ 
        "With a very strong showing in the polls, this party could quite feasibly land an overall majority.",
      sn_emphasised == 1 & polls == 30 ~ 
        "A large vote share like this means that this party will be one of the largest in parliament.",
      sn_emphasised == 1 & polls == 20 ~
        "With a decent vote share in the polls, this party looks set to win a considerable number of seats in parliament.",
      sn_emphasised == 1 & polls == 10 ~
        "With a small but non-trivial vote share in the polls, you would expect this party to win at least one or two seats in parliament.",
      sn_emphasised == 1 & polls == 5 ~
      "With such a small share of the vote, this party looks set to have very few, if any, seats in parliament.",
    TRUE ~ ""
  ),
  sl_emphasis = case_when(
    sl_emphasised == 1 & constituency_margin == "safe" & constituency == 60 ~ 
      "With such strong support in your constituency, this candidate looks certain to win the seat.",
    sl_emphasised == 1 & constituency_margin == "safe" & constituency == 20 ~ 
      "Despite a reasonable level of support in your constituency, this candidate looks to have little chance of winning the seat.",
    sl_emphasised == 1 & constituency_margin == "safe" & constituency < 20 ~ 
      "This candidate is not particularly well supported in your constituency and has no real chance of winning the seat.",
    sl_emphasised == 1 & constituency_margin == "marginal" & constituency == 40   ~ 
      "With strong support in your constituency, this candidate looks the most likely to win the seat.",
    sl_emphasised == 1 & constituency_margin == "marginal" & constituency == 30  ~ 
      "A good level of support in your constituency indicates that this candidate could challenge for the seat.",
    sl_emphasised == 1 & constituency_margin == "marginal" & constituency == 20  ~ 
      "Despite a reasonable level of support in your constituency, it looks unlikely this candidate will challenge for the seat.",
    sl_emphasised == 1 & constituency_margin == "marginal" & constituency < 20  ~ 
      "This candidate does not have nearly enough support in your constituency to be a serious contender for the seat.",
    TRUE ~ ""
  ),
  dn_emphasis = case_when(
    dn_emphasised == 1 & change <= -5 ~ 
      "This party is in a state of crisis, losing considerable ground in the polls.",
    dn_emphasised == 1 & change > -5 & change < 0 ~ 
      "This party appears to be losing ground slightly in the polls.",
    dn_emphasised == 1 & change == 0 ~ 
      "This party has neither lost nor gained ground in the polls.",
    dn_emphasised == 1 & change < 5 & change > 0 ~ 
      "This party appears to be gaining ground slightly in the polls.",
    dn_emphasised == 1 & change >= 5 ~ 
      "This party has the wind in its sails and is gaining considerable ground in the polls.",
    TRUE ~ ""
  ),
  dl_emphasis = case_when(
    dl_emphasised == 1 & constituency_change <= -5 ~ 
      "This candidate appears to be haemorrhaging support in your constituency.",
    dl_emphasised == 1 & constituency_change < 0 & constituency_change > -5  ~ 
      "This candidate seems to have lost support slightly in your constituency.",
    dl_emphasised == 1 & constituency_change == 0 ~ 
      "This candidate looks to have about the same level of support in your constituency as before.",
    dl_emphasised == 1 & constituency_change > 0 & constituency_change < 5  ~ 
      "This candidate appears to have gained support slightly in your constituency.",
    dl_emphasised == 1 & constituency_change >= 5 ~ 
      "This candidate has considerable momentum in your constituency, gaining support since the last election.",
    TRUE ~ ""
  ),
  polls = paste0("National polls: ",
                 floor(jitter(as.numeric(polls), amount = 2)), "%."),
  constituency = paste0("Constituency poll: ", floor(jitter(as.numeric(constituency), amount = 2)), "%"),
  constituency_change = case_when(
    constituency_change >= 0 ~ paste0("Constituency poll change since last election: +", constituency_change),
    constituency_change < 0 ~ paste0("Constituency poll change since last election: ", constituency_change)),
  emphases = paste0(sn_emphasis, " ", sl_emphasis, " ", dn_emphasis, " ", dl_emphasis),
  change = case_when(
    change >= 0 ~ paste0("National change since last poll: +", change),
    change < 0 ~ paste0("National change since last poll: ", change))
  ) %>%
  select(
    party,
    polls, 
    change,
    constituency,
    constituency_change,
    emphases)

saveRDS(experiment, here::here("data", "experiment.rds"))
saveRDS(profiles, here::here("data", "profiles.rds"))
