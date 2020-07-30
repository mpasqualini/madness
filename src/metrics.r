# team evaluation metrics ----

possession <- function(fga, fga3, fta, to, or) {
  poss = fga + fga3 + (0.475*fta) + to - or
}

off_efficiency <- function(score, possession) {
  offe = 100*(score/possession)
}

off_rebound_perc <- function(or, dr) {
  offrperc = or/(or + dr)
}

# player evaluation metrics ----

points_per_shot <- function(made, missed) {
  pps = made/(made+missed)
}