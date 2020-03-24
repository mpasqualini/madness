possession <- function(fga, fga3, fta, to, or) {
  poss = fga + fga3 + (0.475*fta) + to - or
}

off_efficiency <- function(score, possession) {
  offe = (score/possession)
}