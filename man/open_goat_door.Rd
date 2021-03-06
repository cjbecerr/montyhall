% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Open goat door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{'game'}{is used as the game that was created and 'a.pick' is used for the first door that the player selected.}
}
\value{
The function returns one of the doors that hasn't been selected, and has a goat behind it.
}
\description{
This is the door the host opens to show the player where one of the goats exists.
}
\details{
The game setup replicates the game on the TV show "Let's
  Make a Deal" where there are three doors for a contestant
  to choose from, one of which has a car behind it and two
  have goats. The contestant selects a door, then the host
  opens a door to reveal a goat, and then the contestant is
  given an opportunity to stay with their original selection
  or switch to the other unopened door. There was a famous
  debate about whether it was optimal to stay or switch when
  given the option to switch, so this simulation was created
  to test both strategies.
}
\examples{
 `open_goat_door( a.game, a.pick)`
}
