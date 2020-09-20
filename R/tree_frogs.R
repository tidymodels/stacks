#' Tree frog embryo hatching
#'
#' @description 
#' A dataset containing experimental results on hatching behavior of
#' red-eyed tree frog embryos.
#' 
#' Red-eyed tree frog (RETF) embryos can hatch earlier than their normal 7ish 
#' days if they detect potential predator threat. Researchers wanted to
#' determine how, and when, these tree frog embryos were able to detect
#' stimulus from their environment. To do so, they subjected the embryos
#' at varying developmental stages to "predator stimulus" by jiggling 
#' the embryos with a blunt probe. Beforehand, though some of the embryos were
#' treated with gentamicin, a compound that knocks out their lateral line
#' (a sensory organ.) Researcher Julie Jung and her crew found that these 
#' factors inform whether an embryo hatches prematurely or not!
#' 
#' @format A data frame with 1212 rows and 6 variables:
#' \describe{
#'   \item{clutch}{RETFs lay their eggs in gelatinous "clutches" of 30-40
#'   eggs. Eggs with the same clutch ID are siblings of each other! This
#'   variable is useful in mixed effects models. (Factor.)}
#'   \item{age}{Age group of the clutch? egg? Maybe these are the same?
#'   (what is the relationship to stimulus time here?) (Unit?)}
#'   \item{stimulus_time}{Time of stimulus ... (what is the relationship to age group here?
#'   When do the times "start"? By the way, this variable is just 
#'   (hour \* 3600) + (minute \* 60) + second) for the stimulus_\* variables.
#'   I put together hatch in the same way.)
#'   When does this time start? Numeric. (In seconds.)}
#'   \item{treatment}{The treatment group for the embryo. Either "gentamicin",
#'   a compound that knocks out the embryos' lateral line, or "control" for
#'   the negative control group (i.e. sensory organs intact). (Character.)}
#'   \item{hatched}{Whether or not the embryo hatched in response to the
#'   stimulus. (Logical.)}
#'   \item{hatch_time}{The time ... (starts at same time as stimulus, 
#'   presumably?) Numeric. (In seconds.)}
#' }
#' @source \url{https://doi.org/10.5061/dryad.p2ngf1vm7}
"tree_frogs"