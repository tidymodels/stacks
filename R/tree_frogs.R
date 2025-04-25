#' Tree frog embryo hatching data
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
#'   variable is useful in mixed effects models. (Unordered factor.)}
#'   \item{treatment}{The treatment group for the embryo. Either "gentamicin",
#'   a compound that knocks out the embryos' lateral line, or "control" for
#'   the negative control group (i.e. sensory organs intact). (Character.)}
#'   \item{reflex}{A measure of ear function called the vestibulo-ocular
#'   reflex, categorized into bins. Ear function increases from factor
#'   levels "low", to "mid", to "full". (Ordered factor.)}
#'   \item{age}{Age of the embryo, in seconds, at the time
#'   that the embryo was jiggled. (Numeric, in seconds.)}
#'   \item{t_o_d}{The time of day that the stimulus (i.e. jiggle)
#'   was applied. "morning" is 5 a.m. to noon, "afternoon" is noon to 8 p.m., and
#'   "night" is 8 p.m. to 5 a.m. (Character.)}
#'   \item{hatched}{Whether or not the embryo hatched in response to the
#'   jiggling! Either "yes" or "no". (Character.)}
#'   \item{latency}{Time elapsed between the stimulus (i.e. jiggling)
#'   and hatching in response to the stimulus, in seconds. Missing values indicate
#'   that the embryo didn't hatch in response to the stimulus. (Numeric,
#'   in seconds.)}
#' }
#'
#' @details
#' Note that the data included with the `stacks` package is not necessarily
#' a representative or unbiased subset of the complete dataset, and is only
#' for demonstrative purposes.
#'
#' @source
#'
#' Julie Jung et al. (2020) Multimodal mechanosensing enables treefrog
#' embryos to escape egg-predators. \doi{10.1242/jeb.236141}
"tree_frogs"
