glue_stop <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_warn <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::warn(glue::glue(..., .sep = .sep, .envir = .envir))
}

glue_message <- function(..., .sep = "", .envir = parent.frame()) {
  rlang::inform(glue::glue(..., .sep = .sep, .envir = .envir))
}

chr_check <- function(x) {
  cl <- match.call()
  
  if (is.null(x)) {
    glue_stop("Element `{cl$x}` should not be NULL.")
  }
  
  if (!is.character(x)) {
    glue_stop("Element `{cl$x}` should be a character string.")
  }
  
  invisible(TRUE)
}

# more easily arranged names (from recipes)
names0 <- function(num, prefix = "x") {
  if (num < 1)
    rlang::abort("`num` should be > 0.")
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

check_hash <- function(stack, member) {
  if (stack$rs_hash == "init") {stack$rs_hash <- digest::digest(member$splits)}
  
  hash_matches <- stack$rs_hash == digest::digest(member$splits)
  
  if (!hash_matches) {
    glue_stop(
      "It seems like the member you added in {match.call()} doesn't make use ",
      "of the same resampling object as the existing members."
    )
  }
  
  stack
}

check_member_name <- function(stack, member) {
  # check to make sure that the supplied sub-model set (member) 
  # doesn't have the same name as an existing member
  invisible(TRUE)
}



