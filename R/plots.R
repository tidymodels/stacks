#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' Plot results of a linear ensemble model
#' 
#' @param x A `linear_stack` object.
#' @param type A single character string for plot type with values "performance", 
#' "members", or "weights".
#' @param n An integer for how many members to plot when `type = "weights"`. 
#' @param ... Not currently used. 
#' @return A `ggplot` object. 
#' @details 
#' A "performance" plot shows the relationship between the lasso penalty and the
#' resampled performance metrics. The latter includes the average number of 
#' ensemble members. This plot can be helpful for understanding what penalty
#' values are reasonable. 
#' 
#' A "members" plot shows the relationship between the average number of 
#' ensemble members and the performance metrics. Each point is for a different
#' penalty value. 
#' 
#' Neither of the "performance" or "members" plots are helpful when a single
#' penalty is used. 
#' 
#' A "weights" plot shows the blending weights for the top ensemble members. The
#' results are for the final penalty value used to fit the ensemble. 
#' @export
autoplot.linear_stack <- function(x, type = "performance", n = Inf) {
  type <- match.arg(type, c("performance", "members", "weights"))
  dat <- x$metrics
  if (type == "members") {
    p <- member_plot(x)
  } else if (type == "performance") {
    p <- performance_plot(x)
  } else {
    p <- weights_plot(x, n)
  }
  p
}

member_plot <- function(x) {
  dat <- x$metrics
  plot_dat <- 
    dat %>% 
    dplyr::select(penalty, .config, mean, .metric) %>% 
    tidyr::pivot_wider(
      id_cols = c(penalty, .config),
      names_from = ".metric",
      values_from = "mean"
    )
  p <- 
    ggplot2::ggplot(plot_dat, ggplot2::aes(x = num_members, y = roc_auc)) +
    ggplot2::geom_point() + 
    ggplot2::xlab("Average number of members")
  p
}

performance_plot <- function(x) {
  dat <- x$metrics
  p <- 
    ggplot2::ggplot(dat, ggplot2::aes(x = penalty, y = mean)) +
    ggplot2::geom_point() + 
    ggplot2::geom_path() + 
    ggplot2::facet_wrap(~ .metric, scales = "free_y", ncol = 1) + 
    ggplot2::scale_x_log10()
  p
}

weights_plot <- function(x, n = Inf) {
  dat <- stacks:::top_coefs(x, n)
  
  if (any(names(dat) == "class")) {
    
  } else {
    p <- 
      dat %>% 
      dplyr::arrange(weight) %>% 
      dplyr::mutate(member = dplyr::row_number()) %>% 
      ggplot2::ggplot(ggplot2::aes(x = weight, y = format(member), fill = model)) + 
      ggplot2::geom_bar(stat = "identity") + 
      ggplot2::ylab("member") + 
      ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  }
  p
}

