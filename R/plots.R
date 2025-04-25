#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' Plot results of a stacked ensemble model.
#'
#' @param object A `linear_stack` object outputted from [blend_predictions()]
#' or [fit_members()].
#' @param type A single character string for plot type with values "performance",
#' "members", or "weights".
#' @param n An integer for how many members weights to plot when
#' `type = "weights"`. With multi-class data, this is the total number of weights
#' across classes; otherwise this is equal to the number of members.
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
autoplot.linear_stack <- function(object, type = "performance", n = Inf, ...) {
  type <- match.arg(type, c("performance", "members", "weights"))
  dat <- object$metrics
  if (type == "members") {
    p <- member_plot(object)
  } else if (type == "performance") {
    p <- performance_plot(object)
  } else {
    p <- weights_plot(object, penalty = object$penalty$penalty, n = n)
  }
  p
}

member_plot <- function(x) {
  dat <- x$metrics

  plot_dat <-
    dat |>
    dplyr::select(penalty, mixture, .config, mean, .metric)

  memb_data <-
    dplyr::filter(plot_dat, .metric == "num_members") |>
    dplyr::rename(num_members = mean) |>
    dplyr::select(-.metric)

  other_metrics <- dplyr::filter(plot_dat, .metric != "num_members")

  plot_dat <-
    dplyr::full_join(
      memb_data,
      other_metrics,
      by = c("penalty", "mixture", ".config"),
      multiple = "all"
    )

  mult_mix <- length(unique(plot_dat$mixture)) > 1

  if (mult_mix) {
    plot_dat$mixture <- format(plot_dat$mixture)
    p <- ggplot2::ggplot(
      plot_dat,
      ggplot2::aes(x = num_members, y = mean, col = mixture)
    )
  } else {
    p <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = num_members, y = mean))
  }

  p <-
    p +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(
      ~.metric,
      nrow = length(unique(other_metrics$.metric)),
      scales = "free_y"
    ) +
    ggplot2::xlab("Average number of members")

  p
}

performance_plot <- function(x) {
  dat <- x$metrics
  mult_mix <- length(unique(dat$mixture)) > 1

  if (mult_mix) {
    dat$mixture <- format(dat$mixture)
    p <- ggplot2::ggplot(
      dat,
      ggplot2::aes(x = penalty, y = mean, col = mixture)
    )
  } else {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = penalty, y = mean))
  }
  p <-
    p +
    ggplot2::geom_vline(xintercept = x$penalty$penalty, lty = 2) +
    ggplot2::geom_point() +
    ggplot2::geom_path() +
    ggplot2::facet_wrap(~.metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_log10()
  p
}

weights_plot <- function(x, penalty = x$penalty$penalty, n = Inf) {
  dat <- top_coefs(x, penalty = penalty, n = n) |>
    dplyr::rename(terms = member, model = type)

  if (any(names(dat) == "class")) {
    dat_order <-
      dat |>
      dplyr::group_by(model, terms) |>
      dplyr::summarize(mean = max(abs(weight), na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::arrange(mean) |>
      dplyr::mutate(member = dplyr::row_number()) |>
      dplyr::select(-mean)
    dat <- dplyr::full_join(dat, dat_order, by = c("model", "terms"))
  } else {
    dat <-
      dat |>
      dplyr::arrange(abs(weight)) |>
      dplyr::mutate(member = dplyr::row_number())
  }
  p <-
    ggplot2::ggplot(
      dat,
      ggplot2::aes(x = weight, y = format(member), fill = model)
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::ylab("Member") +
    ggplot2::ggtitle(paste(
      "penalty =",
      format(x$coefs$spec$args$penalty, digits = 3, scientific = FALSE)
    )) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::xlab("Stacking Coefficient")

  if (any(names(dat) == "class")) {
    p <- p + ggplot2::facet_wrap(~class)
  }
  p
}
