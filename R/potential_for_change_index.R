#' Potential for Change Index
#'
#' Computes the Potential for change index for one or multiple determinants.
#'
#' The Potential for Change index was developed by Keegan et al. and is a
#' numerical representation of a number of important features in [CIBER()]
#' plots (for more details, please see the references below).
#'
#' The original Potential for Change Index was conceptualized to optimize
#' intervention tailoring and improve the prediction of individual-level
#' intervention effectiveness. This function currently only computes two
#' sample-level variants that can facilitate sub-determinant selection.
#'
#' The first (`type = 'sample-level 1'`) is computed as follows:
#'
#' - For sub-determinants with a positive zero-order correlation with
#' behavior, the sample mean was subtracted from the observed maximum score,
#' and the result was multiplied by the zero-order correlation;
#' - For sub-determinants with a negative zero-order correlation with
#' behavior, the sample mean was subtracted from the observed minimum score,
#' and the result was multiplied by the zero-order correlation.
#'
#' The second (`type = 'sample-level 2'`) is computed as follows:
#'
#' - For sub-determinants with a positive zero-order correlation with
#' behavior, the sample mean was subtracted from the .95 quantile of the
#' scores, and the result was multiplied by the squared zero-order
#' correlation (i.e. the proportion of explained variance);
#' - For sub-determinants with a negative zero-order correlation with
#' behavior, the sample mean was subtracted from the .05 quantile of the
#' scores, and the result was multiplied by the squared zero-order
#' correlation (i.e. the proportion of explained variance);
#'
#' The second variant effectively takes the 5% trimmed maximum and minimum,
#' rendering it less sensitive to outliers, penalizes weak associations with
#' behavior more severely, and decreases sensitivity to differences between
#' correlations. These differences should render the second variant a bit more
#' robust over different samples.
#'
#' @param data The dataframe containing the variables.
#' @param determinants The name(s) of the determinant(s).
#' @param target The target (e.g. behavior or intention).
#' @param type The type of potential for change index. Currently implemented
#' are `'sample-level 1'` and `'sample-level 2'` - see details for more
#' information.
#' @param minimum,maximum The functions to return the minimum and maximum to
#' use in the computation. If `NULL`, [min()] and [max()] are used
#' with `na.rm=TRUE`.
#' @param center The function to return the central tendency estimate to use
#' in the computation. If `NULL`, [mean()] is used with `na.rm=TRUE`.
#' @param weight The function to return the weight/multiplier to use in the
#' computation. If `NULL`, [stats::cor()] is used with `use="complete.obs"`.
#'
#' @return If one determinant, the potential for change index for that
#' detreminant; if multiple, a list with those indices.
#' @export
#' @references Knittle, K. P., Peters, G.-J. Y., Heino, M. T. J., Tobias, R., &
#' Hankonen, N. (2019). Potential for change: New metrics for tailoring and
#' predicting response to behavior change interventions. \doi{10/ghqmg3}
#'
#' @examples dat <- get(data("BBC_pp15.1", package="behaviorchange"))
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants='highDose_attitude',
#'   target='highDose_intention'
#' );
#'
#' ### Or for multiple determinants
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants=c('highDose_attitude', 'highDose_perceivedNorm'),
#'   target='highDose_intention'
#' );
#'
#' ### Get the Potential for Change Index Type 2
#' behaviorchange::potential_for_change_index(
#'   data=dat,
#'   determinants=c('highDose_attitude', 'highDose_perceivedNorm'),
#'   target='highDose_intention',
#'   type = 'sample-level 2'
#' );
#'
potential_for_change_index <- function(data,
                                       determinants,
                                       target,
                                       type = "sample-level 1",
                                       minimum = NULL,
                                       maximum = NULL,
                                       center = NULL,
                                       weight = NULL) {

  type <- trimws(tolower(type));

  if (length(determinants) > 1) {
    res <-
      lapply(
        determinants,
        potential_for_change_index,
        data = data,
        target = target,
        type = type,
        minimum = minimum,
        maximum = maximum,
        center = center,
        weight = weight
      );
    res <- unlist(res);
    names(res) <- determinants;
  } else {
    if (is.null(minimum)) {
      if (type == "sample-level 1") {
        minimum <- min(data[[determinants]], na.rm=TRUE);
      } else if (type == "sample-level 2") {
        minimum <-
          round(stats::quantile(data[[determinants]], .05, na.rm=TRUE), 1);
      }
    } else {
      minimum <- minimum(data[[determinants]]);
    }
    if (is.null(maximum)) {
      if (type == "sample-level 1") {
        maximum <- max(data[[determinants]], na.rm=TRUE);
      } else if (type == "sample-level 2") {
        maximum <-
          round(stats::quantile(data[[determinants]], .95, na.rm=TRUE), 1);
      }
    } else {
      maximum <- maximum(data[[determinants,]]);
    }
    if (is.null(center)) {
      if (type == "sample-level 1") {
        center <- mean(data[[determinants]], na.rm=TRUE);
      } else if (type == "sample-level 2") {
        center <- mean(data[[determinants]], na.rm=TRUE);
      }
    } else {
      center <- center(data[[determinants]]);
    }
    if (is.null(weight)) {
      if (type == "sample-level 1") {
        weight <- stats::cor(data[, determinants],
                             data[, target],
                             use = "complete.obs");
      } else if (type == "sample-level 2") {
        weight <- stats::cor(data[, determinants],
                             data[, target],
                             use = "complete.obs")^2;
      }
    } else {
      weight <- weight(data[, determinants], data[, target]);
    }
    if (weight < 0) {
      res <- (minimum - center) * weight;
    } else {
      res <- (maximum - center) * weight;
    }
  }

  return(res);

}