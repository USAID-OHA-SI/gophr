#' MER Snapshot indicators
#'
#' List of indicators in the MER that are snapshot indicators, eg TX_CURR,
#' as opposed to a cumulative one, like HTS_TST. Snapshot indicators are handled
#' differently for calculating cumulative values and target achievement.
#'
#' @usage data(snapshot_ind)
#'
#' @format A list of all snapshot indicators
#' \describe{
#'   \item{snapshot_ind}{indicator names}
#' }

"snapshot_ind"


#' MER Clinical Cascade indicators
#'
#' List of indicators in the MER that comprise the clinical cascade. These
#' indicators are needed for calculating the 90s/95s plus linkage and viral load
#' coverage.
#'
#' @usage data(cascade_ind)
#'
#' @format A list of clinical cascade indicators
#' \describe{
#'   \item{cascade_ind}{indicator names}
#' }

"cascade_ind"
