#' @title Creates the age mixing matrix
#'
#' @description Creates the age mixing matrix for four age groups: 0-4, 5-19, 20-64 and 65+. Based on (REF)
#'
#' @return Returns one matrix object.
#'
#' @examples
#' contact_mat = load_contact_mat()
#' View(contact_mat)
#'
#' @export

load_contact_mat = function(){

  #create age mixing matrix:
  contact_mat = matrix(c(1.920, 1.760, 4.970, 0.230,
                         0.433, 8.740, 5.477, 0.297,
                         0.434, 1.869, 7.832, 0.666,
                         0.195, 1.265, 5.165, 1.755),
                       ncol = 4, byrow = T,
                       dimnames = list(c("0-4","5-19","20-64","65+"),
                                       c("0-4","5-19","20-64","65+")))

  return(contact_mat)

}
