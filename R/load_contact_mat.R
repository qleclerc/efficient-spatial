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
  # contact_mat = matrix(c(1.920, 1.760, 4.970, 0.230,
  #                        0.433, 8.740, 5.477, 0.297,
  #                        0.434, 1.869, 7.832, 0.666,
  #                        0.195, 1.265, 5.165, 1.755),
  #                      ncol = 4, byrow = T,
  #                      dimnames = list(c("0-4","5-19","20-64","65+"),
  #                                      c("0-4","5-19","20-64","65+")))

  contact_mat = matrix(c(37.4622640266199,13.2337799407673,9.35866526693108,5.27807222067513,
                         17.2304141889828,98.1983003738366,17.0186152145963,10.1131975048866,
                         9.46784315245156,9.4416088929148,16.22285757548,5.7675253611147,
                         1.38284918679668,1.26680284573205,1.08367881504336,3.88324564380799),
                       ncol=4, byrow=T,
                       dimnames = list(c("0-4","5-19","20-64","65+"),
                                      c("0-4","5-19","20-64","65+")))

  #need to normalise?

  return(contact_mat)

}
