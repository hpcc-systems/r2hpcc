#' Creates a connection object for HPCC
#'
#' Takes in the HPCC host name, target cluster, user name, password, reult limit
#' and returns a connection object
#'
#' @param host - ip address of HPCC host
#' @param targetcluster - Target Thor cluster
#' @param userid - user id - if the cluster is secure
#' @param password - password associated with the user
#' @param resultLimit - limits the result at connection level
#' @export
r2hpcc.hpccConnect<- function (host, targetCluster,userId, password,  resultLimit) {
	suppressMessages(library(RCurl))
	suppressMessages(library(XML))
	hpcchost <- host
	hpcctargetCluster <- targetCluster
	hpccuserid <- userId
	hpccpassword <- password  
	hpccresultLimit <- resultLimit
	connectionlist <- c(hpcchost,hpcctargetCluster,hpccuserid,hpccpassword,hpccresultLimit)
	connectionlist
}
