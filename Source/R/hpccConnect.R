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
	hpccpassword <- gsub("\\!","%21",password )
	hpccpassword <- gsub("\\#","%23",hpccpassword )
	hpccpassword <- gsub("\\$","%24",hpccpassword )
	hpccpassword <- gsub("\\&","%26",hpccpassword )
	hpccpassword <- gsub("\\'","%27",hpccpassword )
	hpccpassword <- gsub("\\(","%28",hpccpassword )
	hpccpassword <- gsub("\\)","%29",hpccpassword )
	hpccpassword <- gsub("\\*","%2A",hpccpassword )
	hpccpassword <- gsub("\\+","%2B",hpccpassword )
	hpccpassword <- gsub("\\,","%2C",hpccpassword )
	hpccpassword <- gsub("\\/","%2F",hpccpassword )
	hpccpassword <- gsub("\\:","%3A",hpccpassword )
	hpccpassword <- gsub("\\;","%3B",hpccpassword )
	hpccpassword <- gsub("\\=","%3D",hpccpassword )
	hpccpassword <- gsub("\\?","%3F",hpccpassword )
	hpccpassword <- gsub("\\@","%40",hpccpassword )
	hpccpassword <- gsub("\\[","%5B",hpccpassword )
	hpccpassword <- gsub("\\]","%5D",hpccpassword )
	hpccresultLimit <- resultLimit
	connectionlist <- c(hpcchost,hpcctargetCluster,hpccuserid,hpccpassword,hpccresultLimit)
	connectionlist
}
