#'
#' Check table format
#'
#' Internal function
#'
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#'
#'
#'
#' @return Logical value indicating if the data is properly formatted
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @noRd

chkt_f <- function(dataset)
{


req_col_nam<-c("Class","Order","Family","Subfamily","Tribe","Genus","Species","FG")
chk_form<-(colnames(dataset)[1:8] %in% req_col_nam)
if(any(chk_form==F)){
  message("Missing required columns:")
  message(req_col_nam[which(chk_form==F)])
  return (F)
}
else{
  return (T)
}
}
