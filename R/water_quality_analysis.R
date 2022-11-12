#' Water quality analysis
#'
#' This function performs all the calculations available in metrix package.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{bioind_n}{The numerical values of the biotic indexes.}
#' \item{bioind_c}{The water quality class asign to each sample site according to the numerical value of the biotic index.}
#' \item{densimetrics}{A data.frame with all the calculated density measures.}
#' \item{tolmetrics}{A data.frame with all the calculated tolerance measures.}
#' \item{compmetrics}{A data.frame with all the calculated composition measures.}
#' \item{trophmetrics}{A data.frame with all the calculated trophic measures.}
#' \item{richmetrics}{A data.frame with all the calculated richness measures.}
#'
#' @seealso \link[metrix]{read_data}, \link[metrix]{biotic_ind}, \link[metrix]{densi_metrics}, \link[metrix]{icbrio_ind}, \link[metrix]{tol_metrics}, \link[metrix]{comp_metrics}, \link[metrix]{troph_metrics}, \link[metrix]{rich_metrics}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#' @examples
#'#Load example data
#' example_data
#'
#'#Run water_quality_analysis with that example_data
#' complete<-water_quality_analysis(example_data)
#'
#'#Check results
#' complete$bioind_n
#' complete$bioind_c
#' complete$densimetrics
#' complete$tolmetrics
#' complete$compmetrics
#' complete$trophmetrics
#' complete$richmetrics
#' @export



water_quality_analysis <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  bioind<-biotic_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  densimetrics<-densi_metrics(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  tolmetrics<-tol_metrics(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  compmetrics<-comp_metrics(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  trophmetrics<-troph_metrics(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  richmetrics<-rich_metrics(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_water_quality_analysis.csv",sep="")
    utils::write.table(x = bioind$bioind_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    utils::write.table(x = bioind$bioind_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
    utils::write.table(x = densimetrics, file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = tolmetrics, file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = compmetrics, file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = trophmetrics, file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = richmetrics, file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  wqa<-list(bioind_n = bioind$bioind_n, bioind_c = bioind$bioind_c,
             densimetrics = densimetrics, tolmetrics = tolmetrics, compmetrics = compmetrics,
             trophmetrics = trophmetrics, richmetrics = richmetrics)
  return(wqa)
}
