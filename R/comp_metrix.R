#' Complete metrix analysis
#'
#' This function performs all the calculations availables in metrix package.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{Bioind_n}{The numerical values of the biotic indexes.}
#' \item{Bioind_c}{The water quality class asign to each sample site according to the numerical value of the biotic index.}
#' \item{d_diver}{A data.frame with all the calculated density measures.}
#' \item{Icbrio_n}{The numerical ICBrio index.}
#' \item{Icbrio_c}{The ICBrio water quality class.}
#' \item{Indrel}{A data.frame with all the calculated tolerance measures.}
#' \item{Perdiver}{A data.frame with all the calculated composition measures.}
#' \item{Pred}{A data.frame with all the calculated trophic measures.}
#' \item{Richdiver}{A data.frame with all the calculated richness measures.}
#'
#' @seealso \link[metrix]{read_data}, \link[metrix]{biotic_ind}, \link[metrix]{densi_diver}, \link[metrix]{icbrio_ind}, \link[metrix]{indrel_diver}, \link[metrix]{per_diver}, \link[metrix]{pred_ind}, \link[metrix]{rich_diver}
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @examples
#'#Example data is a properly formatted table with richness measures of two sites
#' example_data
#'
#'#Run comp_metrix with that example_data
#' complete<-comp_metrix(example_data)
#'
#'#Check results
#' complete$Bioind_n
#' complete$Bioind_c
#' complete$d_diver
#' complete$Icb_n
#' complete$Icb_c
#' complete$Indrel
#' complete$Perdiver
#' complete$Pred
#' complete$Richdiver
#' @export



comp_metrix <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  bioind<-biotic_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  d<-densi_diver(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  tol<-indrel_diver(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  per<-per_diver(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  pred<-pred_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  rich<-rich_diver(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_comp_metrix.csv",sep="")
    utils::write.table(x = bioind$Bioind_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    utils::write.table(x = bioind$Bioind_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
    utils::write.table(x = d              , file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = tol            , file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = pred           , file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    utils::write.table(x = rich           , file = result_fname, sep = ";", dec = dec_c, append = TRUE, col.names = FALSE )
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  comp<-list(Bioind_n = bioind$Bioind_n, Bioind_c = bioind$Bioind_c,
             d_diver = d, Indrel = tol, Perdiver = per, Pred = pred,
             Richdiver = rich)
  return(comp)
}
