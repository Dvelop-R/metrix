#' Biotic indexes
#'
#' Calculates BMWP, BMWP', BMWP'', IMRP and ICBrio indexes
#'
#' The biotic indicators consist of the combination of two or three properties of the association: taxa richness and tolerance/intolerance to contamination for qualitative indices, and these together with abundance (absolute or relative) for quantitative indices. They are usually expressed in the form of a single numerical value that synthesizes the characteristics of all the species present.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{bioind_n}{The numerical values of the biotic indexes.}
#' \item{bioind_c}{The water quality class assign to each sample site according to the numerical value of the biotic indexes}
#'
#' @seealso \link[metrix]{read_data}, \link{bmwp_ind}, \link{bmwp_p_ind}, \link{bmwp_p_p_ind}, \link{imrp_ind}, \link{icbrio_ind}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Armitage PD, Moss D, Wright JF & Furse MT (1983). The performance of a new biological water quality score system based on macroinvertebrates over a wide range of unpolluted running-water sites. \doi{10.1016/0043-1354(83)90188-4}
#' @references Alba-Tercedor J & Sánchez-Ortega A (1988). A simple and quick method to evaluate biological quality of running freshwater based on Hellawell (1978). \url{https://www.limnetica.com/documentos/limnetica/limnetica-4-1-p-51.pdf}
#' @references Loyola RGN (2000). Atual estágio do IAP no uso de índices biológicos de qualidade. Bioindicadores qualidade aguas \url{https://www.iat.pr.gov.br/sites/agua-terra/arquivos_restritos/files/documento/2021-03/bioindicadores_qualidade_aguas_2001_2002.pdf}
#' @references Rodrigues Capítulo A (1999). The macroinvertebrate as indicators of water quality in Pampean rivers. \url{https://www.biotaxa.org/RSEA/article/view/32771}
#' @references Kuhlmann M, Imbimbo HV, Ogura LL (2012). Protocolo para o biomonitoramento com as comunidades bentônicas de rios e reservatórios do estado de São Paulo. \url{https://cetesb.sp.gov.br/aguas-interiores/wp-content/uploads/sites/12/2013/11/protocolo-biomonitoramento-2012.pdf}
#'
#' @examples
#'
#'#Load example data
#' example_data
#'
#'#Run biotic_ind with that example_data
#' biotic<-biotic_ind(example_data)
#'
#'#Check results
#' biotic$bioind_n
#' biotic$bioind_c
#' @export


biotic_ind <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  b<-bmwp_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  bp<-bmwp_p_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  bpp<-bmwp_p_p_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  im<-imrp_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)
  ic<-icbrio_ind(dataset, store = FALSE, dec_c = dec_c, verbose=verbose)

  bioind_n<-data.frame(rbind(b$Ibmwp_n,bp$Ibmwp_p_n,bpp$Ibmwp_p_p_n,im$Imrp_n,ic$Icbrio_n))
  bioind_c<-data.frame(rbind(b$Ibmwp_c,bp$Ibmwp_p_c,bpp$Ibmwp_p_p_c,im$Imrp_c,ic$Icbrio_c))
  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_biotic_ind.csv",sep="")
    utils::write.table(x = bioind_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    utils::write.table(x = bioind_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  bioind<-list(bioind_n=bioind_n, bioind_c=bioind_c)
  return(bioind)
}
