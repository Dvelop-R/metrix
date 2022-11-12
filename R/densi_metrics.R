#' Density metrics
#'
#' Calculates density measures
#'
#' Density is a universal measure used in all types of biological studies. Density is best classified with trophic measurements because it is an element of production; however, it is difficult to interpret because it requires careful quantification and is not monotonous in response (i.e., density can decrease or increase in response to contamination) (Barbour et al., 1996).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated density measures:
#' \item{den_chir_dip}{Diptera Chironomidae density.}
#' \item{den_non_chir_dip}{Diptera no Chironomidae density.}
#' \item{den_ephe}{Ephemeroptera density.}
#' \item{den_molus}{Mollusca density.}
#' \item{den_gastr}{Gastropoda density.}
#' \item{den_biv}{Bivalvia density.}
#' \item{den_crus}{Crustacea density.}
#' \item{den_nais}{Naididae density.}
#' \item{den_lhoff}{\emph{Limnodrilus} hoffmeisteri density.}
#' \item{den_bothr}{\emph{Bothrioneurum} density.}
#' \item{den_tubi}{\emph{Tubifex} density.}
#' \item{den_dero}{\emph{Dero} density.}
#' \item{den_prist}{\emph{Pristina} density.}
#' \item{den_chiro}{\emph{Chironomus} density.}
#' \item{den_nais}{\emph{Nais} density.}
#' \item{den_hele}{\emph{Heleobia} density.}
#' \item{den_subchiro}{\emph{Chironominae} density.}
#' \item{den_suborth}{Orthocladiinae density.}
#'\item{den_subtany}{Tanypodinae density.}
#'\item{den_t}{Total density.}
#'\item{den_t_bothr}{\emph{Bothrioneurum}/Total density.}
#'\item{den_t_lhoff}{\emph{Limnodrilus hoffmeisteri}/Total density.}
#'\item{den_t_tubi}{\emph{Tubifex}/Total density.}
#'\item{den_t_dero}{\emph{Dero}/Total density.}
#'\item{den_t_prist}{\emph{Pristina}/Total density.}
#'\item{den_t_chiro}{\emph{Chironomus}/Total density.}
#'\item{den_oli}{Oligochaeta density.}
#'\item{den_tricho}{Trichoptera density.}
#'\item{den_ostr}{Ostracoda density.}
#'\item{den_amph}{Amphipoda density.}
#'\item{den_polym}{Polymitarcidae density.}
#'\item{den_hyal}{Hyalella density.}
#'\item{den_coch}{Cochliopidae density.}
#'\item{den_chironomidae}{Chironomidae density.}
#'
#'
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Barbour MT, Gerritsen J, Griffith GE, Frydenborg R, McCarron E, White JS & Bastian ML (1996). A Framework for Biological Criteria for Florida Streams Using Benthic Macroinvertebrates. \doi{10.2307/1467948}
#'
#' @examples
#'
#'#Load example data
#' example_data
#'
#'#Run densi_metrics with that example_data
#' densimetrics<-densi_metrics(example_data)
#'
#'#Check results
#' densimetrics
#' @export



densi_metrics <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Density measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

  if(ncol(dataset)==9){
    den_chir_dip<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                      dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                      dataset$Order=="Diptera")),9])
    den_non_chir_dip<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                          dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                          dataset$Order=="Diptera")),9])
    den_ephe<-sum(dataset[dataset$Order=="Ephemeroptera",9])
    den_molus<-sum(dataset[dataset$Class=="Mollusca",9])
    den_gastr<-sum(dataset[dataset$Order=="Gastropoda",9])
    den_biv<-sum(dataset[dataset$Order=="Bivalvia",9])
    den_crus<-sum(dataset[dataset$Class=="Crustacea",9])
    den_nai<-sum(dataset[dataset$Family=="Naididae",9])
    den_lhoff<-sum(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9])
    den_bothr<-sum(dataset[dataset$Genus=="Bothrioneurum",9])
    den_tubi<-sum(dataset[dataset$Genus=="Tubifex",9])
    den_dero<-sum(dataset[dataset$Genus=="Dero",9])
    den_prist<-sum(dataset[dataset$Genus=="Pristina",9])
    den_chiro<-sum(dataset[dataset$Genus=="Chironomus",9])
    den_nais<-sum(dataset[dataset$Genus=="Nais",9])
    den_hele<-sum(dataset[dataset$Genus=="Heleobia",9])
    den_subchiro<-sum(dataset[dataset$Subfamily=="Chironominae",9])
    den_suborth<-sum(dataset[dataset$Subfamily=="Orthocladiinae",9])
    den_subtany<-sum(dataset[dataset$Subfamily=="Tanypodinae",9])
    den_t<-sum(dataset[,9])
    den_oli<-sum(dataset[dataset$Order=="Oligochaeta",9])
    den_tricho<-sum(dataset[dataset$Order=="Trichoptera",9])
    den_ostr<-sum(dataset[dataset$Order=="Ostracoda",9])
    den_amph<-sum(dataset[dataset$Order=="Amphipoda",9])
    den_polym<-sum(dataset[dataset$Family=="Polymitarcydae",9])
    den_hyal<-sum(dataset[dataset$Family=="Hyalellidae",9])
    den_coch<-sum(dataset[dataset$Family=="Cochliopidae",9])
    den_chironomidae<-sum(dataset[dataset$Family=="Chironomidae",9])
  }else{
    den_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                        dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                        dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
    den_non_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                            dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                            dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
    den_ephe<-(apply(dataset[dataset$Order=="Ephemeroptera",9:ncol(dataset)],2,sum))
    den_molus<-(apply(dataset[dataset$Class=="Mollusca",9:ncol(dataset)],2,sum))
    den_gastr<-(apply(dataset[dataset$Order=="Gastropoda",9:ncol(dataset)],2,sum))
    den_biv<-(apply(dataset[dataset$Order=="Bivalvia",9:ncol(dataset)],2,sum))
    den_crus<-(apply(dataset[dataset$Class=="Crustacea",9:ncol(dataset)],2,sum))
    den_nai<-(apply(dataset[dataset$Family=="Naididae",9:ncol(dataset)],2,sum))
    den_lhoff<-(apply(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9:ncol(dataset)],2,sum))
    den_bothr<-(apply(dataset[dataset$Genus=="Bothrioneurum",9:ncol(dataset)],2,sum))
    den_tubi<-(apply(dataset[dataset$Genus=="Tubifex",9:ncol(dataset)],2,sum))
    den_dero<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))
    den_prist<-(apply(dataset[dataset$Genus=="Pristina",9:ncol(dataset)],2,sum))
    den_chiro<-(apply(dataset[dataset$Genus=="Chironomus",9:ncol(dataset)],2,sum))
    den_nais<-(apply(dataset[dataset$Genus=="Nais",9:ncol(dataset)],2,sum))
    den_hele<-(apply(dataset[dataset$Genus=="Heleobia",9:ncol(dataset)],2,sum))
    den_subchiro<-(apply(dataset[dataset$Subfamily=="Chironominae",9:ncol(dataset)],2,sum))
    den_suborth<-(apply(dataset[dataset$Subfamily=="Orthocladiinae",9:ncol(dataset)],2,sum))
    den_subtany<-(apply(dataset[dataset$Subfamily=="Tanypodinae",9:ncol(dataset)],2,sum))
    den_t<-apply(dataset[,9:ncol(dataset)],2,sum)
    den_oli<-apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum)
    den_tricho<-(apply(dataset[dataset$Order=="Trichoptera",9:ncol(dataset)],2,sum))
    den_ostr<-(apply(dataset[dataset$Order=="Ostracoda",9:ncol(dataset)],2,sum))
    den_amph<-(apply(dataset[dataset$Order=="Amphipoda",9:ncol(dataset)],2,sum))
    den_polym<-(apply(dataset[dataset$Family=="Polymitarcydae",9:ncol(dataset)],2,sum))
    den_hyal<-(apply(dataset[dataset$Family=="Hyalellidae",9:ncol(dataset)],2,sum))
    den_coch<-(apply(dataset[dataset$Family=="Cochliopidae",9:ncol(dataset)],2,sum))
    den_chironomidae<-(apply(dataset[dataset$Family=="Chironomidae",9:ncol(dataset)],2,sum))}

  den_t_lhoff<-den_lhoff/den_t
  den_t_bothr<-den_bothr/den_t
  den_t_tubi<-den_tubi/den_t
  den_t_dero<-den_dero/den_t
  den_t_prist<-den_prist/den_t
  den_t_chiro<-den_chiro/den_t



  rcd<-data.frame(den_chir_dip, den_non_chir_dip, den_ephe, den_molus, den_gastr, den_biv, den_crus,
                  den_nai, den_lhoff, den_bothr, den_tubi, den_dero, den_prist, den_chiro, den_nais,
                  den_hele, den_subchiro, den_suborth, den_subtany, den_t, den_t_lhoff, den_t_bothr,
                  den_t_tubi, den_t_dero, den_t_prist, den_t_chiro, den_oli, den_tricho, den_ostr,
                  den_amph, den_polym, den_hyal, den_coch, den_chironomidae, stringsAsFactors = TRUE)
  densimetrics<-data.frame(t(rcd))
  colnames(densimetrics)<-rownames(rcd)
  densimetrics<-round(densimetrics,2)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_densi_metrics.csv",sep="")
    utils::write.table(x = densimetrics, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return (densimetrics)
}
