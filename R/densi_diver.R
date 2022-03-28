#' Density measures
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
#' \item{den_gastr}{Gastropoda densityv.}
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
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @references
#' \itemize{
#' \item{Barbour MT, Gerritsen J, Griffith GE, Frydenborg R, McCarron E, White JS & Bastian ML (1996) \doi{10.2307/1467948>}}
#' }
#' @examples
#'
#'#Example data is a properly formatted table with richness measures of two sites
#' example_data
#'
#'#Run densi_diver with that example_data
#' dendiver<-densi_diver(example_data)
#'
#'#Check results
#' dendiver
#' @export



densi_diver <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Density measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

  #den_chir_dip
  den_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                      dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                      dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)

  #den_non_chir_dip
  den_non_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                          dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                          dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)

  #den_ephe
  den_ephe<-(apply(dataset[dataset$Order=="Ephemeroptera",9:ncol(dataset)],2,sum))

  #den_molus
  den_molus<-(apply(dataset[dataset$Class=="Mollusca",9:ncol(dataset)],2,sum))

  #den_gastr
  den_gastr<-(apply(dataset[dataset$Order=="Gastropoda",9:ncol(dataset)],2,sum))

  #den_biv
  den_biv<-(apply(dataset[dataset$Order=="Bivalvia",9:ncol(dataset)],2,sum))

  #den_crus
  den_crus<-(apply(dataset[dataset$Class=="Crustacea",9:ncol(dataset)],2,sum))

  #den_nai
  den_nai<-(apply(dataset[dataset$Family=="Naididae",9:ncol(dataset)],2,sum))

  #den_lhoff
  den_lhoff<-(apply(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9:ncol(dataset)],2,sum))

  #den_bothr
  den_bothr<-(apply(dataset[dataset$Genus=="Bothrioneurum",9:ncol(dataset)],2,sum))

  #den_tubi
  den_tubi<-(apply(dataset[dataset$Genus=="Tubifex",9:ncol(dataset)],2,sum))

  #den_dero
  den_dero<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))

  #den_prist
  den_prist<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))

  #den_chiro
  den_chiro<-(apply(dataset[dataset$Genus=="Chironomus",9:ncol(dataset)],2,sum))

  #den_nais
  den_nais<-(apply(dataset[dataset$Genus=="Nais",9:ncol(dataset)],2,sum))

  #den_hele
  den_hele<-(apply(dataset[dataset$Genus=="Heleobia",9:ncol(dataset)],2,sum))

  #den_subchiro
  den_subchiro<-(apply(dataset[dataset$Subfamily=="Chironominae",9:ncol(dataset)],2,sum))

  #den_suborth
  den_suborth<-(apply(dataset[dataset$Subfamily=="Orthocladiinae",9:ncol(dataset)],2,sum))

  #den_subtany
  den_subtany<-(apply(dataset[dataset$Subfamily=="Tanypodinae",9:ncol(dataset)],2,sum))

  #den_t
  den_t<-apply(dataset[,9:ncol(dataset)],2,sum)

  #den_t_lhoff
  den_t_lhoff<-den_lhoff/den_t

  #den_t_bothr
  den_t_bothr<-den_bothr/den_t

  #den_t_tubi
  den_t_tubi<-den_tubi/den_t

  #den_t_dero
  den_t_dero<-den_dero/den_t

  #den_t_prist
  den_t_prist<-den_prist/den_t

  #den_t_chiro
  den_t_chiro<-den_chiro/den_t

  #den_oli
  den_oli<-apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum)

  #den_tricho
  den_tricho<-(apply(dataset[dataset$Order=="Trichoptera",9:ncol(dataset)],2,sum))

  #den_ostr
  den_ostr<-(apply(dataset[dataset$Order=="Ostracoda",9:ncol(dataset)],2,sum))

  #den_amph
  den_amph<-(apply(dataset[dataset$Order=="Amphipoda",9:ncol(dataset)],2,sum))

  #den_polym
  den_polym<-(apply(dataset[dataset$Family=="Polymitarcydae",9:ncol(dataset)],2,sum))

  #den_hyal
  den_hyal<-(apply(dataset[dataset$Family=="Hyalellidae",9:ncol(dataset)],2,sum))

  #den_coch
  den_coch<-(apply(dataset[dataset$Family=="Cochliopidae",9:ncol(dataset)],2,sum))

  #den_chironomidae
  den_chironomidae<-(apply(dataset[dataset$Family=="Chironomidae",9:ncol(dataset)],2,sum))


  rcd<-data.frame(den_chir_dip, den_non_chir_dip, den_ephe, den_molus, den_gastr, den_biv, den_crus,
                  den_nai, den_lhoff, den_bothr, den_tubi, den_dero, den_prist, den_chiro, den_nais,
                  den_hele, den_subchiro, den_suborth, den_subtany, den_t, den_t_lhoff, den_t_bothr,
                  den_t_tubi, den_t_dero, den_t_prist, den_t_chiro, den_oli, den_tricho, den_ostr,
                  den_amph, den_polym, den_hyal, den_coch, den_chironomidae, stringsAsFactors = TRUE)
  d_diver<-data.frame(t(rcd))
  colnames(d_diver)<-rownames(rcd)
  d_diver<-round(d_diver,2)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_densi_diver.csv",sep="")
    utils::write.table(x = d_diver, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return (d_diver)
}
