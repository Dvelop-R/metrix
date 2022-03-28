#' Composition measures
#'
#' Calculates the relative abundance of particular taxa in the assemblage in percentage terms.
#'
#' Provides information on the makeup of the assemblage and the relative contribution of the populations to the total fauna (Barbour et al., 1996).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated composition measures:
#' \item{per_ephe}{\% Ephemeroptera.}
#' \item{per_molus}{\% Mollusca.}
#' \item{per_gastr}{\%Gastropoda.}
#' \item{per_biv}{\%Bivalvia.}
#' \item{per_crus}{\%Crustacea.}
#' \item{per_oli}{\% Oligochaeta.}
#' \item{per_amph}{\% Amphipoda.}
#' \item{per_ostr}{\% Ostracoda.}
#' \item{per_ephetricho}{\% Ephemeroptera + Trichoptera.}
#' \item{per_naid}{\% Naididae.}
#' \item{per_chir_dip}{\% Diptera Chironomidae.}
#' \item{per_non_chir_dip}{\%Diptera no Chironomidae.}
#' \item{per_polym}{\%Polymitarcidae.}
#' \item{per_hyal}{\%Hyalella.}
#' \item{per_coch}{\%Cochliopidae.}
#' \item{per_tricho}{\%Trichoptera.}
#' \item{per_subchiro}{\%Chironominae.}
#' \item{per_suborth}{\%Orthocladiinae.}
#' \item{per_subtany}{\%Tanypodinae.}
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
#'#Run per_diver with that example_data
#' perdiver<-per_diver(example_data)
#'
#'#Check results
#' perdiver
#' @export


per_diver <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Composition measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

  den_t<-apply(dataset[,9:ncol(dataset)],2,sum)

  #perEphe per_ephe
  den_ephe<-(apply(dataset[dataset$Order=="Ephemeroptera",9:ncol(dataset)],2,sum))
  per_ephe<-den_ephe/den_t*100
  per_ephe[is.na(per_ephe)]<-0


  #perMol per_mol
  den_molus<-(apply(dataset[dataset$Class=="Mollusca",9:ncol(dataset)],2,sum))
  per_molus<-den_molus/den_t*100
  per_molus[is.na(per_molus)]<-0

  #perGast per_gastr
  den_gastr<-(apply(dataset[dataset$Order=="Gastropoda",9:ncol(dataset)],2,sum))
  per_gastr<-den_gastr/den_t*100
  per_gastr[is.na(per_gastr)]<-0


  #perBiv per_biv
  den_biv<-(apply(dataset[dataset$Order=="Bivalvia",9:ncol(dataset)],2,sum))
  per_biv<-den_biv/den_t*100
  per_biv[is.na(per_biv)]<-0


  #perCrus per_crus
  den_crus<-(apply(dataset[dataset$Class=="Crustacea",9:ncol(dataset)],2,sum))
  per_crus<-den_crus/den_t*100
  per_crus[is.na(per_crus)]<-0


  #perOligo per_oli
  den_oli<-(apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum))
  per_oli<-den_oli/den_t*100
  per_oli[is.na(per_oli)]<-0

  #perAmph per_amph
  den_amph<-(apply(dataset[dataset$Order=="Amphipoda",9:ncol(dataset)],2,sum))
  per_amph<-den_amph/den_t*100
  per_amph[is.na(per_amph)]<-0

  #perOst per_ostr
  den_ostr<-(apply(dataset[dataset$Order=="Ostracoda",9:ncol(dataset)],2,sum))
  per_ostr<-den_ostr/den_t*100
  per_ostr[is.na(per_ostr)]<-0

  #perTricho per_tricho
  den_tricho<-(apply(dataset[dataset$Order=="Trichoptera",9:ncol(dataset)],2,sum))
  per_tricho<-den_tricho/den_t*100
  per_tricho[is.na(per_tricho)]<-0


  #perEpheSumTricho per_ephetricho
  per_ephetricho<-(den_ephe+den_tricho)/den_t*100
  per_ephetricho[is.na(per_ephetricho)]<-0

  #perNaid per_naid
  den_nai<-(apply(dataset[dataset$Family=="Naididae",9:ncol(dataset)],2,sum))
  per_naid<-den_nai/den_t*100
  per_naid[is.na(per_naid)]<-0


  #perChir per_chir_dip
  den_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                      dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                      dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
  per_chir_dip<-den_chir_dip/den_t*100
  per_chir_dip[is.na(per_chir_dip)]<-0

  #perNoChir per_non_chir_dip
  den_non_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                          dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                          dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
  per_non_chir_dip<-den_non_chir_dip/den_t*100
  per_non_chir_dip[is.na(per_non_chir_dip)]<-0


  #perPolym per_polym
  den_polym<-(apply(dataset[dataset$Family=="Polymitarcydae",9:ncol(dataset)],2,sum))
  per_polym<-den_polym/den_t*100
  per_polym[is.na(per_polym)]<-0


  #perHyal per_hyal
  den_hyal<-(apply(dataset[dataset$Family=="Hyalellidae",9:ncol(dataset)],2,sum))
  per_hyal<-den_hyal/den_t*100
  per_hyal[is.na(per_hyal)]<-0

  #perCoch per_coch
  den_coch<-(apply(dataset[dataset$Family=="Cochliopidae",9:ncol(dataset)],2,sum))
  per_coch<-den_coch/den_t*100
  per_coch[is.na(per_coch)]<-0

  #per_subchiro
  den_subchiro<-(apply(dataset[dataset$Subfamily=="Chironominae",9:ncol(dataset)],2,sum))
  per_subchiro<-den_subchiro/den_t*100
  per_subchiro[is.na(per_subchiro)]<-0

  #per_suborth
  den_suborth<-(apply(dataset[dataset$Subfamily=="Orthocladiinae",9:ncol(dataset)],2,sum))
  per_suborth<-den_suborth/den_t*100
  per_suborth[is.na(per_suborth)]<-0


  #per_subtany
  den_subtany<-(apply(dataset[dataset$Subfamily=="Tanypodinae",9:ncol(dataset)],2,sum))
  per_subtany<-den_subtany/den_t*100
  per_subtany[is.na(per_subtany)]<-0

  rcd<-data.frame(per_ephe, per_molus, per_gastr, per_biv, per_crus, per_oli, per_amph, per_ostr,
                  per_ephetricho, per_naid, per_chir_dip, per_non_chir_dip, per_polym, per_hyal,
                  per_coch, per_tricho, per_subchiro, per_suborth, per_subtany, stringsAsFactors = TRUE)
  Perdiver<-data.frame(t(rcd))
  colnames(Perdiver)<-colnames(dataset[9:ncol(dataset)])
  Perdiver<-round(Perdiver,2)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_per_diver.csv",sep="")
    utils::write.table(x = Perdiver, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return (Perdiver)
}
