#' Tolerance measures
#'
#' Indicate sensitivity of the assemblage and component species to various types of perturbation.
#'
#' Most of the metrics applied in the study of macroinvertebrates use as a key factor the tolerance or intolerance of the different taxa to a certain disturbance, normally organic contamination. The relationship between the number of organisms that are tolerant and intolerant to contamination is a common resource in the metrics used. Further metrics (multimetric indexes) can be derived from a combination of these primary metrics (Prat et al., 2009). The Limnodrilus hoffmeisteri/total density ratio, which was developed by Marchese & Ezcurra de Drago (1999), increases in environments with organic contamination.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated tolerance measures.:
#' \item{r_oligochir}{Oligochaeta/Chironomidae.}
#' \item{r_oligoset}{Oligochaeta with setaform chaetae/Oligochaeta without setaform chaetae.}
#' \item{r_tanychir}{Tanytarsini/Chironomidae.}
#' \item{den_t_lhoff}{\emph{Limnodrilus hoffmeisteri}/Total density.}
#' \item{den_t_bothr}{\emph{Bothrioneurum}/Total density.}
#' \item{den_t_tubi}{\emph{Tubifex}/Total density.}
#' \item{den_t_dero}{\emph{Dero}/Total density.}
#' \item{den_t_prist}{\emph{Pristina}/Total density.}
#' \item{den_t_chiro}{\emph{Chironomus}/Total density.}
#'
#'
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @references
#' \itemize{
#' \item{Marchese M & Ezcurra de Drago I (1999) <https://agro.icm.edu.pl/agro/element/bwmeta1.element.agro-article-e981d07b-e469-4460-a7fe-3239650cd089>}
#' \item{Prat N, Ríos B, Acosta R & Rieradevall M (2009) <http://www.ub.edu/riosandes/docs/MacroIndLatinAmcompag0908.pdf>}
#' }
#' @examples
#'#Example data is a properly formatted table with richness measures of two sites
#' example_data
#'
#'#Run indrel_diver with that example_data
#' indreld<-indrel_diver(example_data)
#'
#'#Check results
#' indreld
#' @export


indrel_diver <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Tolerance measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}


  #r_oligochir
  den_oli<-apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum)
  den_chironomidae<-(apply(dataset[dataset$Family=="Chironomidae",9:ncol(dataset)],2,sum))
  r_oligochir<-den_oli/den_chironomidae
  r_oligochir[!is.finite(r_oligochir)] <- 0

  #r_oligoset
  non_q<-c("Limnodrilus", "Bothrioneurum" ,"Tubifex","Paranadrilus","Homochaeta",
           "Megadrili" , "Haplotaxis","Amphichaeta","Eiseniella","Brinkhurstia",
           "Chaetogaster","Bratislavia","Paranais")
  q<-c("Aulodrilus","Dero","Pristina","Slavina","Stphensoniana","Styllaria",
       "Allonais","Nais","Opistocysta","Trieminentia","Haemonais")
  oli_q<-apply(dataset[dataset$Genus %in% q,9:ncol(dataset)],2,sum)
  oli_nq<-apply(dataset[dataset$Genus %in% non_q,9:ncol(dataset)],2,sum)
  r_oligoset<-rep(0,ncol(dataset)-8)
  for(i in 1:length(r_oligoset)){
    if(oli_q[i]!=0 & oli_nq[i]!=0){
      r_oligoset[i]=oli_q[i]/oli_nq[i]}
    else{r_oligoset[i]<-0}
  }

  #r_tanychir
  r_tanychir<-rep(0,ncol(dataset)-8)
  if(!anyNA(dataset$Tribe))
  {
    den_tany<-apply(dataset[dataset$Tribe=="Tanytarsini",9:ncol(dataset)],2,sum)
    r_tanychir<-den_tany/den_chironomidae
    r_tanychir[!is.finite(r_tanychir)]<-0
  }

  #den_t
  den_t<-apply(dataset[,9:ncol(dataset)],2,sum)

  #den_lhoff
  den_lhoff<-(apply(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9:ncol(dataset)],2,sum))
  den_t_lhoff<-den_lhoff/den_t
  den_t_lhoff<-as.double(den_t_lhoff)

  #den_bothr
  den_bothr<-(apply(dataset[dataset$Genus=="Bothrioneurum",9:ncol(dataset)],2,sum))
  den_t_bothr<-den_bothr/den_t
  den_t_bothr<-as.double(den_t_bothr)

  #den_tubi
  den_tubi<-(apply(dataset[dataset$Genus=="Tubifex",9:ncol(dataset)],2,sum))
  den_t_tubi<-den_tubi/den_t
  den_t_tubi<-as.double(den_t_tubi)

  #den_dero
  den_dero<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))
  den_t_dero<-den_dero/den_t
  den_t_dero<-as.double(den_t_dero)

  #den_prist
  den_prist<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))
  den_t_prist<-den_prist/den_t
  den_t_prist<-as.double(den_t_prist)

  #den_chiro
  den_chiro<-(apply(dataset[dataset$Genus=="Chironomus",9:ncol(dataset)],2,sum))
  den_t_chiro<-den_chiro/den_t
  den_t_chiro<-as.double(den_t_chiro)

  #den_hele
  den_hele<-(apply(dataset[dataset$Genus=="Heleobia",9:ncol(dataset)],2,sum))
  den_t_hele<-den_hele/den_t
  den_t_hele<-as.double(den_t_hele)

  IdR<-data.frame(r_oligochir=r_oligochir,r_oligoset=r_oligoset, r_tanychir=r_tanychir, den_t_lhoff=den_t_lhoff,
                  den_t_bothr=den_t_bothr,den_t_tubi=den_t_tubi,den_t_dero=den_t_dero,den_t_prist=den_t_prist,
                  den_t_chiro=den_t_chiro,den_t_hele=den_t_hele)
  Indrel<-data.frame(t(IdR))
  colnames(Indrel)<-colnames(dataset[9:ncol(dataset)])
  Indrel<-round(Indrel,4)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_indrel_diver.csv",sep="")
    utils::write.table(x = Indrel, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return(Indrel)

}
