#' IMRP index
#'
#' Calculates the Indice de Macroinvertebrados en Rios Pampeanos Index
#'
#'IMRP was created for the rivers of the Pampean plain (Rodrigues Capítulo 1999). This index is based on the sum of ecological values for each taxon. This value is inversely proportional to the degree of tolerance to contamination, varying from 0.1 for highly tolerant taxa to 2.0 for the most sensitive. Identification to family is sufficient to calculate the IMRP score.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{Imrp_n}{The numerical IMRP index (Rodrigues Capítulo 1999).}
#' \item{Imrp_c}{The IMRP index water quality class (Rodrigues Capítulo 1999).}
#'
#' @seealso \link[metrix]{read_data}, \link{biotic_ind}
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @references
#' \itemize{
#' \item{Rodrigues Capítulo A (1999) < https://www.biotaxa.org/RSEA/article/view/32771>}
#' }
#' @examples
#'
#'#Example data is a properly formatted table with richness measures of two sites
#' example_data
#'
#'#Run imrp_ind with that example_data
#' imrp<-imrp_ind(example_data)
#'
#'#Check results
#' imrp$Imrp_n
#' imrp$Imrp_c
#' @export


imrp_ind <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for IMRP calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

score<-numeric(length = length(9:ncol(dataset)))

for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-imrp_score$Score[match(tolower(unique(as.character(aux$Family))),
                           tolower(as.character(imrp_score$Taxa)))]
  m2<-imrp_score$Score[match(tolower(unique(as.character(aux$Order))),
                            tolower(as.character(imrp_score$Taxa)))]
  score[i-8]<-sum(m[!is.na(m)])+sum(m2[!is.na(m2)])
}

score[is.na(score)]<-0
ind_imrp_total<-score
ind_imrp_class<-ind_imrp_total
for (i in 1:length(ind_imrp_total))
{
  if(ind_imrp_total[i]>=12) ind_imrp_class[i]="Not contaminated or slightly polluted water"
  if(ind_imrp_total[i]<12 && ind_imrp_total[i]>=8) ind_imrp_class[i]="Minor water pollution"
  if(ind_imrp_total[i]<8 && ind_imrp_total[i]>=4) ind_imrp_class[i]="Light water pollution"
  if(ind_imrp_total[i]<4 && ind_imrp_total[i]>=2.6) ind_imrp_class[i]="Moderate water pollution"
  if(ind_imrp_total[i]<2.6 && ind_imrp_total[i]>=1.1) ind_imrp_class[i]="Strong water pollution"
  if(ind_imrp_total[i]<1) ind_imrp_class[i]="Major water pollution"

}

Imrp_n<-data.frame(ind_imrp_total)
Imrp_n<-data.frame(t(Imrp_n))
colnames(Imrp_n)<-colnames(dataset[9:ncol(dataset)])
rownames(Imrp_n)<-c("ind_imrp")
Imrp_c<-data.frame(ind_imrp_class)
Imrp_c<-data.frame(t(Imrp_c))
colnames(Imrp_c)<-colnames(dataset[9:ncol(dataset)])

if(store==TRUE){
  result_fname<-paste(substitute(dataset),"_imrp_ind.csv",sep="")
  utils::write.table(x = Imrp_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
  utils::write.table(x = Imrp_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
  print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
}

Imrp<-list(Imrp_n=Imrp_n, Imrp_c=Imrp_c)

return(Imrp)

}
