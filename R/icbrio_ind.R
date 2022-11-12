#' ICBrio index
#'
#' Calculate ICBrio index
#'
#' ICBrio was created to monitor the quality of inland waters in the state of São Paulo. It is a multimetric index that includes different metrics: richness, Shannon-Wiener diversity index (H’), Sequential Comparison index (ICS), Tanytarsini/Chironomidae ratio, richness of sensitive taxa and dominance of tolerant groups. Only one of the diversity indices (H’ or ICS) is considered to calculate it (in this case, the function H’ from 'vegan' package). The final value, which generates the diagnosis or classification of habitat quality, combines the arithmetic mean of the value obtained with the sum of the points of each metric.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{Icbrio_n}{The numerical ICBrio index (Kuhlmann et al. 2012).}
#' \item{Icbrio_c}{The ICBrio water quality class (Kuhlmann et al. 2012).}
#'
#' @seealso \link[metrix]{read_data}, \link{biotic_ind}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Kuhlmann M, Imbimbo HV, Ogura LL (2012). Protocolo para o biomonitoramento com as comunidades bentônicas de rios e reservatórios do estado de São Paulo. \url{https://cetesb.sp.gov.br/aguas-interiores/wp-content/uploads/sites/12/2013/11/protocolo-biomonitoramento-2012.pdf}
#'
#' @examples
#'
#'#Load example data
#' example_data
#'
#'#Run icbrio_ind with that example_data
#' icb<-icbrio_ind(example_data)
#'
#'#Check results
#' icb$Icbrio_n
#' icb$Icbrio_c
#' @export


icbrio_ind <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for ICbrio index calculation...")}

  if(chkt_f(dataset) == FALSE) {stop("Check table format.")}

  if(ncol(dataset)==9){
    den_t<-sum(dataset[,9])
    n_taxa<-sum(dataset[,9]!=0)
    ind_shan<-vegan::diversity(dataset[,9],index = "shannon")
  }else{
    den_t<-apply(dataset[,9:ncol(dataset)],2,sum)
    n_taxa<-apply(dataset[,9:ncol(dataset)],2,function(c)sum(c!=0))
    ind_shan<-apply(dataset[,9:ncol(dataset)],2,vegan::diversity,index="shannon")
    }


tol_g<-as.factor(c("Naididae","Chironomus","Tubifex","Aulodrilus",
                   "Dero","Pristina","Slavina","Stphensoniana",
                   "Styllaria","Allonais","Nais","Haemonais"))
sen_t<-as.factor(c("EPHEMEROPTERA","PLECOPTERA","TRICHOPTERA","Stempellina"))

#tol
a<-(tolower(as.character(dataset$Genus))%in%tolower(as.character(tol_g)))
b<-(tolower(as.character(dataset$Family))%in%tolower(as.character(tol_g)))
c<-a+b

if(ncol(dataset)==9){
  r_tol<-Matrix::nnzero(dataset[c!=0,9])
  n_tol<-sum(dataset[c!=0,9])
}else{
  r_tol<-apply(dataset[c!=0,9:ncol(dataset)],2,Matrix::nnzero)
  n_tol<-apply(dataset[c!=0,9:ncol(dataset)],2,sum)}

dtol_dtot<-n_tol/den_t
dtol_dtot[is.nan(dtol_dtot)] <- 0

#sens
a<-(tolower(as.character(dataset$Genus))%in%tolower(as.character(sen_t)))
b<-(tolower(as.character(dataset$Order))%in%tolower(as.character(sen_t)))
c<-a+b

if(ncol(dataset)==9){
  r_sen<-Matrix::nnzero(dataset[c!=0,9])
}else{
  r_sen<-apply(dataset[c!=0,9:ncol(dataset)],2,Matrix::nnzero)}



n_taxa_score<-n_taxa
for (i in 1:length(n_taxa))
{
  if(n_taxa[i]>=21) n_taxa_score[i]=1
  if(n_taxa[i]>=14 && n_taxa[i]<21) n_taxa_score[i]=2
  if(n_taxa[i]>=6 && n_taxa[i]<14) n_taxa_score[i]=3
  if(n_taxa[i]<6) n_taxa_score[i]=4
  if(n_taxa[i]==0) n_taxa_score[i]=0
}

sha_score<-ind_shan
for (i in 1:length(ind_shan))
{
  if(ind_shan[i]>2.5) sha_score[i]=1
  if(ind_shan[i]>1.5 && ind_shan[i]<=2.5) sha_score[i]=2
  if(ind_shan[i]>1 && ind_shan[i]<=1.5) sha_score[i]=3
  if(ind_shan[i]<=1) sha_score[i]=4
  if(ind_shan[i]==0) sha_score[i]=0
}

dtot_dtol_score<-dtol_dtot
for (i in 1:length(dtol_dtot))
{
  if(dtol_dtot[i]<=0.25) dtot_dtol_score[i]=1
  if(dtol_dtot[i]>0.25 && dtol_dtot[i]<50) dtot_dtol_score[i]=2
  if(dtol_dtot[i]>=0.50 && dtol_dtot[i]<=0.75) dtot_dtol_score[i]=3
  if(dtol_dtot[i]>0.75) dtot_dtol_score[i]=4
  if(dtol_dtot[i]==0) dtot_dtol_score[i]=0
}

r_sen_score<-r_sen
for (i in 1:length(r_sen))
{
  if(r_sen[i]>=3) r_sen_score[i]=1
  if(r_sen[i]==2) r_sen_score[i]=2
  if(r_sen[i]==1) r_sen_score[i]=3
  if(r_sen[i]==0) r_sen_score[i]=4
}


ind_r_icbrio<-(n_taxa_score+sha_score+dtot_dtol_score+r_sen_score)
ind_icbrio<-ind_r_icbrio

for (i in 1:length(ind_r_icbrio))
{
  n_items<-4
  if(dtol_dtot[i]==0) n_items<-n_items-1
  if(r_sen[i]==0) n_items<-n_items-1
  ind_icbrio[i]<-ind_r_icbrio[i]/n_items
}

ind_icbrio_class<-ind_icbrio
for (i in 1:length(ind_icbrio))
{
  if(ind_icbrio[i]>3) ind_icbrio_class[i]="Bad"
  if(ind_icbrio[i]>2 && ind_icbrio[i]<=3) ind_icbrio_class[i]="Regular"
  if(ind_icbrio[i]>1 && ind_icbrio[i]<=2) ind_icbrio_class[i]="Good"
  if(ind_icbrio[i]<=1) ind_icbrio_class[i]="Optimun"

}


Icbrio_n<-data.frame(ind_icbrio)
Icbrio_n<-data.frame(t(Icbrio_n))
colnames(Icbrio_n)<-colnames(dataset[9:ncol(dataset)])
rownames(Icbrio_n)<-c("ind_icbrio")
Icbrio_c<-(ind_icbrio_class)
Icbrio_c<-data.frame(t(Icbrio_c))
colnames(Icbrio_c)<-colnames(dataset[9:ncol(dataset)])
rownames(Icbrio_c)<-c("ind_icbrio_class")

if(store==TRUE){
  result_fname<-paste(substitute(dataset),"_icbrio_ind.csv",sep="")
  utils::write.table(x = Icbrio_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
  utils::write.table(x = Icbrio_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
  print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
}

Icbrio<-list(Icbrio_n=Icbrio_n, Icbrio_c=Icbrio_c)
return(Icbrio)
}
