#' Trophic metrics
#'
#' Calculates trophic measures
#'
#' Trophic metrics are surrogates of complex processes such as trophic interaction, production and food source availability. Specialized feeders, such as scrapers, piercers, and shredders, are the more sensitive, and are thought to be well represented in healthy streams. Generalists, such as collectors and filterers, have a broader range of acceptable food materials than specialists, and are thus more tolerant to pollution that might alter availability of certain food (Barbour et al., 1996).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated trophic measures:
#' \item{per_pred}{\% Predator.}
#' \item{per_filt}{\% Filtering collector.}
#' \item{per_shred}{\% Shredders.}
#' \item{per_scrap}{\% Scrapers.}
#' \item{per_gath}{\% Gathering collector.}
#' \item{n_pred}{N° of Predator.}
#' \item{n_filt}{N° of Filtering collector.}
#' \item{n_shred}{N° of Shredders.}
#' \item{n_scrap}{N° of Scrapers.}
#' \item{n_gath}{N° of Gathering collector.}
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
#'#Run troph_metrics with that example_data
#' trophmetrics<-troph_metrics(example_data)
#'
#'#Check results
#' trophmetrics
#' @export


troph_metrics <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format Trophic measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}
  if(ncol(dataset)==9){
    den_t<-sum(dataset[,9])
    den_pred<-sum(dataset[dataset$FG=="P",9])
    den_filt<-sum(dataset[dataset$FG=="FC",9])
    den_shred<-sum(dataset[dataset$FG=="SHR",9])
    den_scrap<-sum(dataset[dataset$FG=="SCR",9])
    den_gath<-sum(dataset[dataset$FG=="GC",9])
  }else{
    den_t<-apply(dataset[,9:ncol(dataset)],2,sum)
    den_pred<-apply(dataset[dataset$FG=="P",9:ncol(dataset)],2,sum)
    den_filt<-apply(dataset[dataset$FG=="FC",9:ncol(dataset)],2,sum)
    den_shred<-apply(dataset[dataset$FG=="SHR",9:ncol(dataset)],2,sum)
    den_scrap<-apply(dataset[dataset$FG=="SCR",9:ncol(dataset)],2,sum)
    den_gath<-apply(dataset[dataset$FG=="GC",9:ncol(dataset)],2,sum)}

n_pred<-numeric(length = length(9:ncol(dataset)))
for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-aux$FG=="P"
  n_pred[i-8]<-sum(m[!is.na(m)])
}

n_filt<-numeric(length = length(9:ncol(dataset)))
for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-aux$FG=="FC"
  n_filt[i-8]<-sum(m[!is.na(m)])
}

n_shred<-numeric(length = length(9:ncol(dataset)))
for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-aux$FG=="SHR"
  n_shred[i-8]<-sum(m[!is.na(m)])
}

n_scrap<-numeric(length = length(9:ncol(dataset)))
for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-aux$FG=="SCR"
  n_scrap[i-8]<-sum(m[!is.na(m)])
}

n_gath<-numeric(length = length(9:ncol(dataset)))
for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-aux$FG=="GC"
  n_gath[i-8]<-sum(m[!is.na(m)])
}

per_pred<-den_pred/den_t*100
per_filt<-den_filt/den_t*100
per_shred<-den_shred/den_t*100
per_scrap<-den_scrap/den_t*100
per_gath<-den_gath/den_t*100

trophmetrics<-data.frame(per_pred, per_filt, per_shred, per_scrap, per_gath,
                 n_pred, n_filt, n_shred, n_scrap, n_gath)
trophmetrics<-data.frame(t(trophmetrics))
colnames(trophmetrics)<-colnames(dataset[9:ncol(dataset)])
trophmetrics<-round(trophmetrics,2)

if(store==TRUE){
  result_fname<-paste(substitute(dataset),"_troph_metric.csv",sep="")
  utils::write.table(x = trophmetrics, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
  print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
}
return(trophmetrics)
}
