

#' Title
#'
#' @param df
#' @param colname
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_dq_checkColumn = function(df,colname){
  testData = df %>% select_(colname) %>% pull()

  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime'){
    list('col' = colname,
         'class' = class(testData),
         'num' = length(testData),
         'numMissing' = sum(is.na(testData) | is.nan(testData) | is.infinite(testData)),
         'numInfinite' = sum(is.infinite(testData)),
         'avgVal' = round(mean(testData,na.rm=TRUE),3),
         'minVal' = round(min(testData,na.rm = TRUE)),
         'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,
         'class' = class(testData),
         'num' = length(testData),
         'numMissing' = sum(is.na(testData) | is.nan(testData) | is.infinite(testData)),
         'numInfinite' = NA,
         'avgVal' = NA,
         'minVal' = NA,
         'maxVal' = NA)
  }

}



#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom magrittr "%>%"
pm_dq_check_all_columns = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(pm_dq_checkColumn(df=df,colname=colName)))
  }
  resDF
}
