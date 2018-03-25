devtools::use_package('tidyverse')
devtools::use_package('readxl')
devtools::use_package('janitor')
devtools::use_package('purr')
devtools::use_package('tibble')
devtools::use_package('stringr')



#' Fetch tennis results from www.tennis-data.co.uk
#'
#' @param myyear
#' @param competition 'WTA' by default
#'
#' @return
#' @export
#'
#' @examples
#' #'
#' @importFrom magrittr "%>%"
pm_tennis_fetchDataset <- function(myyear,
                                         competition = 'WTA'){

  availableFiles <- list(
    'y2007' = 'http://www.tennis-data.co.uk/2007w/2007.zip',
    'y2008' = 'http://www.tennis-data.co.uk/2008w/2008.zip',
    'y2009' = 'http://www.tennis-data.co.uk/2009w/2009.zip',
    'y2010' = 'http://www.tennis-data.co.uk/2009w/2009.zip',
    'y2011' = 'http://www.tennis-data.co.uk/2011w/2011.zip',
    'y2012' = 'http://www.tennis-data.co.uk/2012w/2012.zip',
    'y2013' = 'http://www.tennis-data.co.uk/2013w/2013.zip',
    'y2014' = 'http://www.tennis-data.co.uk/2014w/2014.zip',
    'y2015' = 'http://www.tennis-data.co.uk/2015w/2015.zip',
    'y2016' = 'http://www.tennis-data.co.uk/2016w/2016zip',
    'y2017' = 'http://www.tennis-data.co.uk/2017w/2017zip',
    'y2018' = 'http://www.tennis-data.co.uk/2018w/2018zip'
  )

  # remove some extra variables
  extraVars = c(
    'B365W',
    'B365L',
    'B&WW',
    'B&WL',
    'CBW',
    'CBL',
    'EXW',
    'EXL',
    'LBW',
    'LBL',
    'GBW',
    'GBL',
    'IWW',
    'IWL',
    'PSW',
    'PSL',
    'SBW',
    'SBL',
    'SJW',
    'SJL',
    'UBW',
    'UBL',

    'MaxW',
    'MaxL',
    'AvgW',
    'AvgL',
    'W1',
    'W2',
    'W3',
    'W4',
    'W5',
    'L1',
    'L2',
    'L3',
    'L4',
    'L5',
    'wta'
  )

  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir=td, fileext=".zip")

  myfilename <- unlist(availableFiles[paste0("y",myyear)])
  download.file(myfilename,tf)

  fname = unzip(tf, list=TRUE)$Name[1]
  # unzip the file to the temporary directory
  unzip(tf, files=fname, exdir=td, overwrite=TRUE)
  # fpath is the full path to the extracted file
  fpath = file.path(td, fname)

  dfMatch <- readxl::read_excel(fpath, guess_max = Inf) %>%
    janitor::clean_names() %>%
    tibble::as.tibble() %>%
    dplyr::select(-one_of(tolower(extraVars))) %>%
    dplyr::rename(match_date = date,
           series = tier,
           match_location = location) %>%
    dplyr::filter(!is.na(match_date)) %>%
    dplyr::mutate(w_rank = as.integer(w_rank),
           w_pts = as.integer(w_pts),
           l_rank = as.integer(l_rank),
           l_pts = as.integer(l_pts),
           wsets = as.integer(wsets),
           lsets = as.integer(lsets),
           tournament = stringr::str_replace_all(tournament,"[^a-zA-Z\\s]", " "),
           winner = trimws(winner),
           loser = trimws(loser))


  dfMatch
}


#' Get all available historical data
#'
#' @param competition
#'
#' @return
#' @export
#'
#' @examples
pm_tennis_fetchAllDatasets <- function(competition = 'WTA'){

  myyears <- seq(from=2007,to=2018,by=1)

  allTheData <- myyears %>%
    map(~ pm_tennis_fetchDataset(., competition = competition)) %>%
    reduce(bind_rows)

  allTheData
}




pm_tennis_eloify_dataset <- function(my_raw_data){
  mydata <- my_raw_data %>%
    dplyr::mutate(sampleSide = rbinom(nrow(my_raw_data),1,0.5))

  mywinners <- mydata %>% dplyr::filter(sampleSide == 1)
  mylosers <- mydata %>% dplyr::filter(sampleSide == 0)

  mywinners <- mywinners %>%
    dplyr::mutate(actualResult = 1)
  names(mywinners)[names(mywinners) == 'winner'] <- 'player_name'
  names(mywinners)[names(mywinners) == 'loser'] <- 'opponent_name'
  colnums = grep('^w',x=names(mywinners),ignore.case = TRUE)
  names(mywinners)[colnums] = gsub(pattern = 'w',replacement = 'player',x=names(mywinners)[colnums],ignore.case = TRUE)
  colnums = grep('^l',x=names(mywinners),ignore.case = TRUE)
  names(mywinners)[colnums] = gsub(pattern = 'l',replacement = 'opponent',x=names(mywinners)[colnums],ignore.case = TRUE)


  mylosers <- mylosers %>%
    dplyr::mutate(actualResult = 0)
  names(mylosers)[names(mylosers) == 'winner'] <- 'opponent_name'
  names(mylosers)[names(mylosers) == 'loser'] <- 'player_name'
  colnums = grep('^l',x=names(mylosers),ignore.case = TRUE)
  names(mylosers)[colnums] = gsub(pattern = 'l',replacement = 'player',x=names(mylosers)[colnums],ignore.case = TRUE)
  colnums = grep('^w',x=names(mylosers),ignore.case = TRUE)
  names(mylosers)[colnums] = gsub(pattern = 'w',replacement = 'opponent',x=names(mylosers)[colnums],ignore.case = TRUE)

  bind_rows(mywinners,
            mylosers) %>%
    dplyr::arrange(match_date)
}
