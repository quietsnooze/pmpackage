#http://r-pkgs.had.co.nz/git.html


#' pm_gh_config - configure github with username and email
#'
#' @param github_username Your username for github
#' @param github_email Your github email address
#'
#' @return
#' @export
#'
#' @examples
pm_gh_config <- function(github_username,
                         github_email){
  system(paste0('git config --global user.email "',github_email,'"'))
  system(paste0('git config --global user.name "',github_username,'"'))
  system('git config --global --list')
  system('ls -l')
  system('pwd')
}



#' Pull down latest version of the project
#'
#' @return
#' @export
#'
#' @examples
pm_gh_pull_request <- function(){
  system('git pull origin master')
}




#' Push your (previously committed) github changes to github
#'
#' @return
#' @export
#'
#' @examples
pm_gh_push_request <- function(){
  system('git push -u origin master')
}
