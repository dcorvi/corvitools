#' Set Mapped Drive
#' @description This function will map one or multiple network drives to letters using the command prompt - so you dont have to type or click as much
#' Use the keyring package to set your Woods Hole network password for this work station using key_set_with_value(). Retrieve using key_get() - Never store your password in raw code
#'
#' @param ... a set of vectors of where each network location is a assigned to the requested letter drive i.e. a = file.path("noaa.gov","work25", "my_folder") where the filepath is formatted using the file.path function
#' @param user_name a character vector of your Woods Hole network username
#' @param password a character vector of your Woods Hole network password
#'
#' @return sets a network path to a drive letter in Windows
#' @export
#'
#' @import keyring
#' @importFrom stringr str_detect
#' @importFrom parallel detectCores
#'
#' @examples
#' \dontrun{}
#'
#' set_mapped_drive(d = file.path("network_address", "home20", "dcorvi"), a = file.path("network_address", "work30", "ssb"), user_name = "dcorvi", password = "password")
#' set_mapped_drive(a = file.path("network_address", "work25", "ssb"), user_name = "my_username", password = "password1")
set_mapped_drive <- function(..., user_name, password){
  if (isFALSE(unname(Sys.info()["sysname"]) == "Windows")) {
    stop("This function is not compatable with a non-Windows OS.")}
  if (isFALSE(str_detect(system("ipconfig", intern = TRUE)[5], "Ethernet adapter Ethernet 2"))) {stop("Not connected to VPN. This check needs testing. Add issue in repo if incorrect.")}
  message("Disconnecting from all current mapped drives..")
  system("net use")
  system("net use /del * /y")
  loc_list <- list(...)
  for (loc in loc_list) {
    if (match(loc, loc_list) == 1) { # if first network location then use username and pw otherwise input drive letter
      message("Assigning network location: ", loc_list[[match(loc, loc_list)]], " to drive letter ", names(loc_list[match(loc, loc_list)])," ..")
      system(paste0("net use ", names(loc_list[match(loc, loc_list)]), ": \\\\", gsub("/","\\\\", loc_list[[match(loc, loc_list)]]), " ", password," /user:", user_name))
    } else {
      message("Assigning network location: ", loc_list[[match(loc, loc_list)]], " to drive letter ", names(loc_list[match(loc, loc_list)])," ..")
      system(paste0("net use ", names(loc_list[match(loc, loc_list)]), ": \\\\", gsub("/","\\\\", loc_list[[match(loc, loc_list)]])))
    }
  }
  system("net use")
  message("Finished.")
}

#' Number Suffix Creator
#'
#' Input a number and recieve the number as a charcter and the number's suffix to automate reporting.
#' Numbers greater than 999 and less than 999,999  will have a comma. Larger numbers will be
#' returned with a period and runded up to 3 digits. Goes up to quintillion.
#'
#' @return \code{numbersuffix()} returns a character vector of what number suffix the input number is
#' @export
#'
#' @examples
#' numbersuffix(87)
#'
# number suffix function
numbersuffix <- function(num){ifelse(num / 1e21 >= 1,stop(paste0("number ",num," is too large")),
                                     ifelse(num / 1e18 >= 1, paste0(round(num/1e18,3)," Quintillion"),
                                            ifelse(num / 1e15 >= 1, paste0(round(num/1e15,3)," Quadrillion"),
                                                   ifelse(num / 1e12 >= 1, paste0(round(num/1e12,3)," trillion"),
                                                          ifelse(num / 1e9 >= 1, paste0(round(num/1e9,3)," billion"),
                                                                 ifelse(num / 1e6 >= 1, paste0(round(num/1e6,3)," million"),
                                                                        ifelse(num / 1000 >= 1, format(round(num, 0), big.mark=",", scientific=FALSE),
                                                                               ifelse(num < 1000 & num >99, format(round(num, 0), big.mark=",", scientific=FALSE),""))))))))}

#' Number to Words
#'
#' Input a number and recieve the number as a word to automate reporting.
#'
#' @return \code{numbers2words()} returns a character vector of what the name of the number
#' @export
#'
#' @examples
#' numbers2words(25)
#'
# number to word function
numbers2words <- function(x){
  helper <- function(x){
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  #Disable scientific notation
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

#' Convert String To Proper or Sentence Case
#'
#' Input a string and recieve the string as a word or set of words with the first
#' letter capitalized.
#'
#' @return \code{proper()} returns a character vector of the input string in proper case
#' @export
#'
#' @examples
#' proper("some string to convert to proper case")
#'
# string to proper case function
proper <- function(string) gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(string), perl=TRUE)


#' Convert latitude or longitude from degree to decimal format
#'
#' This function takes a set of numbers and converts them to decimal degree without warning messages that spaa generates
#' and doesnt require any outside imports
#'
#' @param angle a character vector of angle or set of cardinates in lat lon format
#'
#' @return \code{angle2dec()} returns a numberic in decimal degrees
#' @export
#'
#' @examples
#' \dontrun{}
#'
#' nums <- ("59 44 50")
#'
#' df <- data.frame("lat" = c(59,44,50), "long" = c(151,45,11))
#'
#' angle2dec(nums)
#'  # apply to each column in dataframe
#'  new_df <- apply(df, 2L, angle2dec)
#'
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}


.onLoad <- function(libnam, pkgname) {
  packageStartupMessage(paste0("Corvi Tools Ver.",utils::packageVersion("CoRviTools"),
                               "\n \n- Number of cores on this machine:  ", parallel::detectCores(),
                               "\n \n- Set Oracle password for this work station using key_set_with_value(). Retrieve using key_get()",
                               " \n- OS: ", toupper(.Platform$OS.type))) # need to create a text file that updates when rasters update
}
