# This file contains simple data functions

#' today.date()
#'
#' This function generates a Date object with the current date. It is a wrapper for Sys.Date() to keep rdateutils function syntax.
#' @examples
#' today.date()

today.date <- function(){
  Sys.Date()
}


#' today.str()
#'
#' This function generates a character string with the current date.
#' @examples
#' today.str()

today.str <- function(){
  as.character(Sys.Date())
}


#' yesterday.date()
#'
#' This function generates a Date object with yesterday's date.
#' @examples
#' yesterday.date()

yesterday.date <- function(){
  Sys.Date() - 1
}


#' yesterday.str()
#'
#' This function generates a character string with yesterday's date.
#' @examples
#' yesterday.str()

yesterday.str <- function(){
  as.character(Sys.Date() - 1)
}


#' first.of.month.date()
#'
#' This function generates the 1st of the current month as a Date object
#' @examples
#' first.of.month.date()

first.of.month.date <- function(){
  as.Date(cut(Sys.Date(), "month"))
}


#' first.of.month.str()
#'
#' This function generates the 1st of the current month as a character string
#' @examples
#' first.of.month.str()

first.of.month.str <- function(){
  as.character(cut(Sys.Date(), "month"))
}


#' first.of.last.month.date()
#'
#' This function generates the 1st of the prior month as a Date object
#' @examples
#' first.of.last.month.date()
first.of.last.month.date <- function(){
  firstmonth <- as.Date(cut(Sys.Date(), "month"))
  endlastmonth <- firstmonth - 1
  firstOfLastMonth <- as.Date(cut(endlastmonth, "month"))
}



#' first.of.last.month.str()
#'
#' This function generates the 1st of the prior month as a character string
#' @examples
#' first.of.last.month.str()
first.of.last.month.str <- function(){
  firstmonth <- as.Date(cut(Sys.Date(), "month"))
  endlastmonth <- firstmonth - 1
  firstOfLastMonth <- as.character(cut(endlastmonth, "month"))
}


#' end.of.last.month.date()
#'
#' This function generates the last day of the prior month as a Date object
#' @examples
#' end.of.last.month.date()
end.of.last.month.date <- function(){
  firstmonth <- as.Date(cut(Sys.Date(), "month"))
  endlastmonth <- firstmonth - 1
}


#' end.of.month.from.datestring(mydate)
#'
#' This function generates the last day of a month from a character string in format yyyy-mm-dd and returns a character string with the end of that month
#' @param mydate A date as character string in yyyy-mm-dd format
#' @examples
#' end.of.month.from.date("2015-05-01")
end.of.month.from.datestring <- function(mydate){
  mydate <- as.Date(mydate)
  mymonth <- format(mydate, "%m")
  myyear <- format(mydate, "%Y")
  mynextmonthnum <- as.numeric(mymonth) + 1
  if(mynextmonthnum > 9){
    mynextmonth <- as.character(mynextmonthnum)
  } else {
    mynextmonth <- paste0("0", as.character(mynextmonthnum))
  }
  firstnextmonth <- as.Date(paste(myyear, mynextmonth, "01", sep="-"))
  endmonth <- firstnextmonth - 1
  endmonth <- as.character(endmonth)
  return(endmonth)
}





#' end.of.last.month.str()
#'
#' This function generates the last day of the prior month as a character string
#' @examples
#' end.of.last.month.str()
end.of.last.month.str <- function(){
  firstmonth <- as.Date(cut(Sys.Date(), "month"))
  endlastmonth <- firstmonth - 1
  as.character(endlastmonth)
}

#' first.of.this.year.date()
#'
#' This function generates Jan. 1 of this year as a date object.
#' @examples
#' first.of.this.year.date()
first.of.this.year.date <- function(){
  jan1 <- as.Date(cut(Sys.Date(), "year"))
}


#' first.of.this.year.str()
#'
#' This function generates Jan. 1 of this year as a character string.
#' @examples
#' first.of.this.year.str()
first.of.this.year.str <- function(){
  jan1 <- as.character(cut(Sys.Date(), "year"))
}


#' first.of.last.year.date()
#'
#' This function generates Jan. 1 of last year as a date object.
#' @examples
#' first.of.last.year.date()
first.of.last.year.date <- function(){
  thisyear.char <- format(Sys.Date(), "%Y")
  firstofyear.char <- paste0(thisyear.char, "-01-01")
  firstofyear.date <- as.Date(firstofyear.char)
}


#' first.of.last.year.str()
#'
#' This function generates Jan. 1 of last year as a character string.
#' @examples
#' first.of.last.year.str()
first.of.last.year.str <- function(){
  thisyear.char <- format(Sys.Date(), "%Y")
  firstofyear.char <- paste0(thisyear.char, "-01-01")
}


#' end.of.last.year.date()
#'
#' This function generates Dec. 31 of prior year as a date object.
#' @examples
#' end.of.last.year.date()
end.of.last.year.date <- function(){
  thisyear.char <- format(Sys.Date(), "%Y")
  firstofyear.char <- paste0(thisyear.char, "-12-31")
  firstofyear.date <- as.Date(firstofyear.char)
}


#' end.of.last.year.str()
#'
#' This function generates Dec. 31 of prior year as a character string.
#' @examples
#' end.of.last.year.str()
end.of.last.year.str <- function(){
  thisyear.char <- format(Sys.Date(), "%Y")
  firstofyear.char <- paste0(thisyear.char, "-12-31")
}


#' start.of.last.week.date()
#'
#' This function generates the date of the Monday of last week as a date object
#' @examples
#' start.of.last.week.date()
#'
start.of.last.week.date <- function(){
  Today.Date <- Sys.Date()
  dayofweek <- format(Today.Date, "%A")
  lastweekstart <- switch(dayofweek,
                          "Monday" = Today.Date - 7,
                          "Tuesday" = Today.Date -8,
                          "Wednesday" = Today.Date - 9,
                          "Thursday" = Today.Date - 10,
                          "Friday" = Today.Date - 11,
                          "Saturday" = Today.Date - 12,
                          "Sunday" = Today.Date - 13
                          )
}

#' start.of.last.week.str()
#'
#' This function generates the date of the Monday of last week as a character string
#' @examples
#' start.of.last.week.str()
#'
start.of.last.week.str <- function(){
  Today.Date <- Sys.Date()
  dayofweek <- format(Today.Date, "%A")
  lastweekstart <- switch(dayofweek,
                          "Monday" = as.character(Today.Date - 7),
                          "Tuesday" = as.character(Today.Date -8),
                          "Wednesday" = as.character(Today.Date - 9),
                          "Thursday" = as.character(Today.Date - 10),
                          "Friday" = as.character(Today.Date - 11),
                          "Saturday" = as.character(Today.Date - 12),
                          "Sunday" = as.character(Today.Date - 13)
  )
}



#' end.of.last.week.date()
#'
#' This function generates the date of the Sunday of last week as a date object
#' @examples
#' end.of.last.week.date()
#'
end.of.last.week.date <- function(){
  lastMon <- start.of.last.week.date()
  answer <- lastMon + 6
}

#' end.of.last.week.str()
#'
#' This function generates the date of the Sunday of last week as a character string
#' @examples
#' end.of.last.week.str()
#'
end.of.last.week.str <- function(){
  lastMon <- start.of.last.week.date()
  answer <- as.character(lastMon + 6)
}



#' start.of.this.week.date()
#'
#' This function generates the date of Monday of this week as a date object
#' @examples
#' start.of.this.week.date()
#'
start.of.this.week.date <- function(){
  Today.Date <- Sys.Date()
  dayofweek <- format(Today.Date, "%A")
  thisweekstart <- switch(dayofweek,
                          "Monday" = Today.Date,
                          "Tuesday" = Today.Date -1,
                          "Wednesday" = Today.Date - 2,
                          "Thursday" = Today.Date - 3,
                          "Friday" = Today.Date - 4,
                          "Saturday" = Today.Date - 5,
                          "Sunday" = Today.Date - 6
                          )
}

#' start.of.this.week.str()
#'
#' This function generates the date of Monday of this week as a character string
#' @examples
#' start.of.this.week.str()
#'
start.of.this.week.str <- function(){
  Today.Date <- Sys.Date()
  dayofweek <- format(Today.Date, "%A")
  thisweekstart <- switch(dayofweek,
                          "Monday" = as.character(Today.Date),
                          "Tuesday" = as.character(Today.Date -1),
                          "Wednesday" = as.character(Today.Date - 2),
                          "Thursday" = as.character(Today.Date - 3),
                          "Friday" = as.character(Today.Date - 4),
                          "Saturday" = as.character(Today.Date - 5),
                          "Sunday" = as.character(Today.Date - 6)
  )
}



