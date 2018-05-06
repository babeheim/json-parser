

parse_null <- function(null_string){
  return(NA)
}

parse_logical <- function(logical_string){
  if(logical_string == "true") output <- TRUE
  if(logical_string == "false") output <- FALSE
  return(output)
}

parse_number <- function(number_string){
  return(as.numeric(number_string))
}

parse_string <- function(string_string){
  string_string <- gsub("(?<!\\\\)\"", "", string_string, perl = TRUE)
  string_string <- gsub("\\\\", "", string_string, perl = TRUE)
  return(string_string)
}

parse_array <- function(array_string){
  array_string <- gsub("^\\[|\\]$", "", array_string)
  array_values <- split_comma_string(array_string)
  output <- list()
  for(i in 1:length(array_values)) output[[i]] <- parse_value(array_values[i])
  return(output)
}

parse_object <- function(object_string){
  object_string <- gsub("^\\{|\\}$", "", object_string)
  object_pairs <- split_comma_string(object_string)
  output <- list()
  for(i in 1:length(object_pairs)) output <- c(output, parse_key_value(object_pairs[i]))
  return(output)
}

identify_value <- function(value_string){
  first_char <- substr(value_string, 1, 1)
  output <- NA
  if (length(grep("[0-9]", first_char) == 1)) output <- "numeric"
  if (length(grep("\"", first_char) == 1)) output <- "string"
  if (length(grep("\\{", first_char) == 1)) output <- "object"
  if (length(grep("\\[", first_char) == 1)) output <- "array"
  if (length(grep("[tf]", first_char) == 1)) output <- "logical"
  if (length(grep("[n]", first_char) == 1)) output <- "null"
  if (length(grep(" ", first_char) == 1)) stop("whitespace is the first character of a value!")
  if (is.na(output)) stop(paste("identify_value cannot classify this value:", first_char))
  return(output)
}

parse_value <- function(value_string){
  value_type <- identify_value(value_string)
  output <- "unknown"
  if(value_type == "numeric") output <- parse_number(value_string)
  if(value_type == "string") output <- parse_string(value_string)
  if(value_type == "logical") output <- parse_logical(value_string)
  if(value_type == "null") output <- parse_null(value_string)
  if(value_type == "array") output <- parse_array(value_string)
  if(value_type == "object") output <- parse_object(value_string)
#  if(!is.na(output) & output == "unknown") stop(paste("could not parse", value_string, "correctly:", value_type))
  return(output)
}

parse_key_value <- function(key_value_string){
  m <- regexpr("\".*?(?<!\\\\)\"", key_value_string, perl = TRUE)
  key <- regmatches(key_value_string, m)
  value <- substr(key_value_string, m + attr(m, "match.length"), nchar(key_value_string))
  value <- gsub("^\\s*:\\s*", "", value)
  output <- list()
  output[[1]] <- parse_value(value)
  names(output) <- parse_string(key)
  return(output)
}

split_comma_string <- function(comma_string){

  quote_pattern <- "\"((?>=\\\\\"|[^\"])|(?R))*?\""
  m <- gregexpr(quote_pattern, comma_string, perl = TRUE)
  embedded_quotes <- regmatches(comma_string, m)[[1]]
  has_quotes <- FALSE
  if(length(embedded_quotes) > 0) has_quotes <- TRUE
  if(has_quotes){
    quote_placeholders <- rep(NA, length(embedded_quotes))
    for(i in 1:length(embedded_quotes)) quote_placeholders[i] <- paste(sample(c(LETTERS, 0:9), nchar(embedded_quotes[i]), replace = TRUE), collapse = "")
    regmatches(comma_string, m)[[1]] <- quote_placeholders
  }
  group_pattern <- "\\{((?>=\\\\\\{|\\\\\\}|[^\\{\\}])|(?R))*\\}"
  m <- gregexpr(group_pattern, comma_string, perl = TRUE)
  embedded_groups <- regmatches(comma_string, m)[[1]]
  has_groups <- FALSE
  if(length(embedded_groups) > 0) has_groups <- TRUE
  if(has_groups){
    group_placeholders <- rep(NA, length(embedded_groups))
    for(i in 1:length(embedded_groups)) group_placeholders[i] <- paste(sample(c(LETTERS, 0:9), nchar(embedded_groups[i]), replace = TRUE), collapse = "")
    regmatches(comma_string, m)[[1]] <- group_placeholders
  }
  array_pattern <- "\\[((?>=\\\\\\[|\\\\\\]|[^\\[\\]])|(?R))*\\]"
  m <- gregexpr(array_pattern, comma_string, perl = TRUE)
  embedded_arrays <- regmatches(comma_string, m)[[1]]
  has_arrays <- FALSE
  if(length(embedded_arrays) > 0) has_arrays <- TRUE
  if(has_arrays){
    array_placeholders <- rep(NA, length(embedded_arrays))
    for(i in 1:length(embedded_arrays)) array_placeholders[i] <- paste(sample(c(LETTERS, 0:9), nchar(embedded_arrays[i]), replace = TRUE), collapse = "")
    regmatches(comma_string, m)[[1]] <- array_placeholders
  }

  output <- strsplit(comma_string, split = ",\\s*")[[1]]
  output <- gsub("^\\s*|\\s*$", "", output)

  # in reverse order from above
  if(has_arrays){
    for(i in 1:length(array_placeholders)) output <- gsub(array_placeholders[i], embedded_arrays[i], output)
  }  
  if(has_groups){
    for(i in 1:length(group_placeholders)) output <- gsub(group_placeholders[i], embedded_groups[i], output)
  }
  if(has_quotes){
    for(i in 1:length(quote_placeholders)) output <- gsub(quote_placeholders[i], embedded_quotes[i], output)
  }
  return(output)
}

read_json <- function(path){
  json_string <- paste(readLines(path, warn = FALSE), collapse = "")
  output <- parse_value(json_string)
  return(output)
}