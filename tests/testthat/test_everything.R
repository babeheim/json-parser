

# we also need an identify_value 

test_that("all value types identified", {

  x <- "\"Charles\""
  expect_true(identify_value(x) == "string")

  x <- "\"\u1234"
  expect_true(identify_value(x) == "string")

  x <- "5"
  expect_true(identify_value(x) == "numeric")

  x <- "true"
  expect_true(identify_value(x) == "logical")

  x <- "false"
  expect_true(identify_value(x) == "logical")

  x <- "null"
  expect_true(identify_value(x) == "null")

  x <- "{ \"name\": \"Charles\" }"
  expect_true(identify_value(x) == "object")

  x <- "[ 5, 4 ]"
  expect_true(identify_value(x) == "array")

  # whitespace and other stuff not allowed

  x <- " [ 5, 4 ]"
  expect_error(identify_value(x))

  x <- "abc"
  expect_error(identify_value(x))

})


# start with the parse_ functions, the simplest ones first


test_that("parse_value functions work", {

  x <- "true"
  expect_true(parse_logical(x) == TRUE)

  x <- "false"
  expect_true(parse_logical(x) == FALSE)

  x <- "null"
  expect_true(is.na(parse_null(x)))

  # numbers

  x <- "5"
  expect_true(parse_number(x) == "5")

  x <- "0"
  expect_true(parse_number(x) == "0")

  x <- "0.001"
  expect_true(parse_number(x) == "0.001")

  x <- "5.32"
  expect_true(parse_number(x) == "5.32")

  x <- "5e10"
  expect_true(parse_number(x) == "5e+10")

  x <- "5e+10"
  expect_true(parse_number(x) == "5e+10")

  x <- "5e-10"
  expect_true(parse_number(x) == "5e-10")

  # strings

  x <- "\"Charles\""
  expect_true(parse_string(x) == "Charles")

  x <- "\"\u00df\""
  expect_true(parse_string(x) == "ß")

  x <- "\"\b\""
  expect_true(parse_string(x) == "\b")

  x <- "\"\n\""
  expect_true(parse_string(x) == "\n")

# ok, time to build parse_value not attempting arrays or objects yet

  array_values <- c("true", "true", "false")

  output <- list()
  for(i in 1:length(array_values)) output[[i]] <- parse_value(array_values[i])

  expect_true(length(output) == 3)
  expect_true(all(unlist(output) == c("TRUE", "TRUE", "FALSE")))

})

# now  on to parse_key_value

# parse_key_value

## code to parse out the key

test_that("parse_key_value stuff works", {

  test_string <- "\"name\": \"Charles\""
  key_pattern <- "\".*?(?<!\\\\)\""
  m <- regexpr(key_pattern, test_string, perl = TRUE)
  key_string <- regmatches(test_string, m)
  expect_true(parse_string(key_string) == "name")

  ## code to parse out the value

  test_string <- "\"name\": \"Charles\""
  key_pattern <- "\".*?(?<!\\\\)\""
  m <- regexpr(key_pattern, test_string, perl = TRUE)
  value <- substr(test_string, m + attr(m, "match.length"), nchar(test_string))
  value <- gsub("^\\s*:\\s*", "", value)
  expect_true(parse_string(value) == "Charles")


  # keys can be any string, which means they can contain escaped quotes (\\\\") right?

  x <- "\"name\": \"Charles\""
  y <- parse_key_value(x)
  expect_true(names(y) == "name")
  expect_true(y == "Charles")

  x <- "\"na\\\\\"me\": \"Charles\""
  y <- parse_key_value(x)
  expect_true(names(y) == "na\"me")
  expect_true(y == "Charles")

  x <- "\"age\": 10"
  y <- parse_key_value(x)
  expect_true(names(y) == "age")
  expect_true(y == 10)

  x <- "\"male\": true"
  y <-parse_key_value(x)
  expect_true(names(y) == "male")
  expect_true(y == TRUE)

  x <- "\"female\": false"
  y <- parse_key_value(x)
  expect_true(names(y) == "female")
  expect_true(y == FALSE)

  x <- "\"computer\": null"
  y <- parse_key_value(x)
  expect_true(names(y) == "computer")
  expect_true(is.na(y))

  x <- "\"esetz\": \"\u00df\""
  y <- parse_key_value(x)
  expect_true(names(y) == "esetz")
  expect_true(y == "ß")

})

## test splitting now

# test patterns used to detect groups

test_that("splitting works", {

  test_string <- "a, b, {c, d}"
  group_pattern <- "\\{((?>=\\\\\\{|\\\\\\}|[^\\{\\}])|(?R))*\\}"
  m <- gregexpr(group_pattern, test_string, perl = TRUE)
  expect_true(regmatches(test_string, m)[[1]] == "{c, d}")

  test_string <- "a, b, \"c, d\""
  quote_pattern <- "\"((?>=\\\\\"|[^\"])|(?R))*?\""
  m <- gregexpr(quote_pattern, test_string, perl = TRUE)
  expect_true(regmatches(test_string, m)[[1]] == "\"c, d\"")

  test_string <- "\"has name\": true, \"has age\": false"
  quote_pattern <- "\"((?>=\\\\\"|[^\"])|(?R))*?\""
  m <- gregexpr(quote_pattern, test_string, perl = TRUE)
  expect_true(all(regmatches(test_string, m)[[1]] == c("\"has name\"", "\"has age\"")))

  test_string <- "a, b, [c, d]"
  array_pattern <- "\\[((?>=\\\\\\[|\\\\\\]|[^\\[\\]])|(?R))*\\]"
  m <- gregexpr(array_pattern, test_string, perl = TRUE)
  expect_true(regmatches(test_string, m)[[1]] == "[c, d]")

  # we define what split_comma_string needs to accept, what it needs to return
  # including things it doesn't do anything with

  # split up array

  x <- "[ 1, 2, 3 ]"
  x <- gsub("^\\[|\\]$", "", x)
  y <- split_comma_string(x)
  expect_true(identical(y, c("1", "2", "3")))

  x <- "[ a, b, c]"
  x <- gsub("^\\[|\\]$", "", x)
  y <- split_comma_string(x)
  expect_true(identical(y, c("a", "b", "c")))

  x <- "[ \"a\", \"b\", [\"c\", \"d\", \"e\"], \"f\"]"
  x <- gsub("^\\[|\\]$", "", x)
  y <- split_comma_string(x)
  expect_true(identical(y, c("\"a\"", "\"b\"", "[\"c\", \"d\", \"e\"]", "\"f\"")))

  x <- "\"has name\": true, \"has age\": false"
  x <- gsub("^\\{|\\}$", "", x)
  y <- split_comma_string(x)
  expect_true(identical(y, c("\"has name\": true", "\"has age\": false")))

})


test_that("split_comma_string works", {

  # split_comma_string

  # splits up simple comma strings

  comma_string <- "here, there"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here", "there")))

  # simply returns non-comma strings unchanged
  object_string <- "\"name\" : \"Charles\""
  y <- split_comma_string(object_string)
  expect_true(identical(y, object_string))

  comma_string <- "here"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, comma_string))

  # will split up strings even if commas are escaped

  comma_string <- "here\\, there"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here\\", "there")))


  # ignores curly brackets, ignores square brackes, ignores stuff between quotes with commas in them

  comma_string <- "here, {fi,rst}, [sec,ond], \"thi,rd\""
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here", "{fi,rst}", "[sec,ond]", "\"thi,rd\"")))

  # not fooled by nesting

  comma_string <- "here, {first, second, third}, [fourth, [fifth]]"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here", "{first, second, third}", "[fourth, [fifth]]")))

  comma_string <- "here, {first, [second, third]}, [fourth, [fifth]]"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here", "{first, [second, third]}", "[fourth, [fifth]]")))

  comma_string <- "here, {first, \"there is, a place\"}"
  y <- split_comma_string(comma_string)
  expect_true(identical(y, c("here", "{first, \"there is, a place\"}")))

  array_string <- "[here, {first, [second, third]}, [fourth, [fifth]]]"
  y <- split_comma_string(array_string)
  expect_true(identical(y, array_string))

  group_string <- "{here, {first, [second, third]}, [fourth, [fifth]]}"
  y <- split_comma_string(group_string)
  expect_true(identical(y, group_string))

})


# now test groups and arrays WITHOUT groups or arrays as values inside of them

test_that("parse_array works", {

  object_pairs <- c("\"name\": \"Charles\"", "\"age\": 10", "\"male\": true", 
    "\"female\": false", "\"computer\": null", "\"esetz\": \"\u00df\"")

  output <- list()
  for(i in 1:length(object_pairs)) output <- c(output, parse_key_value(object_pairs[i]))
  output

  array_string <- "[\"first\", \"second\", \"third\"]"
  parse_array(array_string)
  parse_value(array_string)

  # now arrays with other arrays inside of them

  array_string <- "[\"first\", [\"second\", \"third\"]]"
  parse_array(array_string)

  array_string <- "[\"first\", [\"second\", \"third\"], [\"fourth\", \"fifth\"]]"
  parse_array(array_string)

  array_string <- "[\"here\", \"first\", [\"second\", \"third\"], [\"fourth\", [\"fifth\"]]]"
  parse_array(array_string)

})




# THEN FINALLY test that!

# now combine 

test_that("parse_object works", {

  object_string <- "{\"name\": \"Charles\"}"
  parse_object(object_string)

  object_string <- "{\"name\": \"Charles\", \"age\": 10}"
  parse_object(object_string)

  object_string <- "{\"name\": \"Charles\", \"age\": 10, \"scores\": [10, 11, 10]}"
  parse_object(object_string)

  object_string <- "{\"name\": \"Charles\", \"age\": 10, \"scores\": [10, 11, 10]}"
  parse_object(object_string)

})

# now that all work, you should be able to parse ANYTHING using parse_value


test_that("real valid files work", {

  test <- paste0(readLines("./valid/simple_object.json", warn = FALSE), collapse = "")
  parse_value(test)

  test <- paste0(readLines("./valid/simple_array.json", warn = FALSE), collapse = "")
  parse_value(test)

  my_files <- list.files("./valid", full.names = TRUE)

  for (i in 1:length(my_files)) {
    test <- paste0(readLines("./valid/array_with_colons.json", warn = FALSE), collapse = "")
    expect_silent(parse_value(test))
  }

  test_that("valid reads", {
    read_json("./valid/simple_object.json")
    read_json("./valid/simple_array.json")

    my_files <- list.files("./valid", full.names = TRUE)
    for (i in 1:length(my_files)) {
      expect_silent(read_json(my_files[i]))
    }
  })

})