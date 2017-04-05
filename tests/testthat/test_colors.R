context("Colors and palettes")

test_that("Test color palettes", {
    skip_on_cran()
    
    expect_true(all(is_color(cases_pal(1))))
    expect_true(all(is_color(cases_pal(100))))
    expect_true(all(is_color(spectral(1))))
    expect_true(all(is_color(spectral(100))))

    msg <- "n is not a number"
    expect_error(cases_pal("asd"), msg)
    
})






test_that("Test transp", {
    skip_on_cran()

    col <- cases_pal(100)
    expect_true(all(is_color(transp(col))))
    
})






test_that("Test char2col", {
    skip_on_cran()

    x <- sample(letters[1:5], 50, replace = TRUE)
    expect_true(all(is_color(char2col(x))))
    expect_length(char2col(x), length(x))
    
})
