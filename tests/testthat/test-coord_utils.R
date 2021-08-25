expect_split_coord_equal <- function(val1, val2) {
    # Put different numbers of spaces in the coordinate string
    spaces = expand.grid(s1=0:1, s2=0:1, s3=0:1, s4=0:1)
    for (i in 1:nrow(spaces)) {
        s1 <- paste0(rep(" ", spaces[i, "s1"]), collapse="")
        s2 <- paste0(rep(" ", spaces[i, "s2"]), collapse="")
        s3 <- paste0(rep(" ", spaces[i, "s3"]), collapse="")
        s4 <- paste0(rep(" ", spaces[i, "s4"]), collapse="")
        coord <- paste0("(", s1, val1, s2, ",", s3, val2, s4, ")")
        split <- split_coord(coord)
        val1_expect <- paste0(s1, val1, s2)
        val2_expect <- paste0(s3, val2, s4)
        expect_equal(split, c(val1_expect, val2_expect))
    }
}
test_that("coordinates are split correctly", {
    expect_split_coord_equal("0", "0")
    expect_split_coord_equal("1in", "0")
    expect_split_coord_equal("1in", "15cm")
    expect_split_coord_equal("1 mm", "20 pt")
    expect_split_coord_equal("-1", "1")
    expect_split_coord_equal("-1", "-1")
    expect_split_coord_equal("1", "Inf")
    expect_split_coord_equal("-Inf", "Inf")
    expect_split_coord_equal("-Inf", "1")
})

test_that("coordinates are replaced correctly", {
    f <- function(x) "A"
    expect_equal(replace_coords("\\draw (0,0) (1,1);", f), "\\draw A A;")
})
