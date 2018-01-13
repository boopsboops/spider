context("sequence statistics")

data(anoteropsis)

test_that("seqStat gives correct number of bases", {
    expect_equal(seqStat(anoteropsis)[[1]], 395)
})
