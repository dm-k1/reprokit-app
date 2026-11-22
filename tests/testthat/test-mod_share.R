test_that("no verdict without a stated reason for not sharing", {
  testServer(mod_share_server, {
    session$setInputs(sharing_reason = "", model = NULL, go = 1)
    expect_error(result(), "State why", class = "shiny.silent.error")
  })
})

test_that("a model with a reason gets a reprokit verdict", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(), path)
  testServer(mod_share_server, {
    session$setInputs(sharing_reason = "No consent for sharing.",
                      model = list(datapath = path), go = 1)
    expect_s3_class(result(), "reprokit_triage")
  })
})
