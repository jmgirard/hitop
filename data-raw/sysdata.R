# Internal package data (R/sysdata.rda)
#
# Administration instructions for each instrument. These are internal objects
# used by the generate_{docx,qualtrics,redcap}_* families; they are NOT
# user-facing datasets, so they are written to R/sysdata.rda (internal = TRUE)
# rather than to data/. Regenerate R/sysdata.rda by running this script.

## PID-5 instructions
pid_instructions <- list(
  start = "This is a list of things different people might say about themselves. We are interested in how you would describe yourself. There are no right or wrong answers. So you can describe yourself as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes you.",
  options = data.frame(
    value = 0:3,
    label = c(
      "Very False or Often False",
      "Sometimes or Somewhat False",
      "Sometimes or Somewhat True",
      "Very True or Often True"
    ),
    stringsAsFactors = FALSE
  )
)

## HiTOP-SR instructions
hitopsr_instructions <-
  list(
    start = "Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.",
    options = data.frame(
      value = c(1, 2, 3, 4),
      label = c("Not at all", "A little", "Moderately", "A lot")
    )
  )

## HiTOP-BR instructions
hitopbr_instructions <-
  list(
    start = "Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.",
    options = data.frame(
      value = c(1, 2, 3, 4),
      label = c("Not at all", "A little", "Moderately", "A lot")
    )
  )

## HiTOP-HSUM instructions
hitophsum_instructions <- list(
  start = "Please select which of the following substance(s) you have used in the past 12 months. Please consider only substances that were NOT prescribed to you by a medical professional or that you used in a manner that was NOT prescribed."
)

usethis::use_data(
  pid_instructions,
  hitopsr_instructions,
  hitopbr_instructions,
  hitophsum_instructions,
  internal = TRUE,
  overwrite = TRUE
)
