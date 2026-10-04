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

## PID-5 Informant Form instructions (D-089). From the APA form (Markon et al.,
## 2013; cairn/references/apa2013pid5irf.md): `start` heads the first page and
## `continue` the later pages; `prompt` and `stem` head the item column, each
## item completing the stem. `notice` is the copyright and permission line the
## form prints at the foot of each page (M160). The response labels are the
## self-report form's. data-raw/check_pid_irf_text.R checks start, continue,
## prompt, stem, notice and the labels against the shelf PDF.
pid_irf_instructions <- list(
  start = "This is a list of things different people might say about others. We are interested in how you would describe the person you are rating. There are no right or wrong answers. So you can describe him/her as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes him/her.",
  continue = "Please continue to complete the questionnaire. We are interested in how you would describe the person you are rating. There are no right or wrong answers. So you can describe him/her as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes him/her.",
  prompt = "Please rate how true or false each of the following statements are of the person you are rating.",
  stem = "He or she…",
  notice = "Markon KE, Quilty LC, Bagby RM, Krueger RF. Copyright © 2013 American Psychiatric Association. All Rights Reserved. This material can be reproduced without permission by researchers and by clinicians for use with their patients.",
  options = pid_instructions$options
)

## PID-5 child form instructions, ages 11 to 17 (M161). From the APA child
## forms (Krueger et al., 2013; cairn/references/apa2013pid5child.md), one
## entry per form: `FULL` for the 220-item form and `BF` for the 25-item brief
## form. `start` is the instruction paragraph of the first item page (PDF
## p. 2) without its label ("Instructions to the child receiving care:" or
## "Instructions:"), as
## `pid_instructions$start` omits the adult label. `notice` is the line each
## form prints at the foot of its item pages. The items, keying and response
## labels are the adult forms'. data-raw/check_pid_child_text.R checks start,
## notice and the labels against the shelf PDFs.
pid_child_instructions <- list(
  FULL = list(
    start = "This is a list of things different people might say about themselves. We are interested in how you would describe yourself. There are no \"right\" or \"wrong\" answers. So you can describe yourself as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes you.",
    notice = "Krueger RF, Derringer J, Markon KE, Watson D, Skodol AE. Copyright © 2013 American Psychiatric Association. All rights reserved. This material can be reproduced without permission by researchers and by clinicians for use with their patients.",
    options = pid_instructions$options
  ),
  BF = list(
    start = "This is a list of things different people might say about themselves. We are interested in how you would describe yourself. There are no right or wrong answers. So you can describe yourself as honestly as possible, we will keep your responses confidential. We'd like you to take your time and read each statement carefully, selecting the response that best describes you.",
    notice = "Krueger RF, Derringer J, Markon KE, Watson D, Skodol AE. Copyright © 2013 American Psychiatric Association. All Rights Reserved. This material can be reproduced without permission by researchers and by clinicians for use with their patients.",
    options = pid_instructions$options
  )
)

## HiTOP-SR instructions
hitopsr_instructions <-
  list(
    start = "Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.",
    options = data.frame(
      value = 1:4,
      label = c("Not at all", "A little", "Moderately", "A lot")
    )
  )

## HiTOP-BR instructions
hitopbr_instructions <-
  list(
    start = "Please consider whether there have been significant times during the last 12 months during which the following statements applied to you. Then please select the option that best describes how well each statement described you during that period.",
    options = data.frame(
      value = 1:4,
      label = c("Not at all", "A little", "Moderately", "A lot")
    )
  )

## HiTOP-HSUM instructions
hitophsum_instructions <- list(
  start = "Please select which of the following substance(s) you have used in the past 12 months. Please consider only substances that were NOT prescribed to you by a medical professional or that you used in a manner that was NOT prescribed."
)

## HiTOP-DAT instructions
## One text per measure, named as `hitopdat_items$Measure`, taken from the
## HiTOP-DAT Qualtrics file (cairn/SOURCES.md, "HiTOP-DAT") with the HTML
## removed by `dat_plain()` in data-raw/hitopdat_info.R: line and list breaks
## kept as "\n", other runs of spaces made one space. The IDAS-II and CAT-PD
## texts head each of their grids in the file, the same text on every grid.
## The CAPE block has no instruction text, recorded as NA.
hitopdat_instructions <- list(
  start = c(
    "WHODAS" = "In the last 30 days, how much difficulty did you have in:",
    "IDAS-II" = "Below is a list of feelings, sensations, problems, and experiences that people sometimes have. Read each item to determine how well it describes your recent feelings and experiences. Then, enter the choice that best describes how much you have felt or experienced things this way during THE PAST TWO WEEKS, including today.",
    "AUDIT" = "Because alcohol use can affect your health and can interfere with certain medications and treatments, it is important that we ask some questions about your use of alcohol. Your answers will remain confidential so please be honest. Check the box that best describes your answer to each question.",
    "DUDIT" = "Here are a few questions about drugs. Drugs include:\nMarijuana, hash, hash oil\nMethamphetamine, phenmetraline, khat, betel nut, ritaline (methylphenidate)\nCrack, freebase, coca leaves\nSmoked heroin, heroin, opium\nEcstasy, LSD, mescaline, peyote, PCP (phencyclidine), psilocybin, DMT (dimethyltrypamine)\nThinner, trichlorethylene, gasoline, gas, solution, glue\nGHB, anabolic steroids, laughing gas (halothane), amyl nitrate (poppers), anticholinergics\nPills count as drugs when you take them to feel good or get high. Pills that are sometimes used as drugs include sleeping pills, sedatives, and painkillers.\nPlease answer as correctly and honestly as possible by indicating which answer is right for you.",
    "CAPE" = NA_character_,
    "CAT-PD" = "For the following questions, please describe yourself as you generally are now, not as you wish to be in the future. Describe yourself as you honestly see yourself, in relation to other people you know who are the same sex and roughly the same age as you. So that you can describe yourself in an honest manner, your responses will be kept in absolute confidence.",
    "PHQ-15" = "During the past 4 weeks, how much have you been bothered by any of the following problems?"
  )
)

usethis::use_data(
  pid_instructions,
  pid_irf_instructions,
  pid_child_instructions,
  hitopsr_instructions,
  hitopbr_instructions,
  hitophsum_instructions,
  hitopdat_instructions,
  internal = TRUE,
  overwrite = TRUE
)
