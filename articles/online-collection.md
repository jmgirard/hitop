# Collecting Responses Online with hitop-form

[hitop-form](https://jmgirard.github.io/hitop-form/) is a web page that
shows one of this package’s instruments in the browser: the HiTOP-SR or
one of its modules, the HiTOP-BR, or the PID-5 in its full, short or
brief form. The item text, response options and instructions are this
package’s own, read from its JSON export. The page needs no survey
platform and no account, and it scores nothing. Scoring is this
package’s job.

A study link tells the page what to show and where each participant’s
answers go. Three destinations are offered: a file saved on the
participant’s own device, a web address that accepts one row per
participant, or a table in a Supabase project. This article walks the
web-address route end to end with a Google Sheet as the store: a small
script bound to the sheet receives one row per participant, you download
the sheet as a CSV file, and
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
reads that file for the scoring functions. The steps are in the order
you take them.

## 1. Deploy the sheet’s script

A Google Sheet accepts rows from the page through a Google Apps Script
web app bound to it. The script’s code, and the steps to deploy it, are
in the hitop-form README under [Send responses to a Google
Sheet](https://github.com/jmgirard/hitop-form#send-responses-to-a-google-sheet).
In outline: create a new sheet, open Extensions and then Apps Script,
replace the contents of `Code.gs` with the README’s code, save, and
deploy it as a web app that runs as you and that anyone can access. Copy
the web app URL, which ends in `/exec`.

The script appends one row per request to a tab named `Responses`,
taking the first row’s keys as the header. It writes every cell as text,
so a participant code such as `007` keeps its zeros and a value such as
`=1+1` stays those four characters rather than becoming a formula. It
refuses a request whose body is not one object, holds more than 1000
keys, or has a key outside lower-case letters, digits and underscores,
so no one can grow the header without limit or put a formula in it.

Deploy one sheet and script per form. A key the header lacks is added to
it, so a sheet that receives two forms’ rows holds the union of their
columns. When the two forms are two instruments,
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
refuses the download, because its item columns carry two stems
(`hitopbr_01` beside `pid5bf_01`). When they are a HiTOP-SR module and
the full HiTOP-SR, the columns share one stem, so the download reads as
one data frame with the module’s rows blank on the items outside it,
unless the module’s link asked for a random order: its rows’
`item_order` then lists only the module’s items, and
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
refuses the download. Nothing else in the download or in
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
tells the two apart.

## 2. Make the study link

Open the page’s [link
builder](https://jmgirard.github.io/hitop-form/link.html). Choose the
instrument (and, for a HiTOP-SR module, paste the descriptor written by
[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)),
and name the study. A link builder opened from a “Make a study link”
button on an instrument’s download page arrives with that instrument
chosen, and one opened with a `c` parameter that carries a module
arrives with the descriptor pasted in as well. Give a participant
identifier if the link is for one person; leave it empty for a link
shared with many, and the page asks each participant for one before the
form starts. Under “Send responses to”, choose “A web address” and paste
the `/exec` URL from step 1. Press “Make the link” and send the link to
the participants.

When a participant presses Finish, the page posts their answers to the
address as one row and waits for the script to confirm. On a confirmed
send the page says the responses were sent to the study team, and saves
nothing. When the send is not confirmed, because of an error, a lost
connection, or no answer within the page’s limit, the page saves a CSV
file on the participant’s device and names it, so the participant can
send it to you by hand. That screen also has a “Save the file” button,
which saves the same file again; its instructions tell the participant
to press it if the file did not appear. A row that reached the sheet
after the limit is stored there as well; the `submitted` column
identifies such a pair.

## 3. Download the sheet as CSV

When collection ends, open the sheet’s `Responses` tab and choose File,
then Download, then Comma Separated Values (.csv). The file has one
header row and one row per participant, in the page’s column order:
`study`, `participant`, `instrument`, `form_build`, `submitted`, then
one column per item. A link that asks for a random order adds
`item_order` after `submitted`, and its item columns are in the
instrument’s order rather than the order shown. A link that recruits
through Prolific adds `prolific_study` and `prolific_session` after
`submitted`, or after `item_order` when the file has it, before the item
columns. The script builds the header from the first row it receives and
appends a key it has not seen, so a sheet that already holds rows
without `item_order` puts that column after the item columns.
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
reads it there. Start a new sheet for such a link if you want the column
sixth.

Anyone who has the link can post a row to the sheet, because the
script’s URL is inside the link. Look over the sheet before scoring it.

## 4. Read the file

[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
reads the download. The file below is one such download, from a sheet
that received two HiTOP-BR walks of the page. The package installs it as
an example, and
[`system.file()`](https://rdrr.io/r/base/system.file.html) gives its
path.

``` r

library(hitop)
path <- system.file("examples", "responses-sheet-hitopbr.csv", package = "hitop")
responses <- read_form_responses(path)
responses
#> # A tibble: 2 × 53
#>   study   participant instrument form_build submitted           item_order
#>   <chr>   <chr>       <chr>      <date>     <dttm>              <chr>     
#> 1 fixture =1+1        hitopbr    2026-09-20 2026-09-23 19:33:25 NA        
#> 2 fixture 007         hitopbr    2026-09-20 2026-09-23 19:33:30 NA        
#> # ℹ 47 more variables: prolific_study <chr>, prolific_session <chr>,
#> #   hitopbr_01 <int>, hitopbr_02 <int>, hitopbr_03 <int>, hitopbr_04 <int>,
#> #   hitopbr_05 <int>, hitopbr_06 <int>, hitopbr_07 <int>, hitopbr_08 <int>,
#> #   hitopbr_09 <int>, hitopbr_10 <int>, hitopbr_11 <int>, hitopbr_12 <int>,
#> #   hitopbr_13 <int>, hitopbr_14 <int>, hitopbr_15 <int>, hitopbr_16 <int>,
#> #   hitopbr_17 <int>, hitopbr_18 <int>, hitopbr_19 <int>, hitopbr_20 <int>,
#> #   hitopbr_21 <int>, hitopbr_22 <int>, hitopbr_23 <int>, hitopbr_24 <int>, …
```

Each row of the file is a row of the result. The lead columns are typed:
`participant` is character, so the codes `=1+1` and `007` come back as
the sheet stored them; `form_build` is a date; `submitted` is a UTC
date-time. Every item column is an integer. The sixth column,
`item_order`, is the order the participant saw the items, as item
numbers, when the file records one and `NA` when it does not, and
scoring does not read it. The page records it when the study link asks
for a random order, which the link builder’s “Show the items in a random
order” box sets. The seventh and eighth columns, `prolific_study` and
`prolific_session`, are the study and session identifiers Prolific
passed to the page when the link recruits through Prolific, as [the
Prolific route](#the-prolific-route) below describes, and `NA` when it
does not, as here. Scoring does not read them either.

A folder can hold the download beside any files participants sent by
hand:
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
reads every `.csv` file in it, file by file in path order, and each
file’s rows in its own order. Every file must hold the same item columns
in the same order, so read each form’s files in a call of their own.

## 5. Score it

Pass the item columns to the scoring function for the instrument. For
the HiTOP-BR that is
[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md):

``` r

items <- grep("^hitopbr_", names(responses), value = TRUE)
score_hitopbr(responses, items = items, append = FALSE)
#> # A tibble: 2 × 8
#>   hbr_antagonism hbr_detachment hbr_disinhibition hbr_internalizing
#>            <dbl>          <dbl>             <dbl>             <dbl>
#> 1           3.33            2.4              1.89              2.25
#> 2           3.33            2.4              1.89              2.25
#> # ℹ 4 more variables: hbr_somatoform <dbl>, hbr_thoughtDisorder <dbl>,
#> #   hbr_externalizing <dbl>, hbr_pFactor <dbl>
```

A full HiTOP-SR scores with
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
the same way, and a PID-5 form with
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
and the matching `version`, as
[`vignette("pid5_scoring")`](https://jmgirard.github.io/hitop/articles/pid5_scoring.md)
shows. A HiTOP-SR module scores through the descriptor the page showed,
with `layout = "printed"`; the [Building HiTOP-SR
Modules](https://jmgirard.github.io/hitop/articles/modules-hitopsr.md)
article walks that step.

## The Supabase route

The page can insert each row into a table in a Supabase project instead.
The link builder shows the SQL that creates the table, and the
hitop-form README under [Send responses to
Supabase](https://github.com/jmgirard/hitop-form#send-responses-to-supabase)
walks the setup. The table’s CSV export from the dashboard’s Table
Editor reads with
[`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
exactly as the sheet’s download does.

## The Prolific route

A study recruited through [Prolific](https://www.prolific.com/) gives
each participant a study URL. Prolific fills the participant’s ID, the
study’s ID and the submission’s ID into that URL through three
placeholders, `{{%PROLIFIC_PID%}}`, `{{%STUDY_ID%}}` and
`{{%SESSION_ID%}}`, as [Prolific’s API
reference](https://docs.prolific.com/api-reference/studies/the-study-object)
states under the study’s `external_study_url` field, and it asks that
all three be saved with the data ([What survey / experimental software
is compatible with
Prolific?](https://researcher-help.prolific.com/en/articles/445178-what-survey-experimental-software-is-compatible-with-prolific)).
The page fits that in two steps at the link builder.

Check “Recruit through Prolific” and leave the participant field empty.
The builder then prints the link with the three placeholders on its end.
Paste that link as the study URL on Prolific’s study page. When a
participant opens it, the page takes their Prolific ID as the
participant identifier and asks for none, and it writes the study and
submission IDs into `prolific_study` and `prolific_session`, after
`submitted` (and after `item_order` under the random order). A Supabase
table for such a link takes the two columns too, and the builder’s SQL
adds them. When you preview the study on Prolific, the preview passes a
24-character ID in place of the placeholder ([Previewing your
study](https://researcher-help.prolific.com/en/articles/445131-previewing-your-study)),
so the preview walks the form as a participant would; a link opened with
the placeholders still in it, as the builder’s “Open the link” does,
asks for the identifier instead. Prolific’s own “I’ll use URL
parameters” option, in the Data collection section of the study’s setup,
appends the three parameters to the study URL itself ([the compatibility
article](https://researcher-help.prolific.com/en/articles/445178-what-survey-experimental-software-is-compatible-with-prolific),
under “Recording participants’ Prolific IDs”). A pasted link with the
builder’s placeholders therefore carries each parameter once unless that
option adds it again; then the address holds the parameter twice, and
the page reads the first value that is neither blank nor a placeholder.

Give the completion URL from the study’s page on Prolific, of the form
`https://app.prolific.com/submissions/complete?cc=…` ([the compatibility
article](https://researcher-help.prolific.com/en/articles/445178-what-survey-experimental-software-is-compatible-with-prolific),
under “Returning Participants to Prolific”), in the builder’s
“Completion URL” field. After a confirmed send the page shows the sent
screen, with a link to that address in place of “You can close this
page.”, and then sends the participant there, which is the return
Prolific recommends ([Data
collection](https://researcher-help.prolific.com/en/articles/445127-data-collection)).
After a saved file, because the link names no store or the send was not
confirmed, the page shows the file name first, then the instructions and
the “Save the file” button, and then a link to that address, so the
participant sees the file’s name and can save it again before leaving.

A study can hold more than one completion code, each for its own outcome
([Custom completion
codes](https://researcher-help.prolific.com/en/articles/445170-custom-completion-codes)),
and each code has its own `?cc=` address, as [the API
reference](https://docs.prolific.com/api-reference/studies/the-study-object)
states under `completion_codes`. To give a saved file its own code, put
that code’s address in the builder’s “Completion URL after a saved file”
field, beside the completion URL. The saved-file screens then link to it
in place of the completion URL, and a confirmed send still goes to the
completion URL.

The download then reads as above, with `prolific_study` and
`prolific_session` filled. The file below is one the page saved under
such a link, with the Prolific ID as the participant:

``` r

path <- system.file("examples", "responses-hitopbr-prolific.csv", package = "hitop")
responses <- read_form_responses(path)
responses[, c("participant", "prolific_study", "prolific_session")]
#> # A tibble: 1 × 3
#>   participant              prolific_study           prolific_session        
#>   <chr>                    <chr>                    <chr>                   
#> 1 5a9d64f5f6dfdd0001eaa73d 66f3a1b2c3d4e5f60718293a 66f3a1b2c3d4e5f60718294b
```

## Who holds the data

The answers go to the store the link names, and to a file on the
participant’s device when the link names no store or the send is not
confirmed. They go nowhere else. With a Google Sheet, Google holds the
rows; with a Supabase table, Supabase holds them. The page’s host,
GitHub Pages, receives the study link with each page load, and the link
carries the instrument and any module, the study name, the participant
code, the store’s address and, for Supabase, its key. It receives no
answers.

A participant code is whatever the link or the participant supplies. A
code you put in the link reaches the page’s host with the link, and a
code the participant types is whatever they choose to type. If codes
must not identify anyone, issue pseudonymous codes in the links you send
and keep the key to them yourself. Under the Prolific route the code is
the participant’s Prolific ID, which identifies them to Prolific, and
the page’s host receives the three Prolific parameters, the ID among
them, with each page load, as it receives the rest of the address.

Whether a store may hold your study’s data is a question for your
institution, and any agreement it needs with the store’s vendor, such as
a business associate agreement or a data processing agreement, is the
institution’s to make. This article describes where the rows go; it does
not say which store is suitable for which study.
