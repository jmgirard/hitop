# RR08: PID-5-FFBF keying, forensic domains and missing-data rule (M162)

- **Date:** 2026-10-04
- **Brief:** `cairn/reviews/RB08-ffbf-keying.md`
- **Reviewer:** independent review (Fable), from the materials the brief names only
- **Binding criteria:** not requested. This report is advisory.

Materials read:

- `cairn/references/niemeyer2022.md`.
- The article, through `pdftotext -layout`, pp. 30 to 43.
- Table S3, all 11 pages.
- The authors' code at lines 44 to 190, 375 to 423 and 3300 to 3390.
- `data-raw/pid_ffbf_items.csv` and the FFBF sections of `data-raw/pid_info.R`.
- The FFBF block of `tests/testthat/helper-fixtures.R`.
- `data-raw/check_pid_ffbf_text.R`, run 2026-10-04: PASS, exit 0.
- `cairn/SOURCES.md`, "Note on FULL/SF domain scoring".
- IP1 to IP3 in `cairn/DESIGN.md`, and D-009, D-018, D-088, D-090.
- The M162 milestone file.

I also ran my own comparisons of the code's informant and original-form lists. The check script does not read those lines.

## 1. Facet map

All four sources agree on every item. No item's facet differs between any two of them.

Evidence:

- Table S3 against the shipped table. `check_pid_ffbf_text.R` reads the facet heading above each item and compares it with `pid_ffbf_items$Facet` for all 100 items. PASS. I also read all 11 pages. Each heading holds exactly four items. The headings run in alphabetical order from Anhedonia to Withdrawal. Under heading k, the item numbers are k, k + 25, k + 50 and k + 75.
- Code self-report lists (lines 109 to 133) against the table. The check script compares all 25. PASS.
- Code informant lists (lines 135 to 159) against the self-report lists. I parsed both and compared item numbers facet by facet. 25 lists, 0 differences.
- Code original-form lists (lines 3311 to 3335 self, 3337 to 3361 informant) against the FFBF lists. 25 and 25 lists, 0 differences. The authors numbered the unadapted FBF of their pilot the same way.
- The "k, k + 25, k + 50, k + 75" rule holds for all 25 facets (checked by script against the shipped table).

If a source had differed, Table S3 governs item content and facet membership, because it is the published item table of the supplement. The code governs only what the paper does not print. Nothing turns on it here.

One fact belongs on the help page, though it is not a discrepancy. FFBF item numbers are the authors' own. They do not match the package's Maples numbering in `pid_items$SF`. When FFBF item n is matched to SF item n, 98 of 100 items land in a different facet. A user cannot reuse an SF `items` mapping for FFBF columns.

## 2. Reverse items

Items 12 and 26 are the only reverse-keyed items, for both the self-report and the informant form.

Evidence:

- Table S3 prints "(-)" in exactly two rows, 12 (p. 5) and 26 (p. 1), in both German columns of each. The check script's scan of the whole PDF for "(-)" returns 12 and 26. The table's note defines "(-) = reverse coded".
- The code recodes items 12 and 26 as 3 minus the response for `_self`, `_acqu1` and `_acqu2` (lines 382 to 388). It recodes no other item.
- The article says "two items of the PID-5-FFBF are reverse-coded" (p. 32).

The mark sits in the German columns only because the study administered the German items (p. 33). The English texts of both items point the same way as the German. Item 12 reads "I usually think before I act". Item 26 reads "I enjoy life to the extent it is possible to do so in prison". The informant versions point the same way too. Item 12 reads "usually thinks before they act". Item 26 reads "enjoys life to the extent it is possible to do so in prison". The informant recodes in the code settle the informant form. There is no reason to key the English or the informant version differently.

The SF has no reverse items because Maples' 100 items include none of the 16 APA reverse items. FFBF item 12 is PID-5 item 58 (Table S3 note "G-PID-5 Item 58, adapted"). The APA key reverse-keys item 58 (`pid_items`: FULL 58, Impulsivity, Reverse TRUE, "I usually think before I act"). FFBF item 26 carries no source note. It is a rewrite in the positive direction for prisoners. Both reverse items arrived through the adaptation.

Unmarked reverse wording: I read all 100 English self-report texts and all 100 English informant texts against their facets. Items 1, 4, 30, 38, 50, 66, 68, 75, 79, 93 and 96 contain a negation. Each one points toward its facet, not away from it. Item 38 (Intimacy Avoidance) is the closest call. It says that the current difficulty of sexual relationships does not bother the respondent, which is avoidance, so it is forward. No item is worded in reverse without a mark.

## 3. Forensic domains

The paper's reported four-factor domain scores use the three-facet definition. The paper states that definition in words. It is not an analysis choice confined to the script.

Evidence:

- p. 35: "The three scales that primarily contributed to the third factor were Emotional Lability, Hostility, and Impulsivity. We labeled this factor Disinhibited Aggression." The same page names Separation Insecurity and Anxiousness as two of the three scales of the fourth factor, then: "The third scale that primarily contributed to the fourth factor was Cognitive and Perceptual Dysregulation. We labeled the fourth factor Insecurity."
- p. 36, Reliability: "The 12-item PID-5-FFBF domains of the four-factor solution were reliable, with Cronbach's alphas of .74 to .89". Twelve items is three facets of four items. The five-factor domains are described the same way ("APA-three scales only scoring", p. 33).
- p. 38: "Disinhibited Aggression (main facets: emotional lability, hostility, and impulsivity)" and "Insecurity (main facets: separation insecurity, anxiousness, perceptual dysregulation)".
- p. 39: "the three scales that contribute to the domain scores of Insecurity and Negative Affectivity differ in one facet (perceptual dysregulation vs. emotional lability)". This sentence names the domain score and its three scales directly.
- Table 3 and Table 5 come from these domain scores. Table 3's note says the regressions use "all four domains as independent variables". The code that produces them is lines 182 to 190 and 416 to 423.
- Table 2 shows why these three facets. They are the three largest loadings in each column. For DA: Hostility .86, Emotional lability .80, Impulsivity .57, then Callousness .50. For INS: Anxiousness .59, Separation insecurity .54, Perceptual dysregulation .49, then Submissiveness .45.

The commented-out lines 177 to 180 are the "every facet by its largest loading" sets. They match Table 2's bold pattern. Line 179 has a syntax error (`callou_self.namhostil_self.nam`), so that version never ran. The authors replaced it with the three-facet rule and used the three-facet rule for everything they printed.

Is shipping sound? Yes, under IP3 as the package has already read it for the SF. The development paper defines each domain's composition three times in prose. It reports every domain statistic under that composition. It states the scoring rule for domains (unweighted mean of three scales, p. 33). The public code implements exactly that. This is a stronger basis than the SF's domain scoring, which the package takes from the APA full-form key by analogy. The names "Disinhibited Aggression" and "Insecurity" are the paper's (pp. 30, 35, 38), so D-018 is met.

The help page needs four caveats, because the paper carries them:

- The four-factor structure is exploratory, from one sample of 199 male prisoners in sociotherapeutic departments. The authors call for replication (p. 40).
- The authors write that "it remains an open question for future research whether the new factor Insecurity is necessary" (p. 39).
- The informant-report pattern matrix (Table S10) is not on the shelf. The informant domains use the same three facets by the authors' choice (code lines 189 and 190). The package follows that choice.
- The forensic domains share facets with the APA domains. Emotional Lability is in Negative Affectivity and Disinhibited Aggression. Impulsivity is in Disinhibition and Disinhibited Aggression. Separation Insecurity and Anxiousness are in Negative Affectivity and Insecurity. Perceptual Dysregulation is in Psychoticism and Insecurity. The seven domain columns are not independent. A user must not sum them.

The paper writes "Cognitive and Perceptual Dysregulation" where the package writes "Perceptual Dysregulation". That is the package's standing name for the facet. It needs no change.

These are the first package domains whose composition comes from a development paper's factor analysis. Every earlier domain came from an APA table or a publisher's key sheet. Record the reading as a decision, so that the sign-off is explicit rather than implied by the milestone.

## 4. Missing-data rule

Keep the SF rule as the default. State the difference from the authors' code on the help page.

What the code does (lines 392 to 423). A facet is `rowMeans(na.rm = FALSE)`. When exactly one of its four items is missing (line 398, `== 1`), the facet becomes `rowMeans(na.rm = TRUE)`, the unrounded mean of three. When at most 3 of a domain's 12 items are missing, the domain is the mean of the answered items (lines 410 and 419). The domain is computed from items, not from facets. So the code can score a domain for a respondent whose facet is `NA` (two or three missing items in one facet). In that case the domain is not the mean of its three scales. The paper defines a domain as the mean of its three scales (p. 33).

What the code says it intends. The comment at lines 392 and 393 quotes the APA proration rule: "if 25% or less of the items are unanswered for a specific facet, you are asked to prorate the facet score. Prorated Score = (Partial Raw Score x number of items on the PID-5 FBF)/Number of items that were actually answered". It leaves out the APA rule's rounding sentence. It says nothing about domains. The authors' stated rule is the APA rule. Their implementation is the unrounded form of it plus a looser domain rule.

The SF rule is the right default for four reasons:

- The paper prints no rule, so there is no published FFBF rule to apply. The code is an analysis script, not a scoring key. Its own comment points at the APA rule.
- Under missing data, the code's domain rule contradicts the paper's domain definition. The package's rule is the APA rule: a domain with an `NA` facet is `NA`. It is consistent with "unweighted mean of the three scales".
- `missing = "apa"` promises one rule across versions. D-009 set it. D-090 rejected a version-specific rounding rule and an extra `missing` level for the IRF. An FFBF-specific rule reopens what D-090 closed, for a smaller gain.
- The numerical difference is small. Take a 4-item facet with one item missing and partial sum s. The SF rule gives `round_half_up(4s/3)/4`. The code gives s/3. For s equal to 0, 3, 6 or 9, the two agree. For the other values of s, they differ by 1/12 (0.083). For s = 1 the values are 0.25 and 0.333. For s = 2 they are 0.75 and 0.667. The domain values differ by at most the same amount, plus the cases where the package gives `NA` and the code gives a value.

The help page must say five things:

1. The paper prints no missing-data rule. The package applies the APA full-form rule to the FFBF as it does to the SF. One of four items missing prorates. Two or more make the facet `NA`. A domain with an `NA` facet is `NA`.
2. The authors' public analysis code scores differently. A facet with one missing item is the unrounded mean of the answered items. A domain with up to 3 missing items, anywhere among its 12, is the mean of the answered items. So the code can score a domain whose facet the package sets to `NA`.
3. With complete data the two agree exactly. With missing items a facet can differ by up to 0.083. A domain can be `NA` in the package where the code gives a value.
4. The paper's descriptive statistics (Table S5) were computed under the code's rule.
5. No `missing` mode reproduces the code's rule. `missing = "available"` gives the unrounded facet mean, but with no one-item cap, and with domains from facets.

## 5. Informant scoring

Nothing in the sources makes the informant form's keying or scoring differ from the self-report form's, beyond the averaging.

Evidence. The informant facet lists (lines 135 to 159) equal the self-report lists item for item (my comparison, 0 differences). The informant recodes are the same two items (lines 383, 384, 387, 388). The informant four-factor domains are the same three facets (lines 187 to 190). The response scale is the same 0 to 3 (p. 33). Table S3 marks items 8, 41 and 99 as adapted for self-report but not for informants, and item 33 the other way round. That changes text, not keying.

Three facts about the averaging belong on the help page:

- The paper averages the two informants item by item before scoring (p. 33). When one informant's value is missing, the code uses the other's (comment at line 376: "if one is missing value, then _acqu is identical to the other informants response"). A user who follows the paper computes `rowMeans(cbind(x1, x2), na.rm = TRUE)` per item, not a mean that goes `NA`.
- Reverse-keying after averaging equals the code's recode before averaging, since 3 − mean(x1, x2) = mean(3 − x1, 3 − x2). The one-version plan is sound.
- Averaged items take half-integer values. `score_pid5()` accepts them. `item_numbers()` is `as.numeric()`, and `validate_item_columns()` refuses only unparsable text. The BFPM help page already notes that decimals pass. Under `missing = "apa"` the proration step then rounds a half-integer prorated sum to a whole number. Three answered items that sum to 2.5 give 3.33, rounded to 3, so 0.75 rather than 0.833. A complete facet's half-integer sum is not rounded. The APA rounding step was written for whole-number responses. The help page needs three statements here. Averaged informant data are scored like any other data. Under `"apa"` the rounding applies to them. `missing = "available"` avoids it. The paper does not say what the authors did here. Their code did not round at all (see 4).

The two informants in the study were a psychologist and a prison officer (p. 32). A user with one informant scores that informant's responses directly. The help page need not require two.

## 6. Transcription rule

None of the rule's changes is a change of item wording under IP1. No mark the rule removes is part of an item.

Item by item:

- Source notes such as "(G-PID-5 Item 95)" and "(E-PID-5-IRF Item 13, adapted)". The table's note defines them as references to the 220-item forms. They are provenance annotations, not item text. Removing them is right. They are worth keeping somewhere (see Beyond the brief).
- "(-)". A keying mark, defined in the note, carried in `Reverse`. Right.
- E14, E18 and E77. Each sits in the English informant cell of an item whose note names that number. Item 3 has "(E-PID-5-IRF Item 14)", item 84 has "(E-PID-5-IRF Item 18)", and item 16 has "(E-PID-5-IRF Item 77, adapted)". They are typesetting residue of the source notes, not words of the item. Right.
- Leading ellipsis. The informant items begin with "…" as the placeholder for the person stem. The package stores IRF text without its stem (D-089(b)), so this follows the house convention. Not wording. The CSV has none left (checked: 0 in all four columns).
- Final period. Punctuation, and the `pid_items` convention (no final periods). Not wording. Internal punctuation is kept, as it must be: the colon in item 10 and the parentheses in items 1, 64 and 88.
- Typographic quotes and apostrophes to ASCII, and the acute accent in "doesn´t" (item 50 informant). Character normalization to the `pid_items` convention. The accent is a typist's slip for an apostrophe, and normalizing it changes no word. Not wording. The CSV has none left (checked: 0).
- German in-word hyphens. I listed every hyphen followed by a lowercase letter in the `pdftotext -raw` output. The German cases are "he-reinlegen" (48), "Erleb-nisse" (99), "kennen-zulernen" (75), "Be-ziehungen ein- gehen" (38), "zurück-zugeben" and "be- zahlen" (64), and "Ge-fühle" (93). Every one is a syllable break. The words are hereinlegen, Erlebnisse, kennenzulernen, Beziehungen eingehen, zurückzugeben, bezahlen and Gefühle. None is a hyphenated compound. Four of them sit mid-line in the `-raw` output, so they are hyphens embedded in the authors' document rather than pdftotext line breaks. The reading is the same. Removing them restores the words. Keeping them ships broken words. Not a wording change. The table has no legitimate German hyphen followed by a lowercase letter, so the general rule removes nothing that belongs. The English hyphens that survive ("short-tempered", "sweet-talk", "day-to-day", "off-task") are all compounds and are right.
- Typos kept as printed. This is what IP1 asks. It has a consequence for the forms (see Beyond the brief).

One small tension to record rather than fix. The rule corrects one class of printing defect (hyphenation, the accent) and keeps another (misspellings). The line is defensible, because hyphenation and the accent are typesetting, not spelling. The references page needs one sentence that says so.

## Beyond the brief

1. Item 10 is worded differently in the article body and in Table S3. Page 32 quotes it as "To be honest, I'm just more important than other inmates". Table S3 prints "To be honest: I am just more important than other inmates". The table governs, because it is the item table. Record the difference as an open question (OQ) under IP1, so it is visible.
2. Table S3 has English misspellings and grammar slips that the CSV keeps and that M163's English forms will print. They are "treatly" (73, self), "presense" (24, self and informant), "does't" (30, informant), "insist" for "insists" (44, informant) and "believes, that" (23, informant). IP1 forbids a silent fix and asks for a visible OQ. Decide at M163, with Jeff's sign-off, whether to print them as is or corrected. List them either way.
3. The code's item names cross-check Table S3's footnotes. Items without the `_N_` suffix (unadapted) are 18 in the self-report lists and 20 in the informant lists. The paper says "18 items (20 for informant reports) were not adapted at all" (p. 32). Table S3's marks give the same counts: 17 `a` marks plus 33c for self, and 17 `a` marks plus 8b, 41b and 99b for informants. This is a cheap addition to the check script.
4. The check script reads only the self-report lists (its regex is `^stem_self\.nam`) and the self-report recodes (`_N_self`). It does not read the informant lists (135 to 159), the original-form lists (3311 to 3361) or the informant recodes (383, 384, 387, 388). I compared them by hand this once. If the script reads them too, Q1 and Q2 of this brief become machine-checked.
5. Table S3's footnote letters `d`, `e` and `f` mark the FFBF items of the SD-TD, PRD and INC-S validity scales. SD-TD: 11, 46, 56, 84. PRD: 11, 65, 82, 91. INC-S: 28, 32, 34, 57, 78. The transcription drops them, and the ROADMAP lists FFBF validity scales as a candidate. Record the letters on the references page now, so they are not re-extracted later. The source notes (which 220-item PID-5 and PID-5-IRF item each FFBF item comes from) are the same kind of provenance. Keep them the same way.
6. For M163: Table S3 prints no English informant stem. The cells begin with "…" and use "they/them", except item 48 ("his/her"). The paper prints no response labels beyond the endpoints 0 "very false" and 3 "very true" (p. 33). Both need a source before the English informant form can be built.
7. `pid_ffbf_domains` rows 1 to 5 are `identical()` to `pid_domains`, and the keying test asserts it. That includes the spelling "Negative affectivity". Fine. I note it so the mixed capitalization against "Disinhibited Aggression" is understood as inherited.

## Recommendations

- **Apply.** Ship `pid_ffbf_items`, `pid_scales$FFBF` and `pid_ffbf_domains` as built. Keying, reverse items and the seven domain compositions agree with every source the brief names, and with the two code sections it does not check.
- **Apply.** Record a decision for the two forensic domains. Their composition comes from the development paper's prose (pp. 35, 38, 39) and the authors' code, under the paper's own "APA-three scales" domain rule (p. 33). Names follow D-018. Evidence that reopens it: a replication that changes the three facets, or a published FFBF scoring key.
- **Apply.** Keep the SF missing-data rule as the FFBF default. Put the five facts in section 4 on the help page and in the vignette section. AC6 already promises the first two.
- **Apply.** On the help page, state the informant facts in section 5. Informants are averaged item by item. If one informant's value is missing, the other's value is used. Averaged half-integer values score, and the `"apa"` rounding applies to them. FFBF item numbers are not `pid_items$SF` numbers.
- **Apply.** Add the paper's caveats on the forensic domains to the help page in two or three sentences. These are the exploratory single-sample structure, the open status of Insecurity, and the shared facets with the APA domains.
- **Apply.** Open OQ entries in SOURCES.md for the item 10 wording difference and for the kept misspellings, so that IP1's "visible, never silently patched" holds. Decide the forms' treatment at M163.
- **Consider.** Extend `check_pid_ffbf_text.R` to the informant lists, the original-form lists and the informant recodes (item 4 above). Add the `_N_` count against the footnote marks (item 3 above).
- **Consider.** Record Table S3's `d`, `e` and `f` letters and the source notes on the references page (item 5 above).
- **Consider.** Add one sentence to the references page that says why hyphenation and the accent are corrected while misspellings are kept.
- **Reject with reason.** An FFBF-specific `missing` mode or default that reproduces the authors' code. D-090 rejected version-specific rules. The paper prints no rule. The code's domain rule contradicts the paper's domain definition. The largest facet difference is 0.083.
- **Reject with reason.** Scoring the four-factor Antagonism and Detachment as separate columns. They are the APA domains facet for facet (p. 35, and code lines 182, 183, 187, 188). The plan-gate choice already excludes them.
