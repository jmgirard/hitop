# M049: An original-numbering toggle for the browser module builder

**Status:** done (2026-08-24, PR #55 https://github.com/jmgirard/hitop/pull/55)

**Goal:** The browser module builder lets the person building a Word module form choose which numbers it prints — the default `1..n`, or the HiTOP-SR's own item numbers, which are the field names the same page's REDCap and Qualtrics exports give the collected variables.

**Outcome:** In `jmgirard/hitop-builder` (`aabfdf7`, `36ad567`, `0278967`, pushed at merge): a
*Word item numbering* fieldset interpolates `renumber` into the DOCX call alone;
`wholeInstrument()` drops `module` from all three calls when every scale is ticked, gated on a
load-time R check (`tilesExactly`) that the instrument's scales tile its items `1..N`, so that
Word form is headed `HiTOP-SR (v1.0)` and its downloads are named `hitopsr.*`;
`crosswalkSentence()` writes the shuffle notice per state, replacing an unconditional crosswalk
claim false in three of four. The README gains *Numbering the Word form*, *Ticking every scale*,
and a four-state crosswalk table. Here: a `hitop_module()` help sentence on the module framing an
all-scales module still receives, NEWS entries, a `hitop-builder` entry in `.claude/launch.json`.

**Decisions:** M049-D1 (the control ships, on RR03's reading of its purpose), M049-D2 (original-plus-shuffle stays available, unblocked and unwarned), M049-D3 (the all-scales reconciliation lives in the page, not the package), M049-D4 (RR03's eight recommendations triaged), M049-D5 (the Goal returns to planning, and the four inputs the re-cut inherits). Cross-cutting: D-038. Escalated to RB03/RR03 mid-implementation, then re-planned.

**Review:** Two passes; the first returned on AC7. Three lenses run inline, not in fresh-context
subagents (session configured not to spawn agents). Three findings, none a criterion failure: the
shuffled all-scales form's printed order recoverable from nothing the page hands over (accepted —
DESIGN known issue 8 plus a candidate row); NEWS describing unpushed builder commits (pushed at
merge); the modules article silent on `renumber = FALSE` under `randomize` (pre-existing,
absorbed into its standing row). Nothing retired.
