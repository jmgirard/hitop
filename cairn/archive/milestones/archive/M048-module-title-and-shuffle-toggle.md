# M048: A module-titled Word form and a shuffle toggle in the browser builder

**Status:** done (2026-08-23, PR #54 https://github.com/jmgirard/hitop/pull/54)

**Goal:** A HiTOP-SR module Word form says on its face that it is a module, and the
browser builder lets the person building it shuffle the item order.

**Outcome:** `generate_docx_hitopsr(title = )` takes a `NULL` sentinel resolving to
`"HiTOP-SR Module (v1.0)"` when `module` is supplied and `"HiTOP-SR (v1.0)"` otherwise,
guarded by `validate_string(allow_null = TRUE)`. It resolves after `resolve_module_arg()`,
so the deprecated `subset =` earns the module header too, and only `NULL` is replaced,
so an explicit `title` prints verbatim even when it equals the other default. New helper
`docx_header_title()` reads the header out of `word/header*.xml`, which `read_docx_xml()`
cannot see; `test-docx-title.R` pins both defaults and compares a default rebuild against
the committed `inst/extdata/hitopsr_{US,A4}.docx`. In `jmgirard/hitop-builder` (commit
`5e78f98`, pushed at merge) a *Word item order* checkbox passes `randomize` into the DOCX
call alone, unticked on load, its warning shown only while ticked.

**Decisions:** none milestone-local; the IP1 sign-off is D-037, from plan time.

**Review:** three lenses run inline, not in fresh-context subagents (session configured
not to spawn agents). All seven criteria verified fresh, including driving the locally
served builder page through its own controls. Four findings, none an acceptance-criterion
failure: the unpushed builder page against a changelog claiming it (actioned — pushed at
merge); the modules article silent on the new header (candidate row); two rejected.
