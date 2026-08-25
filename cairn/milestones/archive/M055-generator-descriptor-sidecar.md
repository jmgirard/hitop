# M055: The HiTOP-SR generators write a descriptor beside the file they build

**Status:** done (2026-08-24, PR #61 https://github.com/jmgirard/hitop/pull/61)

**Goal:** Let one generator call produce both the file a researcher fields and the descriptor that scores the data it comes back as, including the printed order of a shuffled Word form.

**Outcome:** `generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()`, and `generate_redcap_hitopsr()` take a `descriptor` path, written through the internal `write_descriptor_sidecar()` before the instrument file, so an unwritable path aborts before any form exists; a failed build removes the sidecar via `file.remove()` on the literal path. `module = NULL` resolves to a module over every scale. `write_module_impl(module, file, call)` carries the writer's body so a refusal blames the exported generator. `write_module()` writes a module's `item_order` attribute as `itemOrder`; the sidecar sets the attribute unconditionally, so a NULL order clears one an incoming module carried. `validate_descriptor_target()` refuses a `descriptor` naming the same path as `file`, and both writers refuse an empty path. DESIGN known issue 8 narrowed to the app remainder M056 carries.

**Decisions:** M055-D1 promoted to D-040 (`write_module()` writes `itemOrder`, annotating D-039's reserved-slot clause). M055-D2: a failed build removes the descriptor path the call was given, a pre-existing file at it included.

**Review:** Two passes. The first returned at the return floor on AC4 — a stale `item_order` attribute leaked into descriptors for forms that never shuffled — repaired with five sibling findings. The second re-verified all seven criteria fresh, three-lens fan-out; blame-history and prior-review lenses clean, diff-bug lens returned eight. Two silent no-descriptor holes fixed at the gate with red-proven tests, one promoted to D-040, two rejected, two routed to candidate rows. Defect returns: 1.
