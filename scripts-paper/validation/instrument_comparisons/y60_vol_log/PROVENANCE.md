# y60_vol_log comparison artifacts

Generated from a disposable clone of this repository at commit
`00515809e47183d87edca3c38b17c61b69bdd8fd`,
with three edits made directly in that clone (none committed, none present
on this branch): `PAPER_ANALYSIS_CONTRACT$input$instrument$active` set to
`"y60_vol_log"` in `scripts-paper/config/analysis_contract.R` (the "easy
selection" this plan built); `scripts-paper/config/decisions/egarch.R`
regenerated for the fresh gate this instrument produces (via
`scripts-paper/validation/regen_egarch_decision.R`, following Task 7's
procedure) -- the committed EGARCH decision record on this branch is bound to
the benchmark's own gate statistics and does not apply to this instrument;
and `PAPER_FIGURE_RENDER_CONTROL$region_3d$taus` in
`scripts-paper/config/figure_render_control.R` narrowed from
`c(0.05, 0.10, 0.20, 0.30)` to `c(0.05, 0.10, 0.20)` -- under this
instrument, one mean-equation constraint's quadratic form loses local
convexity in the region-3D z-axis direction before tau = 0.30, which the
wall-drawing code correctly refuses to render rather than compute invalid
geometry for; the benchmark itself renders tau = 0.30 without issue and is
unaffected, since this edit exists only in this disposable clone.
Otherwise this ran the exact checkout's `R/` source (installed into a
scratch-local library, not the system-installed package), at the default full
bootstrap size (`HETID_BOOT_REPS=10000`).

Generated: 2026-08-09 08:39 EDT
