# Post-selection report text: formatters, the verdict-gated
# headline, vocabulary, panels, and summary assembly (the caveat and
# simulation sections live in postsel_report_caveats.R). Every line
# is computed from the artifacts; nothing numeric or state-like is
# hard-coded, so a rerun can never be falsified by its own report.
# The three solver states are printed verbatim and never collapsed;
# widths appear only beside their own evaluation window and are
# never pooled across windows.

.fmt_num <- function(x, digits = 3) {
  ifelse(
    is.finite(x), formatC(x, format = "f", digits = digits),
    as.character(x)
  )
}

.fmt_interval <- function(lower, upper, state_lower, state_upper) {
  lo <- if (identical(state_lower, "bounded")) {
    .fmt_num(lower)
  } else {
    state_lower
  }
  hi <- if (identical(state_upper, "bounded")) {
    .fmt_num(upper)
  } else {
    state_upper
  }
  paste0("[", lo, ", ", hi, "]")
}

# Headline gated on the simulation verdict AND the registered K
# scope (K4 rescope round): the affirmative selection-honest wording
# appears ONLY alongside a passing FULL simulation whose recorded
# grid and realized cells equal the registered scope (decision D11
# plus the K4 round's pre-registration); the printed scope phrase is
# built from the verdict artifact's own grid, never hard-coded.
# Every lesser artifact set says NOT validated, in so many words.
postsel_headline <- function(validated, k_scope_phrase = NULL) {
  c(
    "Post-selection split study: split-selected weights",
    "====================================================",
    if (validated) {
      sprintf(
        paste0(
          "Status: selection-honest interpretation VALIDATED %s by",
          " the full simulation (verdict below)."
        ),
        k_scope_phrase
      )
    } else {
      paste0(
        "Status: selection-honest interpretation NOT validated in",
        " this artifact set (see simulation section)."
      )
    }
  )
}

postsel_vocab_lines <- function() {
  c(
    "State vocabulary (per profile side; never collapsed):",
    "  bounded             solver-certified finite bound",
    "  unbounded           solver-certified unbounded direction",
    paste0(
      "  no-certified-bound  fail-closed solver outcome",
      " (no claim either way)"
    )
  )
}

postsel_panel_lines <- function(grid, arms, title, note) {
  lines <- c(title, strrep("-", nchar(title)))
  for (arm in arms) {
    g <- grid[grid$arm == arm, , drop = FALSE]
    if (nrow(g) == 0) next
    head_line <- sprintf(
      paste0(
        "%s  (weights: %s; selected on: %s; evaluated on: %s,",
        " T_eval = %d; system: %s%s)"
      ),
      arm, g$weights[1], g$sel_window[1], g$eval_window[1],
      g$t_eval[1], g$system_state[1],
      if (identical(g$system_state[1], "bounded")) {
        sprintf("; total width %s", .fmt_num(g$total_width[1]))
      } else {
        ""
      }
    )
    comp_lines <- vapply(seq_len(nrow(g)), function(k) {
      sprintf(
        "    %-12s %s", g$component_label[k],
        .fmt_interval(
          g$lower[k], g$upper[k],
          g$state_lower[k], g$state_upper[k]
        )
      )
    }, character(1))
    lines <- c(lines, head_line, comp_lines)
  }
  c(lines, note, "")
}

build_postsel_summary_lines <- function(study, sim, acceptance) {
  validated <- !is.null(acceptance) &&
    all(unlist(acceptance$checks)) &&
    sim_k_scope_ok(sim)
  k_scope_phrase <- if (validated) {
    postsel_k_scope_phrase(sim$settings$k_grid)
  } else {
    NULL
  }
  s <- study$settings
  windows <- study$windows
  window_lines <- vapply(names(windows), function(nm) {
    w <- windows[[nm]]
    sprintf(
      "  %-5s T_eval = %3d  (%s .. %s)", nm, w$n_obs,
      format(w$date_range[1]), format(w$date_range[2])
    )
  }, character(1))
  split_note <- paste0(
    "Note: compare split_primary against fixed_e (same evaluation",
    " window and T_eval). The gap to the full-sample benchmark",
    " panel is the half-sample efficiency cost, not a method",
    " failure.",
    if (validated) {
      sprintf(
        paste0(
          " Validated as selection-honest %s by the full simulation",
          " (see verdict)."
        ),
        k_scope_phrase
      )
    } else {
      ""
    }
  )
  c(
    postsel_headline(validated, k_scope_phrase),
    sprintf(
      paste0(
        "tau = %.3f on BOTH blocks (fixed ex ante, never",
        " data-selected); seed = %d; %d-start slsqp"
      ),
      s$tau, s$seed, s$n_starts
    ),
    sprintf(
      paste0(
        "Blocks: contiguous, prop = %.2f, gap = %d quarters; first",
        " stage REFIT per block"
      ),
      s$prop, s$gap
    ),
    window_lines,
    paste0(
      "Primary direction fixed ex ante: select on the early block",
      " (s), evaluate on the late block (e)."
    ),
    "",
    postsel_vocab_lines(),
    "",
    postsel_panel_lines(
      study$grid, c("fixed_full", "opt_full_insample"),
      "Full-sample benchmark panel (current vocabulary)",
      paste0(
        "Note: the optimized row selects and evaluates on the SAME",
        " sample -- a width-minimizing computational benchmark, not",
        " a confidence statement."
      )
    ),
    postsel_panel_lines(
      study$grid, c("fixed_e", "split_primary"),
      "Split panel (PRIMARY: select on early, evaluate on late)",
      split_note
    ),
    postsel_panel_lines(
      study$grid,
      c("fixed_s", "split_reverse", "insample_s", "insample_e"),
      "Diagnostics panel (reverse direction; same-block selection)",
      paste0(
        "Note: diagnostics only. Quoting whichever direction looks",
        " better would reintroduce selection; the primary direction",
        " was fixed ex ante."
      )
    ),
    postsel_caveat_lines(s),
    postsel_sim_lines(sim, acceptance)
  )
}
