# Z-source hook payload: squared principal components as the instrument
# vector. The squares lie outside span(1, PC), so the stage-02 w2-on-Z
# refit carries genuine fitted values (Regime B) and the diagnostics
# suite includes Anscombe. The first stage is unchanged (X = (1, PC)).
#
# Use:
#   HETID_Z_SOURCE=scripts/z_sources/pc_squared.R Rscript <stage script>

build_z <- function(data) {
  pc_cols <- paste0("pc", seq_len(4))
  z <- as.matrix(data[, pc_cols])^2
  colnames(z) <- paste0(pc_cols, "_sq")
  z
}
