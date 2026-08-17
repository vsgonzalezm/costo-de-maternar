# install.packages("skpr")  # if needed
library(skpr)

# 1) Candidate set: 2 x 7 x 2 x 4 = 112
cand <- gen.factorial(
  levels   = c(2, 7, 2, 4),
  varNames = c("F1","F2","F3","F4")
)

# Make sure these are factors (critical for multi-level factors)
cand[] <- lapply(cand, factor)

# 2) Model: all mains + interactions among (1,2,4) only
frm <- ~ F1 + F2 + F3 + F4 + F1:F2 + F1:F4 + F2:F4

# 3) Generate a 60-run design
set.seed(123)
des60 <- gen_design(
  candidateset = cand,
  model        = frm,
  trials       = 60,
  optimality   = "D"   # explicit; D-optimal
)

# 4) Check identifiability (rank) and optimality metrics
Xdes <- model.matrix(frm, des60)
c(p = ncol(Xdes), rank = qr(Xdes)$rank)

get_optimality(des60)  # returns D/A/G type optimality summaries

# 5) Quick balance checks
lapply(des60, table)

with(des60, table(F2, F4))