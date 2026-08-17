install.packages("AlgDesign")

library(AlgDesign)

# Candidate set: 2 × 7 × 2 × 4 = 112 profiles
cand <- gen.factorial(
  levels   = c(2, 7, 2, 4),
  varNames = c("F1","F2","F3","F4")
)ñ

cand[] <- lapply(cand, factor)

# Model: all main effects + selected 2-way interactions
frm <- ~ F1 + F2 + F3 + F4 + F1:F2 + F1:F4 + F2:F4

X <- model.matrix(frm, cand)
ncol(X)

# Generate a D-optimal design with 45 runs
set.seed(123)

opt45 <- optFederov(
  frml      = frm,
  data      = cand,
  nTrials   = 60,
  criterion = "D",
  nRepeats  = 1000
)

des45 <- opt45$design
opt45$D

ed45 <- eval.design(frm, design = des45, X = cand)

ed45$rank          # should be 39
ed45$determinant   # determinant of X'X
ed45$Ge            # G-efficiency–related quantity
ed45$Dea           # lower bound on approximate-theory D-efficiency

lapply(des45, table)