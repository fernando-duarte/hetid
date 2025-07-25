% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_identified_set_quadratic.R
\name{compute_identified_set_quadratic}
\alias{compute_identified_set_quadratic}
\title{Compute Quadratic Form Components for Identified Set}
\usage{
compute_identified_set_quadratic(
  gamma,
  tau,
  L_i,
  V_i,
  Q_i,
  s_i_0,
  s_i_1,
  s_i_2,
  sigma_i_sq,
  maturities = NULL
)
}
\arguments{
\item{gamma}{Matrix (J x I) where each column gamma_i contains the
coefficients for maturity i}

\item{tau}{Vector of positive real numbers (length I) containing tau_i values}

\item{L_i}{Named vector of L_i values from compute_identified_set_components()}

\item{V_i}{Named vector of V_i values from compute_identified_set_components()}

\item{Q_i}{List of vectors Q_i from compute_identified_set_components()}

\item{s_i_0}{Named vector of S_i^(0) values from compute_scalar_statistics()}

\item{s_i_1}{List of vectors S_i^(1) from compute_matrix_statistics()}

\item{s_i_2}{List of matrices S_i^(2) from compute_matrix_statistics()}

\item{sigma_i_sq}{Named vector of sigma_i^2 values from
compute_scalar_statistics()}

\item{maturities}{Vector of maturity indices to compute components for.
Default is all available.}
}
\value{
A list containing:
\describe{
\item{d_i}{Named vector of d_i values for each maturity}
\item{A_i}{List of symmetric matrices A_i for each maturity}
\item{b_i}{List of vectors b_i for each maturity}
\item{c_i}{Named vector of c_i values for each maturity}
}
}
\description{
Computes the quadratic form components d_i, A_i, b_i, and c_i for the
identified set calculation for each maturity i.
}
\details{
For each maturity i, computes:
\deqn{d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
  \frac{\tau_i^2 V_i(\boldsymbol{\Gamma})}{\hat{\sigma}_i^2}}
\deqn{\mathbf{A}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
  \mathbf{Q}_i(\boldsymbol{\Gamma}) \mathbf{Q}_i(\boldsymbol{\Gamma})^{\top} -
  d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{\mathbf{S}}_i^{(2)}}
\deqn{\mathbf{b}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
  -2 L_i(\boldsymbol{\Gamma}) \mathbf{Q}_i(\boldsymbol{\Gamma}) +
  2 d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{\mathbf{S}}_i^{(1)}}
\deqn{c_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
  L_i(\boldsymbol{\Gamma})^2 -
  d_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \hat{S}_i^{(0)}}

where \eqn{\mathbf{A}_i} is a symmetric matrix by construction,
\eqn{\mathbf{b}_i} is a vector, and \eqn{c_i} is a scalar.
The identified set for \eqn{\boldsymbol{\theta} \in \mathbb{R}^I} is:
\deqn{\Theta(\boldsymbol{\Gamma}, \boldsymbol{\tau}) =
  \left\{\boldsymbol{\theta} \in \mathbb{R}^I :
  \boldsymbol{\theta}^{\top} \mathbf{A}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau})
  \boldsymbol{\theta} +
  \mathbf{b}_i(\boldsymbol{\Gamma}, \boldsymbol{\tau})^{\top} \boldsymbol{\theta} +
  c_i(\boldsymbol{\Gamma}, \boldsymbol{\tau}) \leq 0, \; i = 1, \ldots, I\right\}}
}
\examples{
\dontrun{
# Assume we have all required statistics computed
vec_stats <- compute_vector_statistics(w1, w2, pcs)
scalar_stats <- compute_scalar_statistics(w1, w2)
matrix_stats <- compute_matrix_statistics(w1, w2)

# Create gamma matrix and tau vector
J <- 4 # number of PCs
I <- 8 # number of maturities
gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
tau <- rep(1, I) # positive values

# Compute basic components
components <- compute_identified_set_components(
  gamma = gamma,
  r_i_0 = vec_stats$r_i_0,
  r_i_1 = vec_stats$r_i_1,
  p_i_0 = vec_stats$p_i_0
)

# Compute quadratic form components
quad_components <- compute_identified_set_quadratic(
  gamma = gamma,
  tau = tau,
  L_i = components$L_i,
  V_i = components$V_i,
  Q_i = components$Q_i,
  s_i_0 = scalar_stats$s_i_0,
  s_i_1 = matrix_stats$s_i_1,
  s_i_2 = matrix_stats$s_i_2,
  sigma_i_sq = scalar_stats$sigma_i_sq
)
}
}
