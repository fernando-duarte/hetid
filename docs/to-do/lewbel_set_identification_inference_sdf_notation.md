---
title: "Inference for Set-Identified Lewbel Bounds in the SDF-News Notation"
subtitle: "Rewritten using the notation of lewbel_multivariate_set_identification.tex"
date: "16 June 2026"
geometry: margin=0.8in
fontsize: 10pt
mainfont: DejaVu Serif
monofont: DejaVu Sans Mono
toc: true
toc-depth: 2
header-includes:
  - |
    \usepackage{booktabs}
    \usepackage{amsmath,amssymb,mathtools}
    \newcommand{\E}{\mathbb{E}}
    \newcommand{\R}{\mathbb{R}}
    \newcommand{\cF}{\mathcal{F}}
    \newcommand{\cP}{\mathcal{P}}
    \newcommand{\cZ}{\mathcal{Z}}
    \newcommand{\T}{\top}
    \newcommand{\PC}{\mathrm{PC}}
    \newcommand{\Cov}{\operatorname{Cov}}
    \newcommand{\Var}{\operatorname{Var}}
    \newcommand{\Corr}{\operatorname{Corr}}
    \newcommand{\Covhat}{\widehat{\operatorname{Cov}}}
    \newcommand{\Varhat}{\widehat{\operatorname{Var}}}
    \newcommand{\rank}{\operatorname{rank}}
    \newcommand{\col}{\operatorname{col}}
    \newcommand{\se}{\operatorname{se}}
---

# Executive summary

This is the same substantive report as before, but rewritten in the notation of the attached TeX file. The key changes are notational and organizational:

1. The structural coefficient on the endogenous news vector is now \(\theta\in\R^I\). A generic candidate value is \(w\in\R^I\). In the one-endogenous-regressor case, set \(I=1\), so \(\theta\) and \(w\) are scalars.
2. The common conditioning vector is \(X_t\), the outcome is \(Y_{1,t+1}\), and the endogenous news regressors are \(Y_{2,t+1}\).
3. The residuals are \(W_{1,t+1}\) and \(W_{2,t+1}\), formed after projecting both equations on the same \(X_t\).
4. The instrument list is \(\cZ=\{Z_{1,t+1},\dots,Z_{J_Z,t+1}\}\), with imposed component-instrument pairs \((i,k)\in\cP\).
5. The set-identified object is \(\Theta(\cZ,\tau)\). In the scalar case \(I=1\), this becomes the interval \([\underline\theta,\overline\theta]\). I avoid using \(L\) and \(U\) for the endpoints because the TeX notation already uses \(L_{ik}\) for a covariance moment.
6. For every candidate \(w\), the other coefficients are recovered by the affine map
   \[
   \beta_1(w)=\beta_1^R-(\beta_2^R)^\T w.
   \]
   Thus, in the scalar case, once \([\underline\theta,\overline\theta]\) is known, the set for every component of \(\beta_1\) is the image of this interval under that affine map.

The main inference conclusion is unchanged. Lewbel (2012) gives the population set and a plug-in way to estimate the set. The attached Baum-Lewbel advice paper and the standard Lewbel software cover the point-identified estimator, not confidence intervals for the set-identified Theorem 3 bounds. For the scalar triangular case, use a full bootstrap for endpoint uncertainty and then report either an Imbens-Manski/Stoye-style confidence interval for the true \(\theta\) or a confidence set obtained by moment-inequality inversion.

# 1. Notation used in this report

The attached TeX file defines the common conditioning vector as

\[
X_t:=
\begin{cases}
(1,\ \PC_t^\T)^\T, & H=0,\\[0.25em]
(1,\ \PC_t^\T,\ Y_{1,t},\dots,Y_{1,t+1-H})^\T, & H\ge1.
\end{cases}
\]

The triangular system is

\[
Y_{1,t+1}=X_t^\T\beta_1+\theta^\T Y_{2,t+1}+\varepsilon_{1,t+1},
\qquad \theta\in\R^I,
\]

\[
Y_{2,t+1}=\beta_2^R X_t+\varepsilon_{2,t+1},
\qquad \beta_2^R\in\R^{I\times \dim(X)}.
\]

For the original one-endogenous-regressor question, take \(I=1\). Then \(Y_{2,t+1}\), \(\theta\), \(w\), and \(\varepsilon_{2,t+1}\) are scalars. The notation still uses the same objects because the scalar case is just a special case of the attached TeX formulation.

The residualized variables are

\[
\beta_1^R:=\bigl(\E[X_tX_t^\T]\bigr)^{-1}\E[X_tY_{1,t+1}],
\qquad
\beta_2^R:=\E[Y_{2,t+1}X_t^\T]\bigl(\E[X_tX_t^\T]\bigr)^{-1},
\]

\[
W_{1,t+1}:=Y_{1,t+1}-X_t^\T\beta_1^R,
\qquad
W_{2,t+1}:=Y_{2,t+1}-\beta_2^R X_t.
\]

Under \(\E[X_t\varepsilon_{1,t+1}]=0\) and \(\E[X_t\varepsilon_{2,t+1}^\T]=0\), the residualized model is

\[
W_{1,t+1}=\theta^\T W_{2,t+1}+\varepsilon_{1,t+1},
\qquad
W_{2,t+1}=\varepsilon_{2,t+1}.
\]

This residualized system is the object used for both point identification and set identification.

# 2. Exact Lewbel point identification in the attached notation

Let \(Z_{k,t+1}\) be one scalar instrument from \(\cZ\). For each imposed pair \((i,k)\in\cP\), define

\[
L_{ik}:=\Cov\bigl(Z_{k,t+1},\ W_{1,t+1}W_{2,i,t+1}\bigr),
\]

\[
Q_{ik}:=\Cov\bigl(Z_{k,t+1},\ W_{2,t+1}W_{2,i,t+1}\bigr)\in\R^I,
\]

\[
P_{ik}:=\Cov\bigl(Z_{k,t+1},\ W_{2,i,t+1}^2\bigr).
\]

The exact Lewbel condition is the zero-contamination case

\[
\Cov\bigl(Z_{k,t+1},\varepsilon_{1,t+1}\varepsilon_{2,i,t+1}\bigr)=0.
\]

Since the candidate first-equation error is

\[
\varepsilon_{1,t+1}(w):=W_{1,t+1}-w^\T W_{2,t+1},
\]

the exact condition implies

\[
Q_{ik}^\T w=L_{ik}.
\]

Stacking the rows \(Q_{ik}^\T\) into \(\mathcal Q\) and the scalars \(L_{ik}\) into \(\mathcal L\) gives

\[
\mathcal Q w=\mathcal L.
\]

If \(\rank(\mathcal Q)=I\), the exact Lewbel system point-identifies

\[
\theta=(\mathcal Q^\T\mathcal Q)^{-1}\mathcal Q^\T\mathcal L.
\]

In the single-news-component case \(I=1\), a single relevant exact instrument gives

\[
\theta=\frac{L_{1k}}{Q_{1k}},
\qquad Q_{1k}=P_{1k}=\Cov(Z_{k,t+1},W_{2,t+1}^2)\ne0.
\]

The standard Lewbel software implements this point-identified logic by estimating generated instruments and then running IV/GMM. It does not implement the set-identified bounds below.

# 3. Set identification with bounded relative correlations

The attached TeX file relaxes exact validity by imposing, for every \((i,k)\in\cP\),

\[
\left|\Corr\bigl(Z_{k,t+1},\ \varepsilon_{1,t+1}\varepsilon_{2,i,t+1}\bigr)\right|
\le
\tau_{ik}
\left|\Corr\bigl(Z_{k,t+1},\ \varepsilon_{2,i,t+1}^{2}\bigr)\right|,
\qquad 0\le \tau_{ik}<1.
\]

The scalar \(\tau_{ik}\) is a sensitivity parameter. It is not identified from the data without extra assumptions. The special case \(\tau_{ik}=0\) is exact Lewbel validity.

For each news component \(i\), define the product-variance moments

\[
S_i^{(0)}:=\Var\bigl(W_{1,t+1}W_{2,i,t+1}\bigr),
\]

\[
S_i^{(1)}:=\Cov\bigl(W_{2,t+1}W_{2,i,t+1},\ W_{1,t+1}W_{2,i,t+1}\bigr)\in\R^I,
\]

\[
S_i^{(2)}:=\Var\bigl(W_{2,t+1}W_{2,i,t+1}\bigr)\in\R^{I\times I},
\qquad
\sigma_i^2:=\Var\bigl(W_{2,i,t+1}^2\bigr)>0.
\]

For any candidate \(w\),

\[
\Cov\bigl(Z_{k,t+1},\varepsilon_{1,t+1}(w)W_{2,i,t+1}\bigr)
=L_{ik}-Q_{ik}^\T w,
\]

\[
\Var\bigl(\varepsilon_{1,t+1}(w)W_{2,i,t+1}\bigr)
=S_i^{(0)}-2S_i^{(1)\T}w+w^\T S_i^{(2)}w.
\]

After squaring the relative-correlation bound and cancelling \(\Var(Z_{k,t+1})\), each pair \((i,k)\) supplies the quadratic inequality

\[
g_{ik}(w):=(L_{ik}-Q_{ik}^\T w)^2
-d_{ik}\bigl(S_i^{(0)}-2S_i^{(1)\T}w+w^\T S_i^{(2)}w\bigr)\le0,
\]

where

\[
d_{ik}:=\tau_{ik}^2\frac{P_{ik}^2}{\sigma_i^2}.
\]

Equivalently,

\[
g_{ik}(w)=w^\T A_{ik}w+b_{ik}^\T w+c_{ik}\le0,
\]

\[
A_{ik}=Q_{ik}Q_{ik}^\T-d_{ik}S_i^{(2)},
\qquad
b_{ik}=-2L_{ik}Q_{ik}+2d_{ik}S_i^{(1)},
\qquad
c_{ik}=L_{ik}^2-d_{ik}S_i^{(0)}.
\]

The identified set for the structural news coefficients is

\[
\Theta(\cZ,\tau):=\bigl\{w\in\R^I:g_{ik}(w)\le0\text{ for every }(i,k)\in\cP\bigr\}.
\]

Under the maintained assumptions in the attached TeX file, \(\theta\in\Theta(\cZ,\tau)\). The set can be empty if the model and the asserted slack values are mutually inconsistent, and it can be unbounded if the imposed instruments do not restrict some directions in \(\R^I\).

## 3.1 Scalar specialization: the original one-endogenous-regressor case

For \(I=1\), suppress \(i=1\) and write \(w\in\R\). For instrument \(k\), the pair-specific inequality is

\[
g_k(w)=A_k w^2+b_k w+c_k\le0,
\]

where

\[
A_k=Q_k^2-d_k S^{(2)},
\qquad
b_k=-2L_kQ_k+2d_kS^{(1)},
\qquad
c_k=L_k^2-d_kS^{(0)},
\qquad
d_k=\tau_k^2\frac{P_k^2}{\sigma^2}.
\]

In the scalar case,

\[
Q_k=P_k=\Cov(Z_{k,t+1},W_{2,t+1}^2),
\qquad
S^{(2)}=\sigma^2=\Var(W_{2,t+1}^2),
\]

so, with \(0\le\tau_k<1\) and \(P_k\ne0\), \(A_k=(1-\tau_k^2)P_k^2>0\). Therefore the single-pair set is a closed interval bounded by the roots of \(g_k(w)=0\):

\[
\underline\theta_k=\frac{-b_k-\sqrt{b_k^2-4A_kc_k}}{2A_k},
\qquad
\overline\theta_k=\frac{-b_k+\sqrt{b_k^2-4A_kc_k}}{2A_k}.
\]

With multiple scalar instruments, the plug-in scalar identified set is the intersection of the pairwise intervals:

\[
\Theta(\cZ,\tau)=
\bigcap_{k:(1,k)\in\cP}[\underline\theta_k,\overline\theta_k].
\]

When the intersection is nonempty,

\[
\underline\theta=\max_k \underline\theta_k,
\qquad
\overline\theta=\min_k \overline\theta_k.
\]

When the intersection is empty, the maintained model and the chosen slack profile \(\tau\) are refuted by the imposed restrictions.

# 4. What the attached papers provide and what must be added

The papers cover the estimation problem only up to the plug-in set. They do not give a complete inference procedure for the set-identified Theorem 3 case.

| Object | Status in Lewbel (2012) and Baum-Lewbel (2019) | What this report adds |
|---|---|---|
| Point-identified Lewbel estimator | Covered by 2SLS/GMM generated-instrument logic | No special partial-identification issue |
| Point-identified standard errors | Covered by standard IV/GMM asymptotics, with generated-instrument details | No change |
| Set \(\Theta(\cZ,\tau)\) | Population characterization supplied by Lewbel's Theorem 3 and generalized in the attached TeX notation | Restated in \(L_{ik},Q_{ik},P_{ik},S_i^{(j)}\) notation |
| Plug-in set estimator | Suggested by replacing population moments by sample moments | Detailed algorithm below |
| Endpoint standard errors for \(\hat{\underline\theta},\hat{\overline\theta}\) | Not supplied | Bootstrap or delta-method recommendations |
| Confidence set for the population identified set | Not supplied | Conservative outer set or joint endpoint bootstrap |
| Confidence interval for true scalar \(\theta\) | Not supplied | Imbens-Manski/Stoye-style interval or moment-inequality inversion |
| Inference for \(\beta_1(w)\) | Not supplied | Affine mapping and bootstrap/projection recommendations |
| Software for Theorem 3 set inference | No dedicated package located | Custom code plus general moment-inequality tools |

# 5. Sample construction of the plug-in set

Use the common trimmed sample implied by the lag order \(H\). With the \(1/T\) normalization,

\[
\Covhat(a,b):=T^{-1}\sum_t a_tb_t^\T-\bar a\,\bar b^\T,
\qquad
\Varhat(a):=\Covhat(a,a),
\]

the plug-in algorithm is:

1. Regress \(Y_{1,t+1}\) on the full common vector \(X_t\). Save \(\widehat\beta_1^R\) and residuals \(\widehat W_{1,t+1}\).
2. Regress every component of \(Y_{2,t+1}\) on the same \(X_t\). Save \(\widehat\beta_2^R\) and residuals \(\widehat W_{2,t+1}\).
3. Construct the pre-specified instruments \(Z_{k,t+1}\) or the date-\(t\) specialization \(Z_{k,t}\), and specify \(\cP\).
4. Compute sample analogues of \(L_{ik}\), \(Q_{ik}\), \(P_{ik}\), \(S_i^{(0)}\), \(S_i^{(1)}\), \(S_i^{(2)}\), and \(\sigma_i^2\).
5. Build \(\widehat g_{ik}(w)\) and solve the quadratically constrained problems defining \(\widehat\Theta(\cZ,\tau)\).

For \(I=1\), step 5 reduces to solving quadratic roots for each scalar instrument and intersecting the resulting intervals.

# 6. The three different inference targets

The most important practical point is that there are three different objects. They should not be reported as if they were the same.

## 6.1 Endpoint uncertainty

In the scalar case, the plug-in endpoints \(\hat{\underline\theta}\) and \(\hat{\overline\theta}\) are sample statistics. Their standard errors describe sampling variation in the estimated lower and upper bounds. They do not by themselves give a confidence interval for the true \(\theta\).

A practical full bootstrap is:

1. Resample observations. For time-series data, use a block bootstrap; for clustered data, resample clusters.
2. In each resample, recompute \(\widehat\beta_1^R\), \(\widehat\beta_2^R\), \(\widehat W_{1,t+1}\), and \(\widehat W_{2,t+1}\).
3. Recompute all sample analogues \(\widehat L_{ik}\), \(\widehat Q_{ik}\), \(\widehat P_{ik}\), \(\widehat S_i^{(j)}\), and \(\widehat\sigma_i^2\).
4. Recompute \(\widehat\Theta^*(\cZ,\tau)\). In the scalar case, store \(\hat{\underline\theta}^*\) and \(\hat{\overline\theta}^*\).
5. Use the empirical standard deviations of the bootstrap endpoint draws as \(\widehat{\se}(\hat{\underline\theta})\) and \(\widehat{\se}(\hat{\overline\theta})\).

The bootstrap should include failures as diagnostics. Frequent failures can signal weak heteroskedastic relevance \(\widehat P_{ik}\approx0\), a near-zero discriminant, nonempty-set fragility, or a slack value that is too small for the maintained restrictions.

## 6.2 Confidence set for the identified interval

A confidence set for the population identified interval \([\underline\theta,\overline\theta]\) is meant to contain the entire interval. A simple conservative construction is

\[
CS_{\Theta,1-\alpha}^{\mathrm{outer}}
=
\left[
\hat{\underline\theta}-z_{1-\alpha/2}\widehat{\se}(\hat{\underline\theta}),\
\hat{\overline\theta}+z_{1-\alpha/2}\widehat{\se}(\hat{\overline\theta})
\right].
\]

A bootstrap analogue can use one-sided quantiles of the endpoint errors instead of normal critical values. This object answers the question: where is the whole population identified set? It is generally wider than an interval designed only to cover the true scalar \(\theta\).

## 6.3 Confidence interval for the true scalar coefficient \(\theta\)

A confidence interval for the true coefficient covers the unknown scalar \(\theta\), which is known only to lie somewhere inside \([\underline\theta,\overline\theta]\). This is the Imbens-Manski/Stoye problem.

Let

\[
s_\ell:=\widehat{\se}(\hat{\underline\theta}),
\qquad
s_u:=\widehat{\se}(\hat{\overline\theta}).
\]

A common practical critical value \(c\) solves

\[
\Phi\left(c+\frac{\max(\hat{\overline\theta}-\hat{\underline\theta},0)}{\max(s_\ell,s_u)}\right)-\Phi(-c)=1-\alpha.
\]

Then report

\[
CI_{\theta,1-\alpha}^{\mathrm{IM}}
=
\left[
\hat{\underline\theta}-c s_\ell,
\hat{\overline\theta}+c s_u
\right].
\]

When the identified interval is wide relative to sampling error, \(c\) is close to \(z_{1-\alpha}\). When the identified interval collapses toward a point, \(c\) approaches \(z_{1-\alpha/2}\). In nonregular cases - for example nearly equal roots, weak relevance, or multiple instruments where the active endpoint changes across bootstrap samples - moment-inequality inversion is safer.

## 6.4 Moment-inequality inversion

The bounded-correlation restriction can be used directly as a moment-inequality test. For a candidate \(w\), retain it if the sample analogue of

\[
g_{ik}(w)\le0\qquad\text{for all }(i,k)\in\cP
\]

is not rejected.

In the scalar case, this means testing values \(w\in\R\) on a grid or by root-finding. A simple max statistic is

\[
T(w)=\max_{(i,k)\in\cP}\frac{\sqrt{T}\,\widehat g_{ik}(w)}{\widehat s_{ik}(w)},
\]

where \(\widehat s_{ik}(w)\) is a standard error for \(\widehat g_{ik}(w)\). The confidence set is

\[
CS_{\theta,1-\alpha}^{\mathrm{MI}}
=\{w:T(w)\le c_{1-\alpha}(w)\},
\]

with \(c_{1-\alpha}(w)\) obtained by bootstrap, multiplier bootstrap, or generalized moment-selection methods. This is the most defensible route when the endpoints are nonregular or when the model has several imposed instruments and the active inequalities can change.

One can also test the two primitive one-sided restrictions

\[
\Corr\bigl(Z_{k,t+1},\varepsilon_{1,t+1}(w)W_{2,i,t+1}\bigr)
-\tau_{ik}\left|\Corr\bigl(Z_{k,t+1},W_{2,i,t+1}^2\bigr)\right|\le0,
\]

\[
-\Corr\bigl(Z_{k,t+1},\varepsilon_{1,t+1}(w)W_{2,i,t+1}\bigr)
-\tau_{ik}\left|\Corr\bigl(Z_{k,t+1},W_{2,i,t+1}^2\bigr)\right|\le0.
\]

This avoids relying only on endpoint delta-method logic and keeps the inference aligned with the maintained inequality model.

# 7. Mapping inference to the other coefficients

For every candidate \(w\in\Theta(\cZ,\tau)\), the coefficient vector on \(X_t\) is uniquely determined by

\[
\beta_1(w)=\beta_1^R-(\beta_2^R)^\T w.
\]

Therefore the identified set for \(\beta_1\) is

\[
\mathcal A:=\{\beta_1^R-(\beta_2^R)^\T w:w\in\Theta(\cZ,\tau)\}.
\]

For \(I=1\), this becomes the affine curve

\[
\beta_1(w)=\beta_1^R-(\beta_2^R)^\T w,
\qquad w\in[\underline\theta,\overline\theta].
\]

For component \(j\), if \(\beta_{2,j}^R\) denotes the coefficient of the \(j\)-th element of \(X_t\) in the scalar \(Y_2\)-on-\(X_t\) projection, then

\[
\beta_{1,j}(w)=\beta_{1,j}^R-\beta_{2,j}^R w.
\]

The sharp plug-in bounds are

\[
\left[
\min\{\hat\beta_{1,j}(\hat{\underline\theta}),\hat\beta_{1,j}(\hat{\overline\theta})\},
\max\{\hat\beta_{1,j}(\hat{\underline\theta}),\hat\beta_{1,j}(\hat{\overline\theta})\}
\right].
\]

For uncertainty, use the same bootstrap draws used for \(\theta\), recomputing \(\widehat\beta_1^{R*}\), \(\widehat\beta_2^{R*}\), and the endpoint images in every draw. Componentwise beta intervals are not a joint confidence band for all components. For a joint statement about \((\theta,\beta_1)\), use projection or test inversion.

# 8. Practical reporting template in the attached notation

A credible empirical report should include:

1. The common conditioning vector \(X_t\), including the principal components and lag choices.
2. The instrument list \(\cZ\), the pair set \(\cP\), and whether each \(Z_k\) is date-\(t\) or date-\((t+1)\) adapted.
3. A grid of slack values \(\tau\), for example \(0\), \(0.10\), \(0.25\), \(0.50\), and \(0.90\), unless a specific slack is justified externally.
4. Relevance diagnostics: \(\widehat P_{ik}=\Covhat(Z_{k,t+1},\widehat W_{2,i,t+1}^2)\) and, for exact point-identified Lewbel estimates, generated-instrument first-stage diagnostics.
5. The plug-in scalar interval \([\hat{\underline\theta},\hat{\overline\theta}]\) when \(I=1\), or coordinate projections \([\hat{\underline\theta}_m,\hat{\overline\theta}_m]\) when \(I>1\).
6. Endpoint standard errors from the full bootstrap.
7. A confidence interval for the true scalar \(\theta\), preferably Imbens-Manski/Stoye-style or moment-inequality inversion.
8. A separate outer confidence set for the whole identified set only if that is the target.
9. The mapped coefficient set \(\widehat{\mathcal A}=\{\widehat\beta_1^R-(\widehat\beta_2^R)^\T w:w\in\widehat\Theta(\cZ,\tau)\}\).
10. Bootstrap failures, empty-set draws, no-real-root draws, and any numerical nonconvergence.

Important cautions:

- \(\tau_{ik}\) is a maintained sensitivity bound, not a parameter learned from the model.
- Data-adaptive choice of \(\cZ\) requires sample splitting or a uniform inference correction.
- Weak heteroskedastic relevance, \(\widehat P_{ik}\approx0\), makes both the set and the inference unstable.
- Generated Lewbel instruments should not be squared, interacted, or transformed without additional assumptions.
- The Lewbel estimator targets homogeneous linear structural coefficients; it is not automatically a local-average-treatment-effect estimator.

# 9. Software landscape

The software conclusion is the same as in the previous report, updated here in the attached notation.

| Need | R | Stata | Comments |
|---|---|---|---|
| Point-identified Lewbel estimator | `REndo::hetErrorsIV`; older `ivlewbel::lewbel` | `ivreg2h` | These estimate point-identified generated-instrument IV/GMM models and standard errors. |
| Plug-in \(\widehat\Theta(\cZ,\tau)\) for Lewbel's set case | No standard package found | No dedicated command found | Easy to code for \(I=1\); quadratically constrained optimization needed for \(I>1\). |
| Confidence intervals for Lewbel Theorem 3 set bounds | No standard package found | No dedicated command found | Requires custom bootstrap, Imbens-Manski/Stoye logic, or moment-inequality inversion. |
| General moment-inequality inference | No widely used drop-in CRAN package found for this exact task | `cmitest`, `clrbound` | Useful templates, not turnkey Lewbel-set commands. |
| GMM building blocks | `gmm`, `momentfit` | built-in/user-written GMM | Useful for custom moments, not partial-ID inference by themselves. |

Validation notes checked during this rewrite:

- The R package `REndo` manual describes `hetErrorsIV` as implementing Lewbel's 2012 heteroskedasticity approach and lists package version 2.5.0 on r-universe/CRAN infrastructure.
- The older R package `ivlewbel` documents a `lewbel` function for heteroskedasticity-based GMM estimation, but it is old and is not a Theorem 3 bound-inference package.
- The Stata command `ivreg2h` is described on RePEc/SSC as generating Lewbel-style instruments and estimating IV regressions; it is a point-identified IV/GMM implementation, not a set-bound package.
- The Stata `cmitest` and `clrbound` commands implement general conditional moment-inequality/intersection-bound methods, but the Lewbel inequalities \(g_{ik}(w)\le0\) must be supplied or adapted by the user.

# 10. Minimal R template in the attached notation

The following template implements the scalar case \(I=1\). It uses the notation from the attached TeX file: \(\theta\) is the scalar coefficient, the endpoints are \(\underline\theta\) and \(\overline\theta\), and the recovery map is \(\beta_1(w)=\beta_1^R-(\beta_2^R)^\T w\). The code accepts one or more scalar instruments in the columns of `Z` and intersects the resulting intervals.

```r
lewbel_theta_set <- function(y1, y2, X, Z, tau = 0.5, tol = 1e-10) {
  y1 <- as.numeric(y1)
  y2 <- as.numeric(y2)
  X <- as.matrix(X)
  Z <- as.matrix(Z)

  n <- length(y1)
  stopifnot(length(y2) == n, nrow(X) == n, nrow(Z) == n)

  K <- ncol(Z)
  if (length(tau) == 1L) tau <- rep(tau, K)
  stopifnot(length(tau) == K, all(tau >= 0), all(tau < 1))

  XtX <- crossprod(X)
  beta1_R <- as.numeric(qr.solve(XtX, crossprod(X, y1)))
  beta2_R <- as.numeric(qr.solve(XtX, crossprod(X, y2)))

  W1 <- as.numeric(y1 - X %*% beta1_R)
  W2 <- as.numeric(y2 - X %*% beta2_R)

  cv <- function(a, b) mean((a - mean(a)) * (b - mean(b)))
  vv <- function(a) mean((a - mean(a))^2)

  pair_intervals <- matrix(NA_real_, K, 2,
                           dimnames = list(colnames(Z), c("theta_lower", "theta_upper")))
  pair_moments <- matrix(NA_real_, K, 8,
                         dimnames = list(colnames(Z),
                                         c("L", "Q", "P", "S0", "S1", "S2",
                                           "discriminant", "cov_Z_W2sq")))

  for (k in seq_len(K)) {
    z <- Z[, k]
    L <- cv(z, W1 * W2)
    Q <- cv(z, W2^2)
    P <- Q
    S0 <- vv(W1 * W2)
    S1 <- cv(W2^2, W1 * W2)
    S2 <- vv(W2^2)
    sigma2 <- S2

    if (abs(P) < tol) stop("Weak/no relevance: Cov(Z_k, W2^2) is near zero.")
    if (sigma2 < tol) stop("Var(W2^2) is near zero.")

    d <- tau[k]^2 * P^2 / sigma2
    A <- Q^2 - d * S2
    b <- -2 * L * Q + 2 * d * S1
    c0 <- L^2 - d * S0

    disc <- b^2 - 4 * A * c0
    if (disc < -tol) stop("No real roots for at least one instrument/slack pair.")
    disc <- max(disc, 0)

    roots <- sort(c((-b - sqrt(disc)) / (2 * A),
                    (-b + sqrt(disc)) / (2 * A)))

    pair_intervals[k, ] <- roots
    pair_moments[k, ] <- c(L, Q, P, S0, S1, S2, disc, P)
  }

  theta_lower <- max(pair_intervals[, "theta_lower"])
  theta_upper <- min(pair_intervals[, "theta_upper"])
  empty <- theta_lower > theta_upper

  beta1_at <- function(w) beta1_R - beta2_R * w
  beta1_bounds <- if (!empty) {
    B <- cbind(lower_endpoint = beta1_at(theta_lower),
               upper_endpoint = beta1_at(theta_upper))
    out <- t(apply(B, 1, range))
    colnames(out) <- c("beta1_lower", "beta1_upper")
    out
  } else {
    NULL
  }

  list(
    theta = c(theta_lower = theta_lower, theta_upper = theta_upper),
    empty = empty,
    beta1_R = beta1_R,
    beta2_R = beta2_R,
    beta1_bounds = beta1_bounds,
    pair_intervals = pair_intervals,
    pair_moments = pair_moments
  )
}

im_critical <- function(width, se_lower, se_upper, alpha = 0.05) {
  s <- max(se_lower, se_upper)
  if (!is.finite(s) || s <= 0) return(qnorm(1 - alpha))
  delta <- max(width, 0) / s
  f <- function(c) pnorm(c + delta) - pnorm(-c) - (1 - alpha)
  uniroot(f, interval = c(0, 10))$root
}

lewbel_theta_set_boot <- function(y1, y2, X, Z, tau = 0.5,
                                  B = 999, alpha = 0.05, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  base <- lewbel_theta_set(y1, y2, X, Z, tau)
  n <- length(y1)

  boot_theta <- matrix(NA_real_, B, 2,
                       dimnames = list(NULL, c("theta_lower", "theta_upper")))
  failures <- 0L

  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    out <- try(
      lewbel_theta_set(y1[idx], y2[idx], X[idx, , drop = FALSE], Z[idx, , drop = FALSE], tau),
      silent = TRUE
    )
    if (inherits(out, "try-error") || isTRUE(out$empty)) {
      failures <- failures + 1L
    } else {
      boot_theta[b, ] <- out$theta
    }
  }

  ok <- complete.cases(boot_theta)
  se <- apply(boot_theta[ok, , drop = FALSE], 2, sd)
  zcrit <- qnorm(1 - alpha / 2)

  outer_set <- c(
    theta_lower = base$theta["theta_lower"] - zcrit * se["theta_lower"],
    theta_upper = base$theta["theta_upper"] + zcrit * se["theta_upper"]
  )

  c_im <- im_critical(diff(base$theta), se["theta_lower"], se["theta_upper"], alpha)
  ci_theta <- c(
    theta_lower = base$theta["theta_lower"] - c_im * se["theta_lower"],
    theta_upper = base$theta["theta_upper"] + c_im * se["theta_upper"]
  )

  list(
    estimate = base,
    boot_theta = boot_theta,
    se_theta = se,
    outer_confidence_set_for_Theta = outer_set,
    im_critical = c_im,
    confidence_interval_for_theta = ci_theta,
    bootstrap_successes = sum(ok),
    bootstrap_failures = failures
  )
}
```

For time-series applications, replace the row bootstrap in `lewbel_theta_set_boot()` with a block bootstrap. For cluster samples, resample clusters.

# 11. Bottom line

Using the attached notation, the final practical recommendation is:

1. Estimate \(\widehat W_{1,t+1}\) and \(\widehat W_{2,t+1}\) from projections on the common \(X_t\).
2. For pre-specified \(\cZ\), \(\cP\), and \(\tau\), compute \(\widehat\Theta(\cZ,
\tau)\) from the inequalities \(\widehat g_{ik}(w)\le0\).
3. In the scalar case \(I=1\), report \([\hat{\underline\theta},\hat{\overline\theta}]\), obtained as the intersection of quadratic-root intervals.
4. Bootstrap the entire construction for endpoint uncertainty.
5. Report an Imbens-Manski/Stoye-style confidence interval for the true scalar \(\theta\), or use moment-inequality inversion.
6. Recover and report the corresponding set for the other equation coefficients with
   \[
   \widehat\beta_1(w)=\widehat\beta_1^R-(\widehat\beta_2^R)^\T w.
   \]

No standard package appears to implement the full Lewbel set-bound inference procedure in this notation. The point-identified Lewbel estimator is available in R and Stata, but the set-identified case requires custom code or adaptation of general moment-inequality tools.

# References and validated sources

- Lewbel, A. (2012). "Using Heteroscedasticity to Identify and Estimate Mismeasured and Endogenous Regressor Models." *Journal of Business & Economic Statistics*, 30(1), 67-80.
- Baum, C. F., and Lewbel, A. (2019). "Advice on using heteroskedasticity-based identification." *The Stata Journal*, 19(4), 757-767.
- Imbens, G. W., and Manski, C. F. (2004). "Confidence Intervals for Partially Identified Parameters." *Econometrica*, 72(6), 1845-1857.
- Stoye, J. (2009). "More on Confidence Intervals for Partially Identified Parameters." *Econometrica*, 77(4), 1299-1315.
- Kaido, H., Molinari, F., and Stoye, J. (2019). "Confidence Intervals for Projections of Partially Identified Parameters." *Econometrica*, 87(4), 1397-1432.
- Andrews, D. W. K., and Shi, X. (2013). "Inference Based on Conditional Moment Inequalities." *Econometrica*, 81(2), 609-666.
- Canay, I. A., Illanes, G., and Velez, A. (2023). "A User's Guide to Inference in Models Defined by Moment Inequalities." NBER Working Paper 31040.
- R package `REndo`, especially `hetErrorsIV`: https://cran.r-universe.dev/REndo/doc/manual.html
- R package `ivlewbel`, especially `lewbel`: https://rdrr.io/cran/ivlewbel/
- Stata package `ivreg2h`: https://ideas.repec.org/c/boc/bocode/s457555.html
- Stata package `cmitest`: https://ideas.repec.org/c/boc/bocode/s458138.html
- Stata package `clrbound`: https://ifs.org.uk/publications/clrbound-stata-module-perform-estimation-and-inference-intersection-bounds
