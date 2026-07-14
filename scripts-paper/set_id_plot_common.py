"""Shared helpers for the identified-region figures (read set_id_region.json,
written by set_id_region_export.R). Each axis is the standardized coefficient
b_{i,N} * sd(PC_{N,i}), i.e. the effect on log consumption growth of a
one-standard-deviation move in news PC i. That is a per-axis linear scaling
S = diag(sd) of b_N; the quadratic system transforms as A -> S^{-1} A S^{-1},
b -> S^{-1} b, c unchanged, so the region in beta = S b is the identified set
stretched to standard-deviation units.
"""
import json
import os

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
OUT_DIR = os.path.join(HERE, "output")
DATA_PATH = os.path.join(OUT_DIR, "set_id_region.json")

# standardized-coefficient axis labels and their one-line definition
COEFS = [r"$\tilde b_{1,N}$", r"$\tilde b_{2,N}$", r"$\tilde b_{3,N}$"]
SD_NOTE = (r"$\tilde b_{i,N}=b_{i,N}\times\mathrm{sd}(PC_{N,i})$ "
           r"(effect of a 1 SD move in $PC_{N,i}$ on $\Delta c$)")


def load_sd(path=DATA_PATH):
    """Load the exported systems and rescale everything to SD units in place.
    Returns (D, point0, s) with s the vector of news-PC standard deviations."""
    with open(path) as fh:
        D = json.load(fh)
    s = np.array(D["sd_pc_n"])
    ss = np.outer(s, s)
    for sysd in D["systems"].values():
        sysd["A"] = {k: (np.asarray(v) / ss).tolist() for k, v in sysd["A"].items()}
        sysd["b"] = {k: (np.asarray(v) / s).tolist() for k, v in sysd["b"].items()}
        sysd["box_lower"] = (np.asarray(sysd["box_lower"]) * s).tolist()
        sysd["box_upper"] = (np.asarray(sysd["box_upper"]) * s).tolist()
    return D, np.array(D["point0"]) * s, s


def gmax(sysd, B1, B2, B3):
    """G(b) = max_i (b'A_i b + b_i.b + c_i) over one slack's constraints; the
    identified set at that slack is {b : G(b) <= 0}."""
    Bs = (B1, B2, B3)
    G = None
    for Ai, bi, ci in zip(sysd["A"].values(), sysd["b"].values(), sysd["c"]):
        Ai = np.asarray(Ai)
        gi = np.full_like(B1, ci, dtype=float)
        for j in range(3):
            gi = gi + bi[j] * Bs[j]
            for k in range(3):
                gi = gi + Ai[j][k] * Bs[j] * Bs[k]
        G = gi if G is None else np.maximum(G, gi)
    return G


def box(sysd):
    return np.array(sysd["box_lower"]), np.array(sysd["box_upper"])


def expand(lo, hi, frac):
    w = hi - lo
    return lo - frac * w, hi + frac * w


def project_margin(sysd, perp, ca, cb, wgrid, chunk=48):
    """Margin of the set projected out along axis `perp`:
    M(ca, cb) = min over the perpendicular coordinate of max_i G_i, on a fine
    grid (chunked over the projected axis so the shadow boundary is smooth)."""
    a, b = [i for i in range(3) if i != perp]
    U, V = np.meshgrid(ca, cb, indexing="ij")
    M = np.full(U.shape, np.inf)
    for i in range(0, len(wgrid), chunk):
        coords = [None, None, None]
        coords[a] = U[..., None]
        coords[b] = V[..., None]
        coords[perp] = wgrid[i:i + chunk][None, None, :]
        M = np.minimum(M, gmax(sysd, *coords).min(axis=2))
    return M
