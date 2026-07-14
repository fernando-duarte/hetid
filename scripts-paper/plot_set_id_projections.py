"""Projected identified sets for b_N in standard-deviation units: the three
coordinate-plane projections of the joint set at tau = 0.05, 0.1, 0.2, each with
its bounding box (the reported per-coefficient intervals) and the tau = 0 point.
All panels share one x-range and one y-range so magnitudes are comparable, and
each panel frame is square. Reads set_id_region.json (set_id_region_export.R).
Writes set_id_projections_sd.pdf to scripts-paper/output/.
Run via run_all.R.
"""
import os

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

import set_id_plot_common as C

D, POINT0, _ = C.load_sd()
sysd_of = D["systems"].__getitem__

taus = [("tau_0.05", 0.05), ("tau_0.1", 0.1), ("tau_0.2", 0.2)]
tcols = plt.cm.viridis(np.linspace(0.12, 0.75, len(taus)))
planes = [((1, 2), 0), ((0, 2), 1), ((0, 1), 2)]        # (in-plane axes), perp
titles = [r"onto $(\tilde b_{2,N},\tilde b_{3,N})$  (perp. $\tilde b_{1,N}$)",
          r"onto $(\tilde b_{1,N},\tilde b_{3,N})$  (perp. $\tilde b_{2,N}$)",
          r"onto $(\tilde b_{1,N},\tilde b_{2,N})$  (perp. $\tilde b_{3,N}$)"]
m, mw = 280, 380

# one shared x-range (spans every variable on an x-axis: b1, b2) and one shared
# y-range (b2, b3), tightened to the widest set (tau = 0.2)
blo2, bhi2 = C.box(sysd_of("tau_0.2"))
xr_lo, xr_hi = min(blo2[0], blo2[1]), max(bhi2[0], bhi2[1])
yr_lo, yr_hi = min(blo2[1], blo2[2]), max(bhi2[1], bhi2[2])
xpad, ypad = 0.04 * (xr_hi - xr_lo), 0.04 * (yr_hi - yr_lo)
XLO, XHI, YLO, YHI = xr_lo - xpad, xr_hi + xpad, yr_lo - ypad, yr_hi + ypad

fig, axes = plt.subplots(1, 3, figsize=(11.5, 4.8))
for ax, ((ia, ib), ip), title in zip(axes, planes, titles):
    ca = np.linspace(XLO, XHI, m)
    cb = np.linspace(YLO, YHI, m)
    wpad = 0.05 * (bhi2[ip] - blo2[ip])
    wg = np.linspace(blo2[ip] - wpad, bhi2[ip] + wpad, mw)
    for (name, tv), col in zip(taus, tcols):
        sysd = sysd_of(name)
        M = C.project_margin(sysd, ip, ca, cb, wg)
        cs = ax.contour(ca, cb, M.T, [0.0], colors=[col], linewidths=2.0)
        # bounding box = exact extent of the drawn outline, so the set touches
        # the box but never crosses it
        pp = np.vstack([s for s in cs.allsegs[0]])
        (x0, y0), (x1, y1) = pp.min(0), pp.max(0)
        ax.add_patch(plt.Rectangle((x0, y0), x1 - x0, y1 - y0, fill=False,
                                   edgecolor=col, ls=(0, (4, 3)), lw=1.0,
                                   alpha=0.85))
    ax.plot(POINT0[ia], POINT0[ib], "o", mfc="white", mec="crimson", ms=7,
            mew=1.4)
    ax.set_xlim(XLO, XHI)
    ax.set_ylim(YLO, YHI)
    ax.set_box_aspect(1)                                # square frame
    ax.set_xlabel(C.COEFS[ia])
    ax.set_ylabel(C.COEFS[ib])
    ax.set_title(title, fontsize=10)
    ax.grid(alpha=0.25)

handles = [plt.Line2D([], [], color=c, lw=2, label=fr"$\tau={tv}$ set boundary")
           for (n, tv), c in zip(taus, tcols)]
handles += [
    plt.Line2D([], [], color="0.35", ls=(0, (4, 3)), lw=1.0,
               label=r"set bounding box (per $\tau$)"),
    plt.Line2D([], [], color="crimson", marker="o", ls="", mfc="white", mew=1.4,
               label=r"$\tau=0$ point"),
]
fig.subplots_adjust(left=0.06, right=0.99, top=0.84, bottom=0.17, wspace=0.32)
fig.legend(handles=handles, loc="lower center", ncol=5, frameon=False,
           bbox_to_anchor=(0.5, 0.01))
fig.suptitle("Projected identified sets in SD units "
             r"($\tilde b_{i,N}=b_{i,N}\,\mathrm{sd}(PC_{N,i})$): "
             "set boundaries and bounding intervals by $\\tau$", fontsize=11)
out = os.path.join(C.OUT_DIR, "set_id_projections_sd.pdf")
fig.savefig(out)
print("plot_set_id_projections: wrote", out)
