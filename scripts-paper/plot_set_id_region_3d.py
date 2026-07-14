"""The joint identified set for b_N at tau = 0.05 in standard-deviation units,
as a closed black triangular wire mesh with a lightly filled interior. Each
coordinate wall carries the set's shadow (its projection along that axis), and
the tau = 0 point is shown with its projection onto each wall; the shadows sit
on the back walls (away from the camera) for the chosen view. Reads
set_id_region.json (set_id_region_export.R).
Writes set_id_region_3d.pdf to scripts-paper/output/.
Run via run_all.R.
"""
import os

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection

import set_id_plot_common as C
from wire_common import wire_mesh

D, POINT0, _ = C.load_sd()
sys05 = D["systems"]["tau_0.05"]
ELEV, AZIM = 23.1, 152.8

lo, hi = C.expand(*C.box(sys05), 0.25)
NU, NW = 440, 2200
f1 = np.linspace(lo[0], hi[0], NU)
f2 = np.linspace(lo[1], hi[1], NU)
f3 = np.linspace(lo[2], hi[2], NU)
M = [C.project_margin(sys05, 0, f2, f3, np.linspace(lo[0], hi[0], NW)),
     C.project_margin(sys05, 1, f1, f3, np.linspace(lo[1], hi[1], NW)),
     C.project_margin(sys05, 2, f1, f2, np.linspace(lo[2], hi[2], NW))]
FG = [(f2, f3), (f1, f3), (f1, f2)]

# axis limits (b1 extended to 0.28, b2 lowered to -0.08) and round-number ticks
# chosen to avoid the box-corner label collisions
LIMS = [(f1[0], 0.28), (-0.08, f2[-1]), (f3[0], f3[-1])]
TICKS = [[0.15, 0.20, 0.25], [-0.05, 0.00, 0.05], [-0.20, -0.16, -0.12]]

# place each shadow on the back wall (away from the camera) for this view
_az, _el = np.radians(AZIM), np.radians(ELEV)
_eye = [np.cos(_el) * np.cos(_az), np.cos(_el) * np.sin(_az), np.sin(_el)]
walls = [(k, LIMS[k][0] if _eye[k] > 0 else LIMS[k][1]) for k in range(3)]

segs, faces = wire_mesh(C.gmax, sys05, lo, hi, return_faces=True)

fig = plt.figure(figsize=(9.6, 8.4))
ax = fig.add_subplot(111, projection="3d")

for perp, off in walls:                                 # wall shadows
    a, b = [i for i in range(3) if i != perp]
    ca, cb = FG[perp]
    tmp = plt.figure()
    cs0 = tmp.add_subplot(111).contour(ca, cb, M[perp].T, levels=[0.0])
    plt.close(tmp)
    for seg in cs0.allsegs[0]:
        p = [None, None, None]
        p[perp] = np.full(len(seg), off)
        p[a], p[b] = seg[:, 0], seg[:, 1]
        ax.add_collection3d(Poly3DCollection([list(zip(*p))],
                                             facecolors="#9dc3e6", alpha=0.4,
                                             edgecolors="none"))
        ax.plot(*p, color="k", lw=1.4)

for perp, off in walls:                                 # tau=0 point projections
    q = POINT0.copy()
    q[perp] = off
    ax.plot(*[[POINT0[i], q[i]] for i in range(3)], color="crimson", ls=":",
            lw=1.0, alpha=0.65)
    ax.scatter(*q, facecolor="white", edgecolor="crimson", s=42, linewidth=1.4,
               depthshade=False, zorder=5)
ax.scatter(*POINT0, color="crimson", s=60, depthshade=False, edgecolor="k",
           linewidth=0.5, zorder=6)

ax.add_collection3d(Poly3DCollection(faces, facecolors="#4a90d9", alpha=0.10,
                                     edgecolors="none"))
ax.add_collection3d(Line3DCollection(segs, colors="#123", linewidths=0.7))

ax.set_xlim(*LIMS[0])
ax.set_ylim(*LIMS[1])
ax.set_zlim(*LIMS[2])
ax.set_xticks(TICKS[0])
ax.set_yticks(TICKS[1])
ax.set_zticks(TICKS[2])
ax.set_xlabel(C.COEFS[0], labelpad=10)
ax.set_ylabel(C.COEFS[1], labelpad=10)
ax.set_zlabel(C.COEFS[2], labelpad=8)
ax.view_init(elev=ELEV, azim=AZIM)
ax.set_title(r"Identified region $\Theta$ for $b_N$ at $\tau=0.05$, SD units"
             "\n" + C.SD_NOTE
             + "\nwire mesh + lightly filled interior;  each wall = its shadow",
             fontsize=10)
fig.tight_layout()
out = os.path.join(C.OUT_DIR, "set_id_region_3d.pdf")
fig.savefig(out)
print("plot_set_id_region_3d: wrote", out)
