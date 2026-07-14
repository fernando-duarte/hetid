"""Closed-wireframe builder for the b_N identified region (a thin plate along
b1). Both plate faces are triangulated over a staggered + jittered (b2, b3)
lattice and stitched along the rim (a triangulated side band between the two
skins at the triangulation boundary), so the wire mesh closes into a surface
that bounds the region instead of reading as two disconnected sheets.
"""
import warnings
from collections import Counter

import numpy as np
import matplotlib.tri as mtri


def wire_mesh(gmax_fn, sys, lo, hi, h=0.16, seed=0, nfb1=400, min_cells=5,
              return_faces=False):
    """Return a list of (2,3) line segments (x=b1, y=b2, z=b3) for the closed
    triangular wire mesh. gmax_fn(sys, B1, B2, B3) is the caller's constraint.
    With return_faces=True, also return the closed shell's triangular faces
    (top + bottom + rim) as a list of [(x,y,z),(x,y,z),(x,y,z)] for filling."""
    rng = np.random.default_rng(seed)
    dr = h * np.sqrt(3) / 2
    lat = []
    for iy, vy in enumerate(np.arange(-0.05, 1.06, dr)):
        offx = 0.0 if iy % 2 == 0 else h / 2
        for vx in np.arange(-0.05 + offx, 1.06, h):
            lat.append((vx, vy))
    lat = np.array(lat) + rng.uniform(-0.16 * h, 0.16 * h, size=(len(lat), 2))
    wb2 = lo[1] + lat[:, 0] * (hi[1] - lo[1])
    wb3 = lo[2] + lat[:, 1] * (hi[2] - lo[2])
    fb1 = np.linspace(lo[0], hi[0], nfb1)
    WB1 = np.broadcast_to(fb1[None, :], (len(lat), nfb1))
    feas = gmax_fn(sys, WB1, np.broadcast_to(wb2[:, None], WB1.shape),
                   np.broadcast_to(wb3[:, None], WB1.shape)) <= 0
    good = feas.sum(1) >= min_cells
    WB1m = np.where(feas, WB1, np.nan)
    with warnings.catch_warnings():                    # all-NaN cols are masked
        warnings.simplefilter("ignore", RuntimeWarning)
        s_hi = np.nan_to_num(np.where(good, np.nanmax(WB1m, axis=1), np.nan))
        s_lo = np.nan_to_num(np.where(good, np.nanmin(WB1m, axis=1), np.nan))
    gi = np.where(good)[0]
    tri = mtri.Triangulation(lat[gi, 0], lat[gi, 1])
    p = lat[gi]
    elen = np.maximum.reduce([np.hypot(*(p[tri.triangles[:, a]] -
                                         p[tri.triangles[:, b]]).T)
                              for a, b in [(0, 1), (1, 2), (2, 0)]])
    keep = tri.triangles[elen <= 1.5 * h]
    b2g, b3g, hi_s, lo_s = wb2[gi], wb3[gi], s_hi[gi], s_lo[gi]

    def pt(i, skin):
        return (skin[i], b2g[i], b3g[i])

    ec = Counter()
    for t in keep:
        for a, b in [(0, 1), (1, 2), (2, 0)]:
            ec[(min(t[a], t[b]), max(t[a], t[b]))] += 1

    segs = []
    for (i, j) in ec:                                  # both plate faces
        segs.append((pt(i, hi_s), pt(j, hi_s)))
        segs.append((pt(i, lo_s), pt(j, lo_s)))
    bverts = set()
    for (i, j), c in ec.items():                       # rim band (boundary only)
        if c == 1:
            segs.append((pt(i, hi_s), pt(j, lo_s)))     # side diagonal
            bverts.update((i, j))
    for i in bverts:
        segs.append((pt(i, hi_s), pt(i, lo_s)))         # vertical stitch
    segs = [np.array(s) for s in segs]
    if not return_faces:
        return segs

    faces = []
    for t in keep:                                     # top + bottom faces
        faces.append([pt(t[0], hi_s), pt(t[1], hi_s), pt(t[2], hi_s)])
        faces.append([pt(t[0], lo_s), pt(t[1], lo_s), pt(t[2], lo_s)])
    for (i, j), c in ec.items():                       # rim band (2 tris / edge)
        if c == 1:
            faces.append([pt(i, hi_s), pt(j, hi_s), pt(j, lo_s)])
            faces.append([pt(i, hi_s), pt(j, lo_s), pt(i, lo_s)])
    return segs, faces
