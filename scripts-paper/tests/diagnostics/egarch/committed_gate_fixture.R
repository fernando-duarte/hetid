# Output-independent fixture for the scientific gate payload bound by the
# tracked EGARCH decision. The payload is an xz-compressed version-3 RDS encoded
# as base64 so the fixture remains reviewable text and requires only base R.

.egarch_base64_decode <- function(parts) {
  encoded <- sub("=+$", "", paste0(parts, collapse = ""))
  alphabet <- strsplit(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
    "",
    fixed = TRUE
  )[[1L]]
  values <- match(strsplit(encoded, "", fixed = TRUE)[[1L]], alphabet) - 1L
  stopifnot(!anyNA(values))
  bits <- as.vector(vapply(
    values,
    function(value) rev(as.integer(intToBits(value))[seq_len(6L)]),
    integer(6L)
  ))
  byte_count <- length(bits) %/% 8L
  bit_matrix <- matrix(bits[seq_len(byte_count * 8L)], nrow = 8L)
  as.raw(colSums(bit_matrix * 2^(7L:0L)))
}

egarch_committed_gate_fixture <- function() {
  encoded <- c(
    "/Td6WFoAAAFpIt42AgAhARwAAAAQz1jM4Bx+D5RdACwCfBn5r/QdTA+dyOE1mgCOcQKbaOI7VhyK6YadxU6rZr2fg",
    "9b0XIMdANvuj8TqUL+Xl5ndB26veEhRl8eMP3wvNwvTCs11BC+ikx77n7fmukSaR950egunajqlFqn1FGVlZTHiv4",
    "TfD9yOcoC2WS8ZjD1ksAChT7UvOCBWNQcAV3CFs7hAzQZKKiUxjugRqKwvUKw78x3uJJAYLET0tYPVGO1mN+x4SKp",
    "ng6eO2g0oh3/X6fI+K54y3joxFUIZ5P6//Ay38P9+YI5L64+Sb4r1XdmcEfK6QKYvsgWJzlvfgdMc+xWV44RMe+hR",
    "YpIruM3SUPR3JtgkVRWwwnQgfFaYiKL2/qHgYinywd07Mrbcq+MTQ/O4Fu7Rk2gaCwedMkImIx8/UjhdiLiug1+aN",
    "o1ERRnhehPODkDjsfieXLwl6PGN4oNZf9/qhQ6laRANp3EGgQSsAS3cKCvfsYxDYMfKkjjRWBxt42Co4HrJzGcW9s",
    "y+o95V8jp6FEHy/NKETlUxdTYIGSkIHZBZ/6zh19rqNbhVIPWFK8t+thQ1/QMG5VFiF5kgshxToc+KNxaW2JExxxC",
    "wTxYcg+exPQg85LWNzoAurlXrq3ZllKAhVwyAsKs9aaxUh83Z0VvFe299yFcwlYQuGLidRp+/zCc0SRpjtHZX96ON",
    "aWBPqojgv5L59JVTbYDDIoMekZAp+eXBNsVsy3VV2gTExi5XJ5qe0hGUU/S9gfG4OBZYOp+LgyMf7EeSnAI/ZB5OL",
    "Qvt9jLafNE96S0juTxGfjZ4jXtKzmM2zkwMfKYUyF7stYTebLkbdPSYO5hGd05IUHW80H4YtayPEjUVoQcLs+0aFA",
    "nn2qBZff6U3kXwam8YjXnnJoRIMo2dr2D/ywYIHl5nzzQ9vFPgpvHLaZfq7kT/dsMYiHT+ZQw9wH0AKnHn1MiVmE/",
    "x8675C8vHxRhxIYoUksCNuRC/AatozeZY0cyK4n3ppTIIgCrQk7bYd1Qtsnax4LFDeCgYnkmGVtIbD15TKT9xIOY+",
    "0+169X14l4aW+GdfBJFMjiv4Vce0iMTgUKmj94syJ2JXX0k5Bhfwx9YT+qw06BDsJYKtiC1udiQbPUYbgEyJlRNVS",
    "Mk2ZKaFpoSvd7y86+XnFy7nIJHcAfMTmLtEpztR8Y4Bb6n3un0WJvv05ZbhGBB8WXzd+T24TKZOtppj/uzEfVe6wj",
    "SisVo0ww1WO/9Sv2fAHwxflweXEX1Qovs0zgCgtny4Xe1trgfnI13c496ziRg5NUN0mY5pOaWluJm1uaqA3L+BivE",
    "qfWvFSRr82A+zcD42ngKIXhUNgThP+J2jreyqeUj536F8624flxKwoP7GzXjIIfbt9Jqm/q1cxWM5KlKVJMYGgXGF",
    "9Ok0eYz6WxhuhUCnovaf6aqzYYIhl5Sr5mtj4QWYRbm6eef0+UPOEJjBkJNp0MuikA0tJ1jDOt9eBM8exLyDXgXVz",
    "QXSpCU/viWp2dcXPu7gVW08ADrbGULIgCUHv9WcA6LOpL4JRTmFg2acbF4pC4a9o/wufVJ8KaFpIeQ8WMJDUlErJU",
    "E9BKPHlgStetoPRrRm4BuPD9NTmymV90KXlOPfDT74ooW5gQjNc5eNVKiMEa61Ev9aE+7x2ytj/ElZ+5KT7itbbFu",
    "r1uDNn5d0Ch38nnTH1sVwA14tI9RyuL1LbRqH2dT+IDQ74c+xhhUmF2G77D4Ae3Ez9fw2AiF9qiAs5yUbamD9OcHF",
    "NIXfJ1Ps5vRNKI53/Id/pm7U3rumjR6DnZ1H7wLx9GdBMrpfmGLm2T+shcujEhw7nURkI+Zbt0Z3uUHot3GAa/3cq",
    "+nbGQRPz9VezEdipPR78ETW1LY5TdZEw7kuxWoyKhUVc2sYMoH1XSION9WOjGmBR6qpvygP+qzH51kU3UZxIw5qMV",
    "bTgNacQ55mm3Z7/oCfcwM04H9D6BZBmsgk1sayNQFYjx5e4WIP5IeINJ/nm+WarnPvbfn9xQCdrZuwTaBsupoXadr",
    "FC/Qr1inpsdIVznwbKGdvvol4HOlVStpfnYnDiXbqm2714J3W2M6mC66gc1oQKRIYare9Of1foxzvnjgPCS0Ep4ZE",
    "utC/EIsSr8GnIU8BkKswTbVDo4GJApuPRHfFAqEVYiwRKXuyRPSmO2PhEHJWx/HFjK9hkc6eUC3dsVB/jBaMf3yxb",
    "NMUzC5Lwe+7T3sjd6lPpNPs9RyDNY42PRu78faYg5ilDofXDvvtd6neQbtLDguSqrcF920FWnGY5lCAeYfdbtp7J3",
    "qRZOl+9+mmTg0OEI+71U8A+pvxXDGGoeW49RzPvMA8TbDoKpbcIrLr6b55bnqL5BNZlmerNMwYBeJZFpSpwcM7KZA",
    "HDrTEqS67PA855pSTLvQ+Vy05WwgWS4agwulJlGQF6O984YLvGpxhjlQ2b7B/TaFGBy+xPWkDmDohR+xuYR7fX1DE",
    "AjEjLiyS9dz7eOGwDOv9UrFDlOQaASauM2HtQ4HiY5f51xmcDfP6Lsu5lO57EQEDCRmUAgZ1JE2xPye3lYwaRpxaQ",
    "Y2LPcz+JdSTTwGBdwHraMWh+CJJsBtiSGfYBQiLCZvcC4RX/VAdmgIgfChPPHs/wurXd//+RPNDQVcGrETE7kTfYc",
    "kLzhlDPQhJGEqzRiSgSSjvLw+yDyyBXo4ODN++O5p9UXYksTP2qr+aJqwTLsvVKxnegJH6hFZn1M6CG9NOcOVzHof",
    "Zd5D6RksmHavpK773IW9LoXjs+VDbxhGVXOAB6ZF4OqKdZCIZKRfmp2WQvZsJkth9wcZbHVOETLgkC0D78f3Rr61n",
    "z3wFczxaAk5x2SscbJO6wrdcWIDrFiPn964TC4NXAqOVTQ5SeFJBosD0y3joRr+hVCVk7pUol+c7A4Pmsa1Vg2rFW",
    "ZPg7OtxRMl/BNoaSTAsG+Jfou1xxPGCvoQ46YiH+8o/cBY+DfNkgeZxfxwvPrkh7Lc/jK8/+GRT9Q23pJ6l3r+MWm",
    "IZVc4tF1HnqEQrXWvHAbNqqbTis9MW1n6zQzbtzWcAYOmC4U2myFyFZXzZXma120/uFDlc6jCz7re8yObtbnoCCgL",
    "jeL6eYYMhWP4kXYE0TIpZCs+W3x74Tfg43mxqs0t7MmUOg/QoNkH0zCrMlHho5oh1JpLP+mRfAQrA/3FZfLbORecG",
    "R6M7+ssoYUPde6URyiX4Sb8yH+vgXh9veE20IPmOm3W3IprgqH1buHljymuDDxbZXQwZaynnkqPP96fX+zg2QH+2o",
    "83hFMlCaFqoYqgqTvQo6YAQtN4jpXxf9DXdTuGGZXFL+dee7xMT3lbTcAD9sqUrAPT2jwybJJSNSXyEhURzClA+Qv",
    "Al6dwRChLEao0uHJ+sdxoIwFg94EapSe7gxYrydaX3GCVh6gXXtZ2VU1HMKOAvWNY+JR6SaUBVMcTMS/1XXxWOqxB",
    "Ix+mqDEMZVyO+a+nqq3qTiB/i8XaE90oCKHgyH9d2dh+NOdVavGSv/RVBIwoV8jESd8GkSVXDsB12r/tl9irNObrK",
    "0M3E4e/P5u8/q4zwaRj/3a+NGyrgLd0X//tKfsyqPLbDRbOtireC0MHze5YkBoGw/aof9p4omjvu6EZzq/k/seeX0",
    "el9j2urqwy6mkv+CTAHWH4sufTVUF3ArE0tJEb7NroiWh9syEZQK/jCgxK3rQVrs6HIUyZLlu1UxY3L6daL4xvITm",
    "De38GeZR/AItxf850NNM6ab0Why+fOSDzJ0Ctlc9mcjHmWp9DeM59hcn3phfN4Z3MRO9MFScy0xOWpeAHj0fCHtmW",
    "fn+iamyBSfjRUhLTPkJjjcps6QP2gzNdrm5byWXE3rC8Gzckm3wzaVoyAEUHzNGGuqzyxJ5KgyUxAXS1zCVB89LmE",
    "/Enw/Veh469PZIMPVWWy36S8FDxJ7aBnx848V2TlAM5yP546BiCJ+D1L4YuQ3Veh7wxzGQIJ1TIgN/W7/JBUeSN9j",
    "FyceJv9nUUed1B/ybGENBlWVr+3/MyxwVimvMAol0wkk0C8lZGb9v3byFgqY55jj2GBz5c8kK9mvF4oPuvfqh/KQP",
    "u4t5gZxpB5M22q0JhPc5HWHRQj9ENRIcK5KtW6No8JYWYxoPI7oRwfwLagPvrw41C+snCdIHjBmc896bePuuLDeP7",
    "WwnNSZRo3npfxqT2Yyrsw7fEsC96gRkLqOFygaqj/KwlWDQ56dcUDSxsuZ+rOgfHHcPAogXI3uRQpdYATrel1x4NQ",
    "aoqMn+LttP4/yK8qXxVimdp7vtNu2J2fD6zE+eDI8FJDy27SSXm+Bx7AR/fJYwLE95b3NdBlbQV0YQygAnMXoPfb2",
    "Np7oYq8m1Cd8CIgHwI1gmrrUIcSvBnrVxr1fAZ93bJIy555RoXNUBoS+4bsCJCh+fjWRDvTQ44/i0jxzbg3dVW3uO",
    "ZQBqrsKWxjHJEK2d/89xOpm8RVvRf68XArNMClwy8HLrQDRxCMuBtO+fLlO6nf+C2m9ljZPZ4jptjJMpBxgCnWl5/",
    "CUXrLL+vjiTcSjXKc0WRVhu5NTfZrXBDHrZ45UhmvJHS+qOwdANWip9jTYr8hVydNwH1c8PcjXZgAcRCou32fIrRI",
    "eTBYnYtwv6HvW/ls0e3LprwGU7pawhmFvRGpRLevte6WC33Af4sYu0E83NBya605nXOjNTD0bOY5lOcNpGUmgncHZ",
    "pBOYioyT9QsDBQkDhugo7uHCY5+UOflvSugvBKY3QiLWc/MEdQF5uFCJV8IJW1d7htTD1KZqxEw7tWkzEwSUxIkPz",
    "hDH4Lp3moW/1xPtD/2dXL/xNjEauzdn7Nn/LTO2s/iH2cDvWSjoANBG9qQLOkh/IjSlNMBgwWQrZxohiPTn5pXeNv",
    "M0qpNVcl9ILnuJW/cj6X12d0yoc9VG6TYSUHhJgdDWdeS+/yhfnj7an78+RULT9fbNZqRFsgrds+1V8Ychfghn689",
    "7fkbRs4UB4/yQzoJneNf+ZvFMhRaxalkOWDrtNsvw3Iod9QjbzXIvnxLqqTByD7AHH1YNdX45eVWNjdfKyVOCjahW",
    "wWIeBo84ry2fZ9E4GN1Z2jm+e39vVhmLcHe/LeXlE2hDKzMTAdt1kGBO5lqi0OGN60x7FnLqvwY8UgspX8kBdnG5+",
    "IZQhO7eljbbUE6jgU5TenRdhQAZNn/77gw00GJ2kTomXjzaivaSWjYdvtVPhzG+knHFU23dsigNDISouzdBq+Fi3D",
    "xXPjKXE+ohb6HtPpFX/7O4NBArfU2lP8EU7Sc034fZIY4c5iYHShONAtwLeDUYtwvHBkV3gN63t8WdHrYNiAylcxS",
    "WbQF+UNbtqXhcm0hqgAAKf+JsAABrB//OAAAwrdGuz4wDYsCAAAAAAFZWg=="
  )
  payload <- memDecompress(.egarch_base64_decode(encoded), type = "xz")
  connection <- rawConnection(payload)
  on.exit(close(connection), add = TRUE)
  readRDS(connection)
}
