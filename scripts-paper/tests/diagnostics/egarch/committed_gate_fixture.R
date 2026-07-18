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
    "/Td6WFoAAAFpIt42AgAhARwAAAAQz1jM4Bx+D5VdACwCfBn5r/QdTA+dyOE1mgCOcQKbaOI7VhyK6YadxU6rZr2f",
    "g9b0XIMdANvuj8TqUL+Xl5ndB26veEhRl8eMP3wvNwvTCs11BC+ikx77n7fmukSaR950egunajqlFqn1FGVlZTHi",
    "v4TfD9yOcoC2WS8ZjD1ksAChT7UvOCBWNQcAV3CFs7hAzQZKKiUxjugRqKwvUKw78x3uJJAYLET0tYPVGO1mN+x4",
    "SKpng6eO2g0oh3/X6fI+K54y3joxFUIZ5P6//Ay38P9+YI5L64+Sb4r1XdmcEfK6QKYvsgWJzlvfgdMc+xWV44RM",
    "e+hRYpIrucJQRpvGZDB70PQadRK8bHpZqmunNXBtkF3CePIztbO6DOQ2VpSJYHBZiv1vI5JscyBoWXjl8n3MBMNE",
    "nfY5cjgGT8LTpKY0Li036GB/ia8D8mf/cYMtrF3cCEgwJ+/EaipcPtUWJhWDdbjyMrnp8L8y2rSFZYLCsgVqrtxY",
    "FuLToLKebAMfl9nvvws0P6Dtry+5eFkOUvW817jm3VqexN9FeTObE5SWaKfK/W9Ty/VRRHQbhIJokeDF0MvjMLiQ",
    "e+UWBSelGLznt0OSrPbMBOB5irvoRv3lEubRMAypz7whdkzjIZVdS6rcb6+xVI8FMv26+swvxsL4Ip6rXw7DMtbH",
    "wF393DY60BK4iD9AUmi1Tp7bfKC17Dnqgc1Q+TcQr1Qe+8nmEL/mJjbMaqpmCIIWSn4StJLIP1Wc187JDSHI5dOK",
    "po/KchlKZ483jIVZoNY40DdQpcx1KAn0LRisLfmQOhN0TvIJHIf3sr0F8vMVie5ZVEDhZ8wr10GWwXvRfn0cutH0",
    "8CAvT9eysrTxmDLP0I+9K63E28iKbBrwDKagkg6KZ3wc5QVn+99F/v/JFMzGkS39ky7au6PQ6vO1SmaXJOlBKCYX",
    "nomLOrEdVTo6vRk7MHc/LMUu89WRf9euLDnEaX8SkVsld8lsCg9Ac9YQiqZ9P6iAgl1m+BjuZlyrGqL+sECla4lB",
    "8xNXKWt0mlnXwjyOlt48ou/9xYNP1FKLm4MYE9Hx4Uw85qMB0HBtBQGyV1RiTs/Zi7zdxeV4YVJKPy/IQp3YB4YU",
    "Ws+k8gsUbWI4BIB/NHxJX4rSx6ff63SGMhnsnTQwr1rIQCwDZMMt+IrJjSdhpnQuQg40AZaQagZmaTIABuzH0ifL",
    "kV4ZDTQbLpQu+MMnwm0NwXh64/xwqSh4WIn29Oh46xfthOrrWZaltoUfi5gzXMaLb2nt0NWwW80fbLeYsxlL6RxF",
    "sZejiokiA3VGe99HAft5LNtQFiRawoW0xkz4lNJOto4hR6s9vUZ3Yfiuqmk5sN5DsptOBbHUkYJggC0hwbk0GHVO",
    "Pfpho4R5hrE0DFOq1JCZdbtF+WJM+fYZXs1dXVSvWgL+LaBSHP95nv7B7QRTkGY/iuJD4vXuhx5a+MBOkPJMnKck",
    "3XI5tVfeO/nXUyzllU7wQR3TQqF+8lbJ3zQ7xHylOXcR68lWeaUtXTKB+/TynmwxMv5KZuD+JUD3C9wkUFEz3Csu",
    "pevvbPhvlKJmrNYvgjLSW0byF2/xokJfuQrziCFXFbMXw5mfKcQVKcN4dbvGkBwNEYN2Fst9CuVxtn/CLtZTiUay",
    "GzvNMKh6Kt3xTHyOJoekky6YE85zGFOYrC2J53vTjzeqDZLR55Ec6ocWXMtOlE5W3PYt46d0ErkqI1RihLwbzQbX",
    "SzgvFxPECvy5BNQW79Cspn2CfsXYOXOJkTqMzdb9RGPAt/TyOzsazU49pU2A1vfMH6UDJvefyI1BHbr0ZwD3QzH/",
    "t274FMLoenBmGJRUpefIDsZAqytiS78zb00FWtusnwz58YYX8Ce36lkXaYUWCDS7iRNxTuDSIDE1VSMGlnd9JDp9",
    "9lIE4eCDwnz40LBHCeyrynIMlKoYpSS0WsaQa4jCWBO3VfALetf7W2Tg8/eJzZ1AmbLOf5vKZVeGj/tyk5b1vJBG",
    "XgdS6j/Nppae3gZ4PRrpnJZNN2FVJaBprImD6v2iuQShtOBubp62A3uUfD1TVUr1p58WKzBWV7mj24V+jWudPyEv",
    "AWYfbBpTTS6E/ZJMyjYeRdNIUaPmVqbPi3ZpBE7Zp3mmaY7Y/VwW+KqNF4sDJeUr6VOSUpCNYMFs+cma1mwOIvO2",
    "OvG7wlmjKPhze5bXUva3hrj6kK+KakqDVc8UQ2uRFQ5HaAVcF3J1AyAEkL+9x5ngO3HM9NOch3wpAEbyUm2buSKX",
    "4ioRX7USNtddpwrF/knR9E4978t/5PTRL8nOlxr1t4eghZ+CyLnoHb2dXgkMkedacDvd40uKlf3y38DeR0t4Lt4p",
    "XDhF5BwHL2wqcZyOOE3xslDigDbXoxOXBpnONHJNRT372qNNNAMY1Sv/oMQ2kdMiWgDXZ9guG3/DsVFnjUyGXKsT",
    "QCJEDneMZDG633tS+vR5bb8gbWyxZ8B7zbqmMj/TMUiOEU3H3cHdYZ9cUDI8YZ23MouxMV90NHQiHM2dBK0sZPEB",
    "eMs3UByVevPqPZ2bhhJvPMs5KtbLpJ1KooX0fT+dHdrM/6+i+L5+vOiPXu2j73QnedpDVnwbdQSzXKaAy0WkIYv3",
    "rH19uQN26pnmfJeE6xoy9wPKmvClbwGyjcBOqlVT++d43Eg+dsALJhfD6FAYkHnPtRZniD1CDAb/8vylRUA04tS+",
    "5tGL7s4DswXUQvvFJHlxqhYTa6lSekkDp1NKD/p/4QeqluCJh2UwgtMlc5gHRW93QNw/FILubbHKEJzysyX7PGi3",
    "XYcUdGHcip49bvovDZJX6K8sOQbsKSYvkmDcswYBJQWaa18/TRC+PmxNpddnFeC/0mxGnJPQDqKNu3uaztnvits+",
    "bwKImOp3nP328Df1B8XN2S9Fvv1PeJ/NZ/Ou1+S3Ra89S3p3jLAEoCWXfZkCGs1lNZbJcmw9xNBp221TfL2LvMuQ",
    "OtbVwZ2qEDOdQlR2oiQ4IW7B4SQJ7NmUTeNSUFfmA7NJECHWxjcC1wS3EDOoHa5O3OTCapFIfqDgJqXtPClF4DtQ",
    "q+KZI8LmFuTQO6qFZZ2JmbU2Uw3nKGAysLfAfXOyT0cTAfN8vAMTKy3Oh3qYX8RYjBsuQQv/mJzkjn9sHWPiaXv0",
    "NwKczwOVGOMFbDnmcJoFJVPPOpsOzNgPh+uSjhpnlN7NTy5ugZAcgkKPS9zgUA3qDlYyoUG6/eGCl8ofc7HR7wYU",
    "AOefhXCpEKhOfvbtEvZgBbE53cYjBPrNprZyPnLqNmdnBU8IfoqfUZJOQRVY3t7ZWre+4tyRLsDbrEsWE8tJSdI6",
    "XnhrkC4gtl4mXjqj4KxyqTJVAXrJjLFMOh3o2fnLj+DRsyAdgJ1Q3VIld6eiQE+LBViT8FOy5SlpFgatQ9XB3wVO",
    "BsQAXXrQEohGIVNXEufvy4FWNWoSlB0zDxsVnE9gqdUZEnzcHTvB5o130Y1OdCDwpJZfHJkeuLncWB43GjUd9NFf",
    "vJY4dlWWyE/58TXjFGCBEZtjkqJRDgNPiJVwKpm5zNUmuYeuEAdvXE43fFHQ3H04aduyfQpONz5DP2qgAOlnTOo9",
    "/+KYVI//Ze+E3hgN3iXYwuXjvXl1tCutSMramLsDK4Ezo+U5A31ivm2dQ4nrEqw3MvdRNagoDG/a63vu/bgAc58M",
    "db4+BK+P6GT3me1yIhN1EMKAqwV2CJl2Orf0AKYy2sEPv2gJ+Oq9TegzBwyn3Ohlh/kIVsVx4Ai5F9BqkctZUp+k",
    "UiotlJNuXuCE65AttdMe30GSN5kLL15naHRf1VGBQTKrywaRJPZKc1HY0xZpS6Q8LWACwkAhMOyo0yn5TVxq4HOt",
    "GAxhHH+MFUlHERK1EK7OBkytp0wJWyaX7IjCrwix+cQm/mnDC6hD2yK0QpB1v0xJgZK5qJVjoQgwbU1RFdcNKDdw",
    "66Wyb50V8PQ5XBhr80YvQi/GXPBRCFxBqrIfYZeX1Cydg1kZuVB/AHRRTHEhV7j/py070/QdDvzi1tozNWuSGfQQ",
    "sklr2oYCazlY94/e52eqNdhO8XPTbskslZGwpPGNHICuRHbJ0haIn/Ft53HZXd7wqAPRwrEuwXwxQgJ5JbwjSdn6",
    "OJ6slKzogWn4C97Vt5LZ4QZWWhLuUutaD5FghEDYjicdwNaqGwPiSh8Vgu9WFyPbCo9LYLYOvkDcmXYLyPTol7E/",
    "WUCULil13EQgeSVjZaZce2/jCTH86zP09Mp/nLBFNUCmnqCfukNLuIK23fy58XTcmJNPCwB9DHH4mjbiIWvC9ODq",
    "X2EON6P64M27Z6gkzIa/FUus2IKEjIdr4yTxWzMg80gAFmZJhEqQYGqwfQSS6urQpR2hl8mWqpWl5WxZ6M2WAi04",
    "gMx8+nDIXHckmWVMLz9j5AI1rol6cEQNKWm6qn6qp6mxXiNiV0GHRZHTVkdxdpS5KyOiszRL/xBhjdDn4hWbGrff",
    "uiLI1W3gv2aLT+8j7SNeRU5xyOUb5DToSDNdJ7JyEobawn2+Paiq03dkSYwSpjBtqCDRiNBhkvTsygL/7hNxUFL3",
    "dk/WD2gxfZfqQIGt6uqYNY5G9bhxUJeWRn04EN0YChfEuwhQnL/WkwKOqnHpgyogOTxn/Jolr2yovONOK9+0luMn",
    "rxhIvNXILrZGetN10Mf+t+3Q95nhqne3FR3wCKZiZ1yO4Xh+uj9MKqwz7UQfUFE86g5GA3z3/+tb8owSf48FuOmK",
    "2L7v1TyLTVVzL+SsYj7PRPNoKIVpu+S8x0j287xlHAn8tiYxPYg5cWsIejioZEfFtt49mUrqmBR1hct0Ojd59xfo",
    "s1Vd4Z+YSkMVJGpKrNz+4jEYqK5JD1qZCM1Gj9AWmM9h8Nm2naoV+Kbna7NFB12DcP3/4GnYaPmNznXDP6MfhqBM",
    "HXamEviWgDRa9eXrJP7P3SMxtS6d24V1Y2hK6b8wT7dtqE7woLG8ZBc4GL7BdoeWnDnBCPuQ2GSHwHsFuFm/fhmd",
    "UsGzErP3az86Vcf9VkpGwb68W1nacUHF+NjaeeejiI4DcEm4fZHWe6pAsDzLYlLTawkot0zLW+RwGyWXFWaimSoF",
    "likos5AXtu3EOdfH6nzeBGutb7CsLIomfdluFRqKo55OqiJHXm2OUjzoQBV2eN9a7pyPXgRsxz+ZAON/oK4j50Fl",
    "3BPs1+0JD8+Pvvg3Bgq/27YVD9G6pQfBxIrW380LNDmO8Inm4ZeRPqD89TgURVqrdpfYiUiQEvRxnxVkssu0C4fZ",
    "Z9SH7wNu3mY2SK8n3hwiUsdWIoX1eV3HUWCZXQrSLGCX6rbyGWi3kmwb2d1Mw0l8rMrbmS4XExYQCXJcAAAAAM7h",
    "q1gAAa0f/zgAAGdkGnA+MA2LAgAAAAABWVo="
  )
  payload <- memDecompress(.egarch_base64_decode(encoded), type = "xz")
  connection <- rawConnection(payload)
  on.exit(close(connection), add = TRUE)
  readRDS(connection)
}
