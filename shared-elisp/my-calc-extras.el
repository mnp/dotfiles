;;
;; I'm not sure why these aren't part of `math-standard-units`.
;; Source: https://en.wikipedia.org/wiki/Mebibyte
;;

(setq math-additional-units '(
  ;; Base units. Note "b" is Barns in math-standard-units, while convention "B" is Bytes, so
  ;; there's no conflict.
  (bits nil "bits")
  (B "8 * bits" "Bytes")

  ;; Decimal units
  (kB  "1000   B"  "kilobyte")
  (MB  "1000^2 B"  "megabyte")
  (GB  "1000^3 B"  "gigabyte")
  (TB  "1000^4 B"  "terabyte")
  (PB  "1000^5 B"  "petabyte")
  (EB  "1000^6 B"  "exabyte")
  (ZB  "1000^7 B"  "zettabyte")
  (YB  "1000^8 B"  "yottabyte")

  ;; Binary units
  (KiB  "1024   B"  "kibibyte")
  (MiB  "1024^2 B"  "mebibyte")
  (GiB  "1024^3 B"  "gibibyte")
  (TiB  "1024^4 B"  "tebibyte")
  (PiB  "1024^5 B"  "pebibyte")
  (EiB  "1024^6 B"  "exbibyte")
  (ZiB  "1024^7 B"  "zebibyte")
  (YiB  "1024^8 B"  "yobibyte")))

(setq math-units-table nil)
