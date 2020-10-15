;;
;; I'm not sure why these aren't already part of `math-standard-units`.
;;
;; Sources:
;;   https://en.wikipedia.org/wiki/Bit
;;   https://en.wikipedia.org/wiki/Bit_rate
;;
;; TODO:
;;   Install like https://www.emacswiki.org/emacs/Calc#toc8
;;
;; Notes:
;;  - This will override your `math-additional-units` if you have one. You'll need to merge
;;    them if that's the case.
;;  - This would be better as an Emacs patch unless there's a reason it was omitted in the
;;    first place.
;;  - There's a bug around degrees F <--> C in standard units.

(setq math-additional-units '(
  ;; Base units. Note "b" is Barns in math-standard-units, while convention "B" is Bytes, so
  ;; there's no conflict.
  (bits nil "bits")
  (B "8 * bits" "Bytes")

  ;; Decimal byte units (SI)
  (kB  "1000   B"  "kilobyte")
  (MB  "1000^2 B"  "megabyte")
  (GB  "1000^3 B"  "gigabyte")
  (TB  "1000^4 B"  "terabyte")
  (PB  "1000^5 B"  "petabyte")
  (EB  "1000^6 B"  "exabyte")
  (ZB  "1000^7 B"  "zettabyte")
  (YB  "1000^8 B"  "yottabyte")

  ;; Binary byte units
  (KiB  "1024   B"  "kibibyte")
  (MiB  "1024^2 B"  "mebibyte")
  (GiB  "1024^3 B"  "gibibyte")
  (TiB  "1024^4 B"  "tebibyte")
  (PiB  "1024^5 B"  "pebibyte")
  (EiB  "1024^6 B"  "exbibyte")
  (ZiB  "1024^7 B"  "zebibyte")
  (YiB  "1024^8 B"  "yobibyte")

  ;; Bit rates
  ;;
  ;; Anything like X/s is automatically handled by calc. But there are abbreviations in common
  ;; use which this section tries to capture. Note the b=bit and B=byte convention is
  ;; strict, but because there's already the b=Barns unit, we can't depend on "b/s" to mean bit
  ;; per second.
  
  (bps   "1 bits/s"      "bits per second")
  (Bps   "1 B/s"         "bytes per second")

  ;; Decimal bit rates
  (kbps  "1000 bits/sec"  "kilobits per second")
  (mbps  "1000^2 bits/s"  "megabits per second")
  (gbps  "1000^3 bits/s"  "gigabits per second")
  (tbps  "1000^4 bits/s"  "terabits per second")
  (pbps  "1000^5 bits/s"  "petabits per second")
  (ebps  "1000^6 bits/s"  "exabits per second")
  (zbps  "1000^7 bits/s"  "zettabits per second")
  (ybps  "1000^8 bits/s"  "yottabits per second")

  ;; Binary bit rates
  (kibps  "1024 bits/s"    "kibits per second")
  (mibps  "1024^2 bits/s"  "mebibits per second")
  (gibps  "1024^3 bits/s"  "gibibits per second")
  (tibps  "1024^4 bits/s"  "tebibits per second")
  (pibps  "1024^5 bits/s"  "pebibits per second")
  (eibps  "1024^6 bits/s"  "exbibits per second")
  (zibps  "1024^7 bits/s"  "zebibits per second")
  (yibps  "1024^8 bits/s"  "yobibits per second")
))

(setq math-units-table nil)
