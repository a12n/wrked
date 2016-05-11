(require 'generic-x)

(define-generic-mode 'wrk-mode
  ;; Comments
  '((?{ . ?}))
  ;; Keywords
  '("W" "bpm" "h" "hr" "kcal" "km" "km/h" "m" "m/s" "min" "rpm" "s" "zone")
  ;; Font lock list
  `((,(regexp-opt '("active" "cooldown" "rest" "warmup"
                    "cycling" "running" "swimming" "walking")
                  'symbols) . font-lock-constant-face)
    (,(regexp-opt '("cadence" "calories" "distance" "power" "speed" "time")
                  'symbols) . font-lock-type-face))
  ;; Auto mode list
  '("\\.wrk$")
  ;; Function list
  '())

(provide 'wrk-mode)
