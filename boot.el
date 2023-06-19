(message "hello")

(local-set-key
 [?\C-c ?\C-a]
 '(lambda () (interactive) (tidal-send-string ":load Lib.hs")))
