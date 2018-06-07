;; -*- lexical-binding: t -*-

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
(setq ad-redefinition-action 'accept)

(provide 'radian-warnings)
