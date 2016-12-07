;;; This file is run near the beginning of init.el. Here, you can
;;; customize various aspects of Radian.
;;;
;;; There are a number of customizable parameters, which begin with
;;; "radian-customize-". These parameters are set to their default
;;; values using `setq' at the very beginning of init.el, and each
;;; `setq' declaration is accompanied by a comment explaining the
;;; meaning of the parameter.
;;;
;;; If you use the setup script to generate this file, then all of the
;;; `setq' declarations and their explanatory comments are copied into
;;; this file (see below). When a new parameter is added, you have two
;;; options: either copy the declaration from init.el into this file,
;;; or delete this file and run the setup script again. (Obviously,
;;; the second option will cause you to lose anything you previously
;;; had in this file.)
;;;
;;; You can also use this file to inhibit the loading of packages that
;;; would otherwise be included with Radian by default. The following
;;; functions are provided:
;;;
;;; * `radian-disable-package'
;;; * `radian-reenable-package'
;;; * `radian-package-enabled-p'
;;; * `radian-package-disabled-p'
;;;
;;; See their docstrings for more information. As a simple example,
;;; however, here is how you can disable Aggressive Indent:
;;;
;;; (radian-disable-package 'aggressive-indent)
