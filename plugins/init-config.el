
;; 菜单栏
(menu-bar-mode 0)
;; 工具栏
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(winner-mode 1)
(load-file custom-file)
(ido-mode 1)
(ido-everywhere 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq create-lockfiles nil)

(defconst *is-mac* (eq system-type 'darwin))

(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


(provide 'init-config)
