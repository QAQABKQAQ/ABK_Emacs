g;; emacs的自定义设置转移到自定义文件中
(setq custom-file "~/.config/emacs/.emacs.custom.el")

(add-to-list 'load-path
	     (expand-file-name ( concat user-emacs-directory "plugins")))  ;; ~/.config/emacs

(require 'init-config)
(require 'init-package)
