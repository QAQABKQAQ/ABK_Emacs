(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq inhibit-starup-message t)
(setq menu-bar-mode -1)
(setq tool-bar-mode -1)
(setq scroll-bar-mode -1)



(require 'package)
(setq package-enable-at-startup nil);;取消自启

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"));;配置插件连接

(package-initialize);; 显式启用


;; Emacs29以上自带use-package,直接require就可以使用
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; 尝试插件但是不下载到本地
(use-package try
  :ensure t)
;; 指令重启Emacs
(use-package restart-emacs
  :ensure t)


(use-package which-key
  :ensure t
  :config (which-key-mode))















(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
