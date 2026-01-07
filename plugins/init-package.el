(setq use-package-always-ensure t) ;; 每个包都添加:ensure t 关键字(确保安装)
(setq use-package-always-defer t) ;; 延迟加载每个包都添加:defer t
(setq use-package-always-demand nil)
(setq use-package-expand-minimally t) ;; 
(setq use-package-verbose t);; 安装过程打印出来
 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)


(unless package-archive-contents
  (package-refresh-contents))


(require 'use-package)



;; 快速重启 emacs
(use-package restart-emacs)

;; === 主题，用的是和nvim同款
(use-package catppuccin-theme
  :ensure t
  :demand t
  :init (setq catppuccin-flavor 'macchiato)
  :config  (load-theme 'catppuccin t)) 
(load-theme 'catppuccin :no-confirm)

;; === Emacs 中的最强大的 git 工具
(use-package magit
  :bind(("C-x g" . magit-status))
  :config (add-hook 'git-commit-setup-hook 'turn-off-flyspell)
)

(use-package crux
  :bind ("C-c k" . crux-smart-kill-line))

(use-package ivy
  :demand
  :hook (after-init . ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-initial-inputs-alist nil
	ivy-count-format "%d%d"
	enable-recursive-minibuffers t
	ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
)


(use-package counsel
    :after (ivy)
    :bind (("M-x" . counsel-M-x)
	   ("C-x C-f" . counsel-find-file)
	   ("C-c f" . counsel-recentf)
	   ("C-c g" . counsel-git)))

(use-package swiper
    :after (ivy)
    :bind (("C-s" . swiper)
	   ("C-r" . swiper-isearch-backward))
    :config (setq swiper-action-recenter t
		  swiper-include-line-number-in-search t))

(use-package which-key
  :config (which-key-mode))

(require 'lsp)
(provide 'init-package)
 
