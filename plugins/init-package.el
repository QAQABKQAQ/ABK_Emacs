(setq use-package-always-ensure t) ;; 每个包都添加:ensure t 关键字(确保安装)
(setq use-package-always-defer t) ;; 延迟加载每个包都添加:defer t
(setq use-package-always-demand nil)
(setq use-package-expand-minimally t) ;; 
(setq use-package-verbose t);; 安装过程打印出来
 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(use-package restart-emacs)

(use-package catppuccin-theme
  :ensure t
  :demand t
  :init (setq catppuccin-flavor 'macchiato)
  :config  (load-theme 'catppuccin t)) 
(load-theme 'catppuccin :no-confirm)

(use-package magit
  :bind(("C-x g" . magit-status))
  :config (add-hook 'git-commit-setup-hook 'turn-off-flyspell)
)

(provide 'init-package)
