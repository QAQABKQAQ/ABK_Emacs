
(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))
(load custom-file t t)


(setq inhibit-startup-message t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(require 'package)
(setq package-enable-at-startup nil);;取消自启

(setq package-archives '(("gnu". "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa-tuna"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize);; 显式启用

;; Emacs29以上自带use-package,直接require就可以使用
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'cl-lib)
(require 'use-package)

(use-package catppuccin-theme
  :ensure t
  :demand t
  :init
  (setq catppuccin-flavor 'macchiato)
  :config
  (load-theme 'catppuccin t)
  )



(use-package multiple-cursors
  :ensure t
  :bind (
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this))
  )



(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)) 



(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (vertico-posframe-mode 1)

  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))


(use-package consult
  :ensure t
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c f" . consult-find)
         ("C-c g" . consult-ripgrep)
         ("M-g o" . consult-outline))
  :config
  (setq consult-preview-key 'any))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))



;; 尝试插件但是不下载到本地
(use-package try
  :ensure t)

;; 指令重启Emacs
(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package seq
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (("C-'" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   )
  :config
  (setq avy-background t)
  (setq avy-timeout-seconds 0.5)
)




;; ===LSP===
;; === Part 1: Eglot (只负责连接 LSP 服务) ===
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure) ; 在所有编程语言中启动
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package eglot-java
    :ensure t
    :hook (java-mode . eglot-java-mode)
    :init
    (setq eglot-java-server-install-dir (expand-file-name "eclipse.jdt.ls" user-emacs-directory)))



(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map global-map
        ("<C-return>" . completion-at-point)) 
  (:map corfu-map
        ("S-SPC" . corfu-insert-separator))
  :config
  (setq corfu-auto nil
        corfu-cycle t
        ) 

  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


(use-package cape
  :ensure t
  :init
  ;; 全局开启文件路径补全 (比如在 scratch 或 git commit 中)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind
  (:map global-map
	("<M-return>" . completion-at-point))
  :config
  (setq corfu-quit-no-match 'separator)
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point ; 1. 智能代码补全 (Eglot)
                       #'cape-dabbrev              ; 2. 单词补全 (Cape)
                       #'cape-file))))             ; 3. 路径补全 (Cape)
  :hook (eglot-managed-mode . my/eglot-capf))


 (use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; 终端
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook
	    (lambda()
	      (display-line-numbers-mode -1)
	      (hl-line-mode -1)
	      (corfu-mode -1)))
  (defun my/toggle-vterm()
    (interactive)
    (let ((buffer-name "*vterm*"))
      (if (equal (buffer-name) buffer-name)
	  (switch-to-buffer (other-buffer (current-buffer)1))
	(if (get-buffer buffer-name)
	    (switch-to-buffer buffer-name)
	  (vterm buffer-name)))))
  :bind
  (("C-`" . my/toggle-vterm)))


