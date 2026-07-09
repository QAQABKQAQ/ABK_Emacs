;;; -*- lexical-binding: t; -*-


;; 长度为4个字段
(setq-default tab-width 8)
;; `indent-tabs-mode` 不意味只使用制表符，如果缩进级别
;; 可以被`tab-width`整除，则使用制表符，其余部分使用空格
(setq-default indent-tabs-mode t)





(use-package emacs
  :hook
  ;; 光标进不去 minibuffer 的提示文字
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  ;; disable startup buffer
  (inhibit-startup-screen t)
  ;; 关闭铃声
  (ring-bell-function 'ignore)
  ;; 非窗口活动不画光标
  (highlight-nonselected-windows nil)
  ;; 边缘3行自动滚动
  (scroll-margin 3)
  ;; >100 行光标出屏逐行滚动
  (scroll-conservatively 101)
  ;; 翻页往返光标回原位
  (scroll-preserve-screen-position t)
  ;; 在 minibuffer 内还可以再开 minibuffer
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (scroll-margin 3)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  ;;滚动性能优化
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (jit-lock-defer-time 0)
  ;;子进程读取的自适应缓冲
  (process-adaptive-read-buffering nil)
  ;;断行基准
  (fill-column 80)
  ;; 输入期间跳过字体渲染
  (fast-but-imprecise-scrolling t)
  (redisplay-skip-fontification-on-input t)
  (jit-lock-defer-time 0)
  ;; 禁止生成 .# 锁文件
  (create-lockfiles nil)
  (ring-bell-function 'ignore)
  (inhibit-startup-screen t)
  (use-short-answers t)
  :config
  (set-language-environment "UTF-8")
  (setq default-input-method nil)
  )

;; magit 管理 git 
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-flyspell))

;; 最近文件，现实历史文件
(use-package recentf
  :config
  (recentf-mode 1)
  :custom
  (recentf-save-file (locate-user-emacs-file ".local/recentf")))

;; minibuffer 历史 + 剪贴板跨重启存活
(use-package savehist
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (savehist-mode 1)
  :custom
  (savehist-file (locate-user-emacs-file ".local/history")))

(use-package server
  :defer t
  :custom
  (server-lognil))

(use-package dired
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first gls"))

(use-package comp
  :defer t
  :custom
  (native-comp-async-report-warnings-errors 'silent))


(use-package ls-lisp
  :defer t
  :custom
  (ls-lisp-dirs-first t))

(use-package dired
  :defer t
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first"))

(use-package xt-mouse
  :config
  (xterm-mouse-mode 1))

(use-package menu-bar
  :derfer t
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :defer t
  :config
  (tool-bar-mode -1))



(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; 创建文件
(make-directory (locate-user-emacs-file ".local/") t)
(make-directory (locate-user-emacs-file ".local/cache") t)
(make-directory (locate-user-emacs-file ".local/backup/") t)
