
;;===================
;; emacs init       |
;; Author: Ephemera |
;;===================




(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; =====ENV=====





;; =============


(when (file-exists-p custom-file)
  (load custom-file))


(use-package which-key
  :ensure nil
  :init
  (which-key-mode)
  :config
  (which-key-add-key-based-replacements
    ;; prefix labels: these show when you stop at C-c / M-g / C-x p
    "C-c s" '("搜索" . "搜索相关命令")
    "C-c !" '("诊断" . "诊断相关命令")
    "C-c c" "快速记录任务(org-capture)"
    "C-c a" "查看任务清单(org-agenda)"
    "M-g" '("跳转" . "跳转相关命令")
    "C-x p" '("项目" . "项目相关命令")
    ;; leaf labels
    "C-x s p" "项目全局搜索(consult-ripgrep)"
    "C-x s b" "搜索当前项目的所有buffer(consult-project-buffer)"
    "M-g g" "行跳转"
    "M-g m" "跳转到标记点"
    "M-g i" "跳转到当前文件的函数/定义"
    "C-s" "替换原生搜索"
    "M-o" "替换原生切换窗口M-o"
    "C-x p f" "快速找项目内容文件"
    "C-x p b" "只在项目buffer间切换"
    "C-x p c" "项目根目录运行编辑"
    "<C-return>" "补全"
    "S-SPC" "模糊搜索"
    "M-n" "下一个Error"
    "M-p" "上一个Error"
    "C-c ! l" "显示当前文件所有问题"
    "M-." "跳转文档"
    "M-," "返回"
    "M-?" "查找引用"
    "C-c r" "重命名符号"
    "C-c h" "显示完整文档"))





(use-package vertico
  :ensure t
  :custom
  (vertico-preselect 'directory)
  :init
  (vertico-mode))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :custom
  (vertico-multiform-categories '((file (vertico-preselect . prompt))))
  :init
  (vertico-multiform-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-pcm-leading-wildcard t)
  (completion-category-defaults nil))


;; 窗口切换
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)) ; 替换原生的 M-o

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (;; 1. 项目全局搜索
         ("C-c s p" . consult-ripgrep)
         ;; 2. 搜索当前项目的所有 Buffer
         ("C-c s b" . consult-project-buffer)
         ;; 3. 跳转功能
         ("M-g g" . consult-goto-line)     ; 带预览的行跳转
         ("M-g m" . consult-mark)          ; 跳转到标记点
         ("M-g i" . consult-imenu)         ; 跳转到当前文件的函数/类定义 (Java/C++ 必备)
         ;; 4. 搜索增强
         ("C-s" . consult-line))           ; 替换原生搜索，带实时预览
  :config
  (setq consult-preview-key 'any)) ; 实时预览

(use-package org
  :ensure nil
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq org-directory (expand-file-name "org" user-emacs-directory)
        org-default-notes-file (expand-file-name "todo.org" org-directory)
        org-agenda-files (list org-default-notes-file)
        org-log-done 'time)
  :config
  (unless (file-directory-p org-directory)
    (make-directory org-directory t))
  (unless (file-exists-p org-default-notes-file)
    (with-temp-file org-default-notes-file
      (insert "#+title: Todo\n\n")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file org-default-notes-file)
           "* TODO %?\n%U\n"))))

(use-package appt
  :ensure nil
  :after org
  :init
  (setq appt-message-warning-time 15
        appt-display-interval 5
        appt-display-mode-line t
        appt-display-format 'window
        appt-audible nil)
  :config
  (defun my/org-appt-refresh ()
    "Refresh appointment reminders from Org agenda files."
    (interactive)
    (org-agenda-to-appt t))
  (my/org-appt-refresh)
  (appt-activate 1)
  (add-hook 'org-finalize-agenda-hook #'my/org-appt-refresh)
  (add-hook 'org-capture-after-finalize-hook #'my/org-appt-refresh)
  (add-hook 'after-save-hook
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (my/org-appt-refresh)))))

(use-package project
  :ensure nil ; 内置
  :bind (("C-x p f" . project-find-file)    ; 快速找项目内的文件
         ("C-x p b" . project-switch-to-buffer) ; 只在项目 Buffer 间切换
         ("C-x p c" . project-compile)))   ; 在项目根目录运行编译

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map global-map
        ("<C-return>" . completion-at-point)) 
  (:map corfu-map
        ("S-SPC" . corfu-insert-separator))
  :custom
    (corfu-on-exact-match nil)
  :config
  (setq 
   corfu-auto nil
   corfu-cycle t
   corfu-popupinfo-delay 0.1
   corfu-preview-current nil
   )
  (corfu-popupinfo-mode)
 )




(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  )
;;  (setq rust-format-on-save t)) ; 自动格式化


;; ==== emacs 内置
(use-package eglot
  :ensure nil
  :hook ((rust-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (java-mode . eglot-ensure))
  :bind(:map eglot-mode-map
             ("M-." . xref-find-definitions)
             ("M-," . xref-pop-marker-stack)
             ("M-?" . xref-find-references)
             ("C-c r" . eglot-rename)
             ("C-c h" . eldoc-doc-buffer))
  :config
  ;; 自动格式化
  (add-hook 'before-save-hook 
            (lambda () 
              (when (eglot-managed-p) 
                (eglot-format-buffer))))
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-idle-delay 0.2)
  (add-hook 'eglot-managed-mode-hook #'eldoc-mode))






(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)) ; 列表显示当前文件所有问题
  :config
    (setq flymake-mode-line-format
        '(" " flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '("["
          (:propertize flymake-mode-line-error-counter
                       face flymake-error-echo-at-point)
          ":"
          (:propertize flymake-mode-line-warning-counter
                       face flymake-warning-echo-at-point)
          "]"))
  ;; 缩短 ElDoc (显示文档/报错) 的响应时间
  (setq eldoc-idle-delay 0.1)
  ;; 让报错信息显示得更完整，但不要让它自动撑开回显区高度
  ;;(setq eldoc-echo-area-use-multiline-p nil)
  ;; 错误指示灯放在左侧边缘
  (setq flymake-fringe-indicator-position 'left-fringe)
  ;; 没有错误时不显示 0
  (setq flymake-suppress-zero-counters t))


;; modeline 美化
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)     ; 设置合适的高度
  (doom-modeline-bar-width 3)   ; 左侧装饰条宽度
  (doom-modeline-icon t)        ; 开启图标（需安装 nerd-icons）
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)) ; 智能显示路径


;; === Git ===
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-flyspell))

;; ==== defun ====
(defun open-init-file()
  (interactive)
  (find-file "~/.config/emacs/init.el")
  )

(defun my/project-run ()
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (cond
     ((file-exists-p "Cargo.toml") (compile "cargo run"))
     ((file-exists-p "go.mod")     (compile "go run ."))
     ((file-exists-p "pom.xml")    (compile "mvn exec:java"))
     ((file-exists-p "Makefile")   (compile "make -k"))
     (t (call-interactively 'compile)))))

;; ==== key ====
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "<f5>") 'my/project-run)

;; ==== theme ====
(load-theme 'modus-vivendi t)

;; 放在最后一行
;; 降低gc 防止占用过高
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
 
