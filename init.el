
;;===================
;; emacs init       |
;; Author: Ephemera |
;;===================




(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; =====ENV=====

;; 再次确保无启动欢迎页（与 early-init 双保险）
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; 禁用系统警告音(macOS上会从内置扬声器发出)
(setq ring-bell-function 'ignore)  ; 完全禁用bell
(setq visible-bell nil)            ; 也禁用可视化bell

;; 把 Emacs 生成的备份/自动保存文件集中放在配置目录里，避免污染项目目录。
(let ((backup-dir (expand-file-name "backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "auto-save/" user-emacs-directory)))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2))


(require 'package)
;; 代理在 early-init 里已写入 url-proxy-services；这里再确保一次
(when (fboundp 'my/setup-url-proxy-from-env)
  (my/setup-url-proxy-from-env))

;; 清华镜像在部分代理/Emacs url.el 组合下会 403 Forbidden；
;; 中科大 + 官方 melpa 实测可下。代理走 early-init 的 url-proxy-services。
(setq package-archives
      '(("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
        ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
;; 注意拼写：priorities（写成 priorites 不会生效）
(setq package-archive-priorities '(("melpa" . 10) ("gnu" . 5) ("nongnu" . 5)))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; =============


(when (file-exists-p custom-file)
  (load custom-file))

;; ==== 会话持久化 (借鉴 alynx / Vertico 官方推荐) ====
;; 把这些运行时文件集中到 var/,保持配置根目录干净(alynx 的 .local/ 思路)。
;; var/ 已在 .gitignore 中忽略,不会污染仓库。
(defconst my/var-dir (expand-file-name "var/" user-emacs-directory)
  "集中存放运行时状态文件的目录。")
(make-directory my/var-dir t)

;; savehist: 记住 minibuffer 历史,让最近/高频候选在 Vertico 里排到前面。
(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (expand-file-name "history" my/var-dir))
  (savehist-mode 1))

;; saveplace: 重开文件时把光标恢复到上次离开的位置。
(use-package saveplace
  :ensure nil
  :init
  (setq save-place-file (expand-file-name "places" my/var-dir))
  (save-place-mode 1))

;; recentf: 维护最近打开文件列表,喂给 consult-buffer 的「最近文件」区。
(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file (expand-file-name "recentf" my/var-dir)
        recentf-max-saved-items 200)
  (recentf-mode 1))


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
    "M-;" "注释"
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
    "C-c h" "查看当前函数文档"
    "C-c f" "手动格式化当前buffer"
    "C-c d" "切换悬浮函数文档"
    "C-c m" "打开邮箱"
    "C-x SPC" "矩形选择(rectangle-mark-mode)"
    "C-x r" '("矩形/寄存器" . "矩形编辑和寄存器命令")
    "C-x r t" "矩形区域每行插入/替换字符串"
    "C-x r k" "删除矩形区域"
    "C-x r y" "粘贴上次删除的矩形"
    "C-x r N" "给矩形区域的每一行编号"
    "C-c M-f" "跳转到头文件/源文件(C/C++)"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("http_proxy" "https_proxy" "all_proxy" "HTTP_PROXY" "HTTPS_PROXY" "ALL_PROXY")))

;; Git over SSH breaks in Emacs-launched shells unless we strip a few
;; Emacs-specific env vars before delegating to the system ssh binary.
(setenv "GIT_SSH_COMMAND"
        (expand-file-name "~/.local/bin/git-ssh-clean-env"))




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


;;; -------------------- tree-sitter --------------------
;; 只负责高亮/缩进；补全跳转仍由 lsp-mode。
;; 缺 grammar 时: M-x my/treesit-install-grammars（需 git + C 编译器）
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (let ((grammar-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (make-directory grammar-dir t)
    (add-to-list 'treesit-extra-load-path grammar-dir))

  (defun my/treesit-lang-for-ts-mode (ts-mode)
    "Map a *-ts-mode symbol to its tree-sitter language symbol."
    (pcase ts-mode
      ('c++-ts-mode 'cpp)
      ('js-ts-mode 'javascript)
      ('tsx-ts-mode 'tsx)
      (_ (intern (string-remove-suffix "-ts-mode" (symbol-name ts-mode))))))

  ;; 仅当 grammar 已装好才 remap，避免 Warning 刷屏
  (dolist (mapping '((bash-mode . bash-ts-mode)
                     (c-mode . c-ts-mode)
                     (c++-mode . c++-ts-mode)
                     (css-mode . css-ts-mode)
                     (go-mode . go-ts-mode)
                     (html-mode . html-ts-mode)
                     (js-mode . js-ts-mode)
                     (javascript-mode . js-ts-mode)
                     (js-json-mode . json-ts-mode)
                     (json-mode . json-ts-mode)
                     (python-mode . python-ts-mode)
                     (rust-mode . rust-ts-mode)
                     (typescript-mode . typescript-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (when (treesit-language-available-p
           (my/treesit-lang-for-ts-mode (cdr mapping)))
      (add-to-list 'major-mode-remap-alist mapping)))

  (defun my/treesit-install-grammars ()
    "Install missing grammars from `treesit-language-source-alist'."
    (interactive)
    (dolist (entry treesit-language-source-alist)
      (let ((lang (car entry)))
        (condition-case err
            (unless (treesit-language-available-p lang)
              (message "Installing tree-sitter grammar: %s ..." lang)
              (treesit-install-language-grammar lang)
              (message "Installed: %s" lang))
          (error (message "Failed %s: %S" lang err)))))
    (message "Done. Restart Emacs to apply remaps.")))

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
  ;; Enable `<s TAB` style easy templates in Org buffers.
  (require 'org-tempo)
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

(defconst my/code-indent-width 8
  "Preferred indentation width for programming modes.")

(defun my/set-code-indent-width ()
  "Apply `my/code-indent-width' to the current programming buffer."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width my/code-indent-width)
  (setq-local standard-indent my/code-indent-width)
  (dolist (sym '(c-basic-offset
                 js-indent-level
                 js-switch-indent-offset
                 typescript-ts-mode-indent-offset
                 treesit-simple-indent-offset
                 rust-indent-offset
                 rust-ts-mode-indent-offset
                 go-ts-mode-indent-offset
                 css-indent-offset
                 sh-basic-offset
                 python-indent-offset))
    (when (boundp sym)
      (set (make-local-variable sym) my/code-indent-width)))
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'lisp-interaction-mode)
    (setq-local lisp-body-indent my/code-indent-width)
    (setq-local lisp-indent-offset my/code-indent-width)))

(add-hook 'prog-mode-hook #'my/set-code-indent-width)

(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (derived-mode-p 'prog-mode)
      (my/set-code-indent-width))))

(defun my/format-buffer ()
  "Format current buffer on demand.

Prefer lsp-mode formatting when available, else reindent the whole buffer."
  (interactive)
  (cond
   ((bound-and-true-p lsp-mode)
    (call-interactively #'lsp-format-buffer))
   (t (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c f") #'my/format-buffer)

;;; -------------------- 补全 UI (Corfu) --------------------
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map global-map
        ("C-<return>" . completion-at-point))
  (:map corfu-map
        ("TAB" . corfu-next)
        ("S-TAB" . corfu-previous)
        ("<backtab>" . corfu-previous)
        ("S-SPC" . corfu-insert-separator)
        ("M-d" . corfu-popupinfo-toggle)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up))
  :custom
  ;; 输入 2 个字符后自动弹出（嫌烦可改回 nil，只用 C-RET）
  (corfu-auto t)
  (corfu-auto-delay 0.12)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  ;; 弹窗尺寸 / 滚动
  (corfu-count 14)
  (corfu-scroll-margin 2)
  (corfu-min-width 28)
  (corfu-max-width 100)
  ;; 右侧文档卡片
  (corfu-popupinfo-delay '(0.25 . 0.08))
  (corfu-popupinfo-max-width 70)
  (corfu-popupinfo-max-height 20)
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; 与 modus-vivendi 协调的配色（深色、选中项更醒目）
  (defun my/corfu-setup-faces ()
    (set-face-attribute 'corfu-default nil
                        :inherit 'default
                        :background "#16161e"
                        :foreground "#c0caf5")
    (set-face-attribute 'corfu-current nil
                        :background "#2a2a3d"
                        :foreground "#7dcfff"
                        :weight 'semi-bold
                        :extend t)
    (set-face-attribute 'corfu-border nil
                        :background "#3d59a1")
    (set-face-attribute 'corfu-bar nil
                        :background "#7aa2f7")
    (set-face-attribute 'corfu-annotations nil
                        :foreground "#565f89"
                        :slant 'italic)
    (when (facep 'corfu-popupinfo)
      (set-face-attribute 'corfu-popupinfo nil
                          :background "#12121a"
                          :foreground "#a9b1d6")))
  (my/corfu-setup-faces)
  ;; 换主题后重刷 Corfu 脸（Emacs 无标准 after-load-theme-hook）
  (advice-add 'load-theme :after
              (lambda (&rest _) (my/corfu-setup-faces))))

;; 候选项左侧类型图标（函数 / 变量 / 模块…）
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))




(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'")

;;; -------------------- LSP (lsp-mode) --------------------
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((rust-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c-ts-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (c++-ts-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (css-ts-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (json-ts-mode . lsp-deferred)
         (js-json-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-snippet t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-auto-guess-root t)
  (lsp-completion-provider :capf)
  (lsp-session-file (expand-file-name "lsp-session-v1" my/var-dir))
  (lsp-server-install-dir (expand-file-name "lsp-server/" my/var-dir))
  (lsp-client-packages
   '(lsp-clangd lsp-javascript lsp-pylsp lsp-rust lsp-java lsp-css))
  :bind (:map lsp-mode-map
              ("C-c r" . lsp-rename)
              ("C-c h" . lsp-describe-thing-at-point)
              ("C-c d" . lsp-ui-doc-glance)
              ("C-c f" . my/format-buffer))
  :config
  (require 'lsp-pyright nil t)
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t))

(use-package lsp-pyright
  :ensure t
  :after lsp-mode)

(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)))

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


;; === vterm terminal ===
(use-package vterm
  :ensure t
  :custom
  ;; :custom 只接受 (变量 值),不能写 setq——之前那样写等于没生效。
  (vterm-kill-buffer-on-exit t)
  (vterm-shell "/bin/zsh")
  :config
  ;; 平滑滚动是全局行为(不是 vterm 变量),放 :config 里真正开启。
  (pixel-scroll-precision-mode 1))


;; === Git ===
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-flyspell))

;; 编辑时在 fringe 实时显示 VCS 改动行。和 magit 互补:
;; magit 管暂存/提交,diff-hl 管「此刻这个文件改了哪几行」的即时可视。
(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         ;; magit 操作前后刷新,避免 diff-hl 标记与实际状态脱节。
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

;; 高亮 TODO / FIXME / HACK / NOTE 等关键字,一眼看到代码里的待办。
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))


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

;; 智能行首:C-a 在「首个非空白字符」和「真正的行首」之间切换(借鉴 alynx)。
(defun my/smarter-move-beginning-of-line (arg)
  "Move to the first non-whitespace char, or to BOL if already there.
With ARG, move forward ARG-1 lines first."
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                #'my/smarter-move-beginning-of-line)

;; C/C++ 头文件↔源文件互跳(内置 find-sibling-file, Emacs 29+)。
;; 只打开已存在的兄弟文件;.h 会同时匹配 .c/.cpp/.cc。
(setq find-sibling-rules
      '(("\\([^/]+\\)\\.c\\'" "\\1.h")
        ("\\([^/]+\\)\\.h\\'" "\\1.c" "\\1.cpp" "\\1.cc")
        ("\\([^/]+\\)\\.cpp\\'" "\\1.h" "\\1.hpp")
        ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp")))
(global-set-key (kbd "C-c M-f") #'find-sibling-file)







;; ==== key ====
(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "<f5>") 'my/project-run)

;; ==== theme ====
;; 让主题自带的补全/匹配高亮更醒目（与 Corfu 配合）
(setq modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold accented intense))
        (popup . (accented intense))))
(load-theme 'modus-vivendi t)
(when (fboundp 'my/corfu-setup-faces)
  (my/corfu-setup-faces))

;; 放在最后一行
;; LSP 性能：提高子进程单次读取上限，减轻 JSON 推送卡顿。
(setq read-process-output-max (* 1024 1024))
;; 启动后把 GC 阈值提到对 LSP 更友好的值。
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))
