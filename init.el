
;;===================
;; emacs init       |
;; Author: Ephemera |
;;===================




(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; =====ENV=====

;; 禁用系统警告音（macOS上会从内置扬声器发出）
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

(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")))
  :config
  (let ((grammar-dir (expand-file-name "tree-sitter" user-emacs-directory)))
    (unless (file-directory-p grammar-dir)
      (make-directory grammar-dir t))
    (add-to-list 'treesit-extra-load-path grammar-dir))

  (setq major-mode-remap-alist
        '((js-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (tsx-mode . tsx-ts-mode)))

  ;; Rust 走 tree-sitter,但仅当 rust 语法已安装时才重映射;
  ;; 否则回退到经典 rust-mode,避免语法缺失时打开 .rs 直接报错。
  ;; 安装语法后重启即可自动升级: M-x my/treesit-install-grammars
  (when (treesit-language-available-p 'rust)
    (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode)))

  (defun my/treesit-install-grammars ()
    "Install missing tree-sitter grammars for JS, TS, TSX and Rust."
    (interactive)
    (dolist (lang '(javascript typescript tsx rust))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))


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

Prefer lsp-bridge formatting, then Eglot, else reindent the whole buffer with
the current major mode's indentation rules."
  (interactive)
  (cond
   ((bound-and-true-p lsp-bridge-mode)
    (lsp-bridge-code-format))
   ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (eglot-format-buffer))
   (t (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c f") #'my/format-buffer)

(defvar my/eglot-format-on-save-modes nil
  "Major modes where Eglot may format automatically before saving.

Keep this nil by default so saving never rewrites code unless a language is
explicitly opted in. Use `C-c f' in an Eglot buffer to format manually.")

(defun my/eglot-format-on-save ()
  "Format the current buffer before saving when its mode is explicitly opted in."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p)
             (memq major-mode my/eglot-format-on-save-modes))
    (eglot-format-buffer)))

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


;; ==== LSP: lsp-bridge (独立进程异步客户端,借鉴 alynx) ====
;; 从 eglot 迁移到 lsp-bridge:LSP 跑在独立 Python 进程里,永不阻塞主线程;
;; 补全用它自带的 acm(输入时自动弹出,不再是 corfu 的手动 C-return)。
;; 旧的 eglot / eldoc-box 配置已移除,需要回滚见 git 历史或 doc/alynx-精读.org。
;;
;; 依赖(均已 gitignore,换机器需按下面命令重建):
;;   - 代码:    site-lisp/lsp-bridge/        (git clone)
;;   - Python:  var/lsp-bridge-venv/         (Python 3.13 venv)
;;   - 语言服务器: rust-analyzer / clangd / jdtls / pyright /
;;               typescript-language-server (需在 PATH,lsp-bridge 自动探测)
;; 重建 Python 环境:
;;   git clone --depth 1 https://github.com/manateelazycat/lsp-bridge.git \
;;     ~/.config/emacs/site-lisp/lsp-bridge
;;   python3.13 -m venv ~/.config/emacs/var/lsp-bridge-venv
;;   ~/.config/emacs/var/lsp-bridge-venv/bin/pip install \
;;     epc orjson sexpdata six watchdog rapidfuzz

;; yasnippet: lsp-bridge 用它展开补全里的代码片段(是 lsp-bridge 的依赖)。
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package lsp-bridge
  :ensure nil                          ; 不在 MELPA,作为本地代码从 site-lisp/ 加载
  :load-path "site-lisp/lsp-bridge/"
  :hook ((rust-mode . lsp-bridge-mode)
         (rust-ts-mode . lsp-bridge-mode)
         (c-mode . lsp-bridge-mode)
         (c++-mode . lsp-bridge-mode)
         (java-mode . lsp-bridge-mode)
         (python-mode . lsp-bridge-mode)
         (python-ts-mode . lsp-bridge-mode)
         (js-mode . lsp-bridge-mode)
         (js-ts-mode . lsp-bridge-mode)
         (typescript-ts-mode . lsp-bridge-mode)
         (tsx-ts-mode . lsp-bridge-mode))
  :init
  ;; 后端用的 Python:优先隔离的 venv,缺失则回退系统 python3
  ;;(换机器时 var/ 不同步,回退保证至少不报错)。
  (setq lsp-bridge-python-command
        (let ((venv (expand-file-name "var/lsp-bridge-venv/bin/python"
                                      user-emacs-directory)))
          (if (file-exists-p venv) venv "python3")))
  :custom
  ;; 悬停时显示诊断(错误/警告)。
  (lsp-bridge-enable-hover-diagnostic t)
  ;; 函数签名用子帧显示在光标处,不占用 echo area。
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-signature-show-with-frame-position 'point)
  ;; 补全弹窗里不塞文档(太吵),文档用 C-c h / C-c d 手动看。
  (acm-enable-doc nil)
  (acm-enable-tabnine nil)
  :bind (:map lsp-bridge-mode-map
              ;; 沿用你原来的键位,命令换成 lsp-bridge 等价物。
              ("M-." . lsp-bridge-find-def)               ; 跳转定义
              ("M-," . lsp-bridge-find-def-return)        ; 跳回
              ("M-?" . lsp-bridge-find-references)        ; 查找引用
              ("C-c r" . lsp-bridge-rename)               ; 重命名符号
              ("C-c h" . lsp-bridge-popup-documentation)  ; 查看文档
              ("C-c d" . lsp-bridge-popup-documentation)  ; 同上(保留旧习惯)
              ("C-c f" . my/format-buffer)                ; 格式化
              ("M-n" . lsp-bridge-diagnostic-jump-next)   ; 下一个诊断
              ("M-p" . lsp-bridge-diagnostic-jump-prev)   ; 上一个诊断
              ("C-c ! l" . lsp-bridge-diagnostic-list))   ; 诊断列表
  :config
  ;; lsp-bridge 缓冲用 acm 补全,顺手关掉 corfu,免得 C-return 弹出空补全。
  (add-hook 'lsp-bridge-mode-hook
            (lambda ()
              (when (bound-and-true-p corfu-mode) (corfu-mode -1)))))






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
        (setq flymake-fringe-indicator-position 'left-fringe)
        ;; 末尾显示错位
        ;; (setq flymake-show-diagnostics-at-end-of-line 'short)
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

;; ==== mail ====
(use-package mu4e
  :ensure nil
  :load-path "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e"
  :bind (("C-c m" . mu4e))
  :init
  (require 'seq)

  (defconst my/mu4e-proton-domains
    '("albamkin.top" "proton.me" "protonmail.com" "pm.me"))

  (defun my/mu4e-message-text (msg field)
    (when msg
      (let ((value (mu4e-message-field msg field)))
        (cond
         ((stringp value) value)
         (value (prin1-to-string value))
         (t "")))))

  (defun my/mu4e-compose-from ()
    (when (and (derived-mode-p 'message-mode)
               (fboundp 'message-field-value))
      (or (ignore-errors (message-field-value "From")) "")))

  (defun my/mu4e-proton-message-p (&optional msg)
    (let ((text (downcase (mapconcat #'identity
                                     (list (or (my/mu4e-compose-from) "")
                                           (my/mu4e-message-text msg :maildir)
                                           (my/mu4e-message-text msg :from)
                                           (my/mu4e-message-text msg :to)
                                           (my/mu4e-message-text msg :cc))
                                     " "))))
      (or (string-prefix-p "/albamkin-top/" (my/mu4e-message-text msg :maildir))
          (seq-some (lambda (domain)
                      (string-match-p (concat "@" (regexp-quote domain) "\\_>") text))
                    my/mu4e-proton-domains))))

  (defun my/mu4e-folder (kind &optional msg)
    (let ((proton (my/mu4e-proton-message-p msg)))
      (pcase kind
        ('drafts (if proton "/albamkin-top/Drafts" "/gmail/[Gmail]/草稿"))
        ('sent   (if proton "/albamkin-top/Sent" "/gmail/[Gmail]/已发邮件"))
        ('trash  (if proton "/albamkin-top/Trash" "/gmail/[Gmail]/已删除邮件"))
        ('refile (if proton "/albamkin-top/Archive" "/gmail/[Gmail]/所有邮件")))))

  (defun my/mu4e-sent-messages-behavior ()
    (if (my/mu4e-proton-message-p) 'delete 'sent))

  (setq mu4e-maildir "~/Maildir"
        mu4e-get-mail-command "mbsync gmail albamkin-top"
        mu4e-update-interval nil
        mu4e-change-filenames-when-moving t
        mu4e-view-auto-mark-as-read nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-headers-results-limit 200
        mu4e-drafts-folder (lambda (msg) (my/mu4e-folder 'drafts msg))
        mu4e-sent-folder (lambda (msg) (my/mu4e-folder 'sent msg))
        mu4e-trash-folder (lambda (msg) (my/mu4e-folder 'trash msg))
        mu4e-refile-folder (lambda (msg) (my/mu4e-folder 'refile msg))
        mu4e-sent-messages-behavior #'my/mu4e-sent-messages-behavior
        ;; 常用快捷入口
        mu4e-maildir-shortcuts
        '((:maildir "/gmail/INBOX" :name "Inbox" :key ?i)
          (:maildir "/gmail/Later" :name "Later" :key ?l)
          (:maildir "/gmail/[Gmail]/已发邮件" :name "Sent" :key ?s)
          (:maildir "/gmail/[Gmail]/草稿" :name "Drafts" :key ?d)
          (:maildir "/gmail/[Gmail]/已删除邮件" :name "Trash" :key ?t)
          (:maildir "/gmail/[Gmail]/所有邮件" :name "Archive" :key ?a)
          (:maildir "/albamkin-top/INBOX" :name "Top Inbox" :key ?I)
          (:maildir "/albamkin-top/Sent" :name "Top Sent" :key ?S)
          (:maildir "/albamkin-top/Drafts" :name "Top Drafts" :key ?D)
          (:maildir "/albamkin-top/Trash" :name "Top Trash" :key ?T)
          (:maildir "/albamkin-top/Archive" :name "Top Archive" :key ?A))
        ;; 主界面搜索书签
        mu4e-bookmarks
        '((:name "Inbox" :query "maildir:/gmail/INBOX" :key ?i)
          (:name "Top Inbox" :query "maildir:/albamkin-top/INBOX" :key ?I)
          (:name "Unread" :query "flag:unread" :key ?u)
          (:name "Archive" :query "maildir:/gmail/[Gmail]/所有邮件" :key ?a)
          (:name "Top Archive" :query "maildir:/albamkin-top/Archive" :key ?A)
          (:name "Sent" :query "(maildir:/gmail/[Gmail]/已发邮件 OR maildir:/albamkin-top/Sent)" :key ?s)
          (:name "Top Sent" :query "maildir:/albamkin-top/Sent" :key ?S)))
  :config
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy 'ask-if-none
        mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :match-func (lambda (msg)
                        (and msg
                             (string-prefix-p "/gmail/"
                                              (my/mu4e-message-text msg :maildir))))
          :vars '((user-mail-address . "albamkin@gmail.com")
                  (user-full-name . "albamkin")))
         (make-mu4e-context
          :name "albamkin.top"
          :match-func #'my/mu4e-proton-message-p
          :vars '((user-mail-address . "albamkin@albamkin.top")
                  (user-full-name . "albamkin"))))))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :config
  (setq user-full-name "albamkin"
        user-mail-address "albamkin@gmail.com"
        send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        sendmail-program (or (executable-find "https-mail-relay-sendmail")
                             (expand-file-name "~/.local/bin/https-mail-relay-sendmail"))
        message-sendmail-extra-arguments '("--read-envelope-from" "-t")
        message-sendmail-f-is-evil t
        mail-specify-envelope-from t
        mail-envelope-from 'header))
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
(load-theme 'modus-vivendi t)

;; 放在最后一行
;; LSP/eglot 性能：单次从子进程读取的最大字节数。默认仅 4KB，
;; LSP server 一次会推送上百 KB 的 JSON，过小会让 Emacs 反复进出
;; 读循环造成卡顿。调到 1MB 是 eglot 官方推荐做法。
(setq read-process-output-max (* 1024 1024))
;; 启动后把 GC 阈值恢复到一个对 LSP 友好的值。LSP 大量分配内存，
;; 16MB 在重负载下 GC 过于频繁，提到 100MB 减少卡顿。
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))
 
