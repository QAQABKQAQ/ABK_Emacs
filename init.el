
;; custom.el 文件，不然emacs总是把设置存在init里面，不好管理，还不好看
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t t) ;; 加载custom

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; 由于emacs在terminal 下无法粘贴到剪贴板所有写两个函数用来实现
;; (defun abk/clipboard-copy (text)
;;   (with-temp-buffer ;; 以下内容使用一个temp临时buffer
;;   (insert text)
;;   (call-process-region ;; 对区域region 对用进程 call process 
;;    (point-min)(point-max) "/usr/bin/pbcopy")) ;; 将选中区域发给pbcopy
;;   )
(defun abk/clipboard-copy (text)
  (with-temp-buffer
    (insert text)
    (call-process-region
     (point-min) (point-max) "/usr/bin/pbcopy")))
;; 同样，粘贴可以这么写
;; Similarly, the paste can be writen like this
;; (defun abk/clipboard-paste()
;;   (with-temp-buffer
;;     (insert (shell-command-to-string "/usr/bin/pbpaste"))))
(defun abk/clipboard-paste ()
  (shell-command-to-string "/usr/bin/pbpaste"));; 因为直接调用，所以不用insert

(setq interprogram-cut-function #'abk/clipboard-copy
      interprogram-paste-function #'abk/clipboard-paste)

(use-package lsp-mode
  :ensure t
  :init ;; init 插件加载之前就需要进行执行
  :custom
  (lsp-inlay-hint-enable t)
  (setq lsp-keymap-prefix "C-c l") ;; 将lsp按键前置改为C-c l
  :hook(
	(lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)

  )

(use-package lsp-ui
  :ensure t
  ;; lsp-ui的变量赋值
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  ;; lsp-ui的快捷键
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  
  )

(use-package svelte-mode
  :ensure t
  :mode "\\.svelte\\'"
  :hook (svelte-mode . lsp-deferred))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook ((rust-mode . lsp-deferred)
	 (rust-ts-mode . lsp-deferred)))
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))
(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :demand t ;; 立即加载
  :config
  (require 'tree-sitter-hl);; 启用语法高亮
  ;; 将diff-mode 从 tree-sitter-major-mode-language-table 哈希里面删除
  (remhash 'diff-mode tree-sitter-major-mode-language-table)
  ;; 钩子: 缓冲区触发tree-sitter-after-on-hook后开启tree-sitter-hl-mode高亮
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; (全局)为支持tree-sitter的缓冲区启用
  (global-tree-sitter-mode 1)
  )

(require 'tree-sitter)
(require 'tree-sitter-langs)
(require 'tree-sitter-hl)
(global-tree-sitter-mode 1)
(tree-sitter-require 'rust)
;; 语言解析开始，钩子触发高亮
(add-hook 'tree-sitter-after-on-hook
	   #'tree-sitter-hl-mode)

;; 跳转保存
(use-package flycheck
  :ensure t
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  :config
  (global-company-mode 1)
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'turn-off-flyspell)) ;; 关闭拼写检查


(use-package vterm
  :ensure t)

(use-package ace-window
  :ensure t)

(use-package kanagawa-themes
  :ensure t
  :config
  (load-theme 'kanagawa-dragon t))

;;; 居中留白
(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 100)    ; 正文宽度为 100 列
  (olivetti-style nil)
  (olivetti-mode-on-hook nil)
  :hook ((prog-mode . olivetti-mode)
         (text-mode . olivetti-mode)
         (dired-mode . olivetti-mode))
  :bind ("C-c z" . olivetti-mode))

;; 候选结果竖向显示
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

;; 多关键词匹配
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; 查找与跳转
(use-package consult
  :ensure t
  :bind (("C-s"   . consult-line)     ; 查当前文件内容
         ("C-c s" . consult-ripgrep) ; 查整个项目内容
         ("C-c i" . consult-imenu)   ; 查当前文件的函数等定义
         ("C-x b" . consult-buffer))) ; 查找并切换缓冲区
