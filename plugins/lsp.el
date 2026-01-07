(defun my-force-inject-path ()
  "Force inject widely used paths to Emacs environment."
  (let ((new-paths '("/opt/homebrew/bin"          ; brew 安装的 ruff, node 等
                     "/Users/albamkin/.local/bin" ; pipx 安装的 pylsp
                     "/usr/local/bin")))          ; 旧版兼容
    (dolist (path new-paths)
      ;; 1. 添加到 Emacs 内部查找列表 (exec-path)
      (add-to-list 'exec-path path)
      ;; 2. 添加到子进程环境变量 (PATH)
      (setenv "PATH" (concat (getenv "PATH") ":" path)))))

(my-force-inject-path)



(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
	 (java-mode . lsp-deferred)
	 (rust-ts-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-completion-show-detil t)
  (lsp-completion-show-kind t)
  (lsp-completion-enable-additional-text-edit t)
:commands (lsp lsp-deferred))

(use-package lsp-ui   
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-update-mode t ))

(use-package treesit
  :ensure nil  ; 内置功能，无需下载
  :mode (("\\.rs\\'" . rust-ts-mode)       ; 遇到 .rs 文件，自动使用 rust-ts-mode (Tree-sitter 版)
         ("\\.toml\\'" . toml-ts-mode))    ; 遇到 .toml 文件，自动使用 toml-ts-mode
  :config
  ;; 告诉 Emacs 去哪里下载这些语言的语法定义
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")     ;; <--- 这里解决了你的问题
          (toml "https://github.com/tree-sitter/tree-sitter-toml")     ;; <--- Rust 开发必备
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; 一个方便的小函数，一次性安装列表里所有的语法
(defun my-install-all-treesit-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist`."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ;; 自动触发补全 
  (corfu-auto-delay 0.1)         ;; 自动触发延迟 
  (corfu-auto-prefix 1)          ;; 1个字符开始补全
  
  (corfu-cycle t)                ;; 允许循环选择 
  (corfu-preselect 'prompt)      ;; 默认不选中任何内容
                                 ;; 这样可以直接回车换行，而不会误选补全项
  
  (corfu-quit-no-match 'separator) ;; 自动退出补全窗口
  (corfu-quit-at-boundary nil)     ;; 设为 nil 可以让你连续补全参数
  
  ;; === UI 细节 ===
  (corfu-preview-current nil)    ;; 关闭“预览” 
  (corfu-echo-documentation 0.25);; 选中候选项 0.25秒后，在 minibuffer 显示文档简介
  
  :init
  (global-corfu-mode)            ;; 全局开启
  (corfu-history-mode)           ;; 开启历史记录，把常用的补全项往前排
  (corfu-popupinfo-mode)         ;; 开启侧边文档弹窗 (类似 VSCode 的参数提示)
                                 ;; 选中一个函数时，右边会弹出一个框显示它的 docstring
  
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator) ;; 过滤分割符
                                         ;; 比如输入 "py st" 匹配 "python_string"
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package orderless
  :ensure t
  :custom
  ;; 设置补全样式：
  ;; 1. orderless: 空格分隔的模糊匹配
  ;; 2. basic: 基础的前缀匹配 (作为保底)
  (completion-styles '(orderless basic))
  
  ;; 覆盖特定类别的补全策略
  ;; 比如查找文件 (file) 时，我们习惯用 partial-completion (比如 /u/b/z 可以补全 /usr/bin/zsh)
  (completion-category-overrides '((file (styles partial-completion))))
  
  ;; 可以在过滤时使用 & ! 等符号
  ;; 比如 "py !test" 表示包含 py 但不包含 test
  (completion-category-defaults nil))


(use-package cape
  :ensure t
  :init
  ;; 将 Cape 的后端加入到补全列表里
  ;; 这样哪怕 LSP 还没启动，你也能补全文件路径和 Buffer 里的单词
  (add-to-list 'completion-at-point-functions #'cape-file)    ;; 补全路径 /usr/bin/...
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; 补全当前文件里的单词
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; 补全语言关键字
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol) ;; 如果你写 Elisp 可以开这个
 )


(provide 'lsp)
