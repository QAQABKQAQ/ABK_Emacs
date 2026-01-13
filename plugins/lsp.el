;;; lsp.el --- LSP Configuration  -*- lexical-binding: t; -*-

;; --- 1. 性能与环境优化 ---
(setq gc-cons-threshold (* 100 1024 1024)) ;; 100MB GC 阈值
(setq read-process-output-max (* 1024 1024)) ;; 1MB 读取缓存

(defun my-force-inject-path ()
  "强制将常用路径注入到 Emacs 的 exec-path 和环境变量 PATH 中。"
  (interactive)
  (let ((new-paths '("/opt/homebrew/bin/"          ; M1/M2 Mac
                     "/usr/local/bin"              ; Intel Mac / Linux
                     "/Users/albamkin/.local/bin"  ; Python 工具
                     "/Users/albamkin/.cargo/bin"  ; Rust 工具
                     )))
    (dolist (path new-paths)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat (getenv "PATH") ":" path)))))

(my-force-inject-path)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; --- 2. 补全行为优化 ---
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))
;; --- 3. LSP Mode 核心 ---

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")

  (setq lsp-disabled-clients '(semgrep-ls))
  ;;(setq lsp-semgrep-ls-enable nil)      ;; 彻底禁用 Semgrep 
  (setq lsp-enable-file-watchers nil)   ;; 彻底禁用文件监视 
  (setq lsp-log-io nil)                 ;; 禁用 IO 日志 (防止大量 JSON 数据写入导致卡顿)
  (setq lsp-idle-delay 0.5)             ;; 延迟 0.5 秒再请求 (解决 "Timeout" 报错)
  (setq lsp-completion-provider :none)  ;; 配合 Corfu 使用

  :hook 
  ((python-mode . lsp-deferred)
   (java-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (sql-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  
  :custom
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-completion-enable-additional-text-edit t)
  
  (lsp-headerline-breadcrumb-enable nil) ;; 关闭面包屑 
  (lsp-lens-enable nil)                  ;; 新增：关闭代码上方的
  
  :commands (lsp lsp-deferred))





(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-code-actions t))

;; --- 4. Java 专门配置 (已修正参数) ---
(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-java-java-path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin/java")
  ;; 这里的参数必须干净，不能有非法字符或不存在的 -G1
  (setq lsp-java-vmargs
        '("-XX:+UseG1GC"
          "-XX:G1HeapRegionSize=4M"
          "-Xmx2G"
          "-Xms100m"
          "--add-modules=ALL-SYSTEM"
          "--add-opens" "java.base/java.util=ALL-UNNAMED"
          "--add-opens" "java.base/java.lang=ALL-UNNAMED")))

;; --- 5. Tree-sitter ---
(use-package treesit
  :ensure nil
  :mode (("\\.rs\\'" . rust-ts-mode)
         ("\\.toml\\'" . toml-ts-mode))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(defun my-install-all-treesit-grammars ()
  "安装列表中尚未安装的所有 Tree-sitter 语法。"
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (message "Installing grammar for %s..." lang)
        (treesit-install-language-grammar lang)))))

;; --- 6. Corfu 补全界面 ---
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2) ;; 输2个字符才弹
  (corfu-preview-current nil) ;; 不要预先上屏，看着清爽

  
  
  
  
  (corfu-quit-no-match 'separator) 
  (corfu-quit-at-boundary nil)     
  
  :init
  (global-corfu-mode)
  
  :bind
  (:map corfu-map

        

        ("SPC" . nil) 
        

        ("S-SPC" . corfu-insert-separator)
        
       
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert))) 


(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; --- 7. Cape (防止干扰 LSP) ---
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

(use-package cape
  :ensure t
  :init
  
  (add-to-list 'completion-at-point-functions #'cape-file)
  :config
  (global-set-key (kbd "C-c p") 'cape-dabbrev))

(provide 'lsp)
