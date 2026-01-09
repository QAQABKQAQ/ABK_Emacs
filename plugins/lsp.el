
;; --- 1. 环境变量与路径注入 ---
;; 许多 LSP 服务器 (如 pyright, gopls) 依赖系统 PATH 找到可执行文件。
;; MacOS GUI 启动时往往读不到 .zshrc/.bashrc 的 PATH，所以需要手动注入。

(defun my-force-inject-path ()
  "强制将常用路径注入到 Emacs 的 exec-path 和环境变量 PATH 中。"
  (interactive)
  (let ((new-paths '("/opt/homebrew/bin/"          ; Homebrew (M1/M2/M3 Mac)
                     "/usr/local/bin"              ; Intel Mac / Linux
                     "/Users/albamkin/.local/bin"  ; Pipx / Python工具
                     "/Users/albamkin/.cargo/bin"  ; Rust 工具链
                     )))
    (dolist (path new-paths)
      ;; 1. 告诉 Emacs 内部去这些地方找命令
      (add-to-list 'exec-path path)
      ;; 2. 告诉 Emacs 启动的子进程 (LSP Server) 去这些地方找命令
      (setenv "PATH" (concat (getenv "PATH") ":" path)))))

;; 立即执行路径注入
(my-force-inject-path)

;; 从 Shell 读取环境变量 (作为双重保险)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; --- 2. Emacs 内置补全行为调优 ---
(use-package emacs
  :custom
  ;; TAB 键行为：总是尝试补全。
  ;; 如果没有补全项，再执行缩进。这是现代编辑器的标准行为。
  (tab-always-indent 'complete)

  ;; Emacs 30+: 关闭默认的 Ispell (拼写检查) 补全干扰代码补全
  (text-mode-ispell-word-completion nil)

  ;; 优化 M-x 命令列表：隐藏那些不适用于当前模式的命令
  ;; (例如在纯文本模式下不显示只属于 C++ 模式的命令)
  (read-extended-command-predicate #'command-completion-default-include-p))


;; --- 3. LSP Mode (核心语言协议) ---
(use-package lsp-mode
  :ensure t
  :init
  ;; 设置 LSP 的快捷键前缀为 C-c l
  (setq lsp-keymap-prefix "C-c l")
  
  :hook 
  ;; 为以下编程语言自动开启 LSP
  ((python-mode . lsp-deferred)
   (java-mode . lsp-deferred)
   (rust-ts-mode . lsp-deferred)
   (sql-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   ;; 开启 which-key 集成，按下 C-c l 后会有提示
   (lsp-mode . lsp-enable-which-key-integration))

  :custom
  ;; 开启 UI 细节
  (lsp-completion-show-detail t)  ;; 显示补全详情
  (lsp-completion-show-kind t)    ;; 显示类型 (Function, Variable)
  
  ;; 允许 LSP 修改文本 (例如自动引入 import)
  (lsp-completion-enable-additional-text-edit t)
  
  ;; 性能优化：关闭部分冗余功能 (由 Corfu 接管)
  (lsp-headerline-breadcrumb-enable nil) ;; 关闭顶部的面包屑导航 (太占空间且容易卡)
  
  :commands (lsp lsp-deferred))

;; LSP UI (悬浮提示、侧边诊断)
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)                 ;; 开启悬浮文档 (鼠标悬停或按键)
  (lsp-ui-doc-position 'at-point)       ;; 文档显示在光标处
  (lsp-ui-sideline-show-diagnostics t)  ;; 右侧显示错误信息
  (lsp-ui-sideline-show-hover t)        ;; 右侧显示 Hover 信息
  (lsp-ui-sideline-show-code-actions t) ;; 右侧显示可用操作 (如重构)
  )


;; --- 4. Tree-sitter (语法高亮与解析) ---
(use-package treesit
  :ensure nil ;; 内置功能
  :mode (("\\.rs\\'" . rust-ts-mode)     ;; .rs 文件用 rust-ts-mode
         ("\\.toml\\'" . toml-ts-mode))  ;; .toml 文件用 toml-ts-mode
  :config
  ;; 定义语法下载源
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
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; 工具函数：一键安装所有语法
(defun my-install-all-treesit-grammars ()
  "安装列表中尚未安装的所有 Tree-sitter 语法。"
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (message "Installing grammar for %s..." lang)
        (treesit-install-language-grammar lang)))))


;; --- 5. Corfu (补全前端 UI) ---
;; 替代 Company-mode，更加轻量、原生
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)          ;; 自动触发补全
  (corfu-auto-delay 0.1)  ;; 触发延迟 (秒)
  (corfu-auto-prefix 1)   ;; 输入 1 个字符即开始补全
  
  (corfu-cycle t)         ;; 允许循环选择
  (corfu-preselect 'prompt) ;; 默认不选中第一项 (防止误回车上屏)
  
  (corfu-quit-no-match 'separator) ;; 无匹配时自动退出
  (corfu-quit-at-boundary nil)     ;; 允许连续补全参数
  
  ;; UI 设置
  (corfu-preview-current nil)    ;; 关闭“预览” (直接改动文本有时候会乱)
  (corfu-echo-documentation 0.25);; 0.25秒后在底部显示文档
  
  :init
  (global-corfu-mode)      ;; 全局开启
  (corfu-history-mode)     ;; 开启历史记录排序
  (corfu-popupinfo-mode)   ;; 开启侧边文档弹窗 (类似 VSCode)
  
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator) ;; 空格作为分隔符 (Orderless 核心)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))

;; 给补全列表加图标 (Function, Variable, Class 等)
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; --- 6. 辅助补全源 ---

;; Dabbrev: 基于当前 Buffer 内容的文本补全
(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  ;; 忽略一些不适合进行文本补全的 Buffer
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode))

;; Cape: 补全后端扩展 (Completion At Point Extensions)
;; 允许你在写代码时同时补全文件路径、单词等
(use-package cape
  :ensure t
  :init
  ;; 将 Cape 的后端加入到补全列表里
  (add-to-list 'completion-at-point-functions #'cape-file)    ;; 补全文件路径
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;; 补全单词
  (add-to-list 'completion-at-point-functions #'cape-keyword) ;; 补全关键字
  )

(provide 'lsp)
