;;; lsp.el --- Eglot (Lightweight)

;; --- 2. 基础补全优化 ---
(use-package emacs
  :custom
  (tab-always-indent 'complete)
  (read-process-output-max (* 3 1024 1024))) ;; 依然保留 3MB 缓存

;; --- 3. Corfu (必须保留，Eglot 也用它) ---
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2) ;; 稍微快一点，Eglot 反应快
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

;; --- 4. Eglot (主角) ---
(use-package eglot
  :ensure nil ;; Emacs 29 自带，不用下载
  :hook 
  ((java-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure))
  :config
  ;; 优化：只在不需要时才显示文档，防止弹窗卡顿
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  
  ;; 关键：设置 Java 环境变量，让 Eglot 找到你的 Java 21
  (setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home")
  ;; 同时也加到 PATH 里
  (setenv "PATH" (concat "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin"))

;; --- 5. Eglot-Java (帮你自动下载 jdtls) ---
(use-package eglot-java
  :ensure t
  :hook (java-mode . eglot-java-mode)
  :config
  ;; 这里设置 JVM 参数，防止 OOM
  (setq eglot-java-server-install-dir (expand-file-name "~/.emacs.d/.cache/jdtls-eglot"))
  (setq eglot-java-user-init-opts
        '(:bundles [])) ;; 不加载额外的 bundle，纯净启动
  )

(provide 'fast-lsp)
