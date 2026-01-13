;;; lsp.el --- Eglot Manual Completion Config -*- lexical-binding: t; -*-


(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 3 1024 1024))

(defun my-force-inject-path ()

  (interactive)
  (let ((new-paths '("/opt/homebrew/bin"
                     "/usr/local/bin"
                     "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin"))) 
    (dolist (path new-paths)
      (add-to-list 'exec-path path)
      (setenv "PATH" (concat path ":" (getenv "PATH"))))))
(my-force-inject-path)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package corfu
  :ensure t
  :custom
  
  (corfu-auto nil)  
  
  
  (corfu-preselect 'prompt) 
  
  (corfu-preview-current nil)
  (corfu-count 20)
  (corfu-quit-no-match 'separator) 
  (corfu-quit-at-boundary nil)
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . nil)
        ("ESC" . nil)
        ("S-SPC" . corfu-insert-separator)
        ("RET" . corfu-insert)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)))


(use-package orderless
  :ensure t
  :custom  
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))  


(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package eglot
  :ensure t 
  :hook 
  ((java-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure))
  :config
  (setenv "JAVA_TOOL_OPTIONS" 
          (concat "-javaagent:" (expand-file-name "~/.config/emacs/java/lombok.jar")))
  (setq eglot-java-server-install-dir (expand-file-name "~/.config/emacs/.cache/jdtls-eglot"))
  
  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home"))


(use-package eglot-java
  :ensure t
  :hook (java-mode . eglot-java-mode)
  :config
  (setq eglot-java-server-install-dir (expand-file-name "~/.emacs.d/.cache/jdtls-eglot"))
  (setq eglot-java-user-init-opts '(:bundles [])))


(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))


(global-set-key (kbd "C-<tab>") 'completion-at-point)

(provide 'lsp-eglot)
