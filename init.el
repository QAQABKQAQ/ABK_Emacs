
;; 原生光标移动
;; C-f/b 单个字符移动 h/l
;; M-f/b 单个单词移动 e/b
;; M-a/e 语句块移动
;; M-{/} 跳过整个空行分割的块
;; C-M-f/b 括号移动,比如从`(`跳到`)`
;; C-M-n/p 括号跳转

;; === 插件 ===
;; C-' avy 查找字符移动，相当于nvim - flash
;; M-g l 跳转到行
;; M-g w 跳转到单词的首字母
;; M-g e 把远处的行移动过来
;; M-g c 把远处的行拷过来
;; k -> 杀掉那一行
;; y -> 复制那一行
;; t -> 把那一行瞬移到我这里

;; C-s 查找行字符
;; C-x b 查找buffer
;; M-g o `consult-outline` 大范围跳转，原理为将标题，类名，方法名作为了锚点
;; C-c f 查找文件
;; C-c g 查找单词

;; font :
;; Monaspace Neon medium normal




(setq custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory))
(load custom-file t t)


(setq inhibit-startup-message t)




(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sql . t)
   (shell . t)  
   (python . t) 
   (js . t)     
   ))


(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
      
(require 'package)
(setq package-enable-at-startup nil);;取消自启

(setq package-archives '(("gnu". "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa-tuna"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize);; 显式启用

;; Emacs29以上自带use-package,直接require就可以使用
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'cl-lib)
(require 'use-package)





;; === theme ===
(use-package catppuccin-theme
  :ensure t
  :demand t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  (load-theme 'catppuccin t)
  )




(use-package  multiple-cursors
  :ensure t
  :bind (
   ("C-S-c C-S-c" . mc/edit-lines);; 选中区域转换可编辑行
   ("C->" . mc/mark-next-like-this);; 向上延伸光标
   ("C-<" . mc/mark-previous-like-this);; 向上延伸光标
   ("C-c C-<" . mc/mark-all-like-this));; 把当前选中的单词前面都加上光标
  )


(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t)) 



(use-package vertico-posframe
  :ensure t
  :after vertico
  :config
  (vertico-posframe-mode 1)

  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))

  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))


(use-package consult
  :ensure t
  :bind (
         ("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-c f" . consult-find)
         ("C-c g" . consult-ripgrep)
         ("M-g o" . consult-outline))
  :config
  (setq consult-preview-key 'any))


(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))



;; 尝试插件但是不下载到本地
(use-package try
  :ensure t)

;; 指令重启Emacs
(use-package restart-emacs
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package seq
  :ensure t)


(use-package avy
  :ensure t
  :bind
  (("C-'" . avy-goto-char-timer)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)  ; 新增：跳到单词首字母，比 timer 更精准
   ("M-g e" . avy-move-line)    ; 新增：把远处的行移过来
   ("M-g c" . avy-copy-line))   ; 新增：把远处的行拷过来
  :config
  (setq avy-background t)
  (setq avy-timeout-seconds 0.5)
  (setq avy-all-windows t)      ; 允许跨窗口跳转
  
  ;; --- 高级功能：Dispatch (远程操作) ---
  (defun my/avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun my/avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end))
      (message "Copied line"))
    t)

  (defun my/avy-action-yank-whole-line (pt)
    (my/avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  ;; 绑定：
  ;; k -> 杀掉那一行
  ;; y -> 复制那一行
  ;; t -> 把那一行瞬移到我这里
  (setq avy-dispatch-alist
        '((?k . my/avy-action-kill-whole-line)
          (?y . my/avy-action-copy-whole-line)
          (?t . my/avy-action-yank-whole-line)))
  )


;; ==ctags==
(use-package citre
  :ensure t
  :defer t
  :init
  (require 'citre-config)
  (citre-auto-enable-citre-mode)
  :bind
  (
   ("C-x c j" . citre-jump)
   ("C-x c p" . citre-peek)
   ("C-x c u" . citre-update-this-tags-file)
   )
  :config
  (setq citre-use-project-root-when-creating-tags t)
  (setq citre-prompt-language-for-ctags-command t )
  (setq citre-peek-use-icons t)
  (add-hook 'after-save-hook 
            (lambda () 
              (when (and (bound-and-true-p citre-mode)
                         (citre-get-tags-file-path))
                (citre-update-this-tags-file))))
  
  )






;; ===LSP===
;; === Part 1: Eglot (只负责连接 LSP 服务) ===
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure) ; 在所有编程语言中启动
  :config
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider)))

(use-package eglot-java
    :ensure t
    :hook (java-mode . eglot-java-mode)
    :init
    (setq eglot-java-server-install-dir (expand-file-name "eclipse.jdt.ls" user-emacs-directory))
    (setenv "JAVA_HOME" "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home")
        (setenv "PATH" (concat "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk/Contents/Home/bin")
    )

(use-package nerd-icons
	      :ensure t)
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


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


  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))


(use-package cape
  :ensure t
  :init
  ;; 全局开启文件路径补全 (比如在 scratch 或 git commit 中)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :bind
  (:map global-map
	("<M-return>" . completion-at-point))
  :config
  (setq corfu-quit-no-match 'separator)
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point ; 1. 智能代码补全 (Eglot)
		       #'citre-completion-at-point
                       #'cape-dabbrev              ; 2. 单词补全 (Cape)
                       #'cape-file))))             ; 3. 路径补全 (Cape)
  :hook (eglot-managed-mode . my/eglot-capf))


 (use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; 终端
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  (add-hook 'vterm-mode-hook
	    (lambda()
	      (display-line-numbers-mode -1)
	      (hl-line-mode -1)
	      (corfu-mode -1)))
  (defun my/toggle-vterm()
    (interactive)
    (let ((buffer-name "*vterm*"))
      (if (equal (buffer-name) buffer-name)
	  (switch-to-buffer (other-buffer (current-buffer) 1))
	(if (get-buffer buffer-name)
	    (switch-to-buffer buffer-name)
	  (vterm buffer-name)))))
  :bind
  (("C-`" . my/toggle-vterm)))




;; 设置字体
;; 英文字体:https://monaspace.githubnext.com/
;; 中文字体:https://github.com/lxgw/LxgwWenKai
(defun my/apply-font-config ()
  (interactive)
  (when (display-graphic-p)

    (set-face-attribute 'default nil
			:family "Monaspace Neon"
			:height 140
			:weight 'normal
			)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset (font-spec :family "LXGW WenKai Mono")))
    (setq face-font-rescale-alist '(("LXGW WenKai Mono" . 1.25))))
  )
  

(add-hook 'after-init-hook #'my/apply-font-config)
(add-hook 'window-setup-hook #'my/apply-font-config)
(add-hook 'server-after-make-frame-hook #'my/apply-font-config)


(my/apply-font-config)

