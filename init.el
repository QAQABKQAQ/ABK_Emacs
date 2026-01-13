
;; 菜单栏
(menu-bar-mode -1)
;; 工具栏
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq select-enable-primary nil)
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill nil)
(global-set-key (kbd "C-c c") 'compile)    ;; Compile
;; C-g看错误
;; (toggle-debug-on-quit)
;; emacs的自定义设置转移到自定义文件中
(setq custom-file "~/.config/emacs/.emacs.custom.el")

(add-to-list 'load-path
	     (expand-file-name ( concat user-emacs-directory "plugins")))  ;; ~/.
;; 1. 加速启动 (暂时调大垃圾回收阈值)
(setq gc-cons-threshold most-positive-fixnum)

;; 2. 定义软件源
(require 'package)
(setq package-archives '(("gnu". "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize);; 显式启用



(setq url-queue-timeout 5) ;; 网络超时设短一点，别死等
(setq package-check-signature nil)


;; unless 条件为假，执行代码
;; 如果没有安装use-package,下面执行安装
(unless (package-installed-p 'use-package)
  (message "首次启动：正在联网安装 use-package，请耐心等待...")
  (package-refresh-contents)  ;; 只有这一种情况才允许启动时刷新
  (package-install 'use-package))

(require 'use-package)





(require 'init-package)



(require 'init-config)
