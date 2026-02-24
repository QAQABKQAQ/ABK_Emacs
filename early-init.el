



(setq package-archives '(("gnu". "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa-tuna"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
			 ("melpa" . "https://melpa.org/packages/")))


;; gc 设置 256MB，init处需要调低
(setq gc-cons-threshold (* 256 1024 1024))

;; 启动页面不必要的组件
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq inhibit-startup-message t)

;; 进程输出最大值
(setq read-process-output-max (* 1024 1024)) ; 1MB




(setq use-short-answers t) ; yes改为y
(setq scroll-setup 1
      scroll-conservatively 10000) ; 平滑滚动

(global-hl-line-mode 1) ;高亮当前行
(delete-selection-mode 1) ; 选中内容后输入可直接替换
(setq-default indent-tabs-mode nil) ; 使用空格缩进 
(setq-default tab-width 4) ; 缩进宽度为 4

(electric-pair-mode t) ;补全括号
(show-paren-mode t) ; 高亮匹配内容
(setq show-paren-delay 0) ; 高亮延迟0







;;====font====

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

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/apply-font-config)
  (add-hook 'window-setup-hook #'my/apply-font-config))


