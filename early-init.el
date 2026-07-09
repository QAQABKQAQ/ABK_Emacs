;;; early-init.el -*- lexical-binding: t; -*-

;; 初始内存优化，减少垃圾回收GC频率
;; 通过降低垃圾回收频率来加快启动速度
;; 默认为800KB
;; 同时lsp-mode 也需要大量空间
;; See <https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process>
(setq gc-cons-threshold (* 128 1024 1024))

;; 增加emacs从进程读取的数据量
;; emacs默认为4k，但是对于language server来说太低了
;; See <https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process>
(setq read-process-output-max (* 3 1024 1024))

(add-hook 'emacs-startup-hook (lambda()
                                      (setq gc-cons-threshold(* 16 1024 1024))))



;; 标题栏(title bar) 只显示buffer
(setq frame-title-format "%b - GNU Emacs")

;; 启动设置参数，最大化启动，去除菜单栏，工具栏，双向滚动条
(modify-all-frames-parameters '((fullscreen . maximized)
			       (menu-bar-lines . 0)
			       (tool-bar-lines . 0)
			       (internal-border-width . 0)
			       ;; 子帧边框宽度
			       (child-frame-border-width . nil)
			       ;; 竖向滚动条
			       (vertical-scroll-bars . nil)
			       ;; 横向滚动条
			       (horizontal-scroll-bars . nil)))
(when (featurep 'ns)
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)
  (modify-all-frames-parameters '((ns-transparent-titlebar . t))))

;; 在early stage不要调整窗口大小
(setq frame-inhibit-implied-resize t)
;; 以像素而非字符数为单位调整窗口大小(Wayland 不支持按照字符数调整)
;; 这个设置就是为了能够确保初始化最大化的窗口能够填满整个屏幕的
(setq frame-resize-pixelwise t)
