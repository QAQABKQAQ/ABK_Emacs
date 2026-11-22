





;; 显然，这是个注释
;; 不过这个emacs的一些东西真的很垃圾
(setq inhibit-startup-screen t) ;; 关闭(inbibit) 开始的欢迎页面

(setq frame-title-format "%f") 

;; 关闭菜单栏与工具栏
(menu-bar-mode -1) 
(tool-bar-mode -1)


;; vpn config
(setq url-proxy-services
      '(("http" . "127.0.0.1:6152")
	("https". "127.0.0.1:6152")))
(setenv "http_proxy"  "http://127.0.0.1:6152")
(setenv "https_proxy" "http://127.0.0.1:6152")

;; 启动时提高垃圾回收阈值，减少启动期间的回收次数
(setq gc-cons-threshold (* 128 1024 1024))

;; 每次读取子进程输出最多 1 MiB，改善 LSP 通信效率
(setq read-process-output-max (* 1024 1024))

;; 启动结束后，把垃圾回收阈值调整为 100 MiB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))

;; 设置字体
;; Maple Mono NF CN: https://font.subf.dev/
(defun my/apply-font-config ()
  "设置图形 Emacs 的字体。"
  (when (display-graphic-p)
    (set-face-attribute 'default nil
                        :family "Maple Mono NF CN"
                        :height 140
                        :weight 'bold
                        :slant 'italic)

    ;; 中文等字符也使用同款字体
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       t charset
       (font-spec :family "Maple Mono NF CN")))))

;; 普通启动时设置；daemon 模式下在创建窗口后设置
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/apply-font-config)
  (add-hook 'window-setup-hook #'my/apply-font-config))
