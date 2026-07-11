;;; early-init.el -*- lexical-binding: t; -*-

;;; -------------------- 代理 (必须在 package 下载之前) --------------------
;; curl 会读环境变量 HTTP_PROXY；Emacs 的 url.el / package.el 不会，
;; 必须设置 url-proxy-services，否则 GUI 启动常直连，镜像返回 Forbidden。
(defun my/proxy-host-port (url)
  "从 http://host:port 或 socks5://host:port 取出 host:port。"
  (when (and url (stringp url)
             (string-match "\\`\\(?:https?\\|socks5h?\\)://\\([^/?#]+\\)" url))
    (match-string 1 url)))

(defun my/setup-url-proxy-from-env ()
  "让 package.el / url.el 使用与 shell 相同的 HTTP(S) 代理。"
  (let* ((http (or (getenv "http_proxy") (getenv "HTTP_PROXY")))
         (https (or (getenv "https_proxy") (getenv "HTTPS_PROXY") http))
         (no (or (getenv "no_proxy") (getenv "NO_PROXY")
                 "localhost,127.0.0.1,::1"))
         ;; GUI 从 Dock 启动时往往没有代理环境变量；回退到本机常见端口
         ;; （与你当前 Clash/Surge: 6152 一致）。不需要代理时删掉这一段即可。
         (http (or http "http://127.0.0.1:6152"))
         (https (or https http "http://127.0.0.1:6152"))
         (http-hp (my/proxy-host-port http))
         (https-hp (my/proxy-host-port https)))
    (when (or http-hp https-hp)
      (setq url-proxy-services
            `(,@(when http-hp `(("http" . ,http-hp)))
              ,@(when https-hp `(("https" . ,https-hp)))
              ("no_proxy" . ,no)))
      ;; 供子进程 / 其它库使用
      (setenv "http_proxy" http)
      (setenv "https_proxy" https)
      (setenv "HTTP_PROXY" http)
      (setenv "HTTPS_PROXY" https)
      (setenv "no_proxy" no)
      (setenv "NO_PROXY" no))))

(my/setup-url-proxy-from-env)

;; 关闭启动欢迎页（必须尽量早设；只写在旧 init 里换配置后会“又回来”）
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; 启动阶段抬高 GC，减少启动时频繁回收；启动后再降到对 LSP 友好的值。
;; See <https://emacs-lsp.github.io/lsp-mode/page/performance/>
(setq gc-cons-threshold (* 128 1024 1024))
;; 默认 4KB 太小，LSP JSON 推送会卡；1MB 足够（不必 64MB）
(setq read-process-output-max (* 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))



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



;;====font====

;; Maple Mono NF CN: https://font.subf.dev/en/
(defun my/apply-font-config ()
  (interactive)
  (when (display-graphic-p)

    (set-face-attribute 'default nil
			:family "Maple Mono NF CN"
			:height 140
			:weight 'bold
			:slant 'italic
			)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font t charset (font-spec :family "Maple Mono NF CN")))
    (setq face-font-rescale-alist '(("Maple Mono NF CN" . 1.0))))
  )

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/apply-font-config)
  (add-hook 'window-setup-hook #'my/apply-font-config))
