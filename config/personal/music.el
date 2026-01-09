;; Basic EMMS configuration
(use-package emms
  :straight t
  :defer t
  :config
  ;; Initialize EMMS with default players
  (emms-all)
  (emms-default-players)

  ;; Use defcustom to allow customization of the music directory
  (defcustom fg-emms-music-dir
    (if (file-exists-p "~/Music/") "~/Music/" "~/音乐/")
    "The default directory for music files in EMMS."
    :type 'directory)

  ;; Set the music directory
  (setq emms-source-file-default-directory fg-emms-music-dir)

  ;; Enable mode line display of the current song when playing
  (add-hook 'emms-player-started-hook (lambda () (emms-mode-line 1)))
  (add-hook 'emms-player-started-hook (lambda () (emms-playing-time 1)))

  ;; Set the cache file location
  (setq emms-cache-file (concat user-emacs-directory "emms/emms-cache"))

  ;; Improved track description function
  (defun fg-emms-track-description (track)
    "Return a somewhat nice track description."
    (let ((artist (or (emms-track-get track 'info-artist) "Unknown artist"))
          (year (or (emms-track-get track 'info-year) "XXXX"))
          (album (or (emms-track-get track 'info-album) "Unknown album"))
          (tracknumber (or (emms-track-get track 'info-tracknumber) "XX"))
          (title (or (emms-track-get track 'info-title) "Unknown title")))
      (format "%s - %s - %s - %s - %s"
              artist year album (if (stringp tracknumber) (format "%02d" (string-to-number tracknumber)) "XX") title)))

  (setq emms-track-description-function 'fg-emms-track-description))

;; Keybindings for basic control
(global-set-key (kbd "C-c e p") 'emms-pause)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e b") 'emms-previous)
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e +") 'emms-volume-raise)
(global-set-key (kbd "C-c e -") 'emms-volume-lower)

