; Use with this in your Emacs config:
;
; (reformatter-define stylish-haskell-format
;   :program "stylish-haskell")

((haskell-mode
   (mode . stylish-haskell-format-on-save)))
