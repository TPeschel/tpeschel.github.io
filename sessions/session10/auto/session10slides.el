(TeX-add-style-hook "session10slides"
 (lambda ()
    (TeX-add-symbols
     '("ALERT" 1)
     '("Alert" 1)
     '("COMMENT" 1)
     '("Comment" 1)
     '("comment" 1)
     "titleimage"
     "tanedo"
     "CMSSMDM"
     "handwriting"
     "forbold")
    (TeX-run-style-hooks
     "tikzfeynman"
     "arydshln"
     "multirow"
     "amsthm"
     "bbm"
     "slashed"
     "cancel"
     "mathrsfs"
     "graphicx"
     "amssymb"
     "amsfonts"
     "amsmath"
     "fontspec"
     "no-math"
     "gensymb"
     "lmodern"
     ""
     "fontenc"
     "T1"
     "latex2e"
     "beamer12"
     "beamer"
     "xcolor={table}"
     "12pt"
     "contentglm")))

