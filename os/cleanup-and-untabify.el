(c++-mode)
(mandiant-set-c-style)
(beginning-of-buffer)
(mark)
(end-of-buffer)
(whitespace-cleanup)
(indent-region (point-min) (point-max))
; For some reason the order matters here. If I do the untabify first, the
; whitespace-cleanup seems to interfere and leave some tabs.
(untabify (point-min) (point-max))
