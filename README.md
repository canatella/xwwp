# Emacs xwidget-webkit enhancement suite

This package enhance the integrated xwidget-webkit browser with hopefully useful
functionnalities.


## Follow link

Using `xwidget-plus-follow-link` to choose a link on the current web
page. It also highlight the candidates on the web page.

![Imgur](https://i.imgur.com/1KO70FE.gif)

It currently supports `completing-read`, `ido`, `ivy` and `helm` completion
backend. I'm willing to add support for other completion backend, open an issue
with your backend of choice and I'll have a look at what can be done. Or better,
fork and create a pull request, most of the needed code is already there, it
just needs to be hooked.

## Switch to xwidget on browse

This package provides the `xwiget-plus-browse-url` function. Unlike
`xwidget-webkit-browse-url`, when reusing an existing session, it will bring it
to the front.

## How to install

Sorry, no melpa as of now. Should be added quite soon.

```
(use-package xwidget-plus
  :load-path "~/.emacs.d/xwidget-plus"
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwidget-plus-follow-link)))
```
