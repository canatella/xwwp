# Emacs xwidget-webkit enhancement suite

This package enhance the integrated xwidget-webkit browser with hopefully useful
functionnalities.


## Follow link

Using `xwidget-plus-follow-link` uses `ivy` to choose a link on the current web
page. It also highlight the candidates on the web page.

![Imgur](https://i.imgur.com/1KO70FE.gif)

## Switch to xwidget on browse

When opening a url using `xwidget-webkit` in an already opened session that is
not visible, the session is brought to the front.

## How to install

Sorry, no melpa as of now.

```
(use-package xwidget-plus
  :load-path "~/.emacs.d/xwidget-plus"
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwidget-plus-follow-link)))
```
