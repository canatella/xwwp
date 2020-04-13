# Emacs xwidget-webkit enhancement suite

This package enhance the integrated xwidget-webkit browser with hopefully useful
functionnalities.

![CI](https://github.com/canatella/xwwp/workflows/CI/badge.svg?branch=master)

## Follow link

Using `xwwp-follow-link` to choose a link on the current web
page. It also highlight the candidates on the web page.

![Imgur](https://i.imgur.com/1KO70FE.gif)

It currently supports `completing-read`, `ido`, `ivy` and `helm` completion
backend. I'm willing to add support for other completion backend, open an issue
with your backend of choice and I'll have a look at what can be done. Or better,
fork and create a pull request, most of the needed code is already there, it
just needs to be hooked.

## Browse url in other window

The `xwidget-webkit-browse-url` just update the current xwidget-webkit buffer
but does not bring it to the front (at least on MacOS), which is what you would
expect. The `xwwp-browse-url-orther-window` method will also bring it to the
front using `swith-to-buffer-other-window`.

### How to install

Sorry, no melpa as of now. Should be added quite soon.

```
(use-package xwwp-follow-link
  :load-path "~/.emacs.d/xwwp-follow-link"
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))
```

## Development

The goal of this package is to enhance the `xwidget-webkit` browser. If you have
any code or feature suggestion that you think should make it into this package,
please open an issue or better, create a pull request!
