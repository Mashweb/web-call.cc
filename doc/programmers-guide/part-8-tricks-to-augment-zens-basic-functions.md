---
description: >-
  The capabilities of a Zen web page can be augmented through (1) the web server
  that serves Zen web pages, (2) web browser extensions (plugins), and (3) some
  fancy JavaScript and web services.
---

# Part 8: Tricks To Augment Zen's Basic Functions

## The web server that serves Zen web pages

The web server that serves Zen web pages can do such things as spawn a headless web browser with the [Selenium web driver](https://en.wikipedia.org/wiki/Selenium_%28software%29#Selenium_WebDriver) embedded in it. This web driver can be controlled by the web server to do such things as copy a web page and rebase URLs in the web page, so that relative paths are not broken.

## A web browser extension that allows web page embedding

The `<iframe>` HTML tag allows a web page to be embedded in another web page, but due to the [clickjacking](https://developer.mozilla.org/en-US/docs/Web/Security/Types_of_attacks#click-jacking) problem, most web servers send [X-Frame-Options](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options) HTTP response headers with the web pages they serve to prevent the embedding of those web pages in other web pages. Google Chrome \(and probably other web browsers\) has [an extension](https://chrome.google.com/webstore/detail/ignore-x-frame-headers/gleekbfjekiniecknbkamfmkohkpodhe) to ignore these response headers, thus allowing any web page to be embedded. This extension should be used cautiously to avoid clickjacking attacks. This [100-second video](https://youtu.be/66rioWH60Ec) shows how to install and use the Chrome extension.

