cl-web-scrape
=============

A simple web scraper in Common Lisp.

This library provides web scraping functionality used in other projects. It's not intended as a complete web scraping framework that's ready to deploy, but will instead grow as I required new functionaily.

It relies on CLOSURE-HTML and handles it's LHTML format for HTML -> SEXPR parsing.

Most of the functionality is inspired by the Python library Beautiful Soup. Its central feature is the FIND-ALL function that returns all LTHML subtrees from a given LTHML tree of a :TAG or :CLASS.

This library is released under a LLGPL license.
