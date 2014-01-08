cl-web-scrape
=============

A simple web scraper in Common Lisp.

This library provides web scraping functionality used in other projects. It's not intended as a complete web scraping framework that's ready to deploy, but will instead grow as I required new functionaily. This project is highly unstable and changing quickly.

It currently depends on CLOSURE-HTML, DRAKMA, and SELENIUM--all available via quicklisp, and my own CL-COOP library.

CL-WEB-SCRAPE relies on CLOSURE-HTML uses its LHTML document object model (DOM). This however, may change, therefore abstractions for accessing attributes and properties of the components of a document are (being -- in progess) provided.

Most of the functionality is inspired by the Python library Beautiful Soup. Its central feature is the FIND-ALL function that returns all DOM subtrees from a given DOM tree based on tags, classes, or attributes.

This library is released under a LLGPL license.
