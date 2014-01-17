# Content Retrieval Protocol

This protocol provides functions and constants necessary for retrieving content from webpages and converting them to a SEXPR-based document object model (DOM). It contains a wrapper over the SELENIUM library which allows for "processing" page source (i.e. evaluating the Javascript dynamic elements of the page), as well as unifying the treatment of XML with HTML.

## *External Special Variable* `*default-browser*`

### Description

Designates default browser used by Selenium in `get-processed-source` and `get-processed-dom`. Currently set to Firefox. See Selenium documentation for list of available browsers.

## *External Function* `get-raw-source`

### Syntax

**get-raw-source** *uri* => *source*

### Arguments & Values

*uri* -- String designating the path from which to retrieve the page source.

*source* -- String containing HTML source of page.

### Description

Returns the unprocessed (i.e. Javascript unevaluated) source of a page at the given URI path.

### Example

TODO

## *External Function* `get-processed-source`

### Syntax

**get-processed-source** *uri* &key *browser* => *source*

### Arguments & Values

*uri* -- String designating the path from which to retrieve the page source

*browser* -- String indicating which browser to use with Selenium. Set to `*default-browser*` by default.

*source* -- String containing HTML source of page.

### Description

`get-processed-source` uses Selenium in order to retrieve the processed (i.e. Javascript evaluated) source of a page at given URI path. A Selenium server must be running already in order to execute this function, otherwise a condition will be raised.

### Example

TODO

## *External Function* `source->dom`

### Syntax

**source->dom** *source* => *dom*

### Arguments & Values

*source* -- String containing HTML source.

*dom* -- A SEXPR based DOM of the given source.

### Description

Converts HTML to native Lisp SEXPR format as DOM so that it can be manipulated by library.

### Example

TODO

## *External Function* `get-raw-dom`

### Syntax

**get-raw-dom** *uri* => *dom*

### Arguments & Values

*uri* -- String of path to page whose source is to be converted into SEXPR-based DOM.

*dom* -- SEXPR-based DOM of page source.

### Description

Equivilent to `(source->dom (get-raw-source ...))`.

### Example

TODO

## *External Function* `get-processed-dom`

### Syntax

**get-processed-dom** *uri* &key *browser* => *dom*

### Arguments & Values

*uri* -- String of path to page whose source is to be converted into SEXPR-based DOM.

*browser* -- Browser to be used by Selenium. Set to `*default-broswer*` by default.

*dom* -- SEXPR-based DOM of page source.

### Description

Equivilent to `(source->dom (get-processed-dom ...))`

### Example 

TODO

## *External Function* `xml->dom`

### Syntax

**xml->dom** *xml* => *dom*

### Arguments & Values

*xml* -- String containing XML source to be converted to SEXPR-based DOM.

*dom* -- SEXPR-based DOM of XML source.

### Description

XML equivilent of `source->dom`. 

### Example

TODO

## *External Function* `get-xml-dom`

### Syntax

**get-xml-dom** *uri* => *dom*

### Arguments & Values

*uri* -- String of path to XML web-page to be converted to SEXPR-based DOM.

*dom* -- SEXPR-based DOM of XML source.

### Description

XML equivilent of `get-raw-source`.

### Example

TODO
