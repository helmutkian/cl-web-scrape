# Content Retrieval Protocol

## *Special Variable* `*default-browser*`

### Description

Designates default browser used by Selenium in `get-processed-source` and `get-processed-dom`. Currently set to Firefox. See Selenium documentation for list of available browsers.

## *Function* `get-raw-source`

### Syntax

**get-raw-source** *uri* => *source*

### Arguments & Values

*uri* -- String designating the path from which to retrieve the page source.

*source* -- String containing HTML source of page.

### Description

Returns the unprocessed (i.e. Javascript unevaluated) source of a page at the given URI path.

### Example

TODO

## *Function* `get-processed-source`

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
