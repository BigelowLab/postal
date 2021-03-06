postal
================

### Installation

    remotes::install_github("BigelowLab/postal")

A very simple wrapper in R around various email applications on linux
(and possibly macos, too.) Currently supports
[`mail`](https://www.binarytides.com/linux-mail-command-examples/),
[`mutt`](http://www.mutt.org/) and
[`nail`](http://nail.sourceforge.net/). If you don’t have one of these
installed then you need to install at least one.

### Usage

All of the wrappers accept standard arguments: `to`, `subject`,
`message`, `attachment` and `verbose`. The latter just prints the
command that was issued whether or not it was successful.

One function, `sendmail()`, serves a one-stop shopping to access any of
the the three applications; it’s really the only function you need to
call. Choose your mail application using the `app` argument which
defaults to “mail”.

``` r
library(postal)
ok <- sendmail(to = c("someone@somewhere.com", "and_her_uncle@boondocks.com"),
               subject = "Ice out yet?",
               message = c("Hi,", "Has the ice on the pond melted yet?", "Love, Bubby"),
               attachment = "/path/to/a/file.pdf",
               app = "mail",
               verbose = TRUE)
```

    ## /usr/bin/mail -a /path/to/a/file.pdf -s 'Ice out yet?' someone@somewhere.com,and_her_uncle@boondocks.com < /tmp/RtmpCqNfxX/file13458629c74219
