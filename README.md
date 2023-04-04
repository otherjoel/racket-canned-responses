canned
======

This is a small Racket package for producing canned responses that might otherwise
be fussy to type. Right now it includes only one such kind of response: giving notice
to a company that their SPF records are misconfigured.

The `raco canned spf` command only works on Windows for now.

## Installation

Clone the repository and install locally by running this command from inside the folder:

`raco pkg install --link`

This will install a `canned` subcommand for raco. Run `raco canned`  for a list of available
directives.

Place an `options.ini` file in the project folder that looks like this:

    # Options

    company-name: Pizza Hut
    direct-phone: 612 488 8888

## SPF Notices

Running `raco canned spf <domain> <ip4-addr>` will generate a helpful, detailed notice informing
someone that their SPF record is broken and needs fixing.
