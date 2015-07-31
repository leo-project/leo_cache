# leo_cache

## Overview

* "leo_cache" is object cache-controller, which realized layered cache with using RAM cache-server and Disc cache-server
* "leo_cache" uses the "rebar" build system. Makefile so that simply running "make" at the top level should work.
  * [rebar](https://github.com/rebar/rebar)
* "leo_cache" requires Erlang R16B03-1 or later.

## Usage in Leo Project

**leo_cache** is used in [**leo_gateway**](https://github.com/leo-project/leo_gateway) and [**leo_storage**](https://github.com/leo-project/leo_storage)
It is used to store data on RAM/Disk to keep a performance of an applicaion.

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
