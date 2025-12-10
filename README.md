# leo_cache

## Overview

**leo_cache** is an object cache-controller which provides layered caching using RAM cache-server and Disc cache-server.

## Requirements

- Erlang/OTP 22 or later (tested up to OTP 28)
- rebar3

## Build

```bash
$ rebar3 compile
```

## Test

```bash
$ rebar3 eunit
```

## Usage

```bash
$ make all
```

This runs compile, xref, and eunit tests.

## Dependencies

- [leo_mcerl](https://github.com/leo-project/leo_mcerl) - Memory cache library
- [leo_dcerl](https://github.com/leo-project/leo_dcerl) - Disc cache library
- [leo_tran](https://github.com/leo-project/leo_tran) - Transaction manager

## Usage in Leo Project

**leo_cache** is used in [**leo_gateway**](https://github.com/leo-project/apps/leo_gateway) and [**leo_storage**](https://github.com/leo-project/apps/leo_storage).
It is used to store data on RAM/Disk to maintain application performance.

## License

Apache License, Version 2.0

## Sponsors

- LeoProject/LeoFS is sponsored by [Lions Data, Inc.](https://lions-data.com/) from Jan of 2019.
- LeoProject/LeoFS was sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) from 2012 to Dec of 2018.

