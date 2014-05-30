CHANGELOG
=========

0.4.21 (May 30, 2014)
=====================

* Supported to execute dialyzer
* Able to build R17


0.4.17 (Jan 24, 2014)
=====================

* Upgrade leo_dcerl to 0.2.8
* Upgrade leo_mcerl to 0.2.10


0.4.15 (Dec 11, 2013)
=====================

* Upgrade leo_dcerl to 0.2.6
* Upgrade leo_mcerl to 0.2.8


0.4.15 (Nov 26, 2013)
=====================

* Upgrade leo_dcerl to 0.2.5
* Upgrade leo_mcerl to 0.2.7


0.4.13 (Oct 7, 2013)
=====================

* Upgrade leo_commons to 0.14.8


0.4.12 (Sep 26, 2013)
=====================

* Upgrade cache libs, which supported Erlang/OTP-R16B02


0.4.5 (May 21, 2013)
=====================

* Upgrade cache libs
    * from cherly to leo_mcerl
    * from dcerl to leo_dcerl


0.4.4 (Apr 15, 2013)
=====================

* Increased performance +15%


0.4.3 (Apr 15, 2013)
=====================

* Fixed bugs
    * Not handling and checking "process-dead" when occuring a cache-server error
        * Implemented re-launch of a process


0.4.2 (Apr 13, 2013)
=====================

* Upgrade cache-servers
    * [Cherly v0.12.7](https://github.com/leo-project/cherly)
    * [Dcerl v0.4.1](https://github.com/leo-project/dcerl)


0.4.1 (Apr 12, 2013)
=====================

* Improved
    * Output error log when crach cache server
    * Implemented test suit
* Fixed bugs
    * Not supported pattern of omitted "slash" from a path


0.4.0 (Apr 11, 2013)
=====================

* Initial import
    * Implemented cache controller
        * Realized layered cache, which is able to store objects into the RAM and Disc(SSD/HDD)
        * Realized plug-able mechanism, so easily assemble another cache-server

* Milestones
    * Implement ETS-base RAM cache-server

