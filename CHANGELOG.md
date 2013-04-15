CHANGELOG
=========

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

