# orca

Orca, a Guile (https://www.gnu.org/s/guile/) library that aims to provide
Remote Procedure Call (RPC) capabilities using Message Passing Interface (MPI).

## Getting Started

### Dependencies

* libmpich12
* Guile (2.2, 2.4, 3.0)
* GNU make tools

### Installing

* ./bootstrap
* ./configure
* sudo make install

### Using library

* Code to gather computation results from all processes:
```
(use-modules (orca))

(define (process-id-message) (format #f "process ~a" (rpc-worker-process-id)))

(rpc-start)

(format #t "I'm master process. Received ~s ~%" (rpc-make '(process-id-message)))

(rpc-finalize)
```

* Command to run program using 4 processes:
```
mpirun -n 4 ./rpc-print-process-ids.scm
```

* Output of the command:
```
I'm master process. Received ("process 0" "process 1" "process 2" "process 3")
```

See `examples` directory for more examples.

## Authors

- Ahmet Artu Yildirim

## Version History

* 0.1
    * Initial Release

## License

This project is licensed under the GNU Lesser General Public License - see
the COPYING.LESSER file for details.

Please send comments on orca to **ahmetartu at gmail dot com**

Copyright (C) 2020  Ahmet Artu Yildirim
