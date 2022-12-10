# GDB Eterm pretty printer

## Contents
*Background
*EtermMIPrettyPrinter.py
  -VS-Code launch.json
  -GDB MI patch
*EtermCliPrettyPrinter.py

## Background
These python modules implement pretty printers for Erlang Terms (Eterm)
Some functionality from LLDB pretty printer in etp.py and etp-commands
has been reimplemted with GDB python library constricts in to etp_gdb_lib.py.
There is a pretty printer for IDEs that support Machine Interface communication
with GDB, and a pretty printer for CLI instances of GDB. Under the hood these
printers are doing the same thing using the etp_gdb_lib.py module, only formatting
differs.

## EtermMIPrettyPrinter.py
EtermMIPrettyPrinter.py can be loaded in an IDE that supports the Machine interface
api that GDB speaks.

### VS-Code launch.json
In VSCode you can enable this pretty printer with the following launch.json configuration:
`erts/etc/unix/etp_gdb_pp/launch.json`

### GDB MI patch
Note: For this pretty printer to work you will need to apply this patch to GDB and build from source:
    [gdb-project](https://sourceware.org/gdb/)
    [gdb-patch](https://sourceware.org/bugzilla/attachment.cgi?id=10068)


### EtermCliPrettyPrinter.py
EtermCliPrettyPrinter.py can be loaded in a Cli instance of gdb, run:
`source $OTPROOT/erts/etc/unix/etp_gdb_pp/EtermCliPrettyPrinter.py`
Know the pretty printer should be activated, and whenever an Eterm is
being printed, the pretty printer will be called, e.g. `print *arg`.