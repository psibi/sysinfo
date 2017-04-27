# sysinfo

Haskell interface for the `sysinfo` Linux system call. This can be used
to get system statistics like uptime, free memory, system load etc.

Note that the package works *only* on Linux system with kernel
version >= 2.3.23 and uses FFI calls.

## Usage

``` haskell
λ> import System.SysInfo
λ> val <- sysInfo
λ> either (\_ -> "sysinfo failed") show val

"SysInfo {uptime = 121149, loads = Loads {sloads =
[91200,80736,82592]}, totalram = 12286611456, freeram = 967655424,
sharedram = 63033344, bufferram = 838983680, totalswap = 8261726208,
freeswap = 8259276800, procs = 418, totalhigh = 0, freehigh = 0,
memUnit = 1}"
 ```
