elibphonenumber
===============

Erlang port of [libphonenumber](https://github.com/googlei18n/libphonenumber).

##Note

- This project is active. I'm doing updates very offen because I see the original author is no longer maintaining it.
- In case you want to discuss about the project you can find me on [WowApp][1]

## Compile

You need compile `libphonenumber` and install it before compiling the project.

You can use [this script][2] to install libphonenumber on ubuntu.

## Run the tests

```sh
rebar3 compile
rebar3 eunit
```

[1]:https://www.wowapp.com/w/silviu/Silviu-Caragea
[2]:https://github.com/silviucpp/elibphonenumber/blob/1ae8fc28722d60dde76bd723cec625ab447c5dab/build_deps.sh
