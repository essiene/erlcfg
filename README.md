## ErlCfg - configuration file parser ##

This project is designed to parse hierarchical configuration files.

This fork (by Serge Aleynikov) adds the following features:

1. Environment variable substitution
2. Macro substitution (macros are defined using "mustashe" notation: {{macro}})
3. Integer suffixes ('k', 'm', 'g', 'K', 'M', 'G')
4. Date and Path functions ($date{}, $path{})
5. Multi-line C-like string concatenation
6. Comma-separation of key-value pairs on the same line
7. Optional semi-column delimiter.

## Building ##

1. Linux
```
$ make
```
2. Windows

  Run "Developer Command Prompt for VSxxxx" for proper machine target (i.e. 64 or 32 bit).
```
> rebar.cmd get-deps compile
```

## Sample Config ##

```
# Comment
@schema("/path/to/schema"); #a schema directive

common {
    appserver = "www.appserver.com";

    port {
        # Valid integer suffixes:
        # 'k' = 1000x, 'm' = 1000000x,   'g' = 1000000000x
        # 'K' = 1024x, 'M' = 1024*1024x, 'G' = 1024*1024*1024x

        rest = 1K;
        ami  = 5038;
    }

    # Note that semicolons are optional:
    data {
        abc = 10

        # Strings can be broken to several lines, just like in C++:
        efg = "test1"
              " or "
              "test2"
    }

    # pairs can be separated by comma on the same line:
    xxx = 100, yyy = 101

    # Environment variables are retrived using ${Name} notation:
    home = ${HOME}
    
    # Environment variables can be nested (e.g. NEW_HOME="HOME", then
    # ${${NEW_HOME}} is the value of $HOME):
    new-home = ${${NEW_HOME}}

    # Macros use {{MacroName}} notation, they can be passed at run-time
    # as a map:
    my-key = {{MyKey}}

    # Strings can contain environment variables that can be concatenated
    key = "user:" "${USER}"

    # The following functions are supported (using strftime() C notation):
    date = $date{"Today is: %Y-%m-%d"}
    path = $path{"${HOME}/log/mylog.%Y-%m-%d.log"}
}

general {
    listen = ("192.168.5.3", "168.99.5.23");
    port = $common.port.rest;

    wait {
        short = 2;
        long = 10;
    }
}

ami {
    host = $common.appserver;
    port = $common.port.ami;
    username =  "obelisk";
    secret = "obelisk";
}

callentry {
    rttl = 5;
    qttl = 60;

    requeue {
        priority = high;
    }
}



# use case:
# Create new config object:
#
# Config = erlcfg:new(Filename).
# 
# Optionally the function takes a map of macros:
# Config = erlcfg:new(Filename, Validate, #{ 'MyKey' => <<"XYZ">> })
# 
# Get config values, including nested values:
#
# Config:get('general.listen').
# Config:get('ami.host', "localhost").
#
#
# Eventually we should also be able to set:
# Config1 = Config:set('callentry.requeue.priority', high).
```
