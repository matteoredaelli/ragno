# ragno.erl

Ragno is a light crawler for domains. It extract some useful info about the domain

```text
  / _ \
\_\(_)/_/
 _//o\\_ Ragno
  /   \
```

## Usage

From the command line

_rel/ragno_release/bin/ragno_release foreground -run crawler crawl_domains https://www.libero.it/ https://www.redaelli.org

Rest interface:

curl  -d urls="https://www.apache.org/,https://www.libero.it/" http://localhost:8080
