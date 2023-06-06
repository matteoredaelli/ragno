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

_rel/ragno_release/bin/ragno_release foreground -run crawler crawl_domains www.libero.it www.redaelli.org

Rest interface:

curl -d domains="www.apache.org,www.libero.it" http://localhost:8080

## Extract unvisited domains

```bash
cd _rel/ragno_release/data
spark-sql -i ../../../utils/views.sql -f ../../../utils/extract_new_domains.sql
```
