-define(RAGNO_OPTS, [
		     extract_external_domains,
		     extract_external_links,
		     {extract_regex_data, [
					   {og_description, "og:description\" content ?= ?\"(?P<A>[^\"]+)\""},
					   {og_image, "og:image\" content ?= ?\"(?P<A>[^\"]+)\""},
					   {og_title, "og:title\" content ?= ?\"(?P<A>[^\"]+)\""},
					   {og_type, "og:type\" content ?= ?\"(?P<A>[^\"]+)\""},
					   {og_url, "og:url\" content ?= ?\"(?P<A>[^\"]+)\""}
					  ]},
		     extract_samedomain_links,
		     extract_subdomains,
		     extract_tags,
		     extract_social,
		     final_links, %% useful for removing short urls
		     {remove_headers, ["etag", 
				       "content-security-policy",
				       "keep-alive", 
				       "age", 
				       "max-age"]},
		     {save_to_file, json},
		     {httpc_request_timeout, 6},
		     {pool_workers, 8}
		    ]).
