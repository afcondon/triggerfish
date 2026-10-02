# Triggerfish serves one bundle per page; `spago bundle` alone builds only
# bundle.js (index.html), so a change to shared code leaves the other pages
# stale. `make` rebuilds them all.
PAGES = Odonus:odonus Vetula:vetula Balistes:balistes Selene:selene Dashboard:dashboard

.PHONY: bundles
bundles:
	spago build
	spago bundle
	@for p in $(PAGES); do \
	  m=$${p%%:*}; f=$${p##*:}; \
	  spago bundle --module Triggerfish.$$m.Main --outfile public/$$f.js || exit 1; \
	done
