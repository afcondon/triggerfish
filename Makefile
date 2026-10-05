# Triggerfish serves one bundle per page; `spago bundle` alone builds only
# bundle.js (index.html), so a change to shared code leaves the other pages
# stale. `make` rebuilds them all.
PAGES = Odonus:odonus Vetula:vetula Balistes:balistes Selene:selene Dashboard:dashboard

.PHONY: bundles
bundles:
	@touch .bundle-start
	spago build
	spago bundle
	@for p in $(PAGES); do \
	  m=$${p%%:*}; f=$${p##*:}; \
	  spago bundle --module Triggerfish.$$m.Main --outfile public/$$f.js || exit 1; \
	done
	@# Every bundle must have been written by THIS run: a build that quietly
	@# bundled nothing leaves the pages serving yesterday's code. The last line
	@# is the proof to look for.
	@n=0; for f in public/bundle.js $(foreach p,$(PAGES),public/$(lastword $(subst :, ,$(p))).js); do \
	  [ $$f -nt .bundle-start ] || { echo "✗ STALE: $$f was not rewritten by this build"; exit 1; }; n=$$((n+1)); \
	done; echo "✓ $$n bundles fresh"
