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

# The static sites at trigger-fish.app (docs/kb/plans/vetula-one-surface.md):
# one per app that makes music with no rig, staged beside the apex site in
# cloudflare-sites, each its page as index.html, its bundle minified, the
# widgets' stylesheet and the icon. Published with `quartermaster publish`
# from cloudflare-sites/triggerfish-<app>/compose.yml.
SITES = ../../../cloudflare-sites
STATIC = Vetula:vetula Odonus:odonus

.PHONY: static
static:
	spago build
	@for p in $(STATIC); do \
	  m=$${p%%:*}; f=$${p##*:}; d=$(SITES)/triggerfish-$$f; \
	  mkdir -p $$d; \
	  spago bundle --module Triggerfish.$$m.Main --minify --outfile $$d/$$f.js || exit 1; \
	  cp public/$$f.html $$d/index.html; \
	  cp public/halogen-widgets.css public/favicon.svg $$d/; \
	  echo "✓ $$d"; \
	done
	@# the apex, trigger-fish.app: the Gazette, its two playable machines
	@# pointed at their sites and the rest at the installed rig
	@a=$(SITES)/triggerfish; mkdir -p $$a/img; \
	  sed -e 's#href="vetula.html"#href="https://vetula.trigger-fish.app"#g' \
	      -e 's#href="odonus.html"#href="https://odonus.trigger-fish.app"#g' \
	      -e 's#href="\(balistes.html\|selene.html\|dashboard.html\|/limulus/\|/conspicillum/\|/quadrat.html\)"#href="index.html\#installed"#g' \
	      public/about.html > $$a/gazette.html; \
	  cp public/img/verrill-architeuthis-1882.webp public/img/CREDITS.md $$a/img/; \
	  cp public/favicon.svg $$a/; \
	  echo "✓ $$a/gazette.html"
