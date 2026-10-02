# Pages site

The GitHub Pages site for this repo: <https://ocha-dap.github.io/pa-rosea-support/>.

- `index.html` is the landing page, one card per product. `assets/` holds its stylesheet and
  the hero animation (HDX v2 tokens, copied from `ds-seas5-skill`).
- The drought hotspot pages are assembled at deploy time from
  `drought/ago/ago_hotspots.html` and `drought/zmb/zmb_hotspots.html` to `/ago/` and `/zmb/`.
- The Kenya flood triggers page is copied from `flood/ken/ken_khf_trigger_review.html` to `/ken/`.
- `.github/workflows/deploy-pages.yml` runs on a push to `main` that touches these files.

To add a product, put it under `pages/<name>/index.html` (or add a copy step to the
workflow) and add a card to `index.html`. Nested pages carry a small link back to this
landing page at the top.
