"""Build flood/hoa/hoa_flood_timing.html: template.html with data_dir/out/flood_timing.json embedded.

Run: python flood/hoa/build_page.py [data_dir]
The page text, the 2026 report log (ReliefWeb, hand-picked) and the dates in the prose are in
template.html and are written for the 9 November 2026 data; revise them when the data is refreshed.
"""
import json
import sys
from pathlib import Path

DATA = Path(sys.argv[1] if len(sys.argv) > 1 else "data/hoa")
HERE = Path(__file__).parent

page = json.loads((DATA / "out" / "flood_timing.json").read_text(encoding="utf-8"))
tpl = (HERE / "template.html").read_text(encoding="utf-8")
assert tpl.count("/*DATA*/") == 1
html = tpl.replace("/*DATA*/", json.dumps(page, separators=(",", ":"), ensure_ascii=False).replace("</", "<\\/"))
out = HERE / "hoa_flood_timing.html"
with open(out, "w", encoding="utf-8", newline="\n") as fh:
    fh.write(html)
print("wrote", out, f"{len(html) / 1024:.0f} KB")
