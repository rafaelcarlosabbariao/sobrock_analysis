"""Builds sobrock_findings.html from the template and the two JSON files (aggregates only)."""
import json
t = open("sobrock_findings.html.tmpl").read()
t = t.replace("__DATA__", json.dumps(json.load(open("findings.json")), separators=(",", ":")))
t = t.replace("__TJ__", json.dumps(json.load(open("tom_and_jerry.json")), separators=(",", ":")))
open("sobrock_findings.html", "w").write(t)
